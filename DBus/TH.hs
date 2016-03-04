{-# LANGUAGE TemplateHaskell, TypeOperators, DeriveDataTypeable #-}

module DBus.TH
  (module Data.Int,
   module Data.Word,
   Client, BusName,
   ObjectPath, InterfaceName,
   MemberName, Variant,
   connectSession, connectSystem,
   Signature (..),
   signatureResult,
   Function (..),
   (=::), as,
   interface
  ) where

import Control.Monad
import Data.Int
import Data.Word
import Language.Haskell.TH
import qualified Data.Text as Text
import Data.Char
import Data.List
import Data.Generics
import DBus hiding (Type, Signature)
import DBus.Client hiding (Type, Signature)

-- | Function signature
data Signature = Return Name
               | Name :-> Signature
  deriving (Eq, Show, Data, Typeable)

infixr 6 :->

-- | Function with DBus name and Haskell name
data Function = Function {
    fnName      :: String    -- ^ Function name to use in Haskell
  , fnDBusName  :: String    -- ^ Function name to use in DBus
  , fnSignature :: Signature -- ^ Function signature
    }
  deriving (Eq, Show, Data, Typeable)

-- | Create a Function from it's name and Signature.
-- Sets fnDBusName == fnName.
(=::) :: String -> Signature -> Function
name =:: sig = Function name name sig

infixr 5 =::

-- | Set specific Haskell name for Function.
as :: Function -> String -> Function
fn `as` name = fn {fnName = name}

infixl 4 `as`

nArgs :: Signature -> Int
nArgs (Return _) = 0
nArgs (_ :-> s)  = 1 + nArgs s

firstLower :: String -> String
firstLower [] = []
firstLower (x:xs) = toLower x: xs

signatureResult :: Signature -> Name
signatureResult (Return name) = name
signatureResult (_ :-> sig) = signatureResult sig

-- | Generate bindings for methods in specific DBus interface.
-- If second argument is (Just prefix), then prefix will be
-- added to the beginning of all DBus names and removed from all
-- Haskell names.
interface :: String       -- ^ Bus name
          -> String       -- ^ Interface name
          -> String       -- ^ Object name
          -> Maybe String -- ^ Prefix
          -> [Function]   -- ^ List of functions
          -> Q [Dec]
interface busName objectName ifaceName mbPrefix fns = concat `fmap` mapM iface fns
  where
    iface :: Function -> Q [Dec]
    iface (Function name dbusName sig) =
        let name'     = strip name
            dbusName' = addPrefix dbusName
        in sequence [generateSignature name' sig,
                     generateImplementation name' dbusName' sig]

    addPrefix :: String -> String
    addPrefix s =
      case mbPrefix of
        Nothing     -> s
        Just prefix -> prefix ++ s

    strip :: String -> String
    strip s =
      case mbPrefix of
        Nothing     -> s
        Just prefix -> if prefix `isPrefixOf` s
                         then drop (length prefix) s
                         else s

    generateSignature :: String -> Signature -> Q Dec
    generateSignature name sig = do
        dbt <- dbusType (transformType sig)
        return $ SigD (mkName $ firstLower name) dbt

    dbusType :: Type -> Q Type
    dbusType t = [t| Client -> $(return t) |]

    transformType :: Signature -> Type
    transformType (Return t) =
      if t == ''()
        then AppT (ConT ''IO) (ConT ''())
        else AppT (ConT ''IO) (AppT (ConT ''Maybe) (ConT t))
    transformType (t :-> s)  = AppT (AppT ArrowT (ConT t)) (transformType s)

    generateImplementation :: String -> String -> Signature -> Q Dec
    generateImplementation name dbusName sig = do
        let bus  = mkName "bus"
        args <- replicateM (nArgs sig) (newName "x")
        body <- generateBody dbusName sig args
        return $ FunD (mkName $ firstLower name) [Clause (VarP bus: map VarP args) (NormalB body) []]

    generateBody :: String -> Signature -> [Name] -> Q Exp
    generateBody name sig names = do
        [| do
           let baseMethod = methodCall (objectPath_ objectName)
                                       (interfaceName_ ifaceName)
                                       (memberName_ name)
               method = baseMethod {
                          methodCallDestination = Just (busName_ busName),
                          methodCallBody = $(variant names)
                        }
                      
           res <- call_ $(varE $ mkName "bus") method
           $(if signatureResult sig /= ''() 
             then [| return $ fromVariant (methodReturnBody res !! 0) |]
             else [| return () |]
            )
          |]

    variant :: [Name] -> Q Exp
    variant names = do
      exs <- mapM variant1 names
      return $ ListE exs

    variant1 :: Name -> Q Exp
    variant1 name = [| toVariant $(varE name) |]

