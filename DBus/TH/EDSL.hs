{-# LANGUAGE TemplateHaskell, TypeOperators, DeriveDataTypeable #-}

module DBus.TH.EDSL
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
   function, interface,
   function', interface'
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
data Signature = Return Name    -- ^ Call returning a single value.
               | Returns [Name] -- ^ Call returning multiple values.
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
nArgs (Returns _) = 0
nArgs (_ :-> s)  = 1 + nArgs s

firstLower :: String -> String
firstLower [] = []
firstLower (x:xs) = toLower x: xs

-- | Return type name for signature
signatureResult :: Signature -> [Name]
signatureResult (Return name) = [name]
signatureResult (Returns ns) = ns
signatureResult (_ :-> sig) = signatureResult sig

-- | Generate bindings for methods in specific DBus interface.
-- If second argument is (Just prefix), then prefix will be
-- added to the beginning of all DBus names and removed from all
-- Haskell names.
interface :: String       -- ^ Bus name
          -> String       -- ^ Object name
          -> String       -- ^ Interface name
          -> Maybe String -- ^ Prefix
          -> [Function]   -- ^ List of functions
          -> Q [Dec]
interface busName objectName ifaceName mbPrefix fns =
    interface' busName (Just objectName) ifaceName mbPrefix fns

-- | Generate bindings for methods in specific DBus interface.
-- If second argument is (Just prefix), then prefix will be
-- added to the beginning of all DBus names and removed from all
-- Haskell names.
interface' :: String      -- ^ Bus name
          -> Maybe String -- ^ Just name - use fixed object name; Nothing - object name will be 2nd argument of generated functions
          -> String       -- ^ Interface name
          -> Maybe String -- ^ Prefix
          -> [Function]   -- ^ List of functions
          -> Q [Dec]
interface' busName mbObjectName ifaceName mbPrefix fns =
    concat `fmap` mapM (function' busName mbObjectName ifaceName mbPrefix) fns

-- | Generate binding for one method in specific DBus interface.
-- If second argument is (Just prefix), then prefix will be
-- added to the beginning of all DBus names and removed from all
-- Haskell names.
function :: String       -- ^ Bus name
          -> String       -- ^ Object name
          -> String       -- ^ Interface name
          -> Maybe String -- ^ Prefix
          -> Function   -- ^ Function
          -> Q [Dec]
function busName objectName ifaceName mbPrefix fn =
    function' busName (Just objectName) ifaceName mbPrefix fn

-- | Generate binding for one method in specific DBus interface.
-- If second argument is (Just prefix), then prefix will be
-- added to the beginning of all DBus names and removed from all
-- Haskell names.
function' :: String       -- ^ Bus name
          -> Maybe String -- ^ Just name - use fixed object name; Nothing - object name will be 2nd argument of generated function
          -> String       -- ^ Interface name
          -> Maybe String -- ^ Prefix
          -> Function     -- ^ Function
          -> Q [Dec]
function' busName mbObjectName ifaceName mbPrefix (Function name dbusName sig) =
    let name'     = strip name
        dbusName' = addPrefix dbusName
    in sequence [generateSignature name' sig,
                 generateImplementation name' dbusName' sig]
  where
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
    dbusType t =
      case mbObjectName of
        Just _ -> [t| Client -> $(return t) |]
        Nothing -> [t| Client -> String -> $(return t) |]

    transformType :: Signature -> Type
    transformType (Return t) =
      if t == ''()
        then AppT (ConT ''IO) (ConT ''())
        else AppT (ConT ''IO) (AppT (ConT ''Maybe) (ConT t))
    transformType (t :-> s)  = AppT (AppT ArrowT (ConT t)) (transformType s)
    transformType (Returns ts) = AppT (ConT ''IO) (AppT (ConT ''Maybe) (toType ts))
      where
        toType :: [Name] -> Type
        toType [] = ConT ''()
        toType ts = mkTuple (length ts) ts
        mkTuple size [t] = AppT (TupleT size) (ConT t)
        mkTuple size ts = AppT (mkTuple size $ init ts) (ConT $ last ts)

    generateImplementation :: String -> String -> Signature -> Q Dec
    generateImplementation name dbusName sig = do
        let bus  = mkName "bus"
        objectName <- newName "object"
        args <- replicateM (nArgs sig) (newName "x")
        body <- generateBody dbusName objectName sig args
        let varArgs = case mbObjectName of
                        Just _ -> VarP bus : map VarP args
                        Nothing -> VarP bus : VarP objectName : map VarP args
        return $ FunD (mkName $ firstLower name) [Clause varArgs (NormalB body) []]

    generateReturn :: [Name] -> Q Exp
    generateReturn [t] =
      if t == ''()
        then [| () |]
        else [| fromVariant (head (methodReturnBody res)) |]
    generateReturn ts = [| toTuple (methodReturnBody res) >>= fromVariant|]

    generateBody :: String -> Name -> Signature -> [Name] -> Q Exp
    generateBody name objectName sig names = do
        [| do
           let baseMethod = methodCall (objectPath_ $(case mbObjectName of
                                                        Just oname -> litE (StringL oname)
                                                        Nothing -> varE objectName
                                                      ))
                                       (interfaceName_ ifaceName)
                                       (memberName_ name)
               method = baseMethod {
                          methodCallDestination = Just (busName_ busName),
                          methodCallBody = $(variant names)
                        }
           res <- call_ $(varE $ mkName "bus") method
           pure $(returnValue $ signatureResult sig)
          |]
        where
          returnValue :: [Name] -> Q Exp
          returnValue [t]
            | t == ''() = [| () |]
            | otherwise = [| fromVariant (head (methodReturnBody res)) |]
          returnValue ts  = do
            patVars <- traverse newName $ ("a" ++) . show <$> [1..length ts]
            bindVars <- traverse newName $ ("m" ++) . show <$> [1..length ts]
            fn <- newName "parseShape"
            let body :: Quote m => m Body
                body = normalB $ doE
                  (zipWith bindFromVariant bindVars patVars
                  ++ [ mkReturn ])
                bindFromVariant var pat = bindS (varP var) (appE [|fromVariant|] (varE pat))
                mkReturn :: Quote m => m Stmt
                mkReturn = noBindS $ appE [|pure|] (tupE $ varE <$> bindVars)
                clause1 = clause [listP (varP <$> patVars)] body []
                clause2 = clause [wildP] (normalB [| Nothing |]) []
            letE [funD fn [clause1, clause2]] [| $(varE fn) (methodReturnBody res) |]

    variant :: [Name] -> Q Exp
    variant names = do
      exs <- mapM variant1 names
      return $ ListE exs

    variant1 :: Name -> Q Exp
    variant1 name = [| toVariant $(varE name) |]

