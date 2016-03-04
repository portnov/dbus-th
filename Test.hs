{-# LANGUAGE TemplateHaskell, TypeOperators, OverloadedStrings #-}

import Control.Monad
import qualified Data.Map as M
import DBus.TH
import System.Environment

type Ints = [Int32]

type Buddies = M.Map String Int32

interface' "im.pidgin.purple.PurpleService" Nothing "im.pidgin.purple.PurpleInterface" (Just "Purple")
    [ "Strreplace" =:: ''String :-> ''String :-> ''String :-> Return ''String `as` "replace"
    , "AccountsFind" =:: ''String :-> ''String :-> Return ''Int32
    , "AccountsGetAllActive" =:: Return ''Ints
    , "FindBuddy" =:: ''Int32 :-> ''String :-> Return ''Int32
    , "BuddyGetGroup" =:: ''Int32 :-> Return ''Int32
    , "GroupGetName"  =:: ''Int32 :-> Return ''String
    , "BlistGetBuddies" =:: Return ''Ints
    , "BuddyGetAlias" =:: ''Int32 :-> Return ''String ]

main = do
  [account, buddy] <- getArgs
  let obj = "/im/pidgin/purple/PurpleObject" 
  dbus <- connectSession
  Just res <- replace dbus obj "ab12cc 12 ee" "12" "ZZ"
  putStrLn res
  Just acc <- accountsFind dbus obj account "prpl-jabber"
  print acc
  Just buddiesL <- blistGetBuddies dbus obj
  buddies <- forM buddiesL $ \buddy -> do
                    Just name <- buddyGetAlias dbus obj buddy
                    return (name, buddy)
  let buddiesMap = M.fromList buddies
  let Just juick = M.lookup buddy buddiesMap
  Just grp <- buddyGetGroup dbus obj juick
  Just grpName <- groupGetName dbus obj grp
  putStrLn $ buddy ++ "'s group: " ++ grpName

