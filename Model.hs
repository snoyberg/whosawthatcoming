{-# LANGUAGE DeriveDataTypeable #-}
module Model where

import Prelude
import Yesod
import Data.Text (Text, append)
import qualified Data.Text as T
import Data.Char (isUpper)
import Database.Persist.Quasi
import Data.Time (UTCTime)
import Data.Typeable (Typeable)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings
        { psToDBName = \t ->
            if not (T.null t) && isUpper (T.head t)
                then "WhoSawThatComing__" `append` psToDBName upperCaseSettings t
                else psToDBName upperCaseSettings t
        } "config/models")
