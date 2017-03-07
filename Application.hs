{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
#if DEVELOPMENT
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
#else
import Network.Wai.Middleware.RequestLogger (logStdout)
#endif
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Client.TLS (getGlobalManager)
import Control.Monad.Logger (runStdoutLoggingT)
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Gzip
import Data.Aeson (withObject)
import Data.Yaml (decodeFileEither)
import Control.Exception (throwIO)

-- Import all relevant handler modules here.
import Handler.Home

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    return $ gzip def $ autohead $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logStdoutDev
#else
    logWare = logStdout
#endif

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- getGlobalManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/db/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConfig)
    runStdoutLoggingT $ Database.Persist.runPool dbconf (runMigration migrateAll) p
    googleEmail <- decodeFileEither "config/db/google-email.yml" >>= either throwIO return
    return $ App conf s p manager dbconf (geClient googleEmail, geSecret googleEmail)

data GoogleEmail = GoogleEmail
    { geClient :: !Text
    , geSecret :: !Text
    }
instance FromJSON GoogleEmail where
  parseJSON = withObject "GoogleEmail" $ \o -> GoogleEmail
    <$> o .: "client-id"
    <*> o .: "client-secret"

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
