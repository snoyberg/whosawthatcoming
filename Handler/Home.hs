{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import System.Random (getStdRandom, randomR, StdGen)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Data.Time (getCurrentTime)
import Crypto.Hash.SHA512 (hash)
import qualified Data.ByteString.Base64 as B64

predictForm :: Form Prediction
predictForm token = do
    (nameRes, nameView) <- mreq textField "Name"
        { fsAttrs = [("placeholder", "your name"), ("class", "name")]
        } Nothing
    (contentRes, contentView) <- mreq textareaField "Content" Nothing
    (errs, res) <-
        case (,) <$> nameRes <*> contentRes of
            FormSuccess (name, content) -> do
                let go :: State StdGen Text
                    go = T.pack <$> replicateM 10 goC
                    goC :: State StdGen Char
                    goC = state $ randomR ('A', 'Z')
                (public, private) <- liftIO
                                   $ getStdRandom
                                   $ runState
                                   $ (,) <$> go <*> go
                now <- liftIO getCurrentTime
                return ([], FormSuccess $ Prediction name content public private now)
            FormFailure errs -> return (errs, FormFailure errs)
            FormMissing -> return ([], FormMissing)
    let widget = [whamlet|
$if not $ null errs
    <p>There were some errors with your submission:
    <ul>
        $forall err <- errs
            <li>#{err}
<div .row>
    <div .span6 .offset2>
        <form method=post>
            <p>I, ^{fvInput nameView}, predict that:
            <p>
                ^{fvInput contentView}
            <p>
                \#{token}
                <input .btn type=submit value="Make my prediction">
|]
    return (res, widget)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    ((result, formWidget), _) <- runFormPost predictForm
    case result of
        FormSuccess predict -> do
            _ <- runDB $ insert predict
            setMessage "Your prediction has been made"
            redirect $ PrivateR $ predictionPrivate predict
        _ -> defaultLayout $ do
            setTitle "I predict that..."
            $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = getHomeR

getPrivateR :: Text -> Handler RepHtml
getPrivateR private = do
    Entity _ predict <- runDB $ getBy404 $ UniquePrivate private
    defaultLayout $ do
        setTitle "Private prediction page"
        $(widgetFile "private")

getPublicR :: Text -> Handler RepHtml
getPublicR public = do
    Entity _ predict <- runDB $ getBy404 $ UniquePublic public
    let Textarea raw = predictionContent predict
    let sha512 = decodeUtf8 $ B64.encode $ hash $ encodeUtf8 raw
    defaultLayout $ do
        setTitle "Public prediction page"
        $(widgetFile "public")
