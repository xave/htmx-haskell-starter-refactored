{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.UTF8 as BU

import qualified Data.Text as T
import Data.Time

import Data.Either
import Database
import Htmx.Event (HtmxEvent (..))
import Htmx.Lucid.Core (OnEvent (..), hxGet_, hxOn_, hxPost_, hxPushUrl_, hxSwapOob_, hxSwap_, hxTarget_, hxTrigger_)
import Htmx.Lucid.Extra (hxConfirm_, hxDelete_, hxPut_)
import Lucid
import Network.HTTP.Types
    ( Status (statusCode, statusMessage)
    )

import Network.Wai
    ( Application
    , Middleware
    , pathInfo
    , rawPathInfo
    , requestMethod
    , responseStatus
    )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse
    ( lbsBackEnd
    , parseRequestBody
    )
import Routes
import Text.Printf (printf)
import Todo

main :: IO ()
main = do
    setupDB
    putStrLn $ "Listening on http://localhost:" ++ show port
    -- putStrLn $ TL.unpack $ renderText form
    run port $ midLog app -- run :: Port -> Application -> IO ()
  where
    port = 3000

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application
app req respond =
    do
        (_params, _) <- parseRequestBody lbsBackEnd req
        _res <- case mepinf of
            ("DELETE", ["deltodo", deletionId]) -> delTodo $ T.unpack deletionId
            -- ("GET", ["todos"]) -> getTodos
            -- ("POST", ["form"]) -> postForm params
            _nonIO -> return $ case mepinf of
                -- ("GET", []) -> resHtmlFile "app/form.html"
                ("GET", []) -> responseByteString form
                ("GET", ["htmx"]) -> resJs "app/htmx.min.js"
                _undefinedPath -> errorPage $ rawPath ++ " is undefined" where rawPath = toString $ rawPathInfo req
        -- respond res
        routes <- fromRight (return $ errorResponseByteString $ (" woops #404#\n" :: Html ())) $ myRouteWai req myRoutes

        respond (routes)
  where
    pinf = pathInfo req
    meth = requestMethod req
    mepinf = (meth, pinf)

midLog :: Middleware
midLog midApp req res = do
    start <- getCurrentTime
    let
        method = BU.toString $ requestMethod req
        path = BU.toString $ rawPathInfo req
    let res' h = do
            end <- getCurrentTime
            let
                diff = show $ diffUTCTime start end
                status = responseStatus h
                code = statusCode status
            let message = BU.toString $ statusMessage status
            printf "%d %s %s %s %s \n" code method path diff message
            res h
    midApp req res'

useHtmx :: (Monad m) => HtmlT m ()
useHtmx = script_ [defer_ "", src_ htmxSrcPath] ("" :: Html ())
  where
    htmxSrcPath = "/htmx"

form :: Html ()
form =
    doctypehtml_ $ do
        head_ $
            do
                title_
                    "goblins"
                -- Note: For simplicity, you have to write out the entire src= part for scripts. Inconvenient you say? Why are you trying to use so much JS conveniently...?
                -- script_ "/htmx"
                -- script_ [defer_ "", src_ "/htmx"] ("" :: Html ())
                useHtmx
        body_ $
            do
                form_
                    [hxPost_ "/form", hxTarget_ "#todos", hxSwap_ "beforeend", hxOn_ (HtmxOnEvent AfterRequest) "this.reset()"]
                    $ do
                        input_ [type_ "text", name_ "msg"]
                        input_ [type_ "submit", value_ "Add Todo"]
                div_ [hxGet_ "/todos", hxTrigger_ "load", hxSwap_ "outerHTML"] ""
                div_ [hxPost_ "/mouse_entered", hxTrigger_ "mouseenter"] "[Here Mouse, Mouse!]"

                template_
                    $ do
                        tr_ [id_ "message", hxSwapOob_ "true"]
                    $ do
                        td_ "Joe"
                        td_ "Smith"
                        ""

                -- I added this randomly as html exploration
                button_
                    [ hxDelete_ "/account"
                    , hxConfirm_ "Are you sure you wish to delete your account?"
                    ]
                    "Delete My Account"
                div_ [hxConfirm_ "Are you sure?"] $
                    do
                        button_ [hxDelete_ "/account"] "Delete My Account"
                        button_ [hxPut_ "/account"] "Update My Account"
                        button_ [hxConfirm_ "unset", hxPushUrl_ "true", hxGet_ "/fake_path_FOO"] "Cancel"
                        ""
