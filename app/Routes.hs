{-# LANGUAGE OverloadedStrings #-}

module Routes
    ( myRouteWai
    , myRoutes
    , responseByteString
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T

import Htmx.Event (HtmxEvent (..))
import Htmx.Lucid.Core (OnEvent (..), hxGet_, hxOn_, hxPost_, hxPushUrl_, hxSwapOob_, hxSwap_, hxTarget_, hxTrigger_)
import Htmx.Lucid.Extra (hxConfirm_, hxDelete_, hxPut_)
import Lucid
import Network.HTTP.Types
    ( ResponseHeaders
    , Status (..)
    , StdMethod (..)
    , status200
    )
import qualified Network.Wai as Wai (Request (..), Response, defaultRequest, getRequestBodyChunk, responseBuilder)

import Todo

-- import Web.Route.Invertible.Common
import Web.Route.Invertible.Wai

-- 1. Endpoint specification
-- https://hackage.haskell.org/package/web-inv-route-0.1.3.2/docs/Web-Route-Invertible-Common.html
--
-- curl -X GET localhost:3000
getHomeR :: Wai.Request -> RouteAction () (IO Wai.Response)
getHomeR _req =
    routeMethod GET
        *< routeHost ("localhost:3000")
        `RouteAction` \() -> return $ responseByteString $ testForm

-- curl -X GET localhost:3000/todos/42
_getThing :: RouteAction Int (Html ())
_getThing =
    routeMethod GET
        *< routePath ("todos" *< parameter)
        >* routeHost ("localhost:3000")
        `RouteAction` \i -> do
            p_ (toHtml (show i))

-- curl -X GET localhost:3000/todos
-- This makes the infinite printing to screen from getHomeR stop
-- since it has found something with the #todos id.
getTodoR :: Wai.Request -> RouteAction () (IO Wai.Response)
getTodoR _req =
    routeMethod GET
        *< routePath ("todos")
        >* routeHost ("localhost:3000")
        `RouteAction` \() -> do
            -- li_ [id_ "todos"] "the todos"
            getTodos

-- curl -X GET localhost:3000/foo
_complex :: RouteAction () (IO (Html ()))
_complex =
    routeMethod GET
        *< routeSecure False
        *< routePath "foo"
        *< routeHost ("localhost:3000")
        `RouteAction` \() ->
            return (p_ "complex")

postMouseEnteredR :: Wai.Request -> RouteAction () (IO Wai.Response)
postMouseEnteredR _req =
    routeMethod POST
        *< routeSecure False
        *< routePath "mouse_entered"
        *< routeHost ("localhost:3000")
        `RouteAction` \() ->
            -- return $ errorResponseByteString "mouse"
            return $ responseByteString "mouse"

-- curl -X DELETE localhost:3000/deltodo/5
delTodoR :: Wai.Request -> RouteAction (Int) (IO Wai.Response)
delTodoR _req =
    routeMethod DELETE
        *< routePath ("deltodo" *< parameter)
        >* routeHost ("localhost:3000")
        `RouteAction` \deletionId -> do
            delTodo $ T.pack $ show deletionId

-- curl -X POST localhost:3000/form?msg=fooString
postFormR :: Wai.Request -> RouteAction (String) (IO Wai.Response)
postFormR req =
    routeMethod GET
        *< routeSecure False
        *< routePath ("form")
        *< routeQuery "msg" parameter
        >* routeHost ("localhost:3000")
        `RouteAction` \(input) -> do
            postForm input req

postForm2R :: Wai.Request -> RouteAction () (IO Wai.Response)
postForm2R req =
    routeMethod POST
        *< routeSecure False
        *< routePath ("form")
        >* routeHost ("localhost:3000")
        `RouteAction` \() -> do
            reqBody <- BU.toString <$> getRequestBody req
            let outputString = dropMsgFromReqBody reqBody
            -- TODO: This outputString does not render emojis.
            -- Other postFormR can render emojis.
            postForm outputString req
  where
    -- \| Assumes "msg=fooString"; Returns "fooString"
    dropMsgFromReqBody :: String -> String
    dropMsgFromReqBody reqBody = drop 4 reqBody

postForm :: String -> Wai.Request -> IO Wai.Response
postForm msg _req = do
    updateTodos msg

-- curl -X GET localhost:3000/htmx
getHtmxR :: Wai.Request -> RouteAction () (IO Wai.Response)
getHtmxR _req =
    routeMethod GET
        *< routePath "htmx"
        *< routeHost ("localhost:3000")
        `RouteAction` \() -> return $ resJs "app/htmx.min.js"

responseByteString :: Html () -> Wai.Response
responseByteString h = Wai.responseBuilder status200 textHtml $ BSB.lazyByteString (renderBS h :: BL.ByteString)

-- 2. Routes map
myRoutes :: Wai.Request -> RouteMap (IO Wai.Response)
myRoutes req =
    routes
        -- [ pure $ routeNormCase complex
        -- , pure $ routeNormCase getThing
        [ routeNormCase $ getHomeR req
        , routeNormCase $ getTodoR req
        , routeNormCase $ postFormR req
        , routeNormCase $ postForm2R req
        , routeNormCase $ delTodoR req
        , routeNormCase $ getHtmxR req
        -- , routeNormCase $ postMouseEnteredR Wai.defaultRequest
        ]

-- 3. Route map lookup
myRouteWai :: Wai.Request -> RouteMap (IO Wai.Response) -> Either (Status, ResponseHeaders) (IO Wai.Response)
myRouteWai req routeMap = routeWai req routeMap

testForm :: Html ()
testForm =
    doctypehtml_ $ do
        head_ $
            do
                title_
                    "goblins"
                script_ [defer_ "", src_ "/htmx"] ("" :: Html ())
        -- useHtmx
        body_ $
            do
                form_
                    -- [hxPost_ "/form", hxTarget_ "#todos", hxSwap_ "beforeend", hxOn_ (HtmxOnEvent AfterRequest) "if(event.detail.successful) this.reset()"]
                    [hxPost_ "/form", hxTarget_ "#todos", hxSwap_ "beforeend", hxOn_ (HtmxOnEvent AfterRequest) "this.reset()"]
                    $ do
                        input_ [type_ "text", name_ "msg"]
                        input_ [type_ "submit", value_ "Add Todo"]
                        div_ [id_ "todos", hxGet_ "/todos", hxTrigger_ "load", hxSwap_ "outerHTML"] ""
                        -- div_ [hxPost_ "/mouse_entered", hxTrigger_ "mouseenter"] "[Here Mouse, Mouse!]"

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

-- | This will keep going until it gets the entire request body
-- | Might be in Network.HTTP.Req, but hoogle is down.
getRequestBody :: Wai.Request -> IO (B.ByteString)
getRequestBody req = do
    body <- loop id
    return $ mconcat body
  where
    loop front = do
        bs <- Wai.getRequestBodyChunk req
        if B.null bs
            then return $ front []
            else loop $ front . (bs :)
