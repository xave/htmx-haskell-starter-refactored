{-# LANGUAGE OverloadedStrings #-}

module Routes
    ( myRouteWai
    , myRoutes
    , responseByteString
    ) where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (toString)
import Database (getConn)
import Database.SQLite.Simple (Only (..), close, execute, lastInsertRowId)
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
import qualified Network.Wai as Wai (Request, Response, defaultRequest, responseBuilder)

import Network.Wai.Parse (Param, lbsBackEnd, parseRequestBody)
import Todo

import qualified Web.Route.Invertible as WRI (Request)

-- import Web.Route.Invertible.Common
import Web.Route.Invertible.Wai

-- 1. Endpoint specification
-- https://hackage.haskell.org/package/web-inv-route-0.1.3.2/docs/Web-Route-Invertible-Common.html
--
--
-- curl -X GET localhost:3000
getHomeR :: Wai.Request -> RouteAction () (IO Wai.Response)
getHomeR req =
    routeMethod GET
        *< routeHost ("localhost:3000")
        `RouteAction` \() -> return $ Routes.responseByteString $ testForm

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
getTodoR req =
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

_getMouseEntered :: RouteAction () (Html ())
_getMouseEntered =
    routeMethod GET
        *< routeSecure False
        *< routePath "mouseentered"
        *< routeHost ("localhost:3000")
        `RouteAction` \() ->
            ("" :: Html ())

postFormR :: Wai.Request -> RouteAction () (IO Wai.Response)
postFormR req =
    routeMethod POST
        *< routeSecure False
        *< routePath "form"
        *< routeHost ("localhost:3000")
        `RouteAction` \() ->
            (postForm req)

responseByteString :: Html () -> Wai.Response
responseByteString h = Wai.responseBuilder status200 textHtml $ Builder.lazyByteString (renderBS h :: BL.ByteString)

-- 2. Routes map
myRoutes :: Wai.Request -> RouteMap (IO Wai.Response)
myRoutes req =
    routes
        -- [ pure $ routeNormCase complex
        -- , pure $ routeNormCase getThing
        [ routeNormCase $ getHomeR Wai.defaultRequest
        , routeNormCase $ getTodoR Wai.defaultRequest
        , routeNormCase $ postFormR req
        -- , pure $ routeNormCase getMouseEntered
        ]

-- 3. Route map lookup
myRouteWai :: Wai.Request -> RouteMap (IO Wai.Response) -> Either (Status, ResponseHeaders) (IO Wai.Response)
myRouteWai req routeMap = routeWai req routeMap

-- import Network.Wai as Wai

-- |Convert a 'Wai.Request' to a request.
-- waiRequest :: Wai.Request -> Request

-- |Lookup a wai request in a route map, returning either an error code and headers or a successful result.
-- routeWai :: Wai.Request -> RouteMap a -> Either (Status, ResponseHeaders) a

-- |Combine a set of applications in a routing map into a single application, calling a custom error handler in case of routing error.
-- routeWaiError :: (Status -> ResponseHeaders -> Wai.Request -> a) -> RouteMap (Wai.Request -> a) -> Wai.Request -> a

-- |Equivalent to 'routeWaiError'.
-- routeWaiApplicationError :: (Status -> ResponseHeaders -> Wai.Application) -> RouteMap Wai.Application -> Wai.Application

-- |Combine a set of applications in a routing map into a single application, returning an empty error response in case of routing error.
-- routeWaiApplication :: RouteMap Wai.Application -> Wai.Application
-- --------------------------------------------------------------------
_postForm :: [Param] -> IO Wai.Response
_postForm params =
    case params of
        [("msg", msg)] -> do
            conn <- getConn
            execute conn "INSERT INTO todos (todo) VALUES (?)" (Only smsg)
            rowId <- lastInsertRowId conn
            close conn
            return $ responseByteString $ todoToLi $ TodoField rowId smsg
          where
            smsg = toString msg
        _badBody -> return $ errorPage "invalid post body"

postForm :: Wai.Request -> IO Wai.Response
postForm req = do
    (params, _files) <- parseRequestBody lbsBackEnd req
    return $ responseByteString $ "Hello"

-- undefined -- return $ responseByteString $ todoToLi $ TodoField rowId smsg

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
