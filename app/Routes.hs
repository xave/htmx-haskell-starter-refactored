{-# LANGUAGE OverloadedStrings #-}

module Routes (myRouteWai, myRoutes) where

import Htmx.Event (HtmxEvent (..))
import Htmx.Lucid.Core (OnEvent (..), hxGet_, hxOn_, hxPost_, hxPushUrl_, hxSwapOob_, hxSwap_, hxTarget_, hxTrigger_)
import Htmx.Lucid.Extra (hxConfirm_, hxDelete_, hxPut_)
import Lucid
import Network.HTTP.Types (
    ResponseHeaders,
    Status (statusCode, statusMessage),
    status200,
    status404,
 )
import Network.Wai as Wai
import Web.Route.Invertible
import Web.Route.Invertible.Common
import Web.Route.Invertible.Wai

-- 1. Endpoint specification
-- https://hackage.haskell.org/package/web-inv-route-0.1.3.2/docs/Web-Route-Invertible-Common.html
--
--
-- curl -X GET localhost:3000
getHomeR :: RouteAction () (Html ())
getHomeR =
    routeMethod GET
        *< routeHost ("localhost:3000")
        `RouteAction` \() -> testForm

-- curl -X GET localhost:3000/todos/42
getThing :: RouteAction Int (Html ())
getThing =
    routeMethod GET
        *< routePath ("todos" *< parameter)
        >* routeHost ("localhost:3000")
        `RouteAction` \i -> do
            p_ (toHtml (show i))

-- curl -X GET localhost:3000/todos
-- This makes the infinite printing to screen from getHomeR stop
-- since it has found something with the #todos id.
getTodos :: RouteAction () (Html ())
getTodos =
    routeMethod GET
        *< routePath ("todos")
        >* routeHost ("localhost:3000")
        `RouteAction` \() -> do
            li_ [id_ "todos"] "the todos"

-- curl -X GET localhost:3000/foo
complex :: RouteAction () (Html ())
complex =
    routeMethod GET
        *< routeSecure False
        *< routePath "foo"
        *< routeHost ("localhost:3000")
        `RouteAction` \() ->
            (p_ "complex")

getMouseEntered :: RouteAction () (Html ())
getMouseEntered =
    routeMethod GET
        *< routeSecure False
        *< routePath "mouseentered"
        *< routeHost ("localhost:3000")
        `RouteAction` \() ->
            ("" :: Html ())

-- 2. Routes map
myRoutes :: RouteMap (Html ())
myRoutes =
    routes
        [ routeNormCase complex
        , routeNormCase getThing
        , routeNormCase getHomeR
        , routeNormCase getTodos
        , routeNormCase getMouseEntered
        ]

-- 3. Route map lookup
myRouteWai :: Wai.Request -> RouteMap (Html ()) -> Either (Status, ResponseHeaders) (Html ())
myRouteWai req routeMap = routeWai req routeMap

-- import Network.Wai as Wai

{- |Convert a 'Wai.Request' to a request.
 waiRequest :: Wai.Request -> Request
-}

{- |Lookup a wai request in a route map, returning either an error code and headers or a successful result.
 routeWai :: Wai.Request -> RouteMap a -> Either (Status, ResponseHeaders) a
-}

{- |Combine a set of applications in a routing map into a single application, calling a custom error handler in case of routing error.
 routeWaiError :: (Status -> ResponseHeaders -> Wai.Request -> a) -> RouteMap (Wai.Request -> a) -> Wai.Request -> a
-}

{- |Equivalent to 'routeWaiError'.
 routeWaiApplicationError :: (Status -> ResponseHeaders -> Wai.Application) -> RouteMap Wai.Application -> Wai.Application
-}

{- |Combine a set of applications in a routing map into a single application, returning an empty error response in case of routing error.
 routeWaiApplication :: RouteMap Wai.Application -> Wai.Application
 --------------------------------------------------------------------
-}
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
