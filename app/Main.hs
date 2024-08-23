{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.UTF8 as BU
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import Database.SQLite.Simple

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
import Network.Wai (
    Application,
    Middleware,
    Response,
    pathInfo,
    rawPathInfo,
    requestMethod,
    responseBuilder,
    responseFile,
    responseStatus,
 )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse (Param, lbsBackEnd, parseRequestBody)
import Text.Printf (printf)

main :: IO ()
main = do
    setupDB
    putStrLn $ "Listening on http://localhost:" ++ show port
    putStrLn $ TL.unpack $ renderText form
    run port $ midLog app -- run :: Port -> Application -> IO ()
  where
    port = 3000

app :: Application
app req respond = do
    (params, _) <- parseRequestBody lbsBackEnd req
    res <- case mepinf of
        ("DELETE", ["deltodo", deletionId]) -> delTodo $ T.unpack deletionId
        ("GET", ["todos"]) -> getTodos
        ("POST", ["form"]) -> postForm params
        _nonIO -> return $ case mepinf of
            -- ("GET", []) -> resHtmlFile "app/form.html"
            ("GET", []) -> resHtmlFile "../temphtml.html"
            ("GET", ["htmx"]) -> resJs "app/htmx.min.js"
            _undefinedPath -> errorPage $ rawPath ++ " is undefined" where rawPath = toString $ rawPathInfo req
    respond res
  where
    pinf = pathInfo req
    meth = requestMethod req
    mepinf = (meth, pinf)

midLog :: Middleware
midLog midApp req res = do
    start <- getCurrentTime
    let method = BU.toString $ requestMethod req
        path = BU.toString $ rawPathInfo req
    let res' h = do
            end <- getCurrentTime
            let diff = show $ diffUTCTime start end
                status = responseStatus h
                code = statusCode status
            let message = BU.toString $ statusMessage status
            printf "%d %s %s %s %s \n" code method path diff message
            res h
    midApp req res'

setupDB :: IO ()
setupDB = do
    conn <- getConn
    execute_ conn "CREATE TABLE IF NOT EXISTS todos (todo VARCHAR(255))"

data TodoField = TodoField !Int64 !String deriving (Show)

instance FromRow TodoField where
    fromRow = TodoField <$> field <*> field

todoToLi :: TodoField -> Html ()
todoToLi (TodoField i s) = htmlRepresentation
  where
    htmlRepresentation :: Html ()
    htmlRepresentation =
        li_ [hxTarget_ "this"] $ do
            button_ [hxSwap_ "outerHTML", hxDelete_ delTodoWithArg] "x"
            myListItem
    delTodoWithArg = "/deltodo/" <> T.pack (show i) :: Text
    myListItem = toHtml $ " | " <> s

todosToUl :: [TodoField] -> Html ()
todosToUl todos =
    ul_ [id_ "todos"] h -- "<ul id=\"todos\">" ++ h ++ "</ul>"
  where
    h :: Html ()
    h = mconcat $ map todoToLi todos

fetchTodoList :: IO [TodoField]
fetchTodoList = do
    conn <- getConn
    r <- query_ conn "SELECT rowid, todo FROM todos" :: IO [TodoField]
    close conn
    return r

getTodos :: IO Response
getTodos = do responseByteString . todosToUl <$> fetchTodoList

delTodo :: String -> IO Response
delTodo deletionId = do
    conn <- getConn
    execute conn "DELETE FROM todos WHERE rowid=?" (Only deletionId)
    close conn
    return $ responseBuilder status200 textHtml $ fromString ""

postForm :: [Param] -> IO Response
postForm params =
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

getConn :: IO Connection
getConn = open "db.db"

errorPage :: String -> Response
errorPage e = responseBuilder status404 textPlain $ fromString e

contentType :: BU.ByteString -> ResponseHeaders
contentType h = [("Content-Type", h)]

textPlain :: ResponseHeaders
textPlain = contentType "text/plain"

textHtml :: ResponseHeaders
textHtml = contentType "text/html"

textJs :: ResponseHeaders
textJs = contentType "text/javascript"

resFile :: ResponseHeaders -> FilePath -> Response
resFile mime filename = responseFile status200 mime filename Nothing

resHtmlFile :: FilePath -> Response
resHtmlFile = resFile textHtml

responseByteString :: Html () -> Response
responseByteString h = responseBuilder status200 textHtml $ Builder.lazyByteString (renderBS h :: BL.ByteString)

resJs :: FilePath -> Response
resJs = resFile textJs

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
