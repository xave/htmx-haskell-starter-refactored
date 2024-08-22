{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder.Char8 (fromString)
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.UTF8 as BU
import Data.Int (Int64)
import Data.Text (unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time
import Database.SQLite.Simple

-- import Htmx.Lucid.Core (hxSwap_, hxTarget_)
-- import Htmx.Lucid.Extra (hxDelete_)
-- import Lucid (Html (), button_, class_, div_, id_, li_, renderText, toHtml, ul_)

import Lucid
import Lucid.Base
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
    run port $ midLog app -- run :: Port -> Application -> IO ()
  where
    port = 3000

app :: Application
app req respond = do
    (params, _) <- parseRequestBody lbsBackEnd req
    res <- case mepinf of
        ("DELETE", ["deltodo", deletionId]) -> delTodo $ unpack deletionId
        ("GET", ["todos"]) -> getTodos
        ("POST", ["form"]) -> postForm params
        _nonIO -> return $ case mepinf of
            ("GET", []) -> resHtmlFile "app/form.html"
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

todoToLi :: TodoField -> String
todoToLi (TodoField i s) = htmlToString htmlRepresentation
  where
    htmlRepresentation :: Html ()
    htmlRepresentation =
        li_ [hxTarget_ "this"] $ do
            button_ [hxSwap_ "outerHTML", hxDelete_ delTodoWithArg] "x"
            myListItem
    delTodoWithArg = "/deltodo/" <> T.pack (show i) :: T.Text
    myListItem = toHtml $ " | " <> s

todosToUl :: [TodoField] -> String
todosToUl todos =
    htmlToString
        $ ul_
            [id_ "todos"]
        $ toHtml h -- "<ul id=\"todos\">" ++ h ++ "</ul>"
  where
    h = concatMap todoToLi todos

todoList :: IO [TodoField]
todoList = do
    conn <- getConn
    r <- query_ conn "SELECT rowid, todo FROM todos" :: IO [TodoField]
    close conn
    return r

getTodos :: IO Response
getTodos = do resHtmlString . todosToUl <$> todoList

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
            return $ resHtmlString $ todoToLi $ TodoField rowId smsg
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

resHtmlString :: String -> Response
resHtmlString s = responseBuilder status200 textHtml $ fromString s

resJs :: FilePath -> Response
resJs = resFile textJs

-- using lucid and htmx
-- Manually defining these compiles. Using the lucid-htmx packages does not

htmlToString :: Html () -> String
htmlToString a = LT.unpack $ renderText a

-- hxGet_ :: T.Text -> Attributes
-- hxGet_ = makeAttributes "hx-get"

hxTarget_ :: T.Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

hxSwap_ :: T.Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

hxDelete_ :: T.Text -> Attributes
hxDelete_ = makeAttributes "hx-delete"
