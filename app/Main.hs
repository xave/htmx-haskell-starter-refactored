{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.UTF8 as BU
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple

import Htmx.Lucid.Core (hxSwap_, hxTarget_)
import Htmx.Lucid.Extra (hxDelete_)

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
