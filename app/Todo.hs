{-# LANGUAGE OverloadedStrings #-}

module Todo
    ( getTodos
    , textHtml
    , errorResponseByteString
    , delTodo
    , errorPage
    , TodoField (..)
    , todoToLi
    , resJs
    , updateTodos
    ) where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.UTF8 as BU
import Data.Either
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
import Network.HTTP.Types
    ( ResponseHeaders
    , Status (statusCode, statusMessage)
    , status200
    , status404
    )
import Network.Wai
    ( Application
    , Middleware
    , Response
    , pathInfo
    , rawPathInfo
    , requestMethod
    , responseBuilder
    , responseFile
    , responseStatus
    )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse (Param, lbsBackEnd, parseRequestBody)
import Text.Printf (printf)

data TodoField = TodoField !Int64 !String deriving (Show)

instance FromRow TodoField where
    fromRow = TodoField <$> field <*> field

todoToLi :: TodoField -> Html ()
todoToLi (TodoField i s) =
    li_ [hxTarget_ "this"] $ do
        button_ [hxSwap_ "outerHTML", hxDelete_ delTodoWithArg] "x"
        myListItem
  where
    delTodoWithArg = "/deltodo/" <> T.pack (show i) :: Text
    myListItem = toHtml $ " | " <> s

todosToUl :: [TodoField] -> Html ()
todosToUl todos =
    ul_ [id_ "todos"] h -- "<ul id=\"todos\">" ++ h ++ "</ul>"
  where
    h = mconcat $ map todoToLi todos :: Html ()

fetchTodoList :: IO [TodoField]
fetchTodoList = do
    conn <- getConn
    r <- query_ conn "SELECT rowid, todo FROM todos" :: IO [TodoField]
    close conn
    return r

getTodos :: IO Response
getTodos = responseByteString . todosToUl <$> fetchTodoList

-- getTodos :: IO (Html ())
-- getTodos = do todosToUl <$> fetchTodoList

delTodo :: Text -> IO Response
delTodo deletionId = do
    conn <- getConn
    execute conn "DELETE FROM todos WHERE rowid=?" (Only deletionId)
    close conn
    return $ responseBuilder status200 textHtml $ fromString ""

updateTodos :: String -> IO Response
updateTodos smsg = do
    conn <- getConn
    execute conn "INSERT INTO todos (todo) VALUES (?)" (Only smsg)
    rowId <- lastInsertRowId conn
    close conn
    return $ responseByteString $ todoToLi $ TodoField rowId smsg

getConn :: IO Connection
getConn = open "db.db"

responseByteString :: Html () -> Response
responseByteString h = responseBuilder status200 textHtml $ Builder.lazyByteString (renderBS h :: BL.ByteString)

errorResponseByteString :: Html () -> Response
errorResponseByteString h = responseBuilder status404 textHtml $ Builder.lazyByteString (renderBS h :: BL.ByteString)

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

_resHtmlFile :: FilePath -> Response
_resHtmlFile = resFile textHtml

resJs :: FilePath -> Response
resJs = resFile textJs
