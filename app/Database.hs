{-# LANGUAGE OverloadedStrings #-}

module Database (setupDB, getConn) where

import Database.SQLite.Simple

setupDB :: IO ()
setupDB = do
    conn <- getConn
    execute_ conn "CREATE TABLE IF NOT EXISTS todos (todo VARCHAR(255))"

getConn :: IO Connection
getConn = open "db.db"
