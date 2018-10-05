{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                       (AppM (..), Env (envDB),
                                                     liftEither, runAppM)

import           Level07.Types                      (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: AppM Connection
getDBConn =
  AppM $ \env -> pure $ pure $ asks (dbConn . envDB) env

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> AppM b
runDB fe fc =
  AppM $ \env -> do
  eConn <- runAppM getDBConn env
  case eConn of
    Right conn -> do
      a <- liftIO $ fc conn
      pure $ fe a
    Left e     -> pure $ Left e

getComments
  :: Topic
  -> AppM [Comment]
getComments t =
  -- Write the query with an icky string and remember your placeholders!
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
      fc conn = Sql.query conn q (Sql.Only . getTopic $ t)
  in
      runDB (traverse fromDBComment) fc

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM ()
addCommentToTopic t c = do
  nowish <- liftIO getCurrentTime
  let q =
        "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
      fc conn =
          Sql.execute conn q (getTopic t
                             ,getCommentText c
                             ,nowish)
  runDB Right fc

getTopics
  :: AppM [Topic]
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
      fc conn = Sql.query_ conn q
  in
    runDB (traverse ( mkTopic . Sql.fromOnly )) fc

deleteTopic
  :: Topic
  -> AppM ()
deleteTopic t =
  let q = "DELETE FROM comments WHERE topic = ?"
      fc conn = Sql.execute conn q (Sql.Only . getTopic $ t)
  in
    runDB Right fc

-- Go on to 'src/Level07/Core.hs' next.
