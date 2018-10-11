{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import qualified Control.Monad.Except               as E
import           Data.Either                        (partitionEithers)

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (UTCTime, getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.DB.Types                   (DBComment (..),
                                                     DBTopic (..))
import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment, fromDBTopic,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM (..), catchError,
                                                     liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB f ioA = do
  a <- liftIO ioA
  b <- liftEither $ f a
  pure b

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments db topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DBComment
  -- cannot be converted to a Comment, or simply ignoring any DBComment that is
  -- not valid.
    query :: Text -> AppM [DBComment]
    query t = AppM $ Right <$> Sql.query (dbConn db) sql [t]
  in do
    r <- query (getTopic topic)
    case partitionEithers (fromDBComment <$> r) of
      ([], rs) -> pure rs
      (e:_, _) -> liftEither $ Left e

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic db topic ct =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    query :: Topic -> CommentText -> UTCTime -> AppM ()
    query t ct' ut = liftIO $
      Sql.execute (dbConn db) sql (getTopic t, getCommentText ct', ut)
  in do
    time <- liftIO getCurrentTime
    r <- query topic ct time
    pure r

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics db =
  let
    sql = "SELECT DISTINCT topic FROM comments"
  in do
    rs <- liftIO $ Sql.query_ (dbConn db) sql :: AppM [DBTopic]
    case partitionEithers (fromDBTopic <$> rs) of
      ([], rs') -> pure rs'
      (e:_, _)  -> liftEither $ Left e

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic db topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    query = Sql.execute (dbConn db) sql [getTopic topic]
  in do
    rs <- liftIO query
    pure rs

-- Go to 'src/Level05/Core.hs' next.
