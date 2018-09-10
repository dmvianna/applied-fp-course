{-# LANGUAGE OverloadedStrings #-}
module Level04.Types.Error
  ( Error(UnknownRoute, EmptyCommentText, EmptyTopic, DBError)
  , nonEmptyText
  ) where

import           Data.Text (Text)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | DBError
  -- Add another constructor for our DB error types.
  deriving (Eq, Show)

nonEmptyText
  :: (Text -> a)
  -> Error
  -> Text
  -> Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)
