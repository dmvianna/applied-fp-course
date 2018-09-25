{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Text                  (Text)
import           Data.Text.Encoding         as E

import           Data.Bifunctor             (first, second)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (displayException, try)

import           Data.Aeson                 (FromJSON, Object)

import qualified Data.Aeson                 as A

import           Level06.Types              (ConfigError (..),
                                             PartialConf (PartialConf))
-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

-- | File Parsing

-- We're trying to avoid complications when selecting a configuration file
-- package from Hackage. We'll use an encoding that you are probably familiar
-- with, for better or worse, and write a small parser to pull out the bits we
-- need. The package we're using is the ``aeson`` package to parse some JSON and
-- we'll pick the bits off the Object.

-- | Update these tests when you've completed this function.
--
-- | readConfFile
-- >>> readConfFile "badFileName.no"
-- Left (ConfigError "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> IO ( Either ConfigError ByteString )
readConfFile fp = do
  bs <- try $ LBS.readFile fp :: IO (Either IOError ByteString)
  pure $ first (ConfigError . displayException) bs

-- Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> IO ( Either ConfigError PartialConf )
parseJSONConfigFile fp = do
  bs <- readConfFile fp
  case second A.decode bs of
    Left e          -> pure $ Left e
    Right (Just x)  -> pure $ Right x
    Right (Nothing) -> pure $ Left $ ConfigError "empty file"

-- Go to 'src/Level06/Conf.hs' next.
