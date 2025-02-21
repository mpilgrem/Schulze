{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Vote.IO
  ( decodeEither
  ) where

import           Data.Aeson
                   ( FromJSON (..), Value (..), (.:?), (.!=), withObject )
import           Data.Aeson.KeyMap ( KeyMap, delete, elems )
import           Data.ByteString ( ByteString )
import           Data.Vote.Types ( Candidates, Vote )
import           Data.Yaml ( ParseException )
import qualified Data.Yaml as Yaml
import           GHC.Generics

data ResultRaw = ResultRaw
  { candidates :: Candidates
  , votes :: KeyMap VoteRaw
  }
  deriving (Eq, Generic, Show)

instance FromJSON ResultRaw

data VoteRaw = VoteRaw
  { count :: Int
  , voteRaw :: Vote
  }
  deriving (Eq, Generic, Show)

instance FromJSON VoteRaw where

  parseJSON = withObject "VoteRaw" $ \o -> do
    count <- o .:? countKey .!= 1
    voteRaw <- parseJSON $ Object $ delete countKey o
    pure VoteRaw {..}
   where
    countKey = "count"

decodeEither :: ByteString -> Either ParseException (Candidates, [Vote])
decodeEither bs = do
  resultRaw <- Yaml.decodeEither' bs
  pure (candidates resultRaw, concatMap toVotes $ elems $ votes resultRaw )
 where
  toVotes :: VoteRaw -> [Vote]
  toVotes vr = replicate (count vr) (voteRaw vr)
