{-# LANGUAGE DeriveGeneric #-}

-- Copyright Öckeröborgen IT AB (c) 2020

module SlackConnect
    ( postSlack
    , SlackSettings(..)
    , SlackResponse(..)
    ) where

import GHC.Generics

import Data.Aeson

import Data.ByteString.UTF8 as BLU
import Network.Wreq
import Control.Lens
import Data.Function


-- Exposed datatypes

data SlackSettings = SlackSettings
    { slackChannel :: String
    , slackToken :: String
    }

data SlackResponse
    = Ok
    | Error String
    deriving Show


-- Internal datatypes

data PostSlackPayload = PostSlackPayload
    { channel :: String
    , text :: String
    }
    deriving Generic

data RawSlackResponse = RawSlackResponse
    { ok :: Bool
    , error :: Maybe String
    }
    deriving Generic

instance ToJSON PostSlackPayload where

instance FromJSON RawSlackResponse where


-- Internal variables

postSlackUrl :: String
postSlackUrl = "https://slack.com/api/chat.postMessage"

extractSlackResponse :: RawSlackResponse -> SlackResponse
extractSlackResponse rawSlackResponse = withOk (ok rawSlackResponse)
    where
        withOk True = Ok
        withOk False = maybe
            (SlackConnect.Error "unknown")
            SlackConnect.Error
            (SlackConnect.error rawSlackResponse)

postSlack :: SlackSettings -> String -> IO SlackResponse
postSlack slackSettings text = postWith options postSlackUrl payload
    >>= asJSON
    <&> (^.responseBody)
    <&> extractSlackResponse
    where
        payload = toJSON (PostSlackPayload
            { channel = slackChannel slackSettings
            , text = text
            })
        options = defaults & auth ?~ (oauth2Bearer
            $ BLU.fromString
            $ slackToken slackSettings
            )
