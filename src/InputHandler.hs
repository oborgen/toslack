module InputHandler where

-- Copyright Öckeröborgen IT AB (c) 2020

import SlackConnect
import System.IO
import System.Environment
import Data.Functor

postStdinToSlackCmdArgs :: IO ()
postStdinToSlackCmdArgs = getArgs
    <&> argsToSlackSettings
    >>= either putStrLn postStdinToSlack

argsToSlackSettings :: [String] -> Either String SlackSettings
argsToSlackSettings (channel:token:[]) = Right (SlackSettings channel token)
argsToSlackSettings _ = Left "usage: toslack channel token"

postStdinToSlack :: SlackSettings -> IO ()
postStdinToSlack slackSettings = do
    input <- getContents
    let inputLines = lines input
    mapM_ (postLineToSlack slackSettings) inputLines

postLineToSlack :: SlackSettings -> String -> IO ()
postLineToSlack slackSettings line = postSlack slackSettings line
    >>= printSlackResponse line

printSlackResponse :: String -> SlackResponse -> IO ()
printSlackResponse line Ok = putStrLn ("Ok: " ++ line)
printSlackResponse line (Error code) = hPutStrLn stderr ("Error: " ++ code)
