{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Network.HTTP.Simple
    ( getResponseBody,
      httpLBS,
      parseRequestThrow,
      setRequestBodyJSON,
      setRequestHeaders,
      setRequestMethod )
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Types.Header ( hContentType, hAuthorization )
import Messages
import Data.Maybe ( fromJust )
import Data.Aeson (decode)
import Network.HTTP.Client (Request)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad
import System.Console.ANSI
import System.Environment
import GPTPrompt (initialPrompt)

-- | Parses the response from the OpenAI API and extracts the CommandResponse.
-- The response is parsed to a GPTResponse, then the Message field is
-- extracted from the first Choice in the list, and finally the Message
-- content is decoded to a CommandResponse.
parseCommandResponse :: MonadIO m => Request -> m CommandResponse
parseCommandResponse request = do
  response <- httpLBS request
  let responseBody = getResponseBody response
    -- the code below is bad. TODO: replace fromJust with total functions and 
    -- explicitly handle errors. 
      gptResponse  = fromJust ( decode responseBody :: Maybe GPTResponse )
      commandResponse = fromJust (
        decode
        $ LBS.pack
        $ getMessageContent
        $ head
        $ getResponseMessages gptResponse  :: Maybe CommandResponse
        )
  pure commandResponse

data OpenaiAPI = OpenaiAPI
  { apiKey     :: String
  , url        :: String
  , prompt     :: String
  }

-- | Reads the OpenAI API key and initial GPT prompt from files and returns them
-- as an OpenaiAPI record.
readOpenaiAPI :: IO OpenaiAPI
readOpenaiAPI = do
  key    <- fromJust <$> lookupEnv "OPENAI_API_KEY"
  pure $ OpenaiAPI {
    apiKey = key
    , url = "https://api.openai.com/v1/chat/completions"
    , prompt = initialPrompt
  }

-- | Given an OpenaiAPI record and a message, constructs a HTTP request to the
-- OpenAI API to generate a response.
makeGPTRequest :: OpenaiAPI -> String -> IO Request
makeGPTRequest api message = do
  let headers = [ (hContentType  , pack "application/json"  )
                , (hAuthorization , pack $ "Bearer " <> api.apiKey)
                ]
  initialRequest <- parseRequestThrow api.url
  let fullMessage = createMessageUser ( api.prompt ++ message )
      gptMessage = updateGPTMessages defGPTRequest [ fullMessage ]
      request = setRequestHeaders headers $
                setRequestMethod "POST" $
                setRequestBodyJSON gptMessage initialRequest
  pure request

-- | Prints the CommandResponse to the console.
printCommandResponse :: CommandResponse -> IO ()
printCommandResponse CommandResponse{command, safety, safetyExplanation, gpt} = do
  setSGR [Reset]
  putStr command
  case safety of
    Safe -> setSGR [SetColor Foreground Vivid Green]
    Controversial -> setSGR [SetColor Foreground Vivid Yellow]
    Unsafe -> setSGR [SetColor Foreground Vivid Red]
    Other -> setSGR [SetColor Foreground Vivid White]
  putStrLn $ " (" ++ show safety ++ ")"
  setSGR [SetColor Foreground Dull Black]
  putStrLn safetyExplanation
  unless (null gpt) $ putStrLn gpt

-- | The main entry point of the program.
main :: IO ()
main = do
  api <- readOpenaiAPI
  putStrLn ">>- Write your bash command in a natural language -<<"
  message <- getLine
  putStrLn ">>- Answer -<<"
  request <- makeGPTRequest api message
  comResp <- parseCommandResponse request
  printCommandResponse comResp