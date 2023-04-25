{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}


module Messages where


import GHC.Generics ( Generic )
import Data.Aeson
    ( defaultOptions, FromJSON, Options(fieldLabelModifier), ToJSON )
import Data.Aeson.TH ( deriveJSON )


data Message = Message
  { role :: String
  , content :: String
  } deriving (Generic, Show )

instance ToJSON Message
instance FromJSON Message

data GPTRequest = GPTRequest
  { model :: String
  , messages :: [Message]
  , temperature :: Double
  } deriving (Generic, Show)

instance ToJSON GPTRequest
instance FromJSON GPTRequest

defMessage :: Message
defMessage = Message { role = "user", content = "Say Hi!" }

createMessage :: String -> String -> Message
createMessage r c = Message { role = r , content = c }

createMessageUser :: String -> Message
createMessageUser = createMessage "user"

getMessageContent :: Message -> String
getMessageContent m = m.content

defGPTRequest :: GPTRequest
defGPTRequest = GPTRequest 
  { model = "gpt-3.5-turbo"
  , messages = [ defMessage ]
  , temperature = 0.7
  }

updateGPTMessages :: GPTRequest -> [ Message ] -> GPTRequest
updateGPTMessages gptReq messList = gptReq { messages = messList }

data Usage = Usage
  { prompt_tokens :: Int
  , completion_tokens :: Int
  , total_tokens :: Int
  } deriving (Show, Generic)

instance FromJSON Usage
instance ToJSON Usage

data Choice = Choice
  { message :: Message
  , finish_reason :: String
  , index :: Int
  } deriving (Show, Generic)

instance FromJSON Choice
instance ToJSON Choice


data GPTResponse = GPTResponse
  { id :: String
  , object :: String
  , created :: Int
  , respModel :: String
  , usage :: Usage
  , choices :: [Choice]
  } deriving (Show, Generic)

-- | Add instances for JSON serialization/deserialization, 
-- with a custom field label modifier
$(deriveJSON
    defaultOptions
      { fieldLabelModifier = \field -> case field of
          "respModel" -> "model"
          _ -> field
      }
    ''GPTResponse)

getResponseMessages :: GPTResponse -> [ Message ]
getResponseMessages resp = ( \choice -> choice.message ) <$> resp.choices

data SafetyLevel = Safe | Controversial | Unsafe | Other
  deriving (Show, Generic)

instance FromJSON SafetyLevel
instance ToJSON SafetyLevel

data CommandResponse = CommandResponse 
  { command :: String
  , safety  :: SafetyLevel
  , safetyExplanation :: String
  , gpt     :: String
  } deriving (Show, Generic)

instance FromJSON CommandResponse
instance ToJSON CommandResponse