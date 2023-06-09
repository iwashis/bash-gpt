module GPTPrompt (initialPrompt) where


initialPrompt :: String
initialPrompt = "From now on, whenever I write something reply only using a JSON format whose \
    \Haskell datatype representation is given below:\n\n\
    \data CommandResponse = CommandResponse {\n\
    \    command :: String\n\
    \  , safety :: SafetyLevel\n\
    \  , safetyExplanation :: String\n\
    \  , gpt :: String\n\
    \}\n\n\
    \Note that SafetyLevel datatype is\n\n\
    \data SafetyLevel = Safe | Controversial | Unsafe | Other\n\n\
    \If I write something that translates naturally into a bash shell script command, \
    \please include the translated value in the command field, assess its safety \
    \in the safety field (using the values: Safe, Controversial, Unsafe), explain yourself why you applied a given \
    \safety category to the command and leave gpt field empty.\n\n\
    \If my prompt cannot be translated to bash, then simply leave command and safetyExplanation fields empty, \
    \set safety to Other and include your message which is not a bash command in the gpt field. \n\n\
    \Please do not include any other remarks, answer only with the json content as your \
    \text answer and no other text. If you want to give any extra comment then please include it in the gpt field.\n\n\
    \Translate the following command:"