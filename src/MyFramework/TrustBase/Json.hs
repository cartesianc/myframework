module MyFramework.TrustBase.Json
  ( JsonValue (..)
  , jsonArrayItems
  , jsonObjectField
  , jsonStringValue
  , parseJson
  ) where

import Control.Applicative
  ( many
  , optional
  , (<|>)
  )
import Data.Char
  ( chr
  , digitToInt
  , isHexDigit
  )
import Text.ParserCombinators.ReadP
  ( ReadP
  , between
  , char
  , choice
  , eof
  , munch
  , munch1
  , readP_to_S
  , satisfy
  , sepBy
  , skipSpaces
  , string
  )

data JsonValue
  = JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
  | JsonString String
  | JsonNumber String
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Ord, Read, Show)

parseJson :: String -> Either String JsonValue
parseJson currentText =
  case
      [ currentValue
      | (currentValue, remainingText) <-
          readP_to_S
            (skipSpaces *> jsonValueParser <* skipSpaces <* eof)
            currentText
      , null remainingText
      ] of
    [] ->
      Left "invalid JSON"
    currentValues ->
      Right (last currentValues)

jsonObjectField ::
  String ->
  JsonValue ->
  Either String JsonValue
jsonObjectField currentName currentValue =
  case currentValue of
    JsonObject currentFields ->
      case
          [ fieldValue
          | (fieldName, fieldValue) <- currentFields
          , fieldName == currentName
          ] of
        [fieldValue] ->
          Right fieldValue
        [] ->
          Left ("missing JSON object field: " ++ currentName)
        _ ->
          Left ("duplicate JSON object field: " ++ currentName)
    _ ->
      Left "expected JSON object"

jsonArrayItems :: JsonValue -> Either String [JsonValue]
jsonArrayItems currentValue =
  case currentValue of
    JsonArray currentItems ->
      Right currentItems
    _ ->
      Left "expected JSON array"

jsonStringValue :: JsonValue -> Either String String
jsonStringValue currentValue =
  case currentValue of
    JsonString currentText ->
      Right currentText
    _ ->
      Left "expected JSON string"

jsonValueParser :: ReadP JsonValue
jsonValueParser =
  choice
    [ jsonObjectParser
    , jsonArrayParser
    , JsonString <$> jsonStringParser
    , JsonNumber <$> jsonNumberParser
    , JsonBool True <$ string "true"
    , JsonBool False <$ string "false"
    , JsonNull <$ string "null"
    ]

jsonObjectParser :: ReadP JsonValue
jsonObjectParser =
  JsonObject
    <$> between
      (symbol '{')
      (symbol '}')
      (jsonFieldParser `sepBy` symbol ',')

jsonFieldParser :: ReadP (String, JsonValue)
jsonFieldParser = do
  currentName <- lexeme jsonStringParser
  _ <- symbol ':'
  currentValue <- lexeme jsonValueParser
  pure (currentName, currentValue)

jsonArrayParser :: ReadP JsonValue
jsonArrayParser =
  JsonArray
    <$> between
      (symbol '[')
      (symbol ']')
      (lexeme jsonValueParser `sepBy` symbol ',')

jsonStringParser :: ReadP String
jsonStringParser =
  between
    (char '"')
    (char '"')
    (many jsonStringChar)

jsonStringChar :: ReadP Char
jsonStringChar =
  escapedChar
    <|> satisfy
      (\currentChar ->
          currentChar /= '"'
            && currentChar /= '\\'
            && fromEnum currentChar >= 0x20
      )

escapedChar :: ReadP Char
escapedChar = do
  _ <- char '\\'
  choice
    [ '"' <$ char '"'
    , '\\' <$ char '\\'
    , '/' <$ char '/'
    , '\b' <$ char 'b'
    , '\f' <$ char 'f'
    , '\n' <$ char 'n'
    , '\r' <$ char 'r'
    , '\t' <$ char 't'
    , unicodeChar
    ]

unicodeChar :: ReadP Char
unicodeChar = do
  _ <- char 'u'
  currentDigits <- exactly 4 (satisfy isHexDigit)
  pure
    ( chr
        ( foldl
            (\currentValue currentDigit ->
                currentValue * 16 + digitToInt currentDigit
            )
            0
            currentDigits
        )
    )

jsonNumberParser :: ReadP String
jsonNumberParser = do
  currentSign <- optional (char '-')
  currentInteger <-
    string "0"
      <|> do
        firstDigit <- satisfy (\currentChar -> currentChar >= '1' && currentChar <= '9')
        remainingDigits <- munch isDecimalDigit
        pure (firstDigit : remainingDigits)
  currentFraction <-
    optional
      ( do
          currentDot <- char '.'
          currentDigits <- munch1 isDecimalDigit
          pure (currentDot : currentDigits)
      )
  currentExponent <-
    optional
      ( do
          exponentMarker <- satisfy (\currentChar -> currentChar == 'e' || currentChar == 'E')
          exponentSign <- optional (satisfy (\currentChar -> currentChar == '+' || currentChar == '-'))
          exponentDigits <- munch1 isDecimalDigit
          pure
            ( exponentMarker
                : maybe [] (: []) exponentSign
                ++ exponentDigits
            )
      )
  pure
    ( maybe [] (: []) currentSign
        ++ currentInteger
        ++ maybe [] id currentFraction
        ++ maybe [] id currentExponent
    )

isDecimalDigit :: Char -> Bool
isDecimalDigit currentChar =
  currentChar >= '0' && currentChar <= '9'

symbol :: Char -> ReadP Char
symbol currentChar =
  lexeme (char currentChar)

lexeme :: ReadP value -> ReadP value
lexeme currentParser =
  currentParser <* skipSpaces

exactly :: Int -> ReadP value -> ReadP [value]
exactly currentCount currentParser
  | currentCount <= 0 =
      pure []
  | otherwise =
      (:) <$> currentParser <*> exactly (currentCount - 1) currentParser
