module Markup (Document, Structure (..)) where

import Data.Function
import Numeric.Natural

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)

print :: (Show a) => a -> IO ()
print a = a & putStrLn . show

parse :: String -> Document
parse str = str & lines & parseLines []

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context
    currentLine : rest ->
      let
        line = trim currentLine
      in
      case line of
        "" -> maybe id (:) context (parseLines Nothing rest)
        _ ->
          case context of
            Just (Paragraph paragraph) -> parseLines (Just (Paragraph (unwords [paragraph, line])))
            _ -> maybe id (:) context (parseLines (Just (Paragraph line))


-- parseLines :: [String] -> [String] -> Document
-- parseLines currentParagraph txts =
--   let paragraph = Paragraph (currentParagraph & reverse & unlines)
--    in case txts of
--         [] -> [paragraph]
--         currentLine : rest ->
--           case trim currentLine of
--             "" -> paragraph : parseLines [] rest
--             _ -> parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim str = str & unwords . words

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

exactlyTwo :: [a] -> Maybe (a, a)
exactlyTwo [x, y] = Just (x, y)
exactlyTwo _ = Nothing

exactlyTwoVersion2 :: [a] -> Maybe (a, a)
exactlyTwoVersion2 list =
  case list of
    x : y : [] -> Just (x, y)
    _ -> Nothing
