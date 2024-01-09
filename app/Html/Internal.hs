module Html.Internal where

import Control.Arrow
import Data.Function

newtype Html = Html String
  deriving (Show)

newtype Structure = Structure String
  deriving (Show)

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

type Title = String

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> Structure -> Html
html_ title content =
  Html (el "html" (el "head" (el "title" (escape title)) <> el "body" (getStructureString content)))

body_ :: String -> Structure
body_ = Structure . el "body"

head_ :: String -> Structure
head_ = Structure . el "head"

title_ :: String -> Structure
title_ = Structure . el "title"

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ structures = structures & map appendListItem & concat & el "ul" & Structure
  where
    appendListItem :: Structure -> String
    appendListItem (Structure item) = el "li" item

ol_ :: [Structure] -> Structure
ol_ structures = structures & map appendListItem & concat & el "ul" & Structure
  where
    appendListItem :: Structure -> String
    appendListItem (Structure item) = el "li" item

render :: Html -> String
render (Html html) = html

getStructureString :: Structure -> String
getStructureString (Structure str) = str

escape :: String -> String
escape = concat . map escapeChar
  where
    escapeChar :: Char -> String
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
