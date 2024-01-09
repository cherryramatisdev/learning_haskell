module Main where

import Html

myHtml :: Html
myHtml =
  html_ "My title" (h1_ "Heading" <> (p_ "Paragraph #1" <> p_ "Paragraph #2"))

main :: IO ()
main = putStrLn (render myHtml)
