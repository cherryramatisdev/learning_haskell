module Main where

import Html

myHtml :: Html
myHtml =
  html_ "My title" (append_ (h1_ "Heading") (append_ (p_ "Paragraph #1") (p_ "Paragraph #2")))

main :: IO ()
main = putStrLn (render myHtml)
