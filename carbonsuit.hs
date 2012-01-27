module Main where

import System.Environment (getArgs)
import System.FilePath (dropExtension)

import qualified System.IO.UTF8 as IOU

import Text.Parsec

import Text.CarbonSuit
import Text.CarbonSuit.Processing

main :: IO ()
main = do
  [mode,fn] <- getArgs
  content <- readFile fn
  let result = runParser (parseCarbon fn) initialPS "text-notation" content
  case result of
    Left err ->
      do IOU.putStrLn "Error :"
         IOU.print err
    Right a  ->
      case mode of
        "-d" -> IOU.putStrLn (displayCarbon a)
        "-x" -> do
          let a' = mergePromptBlocks a
          IOU.writeFile (dropExtension fn ++ ".html") (renderAsStandAloneXHtml a')
          IOU.writeFile (dropExtension fn ++ ".body.html") (renderAsXHtml a')
        _    -> IOU.putStrLn ("Unrecognised mode : " ++ mode)

