module Main where

import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.List (find, isPrefixOf)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Parser (Parser)
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Fold as Fold (serial_)

haskellCodeStart :: String -> Bool
haskellCodeStart = isPrefixOf "```haskell"

codeEndIndicator :: String -> Bool
codeEndIndicator = isPrefixOf "```"

blockEndIndicator :: String -> Bool
blockEndIndicator [] = False
blockEndIndicator (' ':_) = False
blockEndIndicator _ = True

isSignature :: String -> Bool
isSignature xs =
    case words xs of
        (_:b:_) -> not (isSpace (head xs)) && (b == "::")
        _ -> False

haskellSnippet :: Fold IO String [String]
haskellSnippet =
    Fold.takeEndBy_ haskellCodeStart Fold.drain
        `Fold.serial_` Fold.takeEndBy_ codeEndIndicator Fold.toList

haskellBlock :: Parser String IO [String]
haskellBlock = do
    Just ln <- Parser.fromFold Fold.one
    if isSignature ln
    then do
        Just nextln <- Parser.fromFold Fold.one
        rest <- Parser.takeWhile (not . blockEndIndicator) Fold.toList
        return (ln : nextln : rest)
    else do
        rest <- Parser.takeWhile (not . blockEndIndicator) Fold.toList
        return (ln : rest)

trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

parseString :: String -> IO [String]
parseString s = do
    res <-
        Stream.fromList (lines s) & Stream.foldMany haskellSnippet
            & Stream.mapM
                  (Stream.fold Fold.toList
                       . Stream.parseMany haskellBlock . Stream.fromList)
            & Stream.fold Fold.toList
    return $ filter (not . null) $ fmap unlines $ fmap (fmap unlines) res

hasError :: String -> Bool
hasError out =
    let xs = lines out
     in case find ("interpreted.hs:" `isPrefixOf`) xs of
            Nothing -> False
            Just _ -> True

loopCmd :: [String] -> IO ()
loopCmd [] = do
    putStrLn "All good"
    exitSuccess
loopCmd (snip:snips) = do
    writeFile ("interpreted.hs") snip
    (_, _, resErr) <-
        readProcessWithExitCode "timeout" ["2s", "ghci", "interpreted.hs"] ""
    if hasError resErr
    then do
        putStrLn "Failed"
        putStrLn snip
        putStrLn resErr
        exitFailure
    else loopCmd snips

main :: IO ()
main = do
    args <- getArgs
    let [fileName] = args
    contents <- readFile fileName
    res <- parseString contents
    loopCmd res
