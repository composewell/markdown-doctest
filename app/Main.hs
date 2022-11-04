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

haskellSnippet :: Fold IO (Int, String) [(Int, String)]
haskellSnippet =
    Fold.serial_
        (Fold.takeEndBy_ (haskellCodeStart . snd) Fold.drain)
        (Fold.takeEndBy_ (codeEndIndicator . snd) Fold.toList)

haskellBlock :: Parser (Int, String) IO [(Int, String)]
haskellBlock = do
    Just ln <- Parser.fromFold Fold.one
    if isSignature (snd ln)
    then do
        Just nextln <- Parser.fromFold Fold.one
        rest <- Parser.takeWhile (not . blockEndIndicator . snd) Fold.toList
        return (ln : nextln : rest)
    else do
        rest <- Parser.takeWhile (not . blockEndIndicator . snd) Fold.toList
        return (ln : rest)

extractSrcLoc :: Bool -> [(Int, String)] -> (Int, String)
extractSrcLoc _ [] = (0, [])
extractSrcLoc addNewlines xs@((i, _):_) =
    if addNewlines
    then (i, unlines (map snd xs))
    else (i, concat (map snd xs))

parseString :: String -> IO [(Int, String)]
parseString s = do
    let withLoc = zip [1 ..] (lines s)
    res <-
        Stream.fromList withLoc & Stream.foldMany haskellSnippet
            & Stream.mapM
                  (Stream.fold Fold.toList
                       . Stream.parseMany haskellBlock . Stream.fromList)
            & Stream.fold Fold.toList
    return
        $ filter (not . null . snd)
        $ fmap (extractSrcLoc False) $ fmap (fmap (extractSrcLoc True)) res

hasError :: String -> Bool
hasError out =
    let xs = lines out
     in case find ("interpreted.hs:" `isPrefixOf`) xs of
            Nothing -> False
            Just _ -> True

loopCmd :: [(Int, String)] -> IO (Maybe ((Int, String), String))
loopCmd [] = return Nothing
loopCmd (snip:snips) = do
    let padding = replicate (fst snip - 1) '\n'
    writeFile ("interpreted.hs") (padding ++ snd snip)
    (_, _, resErr) <-
        readProcessWithExitCode "timeout" ["2s", "ghci", "interpreted.hs"] ""
    if hasError resErr
    then return $ Just (snip, resErr)
    else loopCmd snips


main :: IO ()
main = do
    args <- getArgs
    let [fileName] = args
    contents <- readFile fileName
    parsed <- parseString contents
    res <- loopCmd parsed
    case res of
        Just (snip, resErr) -> do
            putStrLn (snd snip)
            putStrLn resErr
            putStrLn
                ("Error at snippet: " ++ fileName ++ ":" ++ show (fst snip))
            exitFailure
        Nothing -> do
            putStrLn "All good"
            exitSuccess
