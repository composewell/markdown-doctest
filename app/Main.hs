module Main (main) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.List (find, isPrefixOf)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Parser (Parser)
import System.Environment (getArgs)

import qualified Language.Haskell.Ghcid as G
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
        $ concat
        $ fmap
              (fmap (fmap (\x -> ":{\n" ++ x ++ ":}") . extractSrcLoc True))
              res

hasError :: String -> Bool
hasError out =
    let xs = lines out
     in case find ("<interactive>:" `isPrefixOf`) xs of
            Nothing -> False
            Just _ -> True

loopCmd :: G.Ghci -> [(Int, String)] -> IO ()
loopCmd _ [] = putStrLn "All good"
loopCmd sess (ln:lns) = do
    putStrLn (snd ln)
    res <- G.exec sess (snd ln)
    if or (map hasError res)
    then do
        mapM_ putStrLn res
        putStrLn ("Error at: " ++ show (fst ln))
    else loopCmd sess lns

ghciSetup :: [String]
ghciSetup = [":set -fobject-code"]

main :: IO ()
main = do
    args <- getArgs
    (ghciSession, _) <-
        G.startGhci
            "ghci"
            (Just ".")
            (\strm str ->
                 case strm of
                     G.Stdout -> putStrLn ("<stdout>: " ++ str)
                     G.Stderr -> putStrLn ("<stderr>: " ++ str))
    sequence_ $ map (G.exec ghciSession) ghciSetup
    void $ G.reload ghciSession
    let fileName =
            case args of
                [x] -> x
                _ -> error "Expecting exactly 1 argument"
    contents <- readFile fileName
    parsed <- parseString contents
    loopCmd ghciSession parsed
