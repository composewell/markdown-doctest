{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

module Main (main) where

import Control.Exception (bracket, throwIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import Data.List (find, isPrefixOf)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Parser (Parser)
import System.Environment (getArgs)

import Control.Applicative (Alternative(..))
import Streamly.Data.Stream (Stream)
import Streamly.FileSystem.Path (Path)
import Streamly.Data.Parser (ParseError)

import qualified Streamly.FileSystem.FileIO as File
import qualified Language.Haskell.Ghcid as G
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.FileSystem.Path as Path

import qualified Streamly.Internal.Data.Parser as Parser (takeFramedBy_)

--------------------------------------------------------------------------------
-- Parse a block
--------------------------------------------------------------------------------

type Line = String

startsWith :: Line -> Line -> Bool
startsWith prefix = isPrefixOf prefix

data Snippet
    = SHaskell [Line]
    | SHaskellUnshared [Line]
    | SHaskellDocspec [Line]
    | SIgnore [Line]
    deriving (Show)

fromSnippet :: Snippet -> [Line]
fromSnippet (SHaskell xs) = xs
fromSnippet (SHaskellUnshared xs) = xs
fromSnippet (SHaskellDocspec xs) = xs
fromSnippet (SIgnore xs) = xs

isSharedSnip :: Snippet -> Bool
isSharedSnip (SHaskell _) = True
isSharedSnip _ = False

isUnsharedSnip :: Snippet -> Bool
isUnsharedSnip (SHaskellUnshared _) = True
isUnsharedSnip _ = False

isDocspecSnip :: Snippet -> Bool
isDocspecSnip (SHaskellDocspec _) = True
isDocspecSnip _ = False

data Block =
    Block
        { isBlockStart :: Line -> Bool
        , isBlockEnd :: Line -> Bool
        , toSnippet :: [Line] -> Snippet
        }

ignoreParser :: Monad m => Parser Line m Snippet
ignoreParser =
    Parser.takeWhile
        (not . startsWith "```haskell")
        (SIgnore <$> Fold.toList)

mkBlockParser :: Monad m => Block -> Parser Line m Snippet
mkBlockParser Block {..} =
    Parser.takeFramedBy_ isBlockStart isBlockEnd (toSnippet <$> Fold.toList)

sharedParser :: Monad m => Parser Line m Snippet
sharedParser =
    mkBlockParser $ Block ((==) "```haskell") ((==) "```") SHaskell

unsharedParser :: Monad m => Parser Line m Snippet
unsharedParser =
    mkBlockParser $
        Block
            ((==) "```haskell unshared")
            ((==) "```")
            SHaskellUnshared

docspecParser :: Monad m => Parser Line m Snippet
docspecParser =
    mkBlockParser $
        Block
            ((==) "```haskell docspec")
            ((==) "```")
            SHaskellDocspec

combinedParser :: Monad m => Parser Line m Snippet
combinedParser
    = sharedParser
  <|> unsharedParser
  <|> docspecParser
  <|> ignoreParser

consumeParseError :: MonadIO m => Either ParseError a -> m a
consumeParseError val =
    case val of
        Left err -> liftIO (throwIO err)
        Right x -> pure x

--------------------------------------------------------------------------------
-- Block indicators
--------------------------------------------------------------------------------

-- XXX Having the following API will be useful:
-- encodeUtf8Fold :: Fold m Char (Array Word8)
getSnippetStream :: Path -> Stream IO Snippet
getSnippetStream fp =
    File.readChunks fp
        & Unicode.decodeUtf8Chunks
        & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
        & Stream.parseMany combinedParser
        & Stream.mapM consumeParseError

--------------------------------------------------------------------------------
-- Consume Snippets
--------------------------------------------------------------------------------

createInterpretedFile :: String -> IO ()
createInterpretedFile contents =
    writeFile "interpreted.hs" contents

loadInterpretedFile :: G.Ghci -> IO ()
loadInterpretedFile session = do
    res <- G.exec session ":load interpreted.hs"
    when (or (map hasErrorFile res)) $ do
        contents <- readFile "interpreted.hs"
        putStrLn $ replicate 80 '='
        putStrLn contents
        putStrLn $ "Error: " ++ replicate 73 '-'
        mapM_ putStrLn res
        putStrLn $ replicate 80 '='
        error "runSharedSnippet: error in interpreted.hs"
    where
    hasErrorFile out =
        let xs = lines out
         in case find ("interpreted.hs:" `isPrefixOf`) xs of
                Nothing -> False
                Just _ -> True

runContents :: G.Ghci -> [Line] -> IO ()
runContents session xs = do
    createInterpretedFile $ unlines xs
    loadInterpretedFile session

drainSharedSnippets :: G.Ghci -> Fold IO Snippet ()
drainSharedSnippets s =
    Fold.filter isSharedSnip
        $ Fold.rmapM (runContents s)
        $ Fold.foldl' (\b a -> b ++ fromSnippet a) []


drainUnsharedSnippets :: G.Ghci -> Fold IO Snippet ()
drainUnsharedSnippets session =
    Fold.filter
        isUnsharedSnip
        (Fold.drainMapM (runContents session . fromSnippet))

--------------------------------------------------------------------------------
-- Running docspec
--------------------------------------------------------------------------------

data DocspecSnippet
    = DocspecSnippet
          { prompt :: [Line]
          , expectedResult :: [Line]
          }

promptParserSingle :: Monad m => Parser Line m Line
promptParserSingle = drop 3 <$> Parser.satisfy (startsWith ">>>")

promptParserMulti :: Monad m => Parser Line m [Line]
promptParserMulti =
    (\a b -> drop 4 a : b)
        <$> Parser.satisfy (startsWith ">>> :{")
        <*> Parser.fromFold (Fold.takeEndBy (startsWith ":}") Fold.toList)

promptParserResult :: Monad m => Parser Line m [Line]
promptParserResult = Parser.takeWhile (not . startsWith ">>>") Fold.toList

promptParser :: Monad m => Parser Line m DocspecSnippet
promptParser =
    DocspecSnippet
        <$> (promptParserMulti <|> ((:[]) <$> promptParserSingle))
        <*> promptParserResult

readDocspecSnippets :: MonadIO m => Stream m Line -> Stream m DocspecSnippet
readDocspecSnippets inp =
    Stream.mapM consumeParseError $ Stream.parseMany promptParser inp

compareMinLines :: [Line] -> [Line] -> Bool
compareMinLines [] [] = True
compareMinLines [] xs = null $ dropWhile (== ' ') $ unwords xs
compareMinLines xs [] = null $ dropWhile (== ' ') $ unwords xs
compareMinLines (x:xs) (y:ys) =
    if x == y
    then compareMinLines xs ys
    else False

runDocspecSnippet :: G.Ghci -> DocspecSnippet -> IO ()
runDocspecSnippet session DocspecSnippet{..} = do
    when (not (null prompt)) $ do
        let combinedP = combinePrompt prompt
        res <- G.exec session combinedP
        if or (map hasErrorREPL res)
        then do
            putStrLn $ replicate 80 '='
            putStrLn combinedP
            putStrLn $ "Error: " ++ replicate 73 '-'
            mapM_ putStrLn res
            putStrLn $ replicate 80 '='
            error "runDocspecSnippet: Error"
        else when (not (compareMinLines res expectedResult)) $ do
            putStrLn $ replicate 80 '='
            putStrLn combinedP
            putStrLn $ "Expected: " ++ replicate 70 '-'
            putStrLn (unlines expectedResult)
            putStrLn $ "Got: " ++ replicate 75 '-'
            putStrLn (unlines res)
            putStrLn $ replicate 80 '='
            error "runDocspecSnippet: Error"
    where
    combinePrompt :: [Line] -> String
    combinePrompt [] = undefined
    combinePrompt [x] = x
    combinePrompt xs = unlines xs

    hasErrorREPL out =
        let xs = lines out
         in case find ("<interactive>:" `isPrefixOf`) xs of
                Nothing -> False
                Just _ -> True

drainDocspecSnippets :: G.Ghci -> Fold IO Snippet ()
drainDocspecSnippets session =
    Fold.filter isDocspecSnip (Fold.drainMapM run)
    where
    run xs =
        Stream.fromList (fromSnippet xs)
            & readDocspecSnippets
            & Stream.fold (Fold.drainMapM (runDocspecSnippet session))

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main =
    bracket alloc release with
    where
    alloc = do
        (session, _) <-
            G.startGhci
                "ghci"
                (Just ".")
                (\strm str ->
                     case strm of
                         G.Stdout -> putStrLn ("<stdout>: " ++ str)
                         G.Stderr -> putStrLn ("<stderr>: " ++ str))
        pure session
    release = G.stopGhci
    with session = do
        args <- getArgs
        let setup = [":set -fobject-code"]
        sequence_ $ map (G.exec session) setup
        void $ G.reload session
        let fileS =
                case args of
                    [x] -> x
                    _ -> error "Expecting exactly 1 argument"
        fileP <- Path.fromString fileS
        getSnippetStream fileP
            & Stream.fold
                  (Fold.distribute
                       [ drainSharedSnippets session
                       , drainUnsharedSnippets session
                       , drainDocspecSnippets session
                       ])
            & void
