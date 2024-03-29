module Main (main) where

import Control.Monad (void, when)
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
import qualified Streamly.Internal.Data.Fold as Fold (split_)

haskellCodeStart :: String -> Bool
haskellCodeStart = (==) "```haskell" . takeWhile (/= '\n')

ghciCodeStart :: String -> Bool
ghciCodeStart = (==) "```haskell ghci" . takeWhile (/= '\n')

docspecCodeStart :: String -> Bool
docspecCodeStart = (==) "```haskell docspec" . takeWhile (/= '\n')

codeEndIndicator :: String -> Bool
codeEndIndicator = isPrefixOf "```"

blockEndIndicator :: String -> Bool
blockEndIndicator [] = False
blockEndIndicator (' ':_) = False
blockEndIndicator _ = True

docspecIndicator :: String -> Bool
docspecIndicator = isPrefixOf ">>>"

docspecMultiIndicator :: String -> Bool
docspecMultiIndicator = isPrefixOf ">>> :{"

docspecMultiEndIndicator :: String -> Bool
docspecMultiEndIndicator = isPrefixOf ":}"

isInlineStatement :: String -> Bool
isInlineStatement str = "{-# INLINE" `isPrefixOf` str

isSignature :: String -> Bool
isSignature xs =
    case words xs of
        (_:b:_) -> not (isSpace (head xs)) && (b == "::")
        _ -> False

snippet :: (String -> Bool) -> Fold IO (Int, String) [(Int, String)]
snippet startIndicator =
    Fold.split_
        (Fold.takeEndBy_ (startIndicator . snd) Fold.drain)
        (Fold.takeEndBy_ (codeEndIndicator . snd) Fold.toList)

data Block a
    = WithMeta1 a
    | WithMeta2 a
    | WithoutMeta a

unBlock :: Block a -> a
unBlock (WithMeta1 a) = a
unBlock (WithMeta2 a) = a
unBlock (WithoutMeta a) = a

haskellBlock :: Parser (Int, String) IO (Block [(Int, String)])
haskellBlock = do
    Just ln <- Parser.fromFold Fold.one
    if isSignature (snd ln) || isInlineStatement (snd ln)
    then do
        Just nextln <- Parser.fromFold Fold.one
        if isSignature (snd nextln) || isInlineStatement (snd nextln)
        then do
            Just nextNextln <- Parser.fromFold Fold.one
            rest <- Parser.takeWhile (not . blockEndIndicator . snd) Fold.toList
            return $ WithMeta2 (ln : nextln : nextNextln : rest)
        else do
            rest <- Parser.takeWhile (not . blockEndIndicator . snd) Fold.toList
            return $ WithMeta1 (ln : nextln : rest)
    else do
        rest <- Parser.takeWhile (not . blockEndIndicator . snd) Fold.toList
        return $ WithoutMeta (ln : rest)

docspecBlock :: Parser (Int, String) IO ([(Int, String)], [(Int, String)])
docspecBlock = do
    Just ln <- Parser.fromFold Fold.one
    rest <- Parser.takeWhile (not . docspecIndicator . snd) Fold.toList
    if docspecMultiIndicator (snd ln)
    then do
        let specFld =
                Fold.takeEndBy_ (docspecMultiEndIndicator . snd) Fold.toList
            resFld = Fold.toList
            fld = Fold.splitWith (,) specFld resFld
        Parser.fromEffect (Stream.fold fld (Stream.fromList rest))
    else do
        let fld = Fold.toList
        res <- Parser.fromEffect (Stream.fold fld (Stream.fromList rest))
        return ([drop 4 <$> ln], res)

extractSrcLoc :: Bool -> [(Int, String)] -> (Int, String)
extractSrcLoc _ [] = (0, [])
extractSrcLoc addNewlines xs@((i, _):_) =
    if addNewlines
    then (i, unlines (map snd xs))
    else (i, concat (map snd xs))

parseDocspecBlocks :: String -> IO [((Int, String), String)]
parseDocspecBlocks s = do
    let withLoc = zip [1 ..] (lines s)
    res <-
        Stream.fromList withLoc
            & Stream.foldMany (snippet docspecCodeStart)
            & Stream.mapM blocks
            & Stream.fold Fold.toList
    return
        $ map (\(a, b) -> (extractSrcLoc True a, concat (map snd b)))
        $ concat res

    where

    blocks =
          Stream.fold Fold.toList
        . fmap (either (error . show) id)
        . Stream.parseMany docspecBlock
        . Stream.fromList

parseString :: (String -> Bool) -> String -> IO [[Block [(Int, String)]]]
parseString startIndicator s = do
    let withLoc = zip [1 ..] (lines s)
    Stream.fromList withLoc & Stream.foldMany (snippet startIndicator)
        & Stream.mapM blocks
        & Stream.fold Fold.toList

    where

    blocks =
        Stream.fold Fold.toList
      . fmap (either (error . show) id)
      . Stream.parseMany haskellBlock
      . Stream.fromList

saitizeREPLStatement :: String -> String
saitizeREPLStatement str =
    if "{-# LANGUAGE" `isPrefixOf` str
    then ":set -X" ++ takeWhile (not . isSpace) (drop 13 str) ++ "\n"
    else str

parseStringREPL :: String -> IO [(Int, String)]
parseStringREPL s = do
    res <- parseString ghciCodeStart s
    let blocks = concat res
    return
        $ filter (not . null . snd)
        $ fmap
              (fmap (\x -> ":{\n" ++ saitizeREPLStatement x ++ ":}")
                   . extractSrcLoc True)
        $ loopBlocks blocks

    where

    -- This is reqiured because for some reason, GhcId can't register signatures
    -- after a while. Functions don't get into scope after a while.
    loopBlocks [] = []
    loopBlocks (WithMeta1 b:bs) = [b, drop 1 b] ++ loopBlocks bs
    loopBlocks (WithMeta2 b:bs) = [b, drop 2 b] ++ loopBlocks bs
    loopBlocks (WithoutMeta b:bs) = [b] ++ loopBlocks bs

hasErrorREPL :: String -> Bool
hasErrorREPL out =
    let xs = lines out
     in case find ("<interactive>:" `isPrefixOf`) xs of
            Nothing -> False
            Just _ -> True

parseStringFile :: String -> IO [(Int, String)]
parseStringFile s = do
    res <- parseString haskellCodeStart s
    return
        $ filter (not . null . snd)
        $ fmap (extractSrcLoc False)
        $ fmap (fmap (extractSrcLoc True . unBlock)) res

hasErrorFile :: String -> Bool
hasErrorFile out =
    let xs = lines out
     in case find ("interpreted.hs:" `isPrefixOf`) xs of
            Nothing -> False
            Just _ -> True

loopCmdDocspec :: G.Ghci -> [((Int, String), String)] -> IO Bool
loopCmdDocspec _ [] = putStrLn "All good" >> return True
loopCmdDocspec sess (((i, spec), result):rest) = do
    putStrLn spec
    res <- G.exec sess (":{\n" ++ spec ++ ":}")
    if or (map hasErrorREPL res)
    then do
        mapM_ putStrLn res
        putStrLn ("Error at: " ++ show i)
        return False
    else let resNN = takeWhile (not . null) res
             resNLen = length (dropWhile (not . null) res)
             resultLines = lines result
             resultNN = takeWhile (not . null) resultLines
             resultNLen = length (dropWhile (not . null) resultLines)
          in if resNN == resultNN && resNLen <= resultNLen
             then loopCmdDocspec sess rest
             else do
                 putStrLn ("Error at: " ++ show i)
                 putStrLn $ unlines ["Expected:", result, "Got:", unlines res]
                 return False

loopCmdREPL :: G.Ghci -> [(Int, String)] -> IO Bool
loopCmdREPL _ [] = putStrLn "All good" >> return True
loopCmdREPL sess (ln:lns) = do
    putStrLn (snd ln)
    res <- G.exec sess (snd ln)
    if or (map hasErrorREPL res)
    then do
        mapM_ putStrLn res
        putStrLn ("Error at: " ++ show (fst ln))
        return False
    else loopCmdREPL sess lns

loopCmdFile :: G.Ghci -> [(Int, String)] -> IO Bool
loopCmdFile _ [] = putStrLn "All good" >> return True
loopCmdFile sess (ln:lns) = do
    let padding = replicate (fst ln - 1) '\n'
    writeFile ("interpreted.hs") (padding ++ snd ln)
    putStrLn (snd ln)
    res <- G.exec sess ":load interpreted.hs"
    if or (map hasErrorFile res)
    then do
        mapM_ putStrLn res
        putStrLn ("Error at: " ++ show (fst ln))
        return False
    else loopCmdFile sess lns

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
    parsedREPL <- parseStringREPL contents
    r1 <- loopCmdREPL ghciSession parsedREPL
    r2 <- if r1
          then do
            parsedDocspec <- parseDocspecBlocks contents
            loopCmdDocspec ghciSession parsedDocspec
          else return False
    when r2 $ do
        parsedFile <- parseStringFile contents
        void $ loopCmdFile ghciSession parsedFile
    G.stopGhci ghciSession
