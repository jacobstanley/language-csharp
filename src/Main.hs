{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import           Control.Concurrent
import           Codec.Text.Detect (detectEncodingName)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process

import           Language.CSharp.Lexer
import           Language.CSharp.Parser
import           Language.CSharp.Pretty

main :: IO ()
main = do
    args <- getArgs
    fs <- getFiles (if null args then "." else head args)

    ts <- forkJoin analyzeFile fs
    putStrLn $ "Lexed " ++ show (sum ts) ++ " tokens"

getFiles :: FilePath -> IO [FilePath]
getFiles path | csharp    = return [path]
              | otherwise = do
    isDir <- doesDirectoryExist path
    if not isDir
      then return []
      else do
        fs <- getDirectoryContents path
        fs' <- mapM getFiles (map prefix $ filter isReal fs)
        return (concat fs')
  where
    csharp = takeExtension path == ".cs"
    prefix = (path </>)

    isReal "."  = False
    isReal ".." = False
    isReal _    = True


analyzeFile :: FilePath -> IO Int
analyzeFile path = {-# SCC "analyze" #-} do
    bs <- B.readFile path

    let encoding = {-# SCC "detect" #-} detect bs
        text     = {-# SCC "decode" #-} decode' encoding bs
        tokens   = {-# SCC "lexer"  #-} lexer path text

    case {-# SCC "parse"  #-} parseCSharp path tokens of
        Left err -> error (show err)
        Right xs -> writeFile path' $ render' xs ++ "\n"

    putStrLn $ file ++ ": " ++ show (length tokens)
            ++ " tokens (" ++ encoding ++ ")"

    putStrLn $ "diff -s " ++ file ++ " " ++ file'
    pid <- runCommand $ "diff -s " ++ path ++ " " ++ path'
    waitForProcess pid

    return (length tokens)
  where
    file  = takeFileName path
    path' = replaceExtension path ".pretty.cs"
    file' = takeFileName path'

    detect bs = case detectEncoding bs of
        Nothing -> error $ file ++ ": could not detect encoding"
        Just x  -> x

    decode' enc bs = case decode enc bs of
        Nothing -> error $ file ++ ": " ++ enc ++ " is not a supported encoding"
        Just x  -> x


detectEncoding :: B.ByteString -> Maybe String
detectEncoding bs = detectEncodingName $ L.fromChunks [bs]

decode :: String -> B.ByteString -> Maybe T.Text
decode "UTF-8"        = Just . T.decodeUtf8With T.lenientDecode
decode "UTF-16LE"     = Just . T.decodeUtf16LE
decode "UTF-16BE"     = Just . T.decodeUtf16BE
decode "UTF-32LE"     = Just . T.decodeUtf32LE
decode "UTF-32BE"     = Just . T.decodeUtf32BE
decode "ASCII"        = Just . T.decodeASCII
decode "windows-1252" = Just . T.decodeASCII
decode _              = const Nothing

------------------------------------------------------------------------

forkJoin :: (a -> IO b) -> [a] -> IO [b]
forkJoin f xs = (fork f xs) >>= join

join :: [MVar b] -> IO [b]
join = mapM takeMVar

fork :: (a -> IO b) -> [a] -> IO [MVar b]
fork f = mapM (fork1 f)

fork1 :: (a -> IO b) -> a -> IO (MVar b)
fork1 f x = do
    cell <- newEmptyMVar
    forkIO $ do { result <- f x; putMVar cell $! result }
    return cell
