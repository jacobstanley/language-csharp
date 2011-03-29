{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import           Codec.Text.Detect
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import           Language.CSharp.Lexer
import           Language.CSharp.Parser
import           Language.CSharp.Pretty
import           System.Environment
import           System.FilePath
import           System.Process

import Debug.Trace (trace)

main :: IO ()
main = do
    args <- getArgs

    let input = if length args == 0 then "test/Test2.cs" else head args
        --output = replaceExtension input ".pretty.cs"

    bs <- B.readFile input
    let n = length (lexer input $ decode input bs)
    putStrLn $ input ++ ": " ++ show n ++ " tokens"

    --result <- parseFromFile compilationUnit input
    --case result of
    --    Left err -> error (show err)
    --    Right xs -> writeFile output $ (render' xs) ++ "\n"

    --pid <- runCommand $ "diff -s " ++ input ++ " " ++ output
    --waitForProcess pid

    return ()

decode :: String -> B.ByteString -> T.Text
decode file bs = go enc
  where
    enc = detectEncodingName (L.fromChunks [bs])

    go (Just "ASCII")    = T.decodeASCII bs
    go (Just "UTF-8")    = T.decodeUtf8    (B.drop 3 bs)
    go (Just "UTF-16LE") = T.decodeUtf16LE (B.drop 2 bs)
    go (Just "UTF-16BE") = T.decodeUtf16BE (B.drop 2 bs)
    go (Just "UTF-32LE") = T.decodeUtf32LE (B.drop 4 bs)
    go (Just "UTF-32BE") = T.decodeUtf32BE (B.drop 4 bs)
    go (Just x)          = error $ file ++ ": unsupported encoding: " ++ x
    go Nothing           = error $ file ++ ": could not detect encoding"
