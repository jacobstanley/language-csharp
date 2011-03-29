{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import           Codec.Text.Detect
import qualified Data.ByteString as B
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
    --putStr $ show $ B.unpack $ B.take 4 bs
    let n = length (lexer input $ decode bs)
    putStrLn $ input ++ ": " ++ show n ++ " tokens"

    --result <- parseFromFile compilationUnit input
    --case result of
    --    Left err -> error (show err)
    --    Right xs -> writeFile output $ (render' xs) ++ "\n"

    --pid <- runCommand $ "diff -s " ++ input ++ " " ++ output
    --waitForProcess pid

    return ()

decode bs | utf8      = T.decodeUtf8    (B.drop 3 bs)
          | utf16le   = T.decodeUtf16LE (B.drop 2 bs)
          | utf16be   = T.decodeUtf16BE (B.drop 2 bs)
          | utf32le   = T.decodeUtf32LE (B.drop 4 bs)
          | utf32be   = T.decodeUtf32BE (B.drop 4 bs)
          | otherwise = T.decodeUtf8 bs
  where
    utf8    =     "\xEF\xBB\xBF" `B.isPrefixOf` bs
    utf16le =         "\xFF\xFE" `B.isPrefixOf` bs
    utf16be =         "\xFE\xFF" `B.isPrefixOf` bs
    utf32le = "\xFF\xFE\x00\x00" `B.isPrefixOf` bs
    utf32be = "\x00\x00\xFE\xFF" `B.isPrefixOf` bs
