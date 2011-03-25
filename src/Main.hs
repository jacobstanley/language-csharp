{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Text.Parsec.ByteString
import Language.CSharp.Parser
import Language.CSharp.Pretty
import System.FilePath
import System.Process

main :: IO ()
main = do
    let input = "test/Test.cs"
        output = addExtension input ".pretty"

    result <- parseFromFile compilationUnit input
    case result of
        Left err -> error (show err)
        Right xs -> writeFile output $ (render' xs) ++ "\n"

    pid <- runCommand $ "diff -s " ++ input ++ " " ++ output
    waitForProcess pid

    return ()
