{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L
import           Language.CSharp.Lexer
import           Language.CSharp.Parser
import           Language.CSharp.Pretty
import           System.Environment
import           System.FilePath
import           System.Process

main :: IO ()
main = do
    args <- getArgs

    let input = if length args == 0 then "test/Test2.cs" else head args
        --output = replaceExtension input ".pretty.cs"

    bs <- L.readFile input
    print (lexer bs)
    --result <- parseFromFile compilationUnit input
    --case result of
    --    Left err -> error (show err)
    --    Right xs -> writeFile output $ (render' xs) ++ "\n"

    --pid <- runCommand $ "diff -s " ++ input ++ " " ++ output
    --waitForProcess pid

    return ()
