{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.CSharp.Arbitrary () where

import           Control.Applicative
import           Data.DeriveTH
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import           Test.QuickCheck

import           Language.CSharp.Syntax

$(derive makeArbitrary ''CompilationUnit)
$(derive makeArbitrary ''Namespace)
$(derive makeArbitrary ''TypeDecl)
$(derive makeArbitrary ''Type)
$(derive makeArbitrary ''LocalType)
$(derive makeArbitrary ''PrimType)
$(derive makeArbitrary ''Method)
$(derive makeArbitrary ''FormalParam)
$(derive makeArbitrary ''Stmt)
$(derive makeArbitrary ''VarDecl)
$(derive makeArbitrary ''VarInit)
$(derive makeArbitrary ''Arg)
$(derive makeArbitrary ''Exp)
$(derive makeArbitrary ''ObjectInit)
$(derive makeArbitrary ''ElementInit)
$(derive makeArbitrary ''MemberInit)
$(derive makeArbitrary ''InitVal)
$(derive makeArbitrary ''ClassMod)
$(derive makeArbitrary ''MethodMod)
$(derive makeArbitrary ''ParamMod)
$(derive makeArbitrary ''ArgMod)

instance Arbitrary Literal where
    arbitrary = oneof
        [ return Null
        , Bool     <$> arbitrary
        , Int      <$> (B.pack . show   <$> int)
        , Real     <$> (B.pack . show   <$> real)
        , Char     <$> (T.pack . return <$> char)
        , String   <$> (T.pack          <$> string)
        , Verbatim <$> (T.pack          <$> string)
        ]
      where
        int    = arbitrary :: Gen Int
        real   = arbitrary :: Gen Float
        char   = arbitrary :: Gen Char
        string = arbitrary :: Gen String

instance Arbitrary Name where
    arbitrary = Name <$> listOf1 arbitrary

instance Arbitrary Ident where
    arbitrary = Ident <$> (ident `suchThat` notKeyword)
      where
        ident = do
            x <- start
            xs <- listOf part
            return $ T.pack (x:xs)
        start = oneof
            [ choose ('a', 'z')
            , choose ('A', 'Z')
            , return '_' ]
        part = oneof
            [ choose ('a', 'z')
            , choose ('A', 'Z')
            , choose ('0', '9')
            , return '_' ]

        notKeyword = not . (`elem` keywords)

keywords :: [T.Text]
keywords =
    [ "abstract", "as", "base", "bool", "break", "byte", "case"
    , "catch", "char", "checked", "class", "const", "continue"
    , "decimal", "default", "delegate", "do", "double", "else"
    , "enum", "event", "explicit", "extern", "false", "finally"
    , "fixed", "float", "for", "foreach", "goto", "if", "implicit"
    , "in", "int", "interface", "internal", "is", "lock", "long"
    , "namespace", "new", "null", "object", "operator", "out"
    , "override", "params", "private", "protected", "public"
    , "readonly", "ref", "return", "sbyte", "sealed", "short"
    , "sizeof", "stackalloc", "static", "string", "struct"
    , "switch", "this", "throw", "true", "try", "typeof", "uint"
    , "ulong", "unchecked", "unsafe", "ushort", "using", "virtual"
    , "void", "volatile", "while" ]
