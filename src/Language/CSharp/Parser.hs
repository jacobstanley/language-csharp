{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.CSharp.Parser
    ( compilationUnit
    ) where

import           Control.Applicative hiding (many, (<|>))
import           Data.ByteString (ByteString)
import           Prelude hiding ((>>), (>>=))
import qualified Prelude as P ((>>), (>>=))
import           Text.Parsec
import           Text.Parsec.ByteString

import           Language.CSharp.Syntax
import           Language.CSharp.Token

-- A trick to allow >> and >>=, normally infixr 1, to be
-- used inside branches of <|>, which is declared as infixl 1.
-- There are no clashes with other operators of precedence 2.
(>>) = (P.>>)
(>>=) = (P.>>=)
infixr 2 >>, >>=

------------------------------------------------------------------------
-- Top level

type P = GenParser ByteString ()

compilationUnit :: P CompilationUnit
compilationUnit = CompilationUnit <$> many namespace

namespace :: P Namespace
namespace = do
    reserved "namespace"
    Namespace <$> name
              <*> braces (many typeDecl)

------------------------------------------------------------------------
-- Declarations

typeDecl :: P TypeDecl
typeDecl = withModifiers class_ <?> "type declaration"

class_ :: [Modifier] -> P TypeDecl
class_ ms = do
    reserved "class"
    Class <$> return ms
          <*> ident
          <*> braces (many member)

member :: P Method
member = withModifiers method

method :: [Modifier] -> P Method
method ms = Method <$> return ms
                   <*> returnType
                   <*> (ident <?> "method identifier")
                   <*> formalParams
                   <*> block
                   <?> "method declaration"

localVar :: P Stmt
localVar = LocalVar <$> type_
                    <*> commaSep varDecl

varDecl :: P VarDecl
varDecl = VarDecl <$> ident
                  <*> optionMaybe varInit

varInit :: P VarInit
varInit = reservedOp "=" >> InitExp <$> expression

------------------------------------------------------------------------
-- Statements

block :: P [Stmt]
block = braces $ many statement

statement :: P Stmt
statement = st localVar
  where
    -- semi-colon terminated
    st p = do { x <- p; semi; return x }

------------------------------------------------------------------------
-- Expressions

expression :: P Exp
expression = Lit <$> literal

literal :: P Literal
literal = reserved "true" >> return (Bool True)
      <|> reserved "false" >> return (Bool False)
      <|> Int <$> natural

------------------------------------------------------------------------
-- Formal Parameters

formalParams :: P [FormalParam]
formalParams = parens (commaSep formalParam) <?> "formal parameter list"

formalParam :: P FormalParam
formalParam = FormalParam <$> many paramModifier
                          <*> type_
                          <*> ident

------------------------------------------------------------------------
-- Modifiers

withModifiers :: ([Modifier] -> P a) -> P a
withModifiers p = many modifier >>= p

modifier :: P Modifier
modifier = reserved "new"       >> return New
       <|> reserved "public"    >> return Public
       <|> reserved "protected" >> return Protected
       <|> reserved "internal"  >> return Internal
       <|> reserved "private"   >> return Private
       <|> reserved "abstract"  >> return Abstract
       <|> reserved "sealed"    >> return Sealed
       <|> reserved "static"    >> return Static
       <|> reserved "unsafe"    >> return Unsafe
       <?> "modifier (e.g. public)"

paramModifier :: P ParamModifier
paramModifier = reserved "ref"  >> return Ref
            <|> reserved "out"  >> return Out
            <|> reserved "this" >> return This

------------------------------------------------------------------------
-- Types

type_ :: P Type
type_ = SimpleType <$> simpleType

returnType :: P (Maybe Type)
returnType = reserved "void" >> return Nothing
         <|> Just <$> type_
         <?> "return type"

simpleType :: P SimpleType
simpleType = reserved "bool"    >> return BoolT
         <|> reserved "sbyte"   >> return SByteT
         <|> reserved "short"   >> return ShortT
         <|> reserved "ushort"  >> return UShortT
         <|> reserved "int"     >> return IntT
         <|> reserved "uint"    >> return UIntT
         <|> reserved "long"    >> return LongT
         <|> reserved "ulong"   >> return ULongT
         <|> reserved "char"    >> return CharT
         <|> reserved "float"   >> return FloatT
         <|> reserved "double"  >> return DoubleT
         <|> reserved "decimal" >> return DecimalT

------------------------------------------------------------------------
-- Names and identifiers

name :: P Name
name = Name <$> dotSep1 ident

ident :: P Ident
ident = Ident <$> identifier
