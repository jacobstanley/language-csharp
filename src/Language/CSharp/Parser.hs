{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.CSharp.Parser
    ( compilationUnit
    ) where

import           Control.Applicative hiding (many, (<|>))
import           Control.Monad.Identity (Identity)
import           Data.ByteString (ByteString)
import           Prelude hiding ((>>), (>>=))
import qualified Prelude as P ((>>), (>>=))
import           Text.Parsec
import           Text.Parsec.ByteString
import qualified Text.Parsec.Token as T

import           Language.CSharp.Syntax

-- A trick to allow >> and >>=, normally infixr 1, to be
-- used inside branches of <|>, which is declared as infixl 1.
-- There are no clashes with other operators of precedence 2.
(>>) = (P.>>)
(>>=) = (P.>>=)
infixr 2 >>, >>=

------------------------------------------------------------------------
-- Parser

type P = GenParser ByteString ()

compilationUnit :: P CompilationUnit
compilationUnit = CompilationUnit <$> many namespace

namespace :: P Namespace
namespace = do
    reserved "namespace"
    Namespace <$> name
              <*> braces (many typeDecl)

typeDecl :: P TypeDecl
typeDecl = withModifiers class_ <?> "type declaration"

class_ :: [Modifier] -> P TypeDecl
class_ ms = do
    reserved "class"
    Class <$> return ms
          <*> identifier
          <*> braces (many member)

member :: P Method
member = withModifiers method

method :: [Modifier] -> P Method
method ms = Method <$> return ms
                   <*> returnType
                   <*> (identifier <?> "method identifier")
                   <*> formalParams
                   <*> methodBody
                   <?> "method declaration"

methodBody :: P MethodBody
methodBody = braces (T.whiteSpace csharp) >> return MethodBody

------------------------------------------------------------------------
-- Formal Parameters

formalParams :: P [FormalParam]
formalParams = parens (commaSep formalParam) <?> "formal parameter list"

formalParam :: P FormalParam
formalParam = FormalParam <$> many paramModifier
                          <*> type_
                          <*> identifier

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
simpleType = reserved "int"    >> return Int
         <|> reserved "double" >> return Double

------------------------------------------------------------------------
-- General C# token parsers

name :: P Name
name = Name <$> dotSep1 identifier

identifier :: P Ident
identifier = Ident <$> T.identifier csharp

reserved :: String -> P ()
reserved = T.reserved csharp

braces :: P a -> P a
braces = T.braces csharp

parens :: P a -> P a
parens = T.parens csharp

commaSep :: P a -> P [a]
commaSep = T.commaSep csharp

dotSep1 :: P a -> P [a]
dotSep1 p = sepBy1 p dot

dot :: P String
dot = T.dot csharp

lexeme :: P a -> P a
lexeme = T.lexeme csharp

------------------------------------------------------------------------
-- Lexer

csharp :: T.GenTokenParser ByteString st Identity
csharp = T.makeTokenParser csharpDef

csharpDef :: T.GenLanguageDef ByteString st Identity
csharpDef = T.LanguageDef
    { T.commentStart   = "/*"
    , T.commentEnd	   = "*/"
    , T.commentLine	   = "//"
    , T.nestedComments = True
    , T.identStart	   = letter <|> char '_'
    , T.identLetter	   = alphaNum <|> char '_'
    , T.opStart        = T.opLetter csharpDef
    , T.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , T.reservedNames  =
        [ "abstract" , "as" , "base" , "bool" , "break"
        , "byte" , "case" , "catch" , "char" , "checked"
        , "class" , "const" , "continue" , "decimal" , "default"
        , "delegate" , "do" , "double" , "else" , "enum"
        , "event" , "explicit" , "extern" , "false" , "finally"
        , "fixed" , "float" , "for" , "foreach" , "goto"
        , "if" , "implicit" , "in" , "int" , "interface"
        , "internal" , "is" , "lock" , "long" , "namespace"
        , "new" , "null" , "object" , "operator" , "out"
        , "override" , "params" , "private" , "protected" , "public"
        , "readonly" , "ref" , "return" , "sbyte" , "sealed"
        , "short" , "sizeof" , "stackalloc" , "static" , "string"
        , "struct" , "switch" , "this" , "throw" , "true"
        , "try" , "typeof" , "uint" , "ulong" , "unchecked"
        , "unsafe" , "ushort" , "using" , "virtual" , "void"
        , "volatile" , "while"]
    , T.reservedOpNames = []
    , T.caseSensitive = True
    }
