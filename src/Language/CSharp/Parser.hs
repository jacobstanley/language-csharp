{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.CSharp.Parser
    ( parseCSharp
    , CompilationUnit (..)
    ) where

import           Control.Applicative hiding (many, (<|>))
import           Data.ByteString (ByteString)
import           Prelude hiding ((>>), (>>=))
import qualified Prelude as P ((>>), (>>=))
import           Text.Parsec
import           Text.Parsec.String (GenParser)
import           Text.Parsec.Pos
import qualified Text.Parsec as Parsec

import           Language.CSharp.Syntax
import           Language.CSharp.Tokens

-- A trick to allow >> and >>=, normally infixr 1, to be
-- used inside branches of <|>, which is declared as infixl 1.
-- There are no clashes with other operators of precedence 2.
(>>) = (P.>>)
(>>=) = (P.>>=)
infixr 2 >>, >>=

------------------------------------------------------------------------
-- Top level

type P = GenParser (L Token) ()

parseCSharp :: SourceName -> [L Token] -> Either ParseError CompilationUnit
parseCSharp name toks = runParser compilationUnit () name toks

compilationUnit :: P CompilationUnit
compilationUnit = CompilationUnit <$> many namespace

namespace :: P Namespace
namespace = do
    tok Tok_Namespace
    Namespace <$> name
              <*> braces (many typeDecl)

------------------------------------------------------------------------
-- Declarations

typeDecl :: P TypeDecl
typeDecl = withModifiers class_ <?> "type declaration"

class_ :: [Modifier] -> P TypeDecl
class_ ms = do
    tok Tok_Class
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
varInit = tok Tok_Assign >> InitExp <$> expression

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
literal = maybeToken $ \t -> case t of
    Tok_Null     -> Just Null
    Tok_True     -> Just (Bool True)
    Tok_False    -> Just (Bool False)
    Tok_IntLit i -> Just (Int i)
    _            -> Nothing

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
modifier = tok Tok_New       >> return New
       <|> tok Tok_Public    >> return Public
       <|> tok Tok_Protected >> return Protected
       <|> tok Tok_Internal  >> return Internal
       <|> tok Tok_Private   >> return Private
       <|> tok Tok_Abstract  >> return Abstract
       <|> tok Tok_Sealed    >> return Sealed
       <|> tok Tok_Static    >> return Static
       <|> tok Tok_Unsafe    >> return Unsafe
       <?> "modifier (e.g. public)"

paramModifier :: P ParamModifier
paramModifier = tok Tok_Ref  >> return Ref
            <|> tok Tok_Out  >> return Out
            <|> tok Tok_This >> return This

------------------------------------------------------------------------
-- Types

type_ :: P Type
type_ = SimpleType <$> simpleType

returnType :: P (Maybe Type)
returnType = tok Tok_Void >> return Nothing
         <|> Just <$> type_
         <?> "return type"

simpleType :: P SimpleType
simpleType = tok Tok_Bool    >> return BoolT
         <|> tok Tok_Sbyte   >> return SByteT
         <|> tok Tok_Short   >> return ShortT
         <|> tok Tok_Ushort  >> return UShortT
         <|> tok Tok_Int     >> return IntT
         <|> tok Tok_Uint    >> return UIntT
         <|> tok Tok_Long    >> return LongT
         <|> tok Tok_Ulong   >> return ULongT
         <|> tok Tok_Char    >> return CharT
         <|> tok Tok_Float   >> return FloatT
         <|> tok Tok_Double  >> return DoubleT
         <|> tok Tok_Decimal >> return DecimalT

------------------------------------------------------------------------
-- Names and identifiers

name :: P Name
name = Name <$> dotSep1 ident

ident :: P Ident
ident = maybeToken $ \t -> case t of
    Tok_Ident i -> Just (Ident i)
    _           -> Nothing

------------------------------------------------------------------------
-- Punctuation

braces, parens :: P a -> P a
braces = between (tok Tok_LBrace) (tok Tok_RBrace)
parens = between (tok Tok_LParen) (tok Tok_RParen)

semi, comma, dot :: P ()
semi  = tok Tok_Semi
comma = tok Tok_Comma
dot   = tok Tok_Dot

commaSep :: P a -> P [a]
commaSep p = p `sepBy` comma

dotSep1 :: P a -> P [a]
dotSep1 p = p `sepBy1` dot

------------------------------------------------------------------------
-- Token parsing

maybeToken :: (Token -> Maybe a) -> P a
maybeToken test = token showT posT testT
  where showT (L _ t) = show t
        posT  (L p _) = pos2sourcePos p
        testT (L _ t) = test t

tok :: Token -> P ()
tok t = maybeToken $ \r -> if r == t then Just () else Nothing

pos2sourcePos :: Pos -> SourcePos
pos2sourcePos (l, c) = newPos "" l c
