{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.CSharp.Parser
    ( parseCSharp
    , CompilationUnit (..)
    ) where

import           Control.Applicative hiding (many, optional, (<|>))
import           Data.ByteString (ByteString)
import           Data.List (foldl')
import           Data.Text (Text)
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

class_ :: [Mod] -> P TypeDecl
class_ ms = do
    tok Tok_Class
    Class <$> return ms
          <*> ident
          <*> braces (many member)

member :: P Method
member = withModifiers method

method :: [Mod] -> P Method
method ms = Method <$> return ms
                   <*> returnType
                   <*> (ident <?> "method identifier")
                   <*> formalParams
                   <*> block
                   <?> "method declaration"

localVar :: P Stmt
localVar = LocalVar <$> localType
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
expression = primary

primary :: P Exp
primary = primary' `followedBy` primarySuffix

--------------------------------
-- Standard Expressions

primary' :: P Exp
primary' = Lit <$> literal
       <|> SimpleName <$> ident <*> typeArgs
       <|> ParenExp <$> parens expression
       <|> thisAccess

literal :: P Literal
literal = maybeToken $ \t -> case t of
    Tok_Null           -> Just Null
    Tok_True           -> Just (Bool True)
    Tok_False          -> Just (Bool False)
    Tok_IntLit n       -> Just (Int n)
    Tok_RealLit f      -> Just (Real f)
    Tok_CharLit c      -> Just (Char c)
    Tok_StringLit cs   -> Just (String cs)
    Tok_VerbatimLit cs -> Just (Verbatim cs)
    _                  -> Nothing

thisAccess :: P Exp
thisAccess = tok Tok_This *> pure ThisAccess

--------------------------------
-- Suffix Expressions

primarySuffix :: P (Exp -> Exp)
primarySuffix = memberAccess <|> invocation <|> elementAccess

memberAccess :: P (Exp -> Exp)
memberAccess = do
    i <- dot >> ident
    ts <- typeArgs
    return $ \e -> MemberAccess e i ts

invocation :: P (Exp -> Exp)
invocation = do
    as <- arguments
    return $ \e -> Invocation e as

elementAccess :: P (Exp -> Exp)
elementAccess = do
    idx <- brackets expression
    return $ \e -> ElementAccess e idx

arguments :: P [Arg]
arguments = parens (commaSep argument)

argument :: P Arg
argument = Arg <$> optionMaybe (try $ ident <* colon)
               <*> optionMaybe argModifier
               <*> expression

------------------------------------------------------------------------
-- Formal Parameters

formalParams :: P [FormalParam]
formalParams = parens (commaSep formalParam) <?> "formal parameter list"

formalParam :: P FormalParam
formalParam = FormalParam <$> optionMaybe paramModifier
                          <*> type_
                          <*> ident

------------------------------------------------------------------------
-- Modifiers

withModifiers :: ([Mod] -> P a) -> P a
withModifiers p = many modifier >>= p

modifier :: P Mod
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

paramModifier :: P ParamMod
paramModifier = tok Tok_Ref  >> return RefParam
            <|> tok Tok_Out  >> return OutParam
            <|> tok Tok_This >> return ThisParam

argModifier :: P ArgMod
argModifier = tok Tok_Ref >> return RefArg
          <|> tok Tok_Out >> return OutArg

------------------------------------------------------------------------
-- Types

localType :: P LocalType
localType = Type <$> type_
        <|> var >> return Var

returnType :: P (Maybe Type)
returnType = tok Tok_Void >> return Nothing
         <|> Just <$> type_
         <?> "return type"

typeArgs :: P [TypeArg]
typeArgs = option [] (angles $ commaSep1 type_)

type_ :: P Type
type_ = do
    t <- coreType
    rs <- many arrayRank
    return (foldl ArrayType t rs)
  where
    coreType = PrimType <$> primType
           <|> UserType <$> name <*> typeArgs

arrayRank :: P ArrayRank
arrayRank = do
    dims <- brackets $ many (tok Tok_Comma)
    return (length dims + 1)

primType :: P PrimType
primType = tok Tok_Bool    >> return BoolT
       <|> tok Tok_Sbyte   >> return SByteT
       <|> tok Tok_Short   >> return ShortT
       <|> tok Tok_UShort  >> return UShortT
       <|> tok Tok_Int     >> return IntT
       <|> tok Tok_UInt    >> return UIntT
       <|> tok Tok_Long    >> return LongT
       <|> tok Tok_ULong   >> return ULongT
       <|> tok Tok_Char    >> return CharT
       <|> tok Tok_Float   >> return FloatT
       <|> tok Tok_Double  >> return DoubleT
       <|> tok Tok_Decimal >> return DecimalT
       <|> tok Tok_Object  >> return ObjectT
       <|> tok Tok_String  >> return StringT
       <|> dynamic         >> return DynamicT

------------------------------------------------------------------------
-- Names and identifiers

name :: P Name
name = Name <$> dotSep1 ident

ident :: P Ident
ident = maybeToken $ \t -> case t of
    Tok_Ident i -> Just (Ident i)
    _           -> Nothing

------------------------------------------------------------------------
-- Contextual keywords

dynamic :: P ()
dynamic = keyword "dynamic"

var :: P ()
var = keyword "var"

keyword :: Text -> P ()
keyword = tok . Tok_Ident

------------------------------------------------------------------------
-- Punctuation

angles, braces, parens, brackets :: P a -> P a
angles   = between (tok Tok_Lt)       (tok Tok_Gt)
braces   = between (tok Tok_LBrace)   (tok Tok_RBrace)
parens   = between (tok Tok_LParen)   (tok Tok_RParen)
brackets = between (tok Tok_LBracket) (tok Tok_RBracket)

semi, comma, dot :: P ()
semi  = tok Tok_Semi
comma = tok Tok_Comma
dot   = tok Tok_Dot
colon = tok Tok_Colon

commaSep :: P a -> P [a]
commaSep p = p `sepBy` comma

commaSep1 :: P a -> P [a]
commaSep1 p = p `sepBy1` comma

dotSep1 :: P a -> P [a]
dotSep1 p = p `sepBy1` dot

followedBy :: P a -> P (a -> a) -> P a
followedBy p suffix = do
    x <- p
    ss <- many suffix
    return $ foldl' (\a s -> s a) x ss

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
