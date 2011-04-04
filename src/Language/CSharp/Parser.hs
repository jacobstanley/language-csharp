{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.CSharp.Parser
    ( parseCSharp
    , CompilationUnit (..)
    ) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.List (foldl')
import           Data.Text (Text)
import           Text.Parsec hiding (many, optional, (<|>))
import           Text.Parsec.String (GenParser)
import           Text.Parsec.Pos
import qualified Text.Parsec as Parsec

import           Language.CSharp.Syntax
import           Language.CSharp.Tokens

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
    Class <$> pure ms
          <*> ident
          <*> braces (many member)

member :: P Method
member = withModifiers method

method :: [Mod] -> P Method
method ms = Method <$> pure ms
                   <*> returnType
                   <*> (ident <?> "method identifier")
                   <*> formalParams
                   <*> block
                   <?> "method declaration"

localVar :: P Stmt
localVar = LocalVar <$> localType
                    <*> varDecl `sepBy` comma

varDecl :: P VarDecl
varDecl = VarDecl <$> ident
                  <*> optionMaybe varInit

varInit :: P VarInit
varInit = VarInitExp <$> (tok Tok_Assign *> expression)

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
       <|> baseAccess
       <|> objectCreation

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

baseAccess :: P Exp
baseAccess = tok Tok_Base *> access
  where
    access = BaseElement <$> brackets (expression `sepBy1` comma)
         <|> BaseMember  <$> (dot *> ident)

--------------------------------
-- Object Creation

objectCreation :: P Exp
objectCreation = do
    tok Tok_New
    t <- type_
    as <- optional arguments
    mi <- initializer as
    return $ ObjectCreation t (maybe [] id as) mi
  where
    initializer Nothing  = Just <$> objectInitializer
    initializer (Just _) = optional objectInitializer

objectInitializer :: P ObjectInit
objectInitializer = ObjectInit <$> try (braces $ memberInitializer `sepEndBy` comma)
                <|> CollectionInit <$> braces (elementInitializer `sepEndBy` comma)

memberInitializer :: P MemberInit
memberInitializer = MemberInit <$> ident <*> (assign *> initializerValue)

initializerValue :: P InitVal
initializerValue = InitObject <$> objectInitializer
               <|> InitVal <$> expression

elementInitializer :: P ElementInit
elementInitializer = ElementInit <$> braces (expression `sepBy` comma)
                 <|> ElementInit <$> (return <$> expression)

--------------------------------
-- Suffix Expressions

primarySuffix :: P (Exp -> Exp)
primarySuffix = memberAccess
            <|> invocation
            <|> elementAccess
            <|> postIncrement
            <|> postDecrement

memberAccess :: P (Exp -> Exp)
memberAccess = do
    i <- dot *> ident
    ts <- typeArgs
    return $ \e -> MemberAccess e i ts

invocation :: P (Exp -> Exp)
invocation = do
    as <- arguments
    return $ \e -> Invocation e as

elementAccess :: P (Exp -> Exp)
elementAccess = do
    ixs <- brackets (expression `sepBy1` comma)
    return $ \e -> ElementAccess e ixs

postIncrement :: P (Exp -> Exp)
postIncrement = tok Tok_Increment *> pure PostIncrement

postDecrement :: P (Exp -> Exp)
postDecrement = tok Tok_Decrement *> pure PostDecrement

------------------------------------------------------------------------
-- Formal Parameters / Invocation Arguments

formalParams :: P [FormalParam]
formalParams = parens (formalParam `sepBy` comma) <?> "formal parameter list"

formalParam :: P FormalParam
formalParam = FormalParam <$> optionMaybe paramModifier
                          <*> type_
                          <*> ident

arguments :: P [Arg]
arguments = parens (argument `sepBy` comma)

argument :: P Arg
argument = Arg <$> optionMaybe (try $ ident <* colon)
               <*> optionMaybe argModifier
               <*> expression

------------------------------------------------------------------------
-- Modifiers

withModifiers :: ([Mod] -> P a) -> P a
withModifiers p = many modifier >>= p

modifier :: P Mod
modifier = tok Tok_New       *> pure New
       <|> tok Tok_Public    *> pure Public
       <|> tok Tok_Protected *> pure Protected
       <|> tok Tok_Internal  *> pure Internal
       <|> tok Tok_Private   *> pure Private
       <|> tok Tok_Abstract  *> pure Abstract
       <|> tok Tok_Sealed    *> pure Sealed
       <|> tok Tok_Static    *> pure Static
       <|> tok Tok_Unsafe    *> pure Unsafe
       <?> "modifier (e.g. public)"

paramModifier :: P ParamMod
paramModifier = tok Tok_Ref  *> pure RefParam
            <|> tok Tok_Out  *> pure OutParam
            <|> tok Tok_This *> pure ThisParam

argModifier :: P ArgMod
argModifier = tok Tok_Ref *> pure RefArg
          <|> tok Tok_Out *> pure OutArg

------------------------------------------------------------------------
-- Types

localType :: P LocalType
localType = Type <$> type_
        <|> var *> pure Var

returnType :: P (Maybe Type)
returnType = tok Tok_Void *> pure Nothing
         <|> Just <$> type_
         <?> "return type"

typeArgs :: P [TypeArg]
typeArgs = option [] (angles $ type_ `sepBy1` comma)

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
primType = tok Tok_Bool    *> pure BoolT
       <|> tok Tok_Sbyte   *> pure SByteT
       <|> tok Tok_Short   *> pure ShortT
       <|> tok Tok_UShort  *> pure UShortT
       <|> tok Tok_Int     *> pure IntT
       <|> tok Tok_UInt    *> pure UIntT
       <|> tok Tok_Long    *> pure LongT
       <|> tok Tok_ULong   *> pure ULongT
       <|> tok Tok_Char    *> pure CharT
       <|> tok Tok_Float   *> pure FloatT
       <|> tok Tok_Double  *> pure DoubleT
       <|> tok Tok_Decimal *> pure DecimalT
       <|> tok Tok_Object  *> pure ObjectT
       <|> tok Tok_String  *> pure StringT
       <|> dynamic         *> pure DynamicT

------------------------------------------------------------------------
-- Names and identifiers

name :: P Name
name = Name <$> ident `sepBy1` dot

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

semi, comma, dot, colon, assign :: P ()
semi   = tok Tok_Semi
comma  = tok Tok_Comma
dot    = tok Tok_Dot
colon  = tok Tok_Colon
assign = tok Tok_Assign

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
