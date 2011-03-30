module Language.CSharp.Syntax where

import Data.ByteString (ByteString)
import Data.Text (Text)

------------------------------------------------------------------------
-- Top level

data CompilationUnit = CompilationUnit [Namespace]
    deriving (Eq, Show)

data Namespace = Namespace Name [TypeDecl]
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Declarations

data TypeDecl = Class [Modifier] Ident [Method]
    deriving (Eq, Show)

data Method = Method [Modifier] (Maybe Type) Ident [FormalParam] [Stmt]
    deriving (Eq, Show)

data FormalParam = FormalParam [ParamModifier] Type Ident
    deriving (Eq, Show)

data VarDecl = VarDecl Ident (Maybe VarInit)
    deriving (Eq, Show)

data VarInit = InitExp Exp
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Statements

data Stmt = LocalVar LocalType [VarDecl]
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Expressions

data Exp = Lit Literal
    deriving (Eq, Show)

data Literal
    = Null
    | Bool Bool
    | Int ByteString
    | Real ByteString
    | Char Text
    | String Text
    | Verbatim Text
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Types

data LocalType = Type Type | Var
    deriving (Eq, Show)

data Type
    = UserType Ident
    | PrimType PrimType
    | ArrayType Type ArrayRank
    deriving (Eq, Show)

type ArrayRank = Int

data PrimType
    -- Value types
    = BoolT
    | SByteT
    | ByteT
    | ShortT
    | UShortT
    | IntT
    | UIntT
    | LongT
    | ULongT
    | CharT
    | FloatT
    | DoubleT
    | DecimalT
    -- Reference types
    | ObjectT
    | StringT
    | DynamicT
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Modifiers

data Modifier
    = New
    | Public
    | Protected
    | Internal
    | Private
    | Abstract
    | Virtual
    | Override
    | Static
    | Sealed
    | Extern
    | Unsafe
    deriving (Eq, Show)

data ParamModifier = Ref | Out | This
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Identifiers

data Ident = Ident Text
    deriving (Eq, Show)

data Name = Name [Ident]
    deriving (Eq, Show)
