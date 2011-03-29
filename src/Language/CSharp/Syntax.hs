module Language.CSharp.Syntax where

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

data Stmt = LocalVar Type [VarDecl]
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Expressions

data Exp = Lit Literal
    deriving (Eq, Show)

data Literal
    = Null
    | Bool Bool
    | Int String
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Types

data Type = SimpleType SimpleType
    deriving (Eq, Show)

data SimpleType
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
    | VarT
    deriving (Eq, Show)

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

data Ident = Ident String
    deriving (Eq, Show)

data Name = Name [Ident]
    deriving (Eq, Show)
