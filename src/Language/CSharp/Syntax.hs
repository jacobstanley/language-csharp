module Language.CSharp.Syntax where

------------------------------------------------------------------------

data CompilationUnit = CompilationUnit [Namespace]
    deriving (Eq, Show)

data Namespace = Namespace Name [TypeDecl]
    deriving (Eq, Show)

data TypeDecl = Class [Modifier] Ident [Method]
    deriving (Eq, Show)

data Method = Method [Modifier] (Maybe Type) Ident [FormalParam] MethodBody
    deriving (Eq, Show)

data FormalParam = FormalParam [ParamModifier] Type Ident
    deriving (Eq, Show)

data MethodBody = MethodBody
    deriving (Eq, Show)

data Type = SimpleType SimpleType
    deriving (Eq, Show)

data SimpleType
    = Int
    | Double
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
    | Sealed
    | Static
    | Extern
    | Unsafe
    deriving (Eq, Show)

data ParamModifier = Ref | Out | This
    deriving (Eq, Show)

data Ident = Ident String
    deriving (Eq, Show)

data Name = Name [Ident]
    deriving (Eq, Show)
