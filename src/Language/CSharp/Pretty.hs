{-# LANGUAGE FlexibleInstances #-}

module Language.CSharp.Pretty where

import Data.List (intersperse)
import Text.PrettyPrint
import Language.CSharp.Syntax

------------------------------------------------------------------------
-- Pretty typeclass

class Pretty a where
    pretty :: a -> Doc
    pretty = prettyPrec 0

    prettyPrec :: Int -> a -> Doc
    prettyPrec _ = pretty

render' :: Pretty a => a -> String
render' = render . pretty

------------------------------------------------------------------------
-- Top level

instance Pretty CompilationUnit where
    pretty (CompilationUnit ns) = vcatSep ns

instance Pretty Namespace where
    pretty (Namespace n ts) =
        text "namespace" <+> pretty n
        $+$ block (vcatSep ts)

------------------------------------------------------------------------
-- Declarations

instance Pretty TypeDecl where
    pretty (Class mds n ms) =
        hcatMap mds <+> text "class" <+> pretty n
        $+$ block (vcatSep ms)

instance Pretty Method where
    pretty (Method ms t n ps stmts) =
        hcatMap ms <+> pretty t <+> pretty n <> parens (hcatComma ps)
        $+$ block (vcatMap stmts)

instance Pretty FormalParam where
    pretty (FormalParam ms t n) =
        hcatMap ms <+> pretty t <+> pretty n

instance Pretty VarDecl where
    pretty (VarDecl n Nothing)  = pretty n
    pretty (VarDecl n (Just d)) =
        pretty n <+> equals <+> pretty d

instance Pretty VarInit where
    pretty (InitExp exp) = pretty exp

------------------------------------------------------------------------
-- Statements

instance Pretty Stmt where
    pretty (LocalVar t vds) = pretty t <+> hcatComma vds <> semi

------------------------------------------------------------------------
-- Expressions

instance Pretty Exp where
    pretty (Lit lit) = pretty lit

instance Pretty Literal where
    pretty (Bool True)  = text "true"
    pretty (Bool False) = text "false"
    pretty (Int n)      = integer n

------------------------------------------------------------------------
-- Types

instance Pretty (Maybe Type) where
    pretty Nothing  = text "void"
    pretty (Just t) = pretty t

instance Pretty Type where
    pretty (SimpleType st) = pretty st

instance Pretty SimpleType where
    pretty BoolT    = text "bool"
    pretty SByteT   = text "sbyte"
    pretty ByteT    = text "byte"
    pretty ShortT   = text "short"
    pretty UShortT  = text "ushort"
    pretty IntT     = text "int"
    pretty UIntT    = text "uint"
    pretty LongT    = text "long"
    pretty ULongT   = text "ulong"
    pretty CharT    = text "char"
    pretty FloatT   = text "float"
    pretty DoubleT  = text "double"
    pretty DecimalT = text "decimal"

instance Pretty Modifier where
    pretty New       = text "new"
    pretty Public    = text "public"
    pretty Protected = text "protected"
    pretty Internal  = text "internal"
    pretty Private   = text "private"
    pretty Abstract  = text "abstract"
    pretty Virtual   = text "virtual"
    pretty Override  = text "override"
    pretty Sealed    = text "sealed"
    pretty Static    = text "static"
    pretty Extern    = text "extern"
    pretty Unsafe    = text "unsafe"

instance Pretty ParamModifier where
    pretty Ref  = text "ref"
    pretty Out  = text "out"
    pretty This = text "this"

------------------------------------------------------------------------
-- Names and identifiers

instance Pretty Name where
    pretty (Name is) = hcat $ punctuate (char '.') $ map pretty is

instance Pretty Ident where
    pretty (Ident s) = text s

------------------------------------------------------------------------
-- Helpers

indent :: Int
indent = 4

block :: Doc -> Doc
block x = char '{'
      $+$ nest indent x
      $+$ char '}'

blank :: Doc
blank = nest (-10000) (text "")

vcatSep :: Pretty a => [a] -> Doc
vcatSep = vcat . intersperse blank . map pretty

vcatMap :: Pretty a => [a] -> Doc
vcatMap = vcat . map pretty

hcatMap :: Pretty a => [a] -> Doc
hcatMap = hcat . map pretty

hcatComma :: Pretty a => [a] -> Doc
hcatComma = hcatSep (comma <> space)

hcatSep :: Pretty a => Doc -> [a] -> Doc
hcatSep s xs = hcat $ intersperse s $ map pretty xs
