{-# LANGUAGE FlexibleInstances #-}

module Language.CSharp.Pretty where

import qualified Data.ByteString.Char8 as B
import           Data.List (intersperse)
import qualified Data.Text as T
import           Text.PrettyPrint
import           Language.CSharp.Syntax

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
    pretty (CompilationUnit ns) = vsep' ns

instance Pretty Namespace where
    pretty (Namespace n ts) =
        text "namespace" <+> pretty n
        $+$ block (vsep' ts)

------------------------------------------------------------------------
-- Declarations

instance Pretty TypeDecl where
    pretty (Class mds n ms) =
        hcat' mds <+> text "class" <+> pretty n
        $+$ block (vsep' ms)

instance Pretty Method where
    pretty (Method ms t n ps stmts) =
        hcat' ms <+> pretty t <+> pretty n <> parens (params ps)
        $+$ block (vcat' stmts)

instance Pretty FormalParam where
    pretty (FormalParam ms t n) =
        hcat' ms <+> pretty t <+> pretty n

instance Pretty VarDecl where
    pretty (VarDecl n Nothing)  = pretty n
    pretty (VarDecl n (Just d)) =
        pretty n <+> equals <+> pretty d

instance Pretty VarInit where
    pretty (InitExp exp) = pretty exp

------------------------------------------------------------------------
-- Statements

instance Pretty Stmt where
    pretty (LocalVar t vds) = pretty t <+> params vds <> semi

------------------------------------------------------------------------
-- Expressions

instance Pretty Exp where
    pretty (Lit lit) = pretty lit

instance Pretty Literal where
    pretty (Null)        = text "null"
    pretty (Bool True)   = text "true"
    pretty (Bool False)  = text "false"
    pretty (Int n)       = pretty n
    pretty (Real f)      = pretty f
    pretty (Char c)      = quotes (pretty c)
    pretty (String cs)   = doubleQuotes (pretty cs)
    pretty (Verbatim cs) = char '@' <> doubleQuotes (pretty cs)

------------------------------------------------------------------------
-- Types

instance Pretty (Maybe Type) where
    pretty Nothing  = text "void"
    pretty (Just t) = pretty t

instance Pretty LocalType where
    pretty (Type t) = pretty t
    pretty Var      = text "var"

instance Pretty Type where
    pretty (UserType t)    = pretty t
    pretty (PrimType t)    = pretty t
    pretty (ArrayType t r) = pretty t <> prettyRank r
      where
        prettyRank r = brackets (hcatSep comma $ replicate r empty)

instance Pretty PrimType where
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
    pretty ObjectT  = text "object"
    pretty StringT  = text "string"
    pretty DynamicT = text "dynamic"

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
    pretty (Name is) = hcatSep' (char '.') is

instance Pretty Ident where
    pretty (Ident s) = pretty s

------------------------------------------------------------------------
-- ByteString / Text

instance Pretty B.ByteString where
    pretty = text . B.unpack

instance Pretty T.Text where
    pretty = text . T.unpack

------------------------------------------------------------------------
-- Helpers

indent :: Int
indent = 4

block :: Doc -> Doc
block x = char '{'
      $+$ nest indent x
      $+$ char '}'

params :: Pretty a => [a] -> Doc
params = hcatSep' (comma <> space)

blank :: Doc
blank = nest (-1000) (text "")

vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

vsep' :: Pretty a => [a] -> Doc
vsep' = vsep . intersperse blank . map pretty

vcat' :: Pretty a => [a] -> Doc
vcat' = vcat . map pretty

hcat' :: Pretty a => [a] -> Doc
hcat' = hcat . map pretty

hcatSep :: Doc -> [Doc] -> Doc
hcatSep s = hcat . intersperse s

hcatSep' :: Pretty a => Doc -> [a] -> Doc
hcatSep' s = hcatSep s . map pretty
