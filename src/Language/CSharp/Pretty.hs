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
    pretty (CompilationUnit ns) = vcatMap ns

instance Pretty Namespace where
    pretty (Namespace n ts) =
        text "namespace" <+> pretty n
        $+$ vcatBlock ts

instance Pretty TypeDecl where
    pretty (Class mds n ms) =
        hcatMap mds <+> text "class" <+> pretty n
        $+$ vcatBlock ms

instance Pretty Method where
    pretty (Method ms t n ps b) =
        hcatMap ms <+> pretty t <+> pretty n <> parens (hcatComma ps)
        $+$ pretty b

instance Pretty FormalParam where
    pretty (FormalParam ms t n) =
        hcatMap ms <+> pretty t <+> pretty n

instance Pretty MethodBody where
    pretty MethodBody = block empty

instance Pretty (Maybe Type) where
    pretty Nothing  = text "void"
    pretty (Just t) = pretty t

instance Pretty Type where
    pretty (SimpleType st) = pretty st

instance Pretty SimpleType where
    pretty Int    = text "int"
    pretty Double = text "double"

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

vcatBlock :: Pretty a => [a] -> Doc
vcatBlock = block . vcatMap

vcatMap :: Pretty a => [a] -> Doc
vcatMap = vcat . map pretty

hcatMap :: Pretty a => [a] -> Doc
hcatMap = hcat . map pretty

hcatComma :: Pretty a => [a] -> Doc
hcatComma = hcatSep (comma <> space)

hcatSep :: Pretty a => Doc -> [a] -> Doc
hcatSep s xs = hcat $ intersperse s $ map pretty xs
