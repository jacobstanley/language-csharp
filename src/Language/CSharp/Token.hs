module Language.CSharp.Token
    ( identifier
    , reserved
    , reservedOp
    , natural
    , braces
    , parens
    , commaSep
    , dotSep1
    , dot
    , semi
    ) where

import           Control.Monad.Identity (Identity)
import           Data.ByteString (ByteString)
import           Prelude hiding ((>>), (>>=))
import           Text.Parsec
import           Text.Parsec.ByteString
import qualified Text.Parsec.Token as T

------------------------------------------------------------------------

type P = GenParser ByteString ()

identifier :: P String
identifier = T.identifier csharp

reserved :: String -> P ()
reserved = T.reserved csharp

reservedOp :: String -> P ()
reservedOp = T.reservedOp csharp

natural :: P Integer
natural = T.natural csharp

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

semi :: P String
semi = T.semi csharp

------------------------------------------------------------------------

csharp :: T.GenTokenParser ByteString st Identity
csharp = T.makeTokenParser csharpDef

csharpDef :: T.GenLanguageDef ByteString st Identity
csharpDef = T.LanguageDef
    { T.commentStart   = "/*"
    , T.commentEnd     = "*/"
    , T.commentLine    = "//"
    , T.nestedComments = True
    , T.identStart     = letter <|> char '_'
    , T.identLetter    = alphaNum <|> char '_'
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
    , T.caseSensitive   = True
    }
