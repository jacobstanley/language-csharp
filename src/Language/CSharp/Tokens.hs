module Language.CSharp.Tokens
    ( L (..)
    , Pos
    , Token (..)
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

------------------------------------------------------------------------
-- Position info

data L a = L Pos a
  deriving (Show, Eq)

-- (line, column)
type Pos = (Int, Int)

------------------------------------------------------------------------
-- Tokens

data Token
    -- Keywords
    = Tok_Abstract
    | Tok_As
    | Tok_Base
    | Tok_Bool
    | Tok_Break
    | Tok_Byte
    | Tok_Case
    | Tok_Catch
    | Tok_Char
    | Tok_Checked
    | Tok_Class
    | Tok_Const
    | Tok_Continue
    | Tok_Decimal
    | Tok_Default
    | Tok_Delegate
    | Tok_Do
    | Tok_Double
    | Tok_Else
    | Tok_Enum
    | Tok_Event
    | Tok_Explicit
    | Tok_Extern
    | Tok_False
    | Tok_Finally
    | Tok_Fixed
    | Tok_Float
    | Tok_For
    | Tok_Foreach
    | Tok_Goto
    | Tok_If
    | Tok_Implicit
    | Tok_In
    | Tok_Int
    | Tok_Interface
    | Tok_Internal
    | Tok_Is
    | Tok_Lock
    | Tok_Long
    | Tok_Namespace
    | Tok_New
    | Tok_Null
    | Tok_Object
    | Tok_Operator
    | Tok_Out
    | Tok_Override
    | Tok_Params
    | Tok_Private
    | Tok_Protected
    | Tok_Public
    | Tok_Readonly
    | Tok_Ref
    | Tok_Return
    | Tok_Sbyte
    | Tok_Sealed
    | Tok_Short
    | Tok_Sizeof
    | Tok_Stackalloc
    | Tok_Static
    | Tok_String
    | Tok_Struct
    | Tok_Switch
    | Tok_This
    | Tok_Throw
    | Tok_True
    | Tok_Try
    | Tok_Typeof
    | Tok_UInt
    | Tok_ULong
    | Tok_Unchecked
    | Tok_Unsafe
    | Tok_UShort
    | Tok_Using
    | Tok_Virtual
    | Tok_Void
    | Tok_Volatile
    | Tok_While

    -- Punctuators
    | Tok_LParen
    | Tok_RParen
    | Tok_LBracket
    | Tok_RBracket
    | Tok_LBrace
    | Tok_RBrace
    | Tok_Colon
    | Tok_Semi
    | Tok_Comma
    | Tok_Dot

    -- Operators
    | Tok_Plus
    | Tok_Minus
    | Tok_Star
    | Tok_Slash
    | Tok_Percent
    | Tok_Amp
    | Tok_Pipe
    | Tok_Caret
    | Tok_Not
    | Tok_Tilde
    | Tok_Assign
    | Tok_Question
    | Tok_Coalesce
    | Tok_NameQual
    | Tok_Inc
    | Tok_Dec
    | Tok_And
    | Tok_Or
    | Tok_Arrow
    | Tok_Eq
    | Tok_NotEq
    | Tok_Lt
    | Tok_LtEq
    | Tok_Gt
    | Tok_GtEq
    | Tok_ShiftL
    | Tok_ShiftR
    | Tok_AssPlus
    | Tok_AssMinus
    | Tok_AssStar
    | Tok_AssSlash
    | Tok_AssPercent
    | Tok_AssAmp
    | Tok_AssPipe
    | Tok_AssCaret
    | Tok_AssShiftL
    | Tok_AssShiftR
    | Tok_Lambda

    -- Identifiers
    | Tok_Ident Text

    -- Literals
    | Tok_IntLit ByteString
    | Tok_RealLit ByteString
    | Tok_CharLit Text
    | Tok_StringLit Text
    | Tok_VerbatimLit Text

  deriving (Eq, Show)
