{
module Language.CSharp.Lexer
    ( L (..)
    , Token (..)
    , lexer
    ) where

import qualified Data.Text as T
import           Numeric
}

--%wrapper "posn-bytestring"

$any     = [.\n\r]
@newline = [\n\r] | \r\n
@comment = "/*" $any* "*/"
         | "//" .* @newline

@preprocessor = \# .* @newline

-- C# actually defines a letter to be any character (or escape sequence)
-- from the Unicode classes Lu, Ll, Lt, Lm, Lo or Nl. Identifiers must
-- start with a letter or an underscore, but can then also contain
-- characters from the classes Mn, Mc, Nd, Pc or Cf.
$ident_start = [a-zA-Z_\@]
$ident_part  = [a-zA-Z_0-9]

$digit     = [0-9]
$hex_digit = [0-9a-fA-F]
$sign      = [\+\-]

@int_suffix  = [uU][lL]? | [lL][uU]?
@real_suffix = [fFdDmM]
@exponent    = [eE] $sign? $digit+

@simple_escape  = \\ [0abfnrtv\'\"\\]
@hex_escape     = \\x $hex_digit{1,4}
@unicode_escape = \\u $hex_digit{4} | \\U $hex_digit{8}
@escapes        = @simple_escape | @hex_escape | @unicode_escape

@character          = [^\'\\] | @escapes
@string_character   = [^\"\\] | @escapes
@verbatim_character = $any # \" | \"\"

-- Not sure how to deal with these yet, so we'll just ignore them
--@bom = \xEF \xBB \xBF -- UTF-8
--     | \uFEFF         -- UTF-16BE
--     | \uFFFE         -- UTF-16LE

tokens :-

$white+       ;
\0+           ;
@comment      ;
@preprocessor ;
--@bom          ;

-- Keywords
abstract   { constTok Tok_Abstract   }
as         { constTok Tok_As         }
base       { constTok Tok_Base       }
bool       { constTok Tok_Bool       }
break      { constTok Tok_Break      }
byte       { constTok Tok_Byte       }
case       { constTok Tok_Case       }
catch      { constTok Tok_Catch      }
char       { constTok Tok_Char       }
checked    { constTok Tok_Checked    }
class      { constTok Tok_Class      }
const      { constTok Tok_Const      }
continue   { constTok Tok_Continue   }
decimal    { constTok Tok_Decimal    }
default    { constTok Tok_Default    }
delegate   { constTok Tok_Delegate   }
do         { constTok Tok_Do         }
double     { constTok Tok_Double     }
else       { constTok Tok_Else       }
enum       { constTok Tok_Enum       }
event      { constTok Tok_Event      }
explicit   { constTok Tok_Explicit   }
extern     { constTok Tok_Extern     }
false      { constTok Tok_False      }
finally    { constTok Tok_Finally    }
fixed      { constTok Tok_Fixed      }
float      { constTok Tok_Float      }
for        { constTok Tok_For        }
foreach    { constTok Tok_Foreach    }
goto       { constTok Tok_Goto       }
if         { constTok Tok_If         }
implicit   { constTok Tok_Implicit   }
in         { constTok Tok_In         }
int        { constTok Tok_Int        }
interface  { constTok Tok_Interface  }
internal   { constTok Tok_Internal   }
is         { constTok Tok_Is         }
lock       { constTok Tok_Lock       }
long       { constTok Tok_Long       }
namespace  { constTok Tok_Namespace  }
new        { constTok Tok_New        }
null       { constTok Tok_Null       }
object     { constTok Tok_Object     }
operator   { constTok Tok_Operator   }
out        { constTok Tok_Out        }
override   { constTok Tok_Override   }
params     { constTok Tok_Params     }
private    { constTok Tok_Private    }
protected  { constTok Tok_Protected  }
public     { constTok Tok_Public     }
readonly   { constTok Tok_Readonly   }
ref        { constTok Tok_Ref        }
return     { constTok Tok_Return     }
sbyte      { constTok Tok_Sbyte      }
sealed     { constTok Tok_Sealed     }
short      { constTok Tok_Short      }
sizeof     { constTok Tok_Sizeof     }
stackalloc { constTok Tok_Stackalloc }
static     { constTok Tok_Static     }
string     { constTok Tok_String     }
struct     { constTok Tok_Struct     }
switch     { constTok Tok_Switch     }
this       { constTok Tok_This       }
throw      { constTok Tok_Throw      }
true       { constTok Tok_True       }
try        { constTok Tok_Try        }
typeof     { constTok Tok_Typeof     }
uint       { constTok Tok_Uint       }
ulong      { constTok Tok_Ulong      }
unchecked  { constTok Tok_Unchecked  }
unsafe     { constTok Tok_Unsafe     }
ushort     { constTok Tok_Ushort     }
using      { constTok Tok_Using      }
virtual    { constTok Tok_Virtual    }
void       { constTok Tok_Void       }
volatile   { constTok Tok_Volatile   }
while      { constTok Tok_While      }

-- Punctuators
\( { constTok Tok_LParen   }
\) { constTok Tok_RParen   }
\[ { constTok Tok_LBracket }
\] { constTok Tok_RBracket }
\{ { constTok Tok_LBrace   }
\} { constTok Tok_RBrace   }
\: { constTok Tok_Colon    }
\; { constTok Tok_Semi     }
\, { constTok Tok_Comma    }
\. { constTok Tok_Dot      }

-- Operators
\+     { constTok Tok_Plus       }
\-     { constTok Tok_Minus      }
\*     { constTok Tok_Star       }
\/     { constTok Tok_Slash      }
\%     { constTok Tok_Percent    }
\&     { constTok Tok_Amp        }
\|     { constTok Tok_Pipe       }
\^     { constTok Tok_Caret      }
\!     { constTok Tok_Not        }
\~     { constTok Tok_Tilde      }
\=     { constTok Tok_Assign     }
\?     { constTok Tok_Question   }
\?\?   { constTok Tok_Coalesce   }
\:\:   { constTok Tok_NameQual   }
\+\+   { constTok Tok_Inc        }
\-\-   { constTok Tok_Dec        }
\&\&   { constTok Tok_And        }
\|\|   { constTok Tok_Or         }
\-\>   { constTok Tok_Arrow      }
\=\=   { constTok Tok_Eq         }
\!\=   { constTok Tok_NotEq      }
\<     { constTok Tok_Lt         }
\<\=   { constTok Tok_LtEq       }
\>     { constTok Tok_Gt         }
\>\=   { constTok Tok_GtEq       }
\<\<   { constTok Tok_ShiftL     }
\>\>   { constTok Tok_ShiftR     }
\+\=   { constTok Tok_AssPlus    }
\-\=   { constTok Tok_AssMinus   }
\*\=   { constTok Tok_AssStar    }
\/\=   { constTok Tok_AssSlash   }
\%\=   { constTok Tok_AssPercent }
\&\=   { constTok Tok_AssAmp     }
\|\=   { constTok Tok_AssPipe    }
\^\=   { constTok Tok_AssCaret   }
\<\<\= { constTok Tok_AssShiftL  }
\>\>\= { constTok Tok_AssShiftR  }
\=\>   { constTok Tok_Lambda     }

-- Integer literals
      $digit+     @int_suffix? { stringTok Tok_IntLit }
0[xX] $hex_digit+ @int_suffix? { stringTok Tok_IntLit }

-- Real literals
$digit+ \. $digit+ @exponent? @real_suffix? { stringTok Tok_RealLit }
        \. $digit+ @exponent? @real_suffix? { stringTok Tok_RealLit }
           $digit+ @exponent  @real_suffix? { stringTok Tok_RealLit }
           $digit+            @real_suffix  { stringTok Tok_RealLit }

-- Character / String literals
\' @character \'             { stringTok (Tok_CharLit . drop 1 . init)     }
\" @string_character* \"     { stringTok (Tok_StringLit . drop 1 . init)   }
\@\" @verbatim_character* \" { stringTok (Tok_VerbatimLit . drop 2 . init) }

-- Identifiers
$ident_start $ident_part* { stringTok Tok_Ident }
{

wrap :: (str -> tok) -> AlexPosn -> str -> L tok
wrap f (AlexPn _ line col) s = L (line, col) (f s)

constTok = wrap . const
stringTok f = wrap (f . T.unpack)
--stringTok f = wrap f

data L a = L Pos a
  deriving (Show, Eq)

-- (line, column)
type Pos = (Int, Int)

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
    | Tok_Uint
    | Tok_Ulong
    | Tok_Unchecked
    | Tok_Unsafe
    | Tok_Ushort
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
    | Tok_Ident String

    -- Literals
    | Tok_IntLit String
    | Tok_RealLit String
    | Tok_CharLit String
    | Tok_StringLit String
    | Tok_VerbatimLit String

  deriving (Eq, Show)

--lexer :: String -> L.ByteString -> [L Token]
lexer :: String -> T.Text -> [L Token]
lexer file str = go (alexStartPos,'\n',str)
  where
    go inp@(pos,_,str) = case alexScan inp 0 of
        AlexEOF                -> []
        AlexError (p,_,_)      -> error (errMsg p)
        AlexSkip  inp' len     -> go inp'
        AlexToken inp' len act -> act pos (T.take (fromIntegral len) str) : go inp'
        --AlexToken inp' len act -> act pos (take len str) : go inp'

    errMsg (AlexPn _ l c) = file ++ ": lexical error (line " ++ show l ++ ", col " ++ show c ++ ")  "
                         ++ "start: " ++ show (T.unpack $ T.take 4 str)

-----------------------------------------------------------

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  T.Text)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,cs) | T.null cs = Nothing
                     | otherwise = let c   = T.head cs
                                       cs' = T.tail cs
                                       p'  = alexMove p c
                                    in p' `seq` cs' `seq` Just (c, (p', c, cs'))

-----------------------------------------------------------

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

}
