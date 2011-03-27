{
module Language.CSharp.Lexer
    ( L (..)
    , Token (..)
    , lexer
    ) where

import qualified Data.ByteString.Lazy.Char8 as L
import           Numeric
}

%wrapper "posn-bytestring"

-- C# actually defines a letter to be any character (or escape sequence)
-- from the Unicode classes Lu, Ll, Lt, Lm, Lo or Nl. Identifiers must
-- start with a letter or an underscore, but can then also contain
-- characters from the classes Mn, Mc, Nd, Pc or Cf.
$ident_start = [a-zA-Z_]
$ident_part  = [a-zA-Z_0-9]

$digit    = [0-9]
$hexdigit = [0-9a-fA-F]

@newline = [\n\r] | \r\n
@any     = . | @newline
@comment = "/*" @any* "*/"
         | "//" .* @newline

tokens :-

  $white+  ;
  @comment ;

  -- Keywords
  abstract   { constTok KW_Abstract   }
  as         { constTok KW_As         }
  base       { constTok KW_Base       }
  bool       { constTok KW_Bool       }
  break      { constTok KW_Break      }
  byte       { constTok KW_Byte       }
  case       { constTok KW_Case       }
  catch      { constTok KW_Catch      }
  char       { constTok KW_Char       }
  checked    { constTok KW_Checked    }
  class      { constTok KW_Class      }
  const      { constTok KW_Const      }
  continue   { constTok KW_Continue   }
  decimal    { constTok KW_Decimal    }
  default    { constTok KW_Default    }
  delegate   { constTok KW_Delegate   }
  do         { constTok KW_Do         }
  double     { constTok KW_Double     }
  else       { constTok KW_Else       }
  enum       { constTok KW_Enum       }
  event      { constTok KW_Event      }
  explicit   { constTok KW_Explicit   }
  extern     { constTok KW_Extern     }
  false      { constTok KW_False      }
  finally    { constTok KW_Finally    }
  fixed      { constTok KW_Fixed      }
  float      { constTok KW_Float      }
  for        { constTok KW_For        }
  foreach    { constTok KW_Foreach    }
  goto       { constTok KW_Goto       }
  if         { constTok KW_If         }
  implicit   { constTok KW_Implicit   }
  in         { constTok KW_In         }
  int        { constTok KW_Int        }
  interface  { constTok KW_Interface  }
  internal   { constTok KW_Internal   }
  is         { constTok KW_Is         }
  lock       { constTok KW_Lock       }
  long       { constTok KW_Long       }
  namespace  { constTok KW_Namespace  }
  new        { constTok KW_New        }
  null       { constTok KW_Null       }
  object     { constTok KW_Object     }
  operator   { constTok KW_Operator   }
  out        { constTok KW_Out        }
  override   { constTok KW_Override   }
  params     { constTok KW_Params     }
  private    { constTok KW_Private    }
  protected  { constTok KW_Protected  }
  public     { constTok KW_Public     }
  readonly   { constTok KW_Readonly   }
  ref        { constTok KW_Ref        }
  return     { constTok KW_Return     }
  sbyte      { constTok KW_Sbyte      }
  sealed     { constTok KW_Sealed     }
  short      { constTok KW_Short      }
  sizeof     { constTok KW_Sizeof     }
  stackalloc { constTok KW_Stackalloc }
  static     { constTok KW_Static     }
  string     { constTok KW_String     }
  struct     { constTok KW_Struct     }
  switch     { constTok KW_Switch     }
  this       { constTok KW_This       }
  throw      { constTok KW_Throw      }
  true       { constTok KW_True       }
  try        { constTok KW_Try        }
  typeof     { constTok KW_Typeof     }
  uint       { constTok KW_Uint       }
  ulong      { constTok KW_Ulong      }
  unchecked  { constTok KW_Unchecked  }
  unsafe     { constTok KW_Unsafe     }
  ushort     { constTok KW_Ushort     }
  using      { constTok KW_Using      }
  virtual    { constTok KW_Virtual    }
  void       { constTok KW_Void       }
  volatile   { constTok KW_Volatile   }
  while      { constTok KW_While      }

  \( { constTok OpenParen    }
  \) { constTok CloseParen   }
  \[ { constTok OpenBracket  }
  \] { constTok CloseBracket }
  \{ { constTok OpenBrace    }
  \} { constTok CloseBrace   }
  \; { constTok Semi         }
  \, { constTok Comma        }
  \. { constTok Dot          }

  $ident_start $ident_part* { wrap (IdentTok . L.unpack) }
{

wrap :: (str -> tok) -> AlexPosn -> str -> L tok
wrap f (AlexPn _ line col) s = L (line, col) (f s)

constTok = wrap . const

--parseDec = wrap $ parse Int (readSigned readDec) "parseDec"
--
--parse :: (a -> Token) -> ReadS a -> String -> L.ByteString -> Token
--parse mkTok read name s =
--    maybe (Error $ name ++ ": failed") mkTok (tryRead read s)
--
--tryRead :: ReadS a -> L.ByteString -> Maybe a
--tryRead f s = case f (L.unpack s) of
--    [(a, "")] -> Just a
--    _         -> Nothing

data L a = L Pos a
  deriving (Show, Eq)

-- (line, column)
type Pos = (Int, Int)

pos :: AlexPosn -> Pos
pos (AlexPn _ l c) = (l,c)

data Token
    -- Keywords
    = KW_Abstract
    | KW_As
    | KW_Base
    | KW_Bool
    | KW_Break
    | KW_Byte
    | KW_Case
    | KW_Catch
    | KW_Char
    | KW_Checked
    | KW_Class
    | KW_Const
    | KW_Continue
    | KW_Decimal
    | KW_Default
    | KW_Delegate
    | KW_Do
    | KW_Double
    | KW_Else
    | KW_Enum
    | KW_Event
    | KW_Explicit
    | KW_Extern
    | KW_False
    | KW_Finally
    | KW_Fixed
    | KW_Float
    | KW_For
    | KW_Foreach
    | KW_Goto
    | KW_If
    | KW_Implicit
    | KW_In
    | KW_Int
    | KW_Interface
    | KW_Internal
    | KW_Is
    | KW_Lock
    | KW_Long
    | KW_Namespace
    | KW_New
    | KW_Null
    | KW_Object
    | KW_Operator
    | KW_Out
    | KW_Override
    | KW_Params
    | KW_Private
    | KW_Protected
    | KW_Public
    | KW_Readonly
    | KW_Ref
    | KW_Return
    | KW_Sbyte
    | KW_Sealed
    | KW_Short
    | KW_Sizeof
    | KW_Stackalloc
    | KW_Static
    | KW_String
    | KW_Struct
    | KW_Switch
    | KW_This
    | KW_Throw
    | KW_True
    | KW_Try
    | KW_Typeof
    | KW_Uint
    | KW_Ulong
    | KW_Unchecked
    | KW_Unsafe
    | KW_Ushort
    | KW_Using
    | KW_Virtual
    | KW_Void
    | KW_Volatile
    | KW_While

    -- Punctuators
    | OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | OpenBrace
    | CloseBrace
    | Semi
    | Comma
    | Dot

    -- Identifiers
    | IdentTok String

  deriving (Eq, Show)

lexer :: L.ByteString -> [L Token]
lexer = alexScanTokens
}
