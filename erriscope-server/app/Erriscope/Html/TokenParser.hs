{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
module Erriscope.Html.TokenParser
  ( makeTokenParser
  ) where

import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )
import Data.List ( nub, sort )

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Token (GenLanguageDef(..), GenTokenParser(..))

-- | This is copied out of the Parsec source with uses of `lexeme` removed so
-- that whitespace is not consumed.
makeTokenParser :: (Stream s m Char)
                => GenLanguageDef s u m -> GenTokenParser s u m
{-# INLINABLE makeTokenParser #-}
makeTokenParser languageDef
    = TokenParser{ identifier = identifier
                 , reserved = reserved
                 , operator = operator
                 , reservedOp = reservedOp

                 , charLiteral = charLiteral
                 , stringLiteral = stringLiteral
                 , natural = natural
                 , integer = integer
                 , float = float
                 , naturalOrFloat = naturalOrFloat
                 , decimal = decimal
                 , hexadecimal = hexadecimal
                 , octal = octal

                 , symbol = symbol
                 , lexeme = lexeme
                 , whiteSpace = whiteSpace

                 , parens = parens
                 , braces = braces
                 , angles = angles
                 , brackets = brackets
                 , squares = brackets
                 , semi = semi
                 , comma = comma
                 , colon = colon
                 , dot = dot
                 , semiSep = semiSep
                 , semiSep1 = semiSep1
                 , commaSep = commaSep
                 , commaSep1 = commaSep1
                 }
    where

    -----------------------------------------------------------
    -- Bracketing
    -----------------------------------------------------------
    parens p        = between (symbol "(") (symbol ")") p
    braces p        = between (symbol "{") (symbol "}") p
    angles p        = between (symbol "<") (symbol ">") p
    brackets p      = between (symbol "[") (symbol "]") p

    semi            = symbol ";"
    comma           = symbol ","
    dot             = symbol "."
    colon           = symbol ":"

    commaSep p      = sepBy p comma
    semiSep p       = sepBy p semi

    commaSep1 p     = sepBy1 p comma
    semiSep1 p      = sepBy1 p semi


    -----------------------------------------------------------
    -- Chars & Strings
    -----------------------------------------------------------
    charLiteral     = (between (char '\'')
                                      (char '\'' <?> "end of character")
                                      characterChar )
                    <?> "character"

    characterChar   = charLetter <|> charEscape
                    <?> "literal character"

    charEscape      = do{ _ <- char '\\'; escapeCode }
    charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))



    stringLiteral   = do{ str <- between (char '"')
                                         (char '"' <?> "end of string")
                                         (many stringChar)
                        ; return (foldr (maybe id (:)) "" str)
                        }
                      <?> "literal string"

    stringChar      =   do{ c <- stringLetter; return (Just c) }
                    <|> stringEscape
                    <?> "string character"

    stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape    = do{ _ <- char '\\'
                        ;     do{ _ <- escapeGap  ; return Nothing }
                          <|> do{ _ <- escapeEmpty; return Nothing }
                          <|> do{ esc <- escapeCode; return (Just esc) }
                        }

    escapeEmpty     = char '&'
    escapeGap       = do{ _ <- many1 space
                        ; char '\\' <?> "end of string gap"
                        }



    -- escape codes
    escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                    <?> "escape code"

    charControl     = do{ _ <- char '^'
                        ; code <- upper
                        ; return (toEnum (fromEnum code - fromEnum 'A' + 1))
                        }

    charNum         = do{ code <- decimal
                                  <|> do{ _ <- char 'o'; number 8 octDigit }
                                  <|> do{ _ <- char 'x'; number 16 hexDigit }
                        ; if code > 0x10FFFF
                          then fail "invalid escape sequence"
                          else return (toEnum (fromInteger code))
                        }

    charEsc         = choice (map parseEsc escMap)
                    where
                      parseEsc (c,code)     = do{ _ <- char c; return code }

    charAscii       = choice (map parseAscii asciiMap)
                    where
                      parseAscii (asc,code) = try (do{ _ <- string asc; return code })


    -- escape code tables
    escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
    asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"]
    ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"]

    ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP']
    ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


    -----------------------------------------------------------
    -- Numbers
    -----------------------------------------------------------
    naturalOrFloat  = natFloat <?> "number"

    float           = floating   <?> "float"
    integer         = int        <?> "integer"
    natural         = nat        <?> "natural"


    -- floats
    floating        = do{ n <- decimal
                        ; fractExponent n
                        }


    natFloat        = do{ _ <- char '0'
                        ; zeroNumFloat
                        }
                      <|> decimalFloat

    zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                         ; return (Left n)
                         }
                    <|> decimalFloat
                    <|> fractFloat (0 :: Integer)
                    <|> return (Left 0)

    decimalFloat    = do{ n <- decimal
                        ; option (Left n)
                                 (fractFloat n)
                        }

    fractFloat n    = do{ f <- fractExponent n
                        ; return (Right f)
                        }

    fractExponent n = do{ fract <- fraction
                        ; expo  <- option "" exponent'
                        ; readDouble (show n ++ fract ++ expo)
                        }
                    <|>
                      do{ expo <- exponent'
                        ; readDouble (show n ++ expo)
                        }
                      where
                        readDouble s =
                          case reads s of
                            [(x, "")] -> return x
                            _         -> parserZero

    fraction        = do{ _ <- char '.'
                        ; digits <- many1 digit <?> "fraction"
                        ; return ('.' : digits)
                        }
                      <?> "fraction"

    exponent'       = do{ _ <- oneOf "eE"
                        ; sign' <- fmap (:[]) (oneOf "+-") <|> return ""
                        ; e <- decimal <?> "exponent"
                        ; return ('e' : sign' ++ show e)
                        }
                      <?> "exponent"


    -- integers and naturals
    int             = do{ f <- sign
                        ; n <- nat
                        ; return (f n)
                        }

    sign            =   (char '-' >> return negate)
                    <|> (char '+' >> return id)
                    <|> return id

    nat             = zeroNumber <|> decimal

    zeroNumber      = do{ _ <- char '0'
                        ; hexadecimal <|> octal <|> decimal <|> return 0
                        }
                      <?> ""

    decimal         = number 10 digit
    hexadecimal     = do{ _ <- oneOf "xX"; number 16 hexDigit }
    octal           = do{ _ <- oneOf "oO"; number 8 octDigit  }

    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }

    -----------------------------------------------------------
    -- Operators & reserved ops
    -----------------------------------------------------------
    reservedOp name =
        try $
        do{ _ <- string name
          ; notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)
          }

    operator =
        try $
        do{ name <- oper
          ; if (isReservedOp name)
             then unexpected ("reserved operator " ++ show name)
             else return name
          }

    oper =
        do{ c <- (opStart languageDef)
          ; cs <- many (opLetter languageDef)
          ; return (c:cs)
          }
        <?> "operator"

    isReservedOp name =
        isReserved (sort (reservedOpNames languageDef)) name


    -----------------------------------------------------------
    -- Identifiers & Reserved words
    -----------------------------------------------------------
    reserved name =
        try $
        do{ _ <- caseString name
          ; notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)
          }

    caseString name
        | caseSensitive languageDef  = string name
        | otherwise               = do{ walk name; return name }
        where
          walk []     = return ()
          walk (c:cs) = do{ _ <- caseChar c <?> msg; walk cs }

          caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                      | otherwise  = char c

          msg         = show name


    identifier =
        try $
        do{ name <- ident
          ; if (isReservedName name)
             then unexpected ("reserved word " ++ show name)
             else return name
          }


    ident
        = do{ c <- identStart languageDef
            ; cs <- many (identLetter languageDef)
            ; return (c:cs)
            }
        <?> "identifier"

    isReservedName name
        = isReserved theReservedNames caseName
        where
          caseName      | caseSensitive languageDef  = name
                        | otherwise               = map toLower name


    isReserved names name
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

    theReservedNames
        | caseSensitive languageDef  = sort reserved'
        | otherwise                  = sort . map (map toLower) $ reserved'
        where
          reserved' = reservedNames languageDef



    -----------------------------------------------------------
    -- White space & symbols
    -----------------------------------------------------------
    symbol name
        = string name

    lexeme p
        = do{ x <- p; whiteSpace; return x  }


    --whiteSpace
    whiteSpace
        | noLine && noMulti  = skipMany (simpleSpace <?> "")
        | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
        | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
        | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
        where
          noLine  = null (commentLine languageDef)
          noMulti = null (commentStart languageDef)


    simpleSpace =
        skipMany1 (satisfy isSpace)

    oneLineComment =
        do{ _ <- try (string (commentLine languageDef))
          ; skipMany (satisfy (/= '\n'))
          ; return ()
          }

    multiLineComment =
        do { _ <- try (string (commentStart languageDef))
           ; inComment
           }

    inComment
        | nestedComments languageDef  = inCommentMulti
        | otherwise                = inCommentSingle

    inCommentMulti
        =   do{ _ <- try (string (commentEnd languageDef)) ; return () }
        <|> do{ multiLineComment                     ; inCommentMulti }
        <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
        <|> do{ _ <- oneOf startEnd                  ; inCommentMulti }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

    inCommentSingle
        =   do{ _ <- try (string (commentEnd languageDef)); return () }
        <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
        <|> do{ _ <- oneOf startEnd                 ; inCommentSingle }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)
