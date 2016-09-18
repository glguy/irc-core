{-# Language OverloadedStrings #-}
{-|
Module      : Digraphs
Description : Character mnemonics
Copyright   : (c) Eric Mertens, 2016
License     : ISC
Maintainer  : emertens@gmail.com

This module provides an implementation of /digraphs/ as implemented
in Vim and as specified in RFC 1345 (2-character only).

<https://tools.ietf.org/html/rfc1345>
<http://vimdoc.sourceforge.net/htmldoc/digraph.html>

-}
module Digraphs
  ( lookupDigraph
  , digraphs
  , digraphListToList
  ) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Char


-- | Look up 2-character digraph.
lookupDigraph :: Char -> Char -> Maybe Char
lookupDigraph x y
  | isAscii x, isAscii y = digraphToInt x y `IntMap.lookup` digraphMap
  | otherwise            = Nothing


digraphMap :: IntMap Char
digraphMap = IntMap.fromList
               [ (digraphToInt x y, z) | (x,y,z) <- digraphListToList digraphs ]


-- | Injective mapping from 2-character ASCII digraph to 'Int'
digraphToInt :: Char -> Char -> Int
digraphToInt x y = ord x * 127 + ord y

------------------------------------------------------------------------

digraphListToList :: DigraphList -> [(Char,Char,Char)]
digraphListToList (Entry x y z xs) = (x,y,z) : digraphListToList xs
digraphListToList Nil              = []


-- | Packed list of digraph entries
data DigraphList
  = Entry {-# UNPACK #-}!Char {-# UNPACK #-}!Char {-# UNPACK #-}!Char DigraphList
  | Nil


digraphs :: DigraphList
digraphs
  = Entry 'N' 'U' '\x00'      -- NULL (NUL)
  $ Entry 'S' 'H' '\x01'      -- START OF HEADING (SOH)
  $ Entry 'S' 'X' '\x02'      -- START OF TEXT (STX)
  $ Entry 'E' 'X' '\x03'      -- END OF TEXT (ETX)
  $ Entry 'E' 'T' '\x04'      -- END OF TRANSMISSION (EOT)
  $ Entry 'E' 'Q' '\x05'      -- ENQUIRY (ENQ)
  $ Entry 'A' 'K' '\x06'      -- ACKNOWLEDGE (ACK)
  $ Entry 'B' 'L' '\x07'      -- BELL (BEL)
  $ Entry 'B' 'S' '\x08'      -- BACKSPACE (BS)
  $ Entry 'H' 'T' '\x09'      -- CHARACTER TABULATION (HT)
--  $ Entry 'L' 'F' '\x0a'    -- LINE FEED (LF)
  $ Entry 'V' 'T' '\x0b'      -- LINE TABULATION (VT)
  $ Entry 'F' 'F' '\x0c'      -- FORM FEED (FF)
--  $ Entry 'C' 'R' '\x0d'    -- CARRIAGE RETURN (CR)
  $ Entry 'S' 'O' '\x0e'      -- SHIFT OUT (SO)
  $ Entry 'S' 'I' '\x0f'      -- SHIFT IN (SI)
  $ Entry 'D' 'L' '\x10'      -- DATALINK ESCAPE (DLE)
  $ Entry 'D' '1' '\x11'      -- DEVICE CONTROL ONE (DC1)
  $ Entry 'D' '2' '\x12'      -- DEVICE CONTROL TWO (DC2)
  $ Entry 'D' '3' '\x13'      -- DEVICE CONTROL THREE (DC3)
  $ Entry 'D' '4' '\x14'      -- DEVICE CONTROL FOUR (DC4)
  $ Entry 'N' 'K' '\x15'      -- NEGATIVE ACKNOWLEDGE (NAK)
  $ Entry 'S' 'Y' '\x16'      -- SYNCHRONOUS IDLE (SYN)
  $ Entry 'E' 'B' '\x17'      -- END OF TRANSMISSION BLOCK (ETB)
  $ Entry 'C' 'N' '\x18'      -- CANCEL (CAN)
  $ Entry 'E' 'M' '\x19'      -- END OF MEDIUM (EM)
  $ Entry 'S' 'B' '\x1a'      -- SUBSTITUTE (SUB)
  $ Entry 'E' 'C' '\x1b'      -- ESCAPE (ESC)
  $ Entry 'F' 'S' '\x1c'      -- FILE SEPARATOR (IS4)
  $ Entry 'G' 'S' '\x1d'      -- GROUP SEPARATOR (IS3)
  $ Entry 'R' 'S' '\x1e'      -- RECORD SEPARATOR (IS2)
  $ Entry 'U' 'S' '\x1f'      -- UNIT SEPARATOR (IS1)
  $ Entry 'S' 'P' '\x20'      -- SPACE
  $ Entry 'N' 'b' '\x23'      -- NUMBER SIGN
  $ Entry 'D' 'O' '\x24'      -- DOLLAR SIGN
  $ Entry 'A' 't' '\x40'      -- COMMERCIAL AT
  $ Entry '<' '(' '\x5b'      -- LEFT SQUARE BRACKET
  $ Entry '/' '/' '\x5c'      -- REVERSE SOLIDUS
  $ Entry ')' '>' '\x5d'      -- RIGHT SQUARE BRACKET
  $ Entry '\'' '>' '\x5e'      -- CIRCUMFLEX ACCENT
  $ Entry '\'' '!' '\x60'      -- GRAVE ACCENT
  $ Entry '(' '!' '\x7b'      -- LEFT CURLY BRACKET
  $ Entry '!' '!' '\x7c'      -- VERTICAL LINE
  $ Entry '!' ')' '\x7d'      -- RIGHT CURLY BRACKET
  $ Entry '\'' '?' '\x7e'      -- TILDE
  $ Entry 'D' 'T' '\x7f'      -- DELETE (DEL)
  $ Entry 'P' 'A' '\x80'      -- PADDING CHARACTER (PAD)
  $ Entry 'H' 'O' '\x81'      -- HIGH OCTET PRESET (HOP)
  $ Entry 'B' 'H' '\x82'      -- BREAK PERMITTED HERE (BPH)
  $ Entry 'N' 'H' '\x83'      -- NO BREAK HERE (NBH)
  $ Entry 'I' 'N' '\x84'      -- INDEX (IND)
  $ Entry 'N' 'L' '\x85'      -- NEXT LINE (NEL)
  $ Entry 'S' 'A' '\x86'      -- START OF SELECTED AREA (SSA)
  $ Entry 'E' 'S' '\x87'      -- END OF SELECTED AREA (ESA)
  $ Entry 'H' 'S' '\x88'      -- CHARACTER TABULATION SET (HTS)
  $ Entry 'H' 'J' '\x89'      -- CHARACTER TABULATION WITH JUSTIFICATION (HTJ)
  $ Entry 'V' 'S' '\x8a'      -- LINE TABULATION SET (VTS)
  $ Entry 'P' 'D' '\x8b'      -- PARTIAL LINE FORWARD (PLD)
  $ Entry 'P' 'U' '\x8c'      -- PARTIAL LINE BACKWARD (PLU)
  $ Entry 'R' 'I' '\x8d'      -- REVERSE LINE FEED (RI)
  $ Entry 'S' '2' '\x8e'      -- SINGLE-SHIFT TWO (SS2)
  $ Entry 'S' '3' '\x8f'      -- SINGLE-SHIFT THREE (SS3)
  $ Entry 'D' 'C' '\x90'      -- DEVICE CONTROL STRING (DCS)
  $ Entry 'P' '1' '\x91'      -- PRIVATE USE ONE (PU1)
  $ Entry 'P' '2' '\x92'      -- PRIVATE USE TWO (PU2)
  $ Entry 'T' 'S' '\x93'      -- SET TRANSMIT STATE (STS)
  $ Entry 'C' 'C' '\x94'      -- CANCEL CHARACTER (CCH)
  $ Entry 'M' 'W' '\x95'      -- MESSAGE WAITING (MW)
  $ Entry 'S' 'G' '\x96'      -- START OF GUARDED AREA (SPA)
  $ Entry 'E' 'G' '\x97'      -- END OF GUARDED AREA (EPA)
  $ Entry 'S' 'S' '\x98'      -- START OF STRING (SOS)
  $ Entry 'G' 'C' '\x99'      -- SINGLE GRAPHIC CHARACTER INTRODUCER (SGCI)
  $ Entry 'S' 'C' '\x9a'      -- SINGLE CHARACTER INTRODUCER (SCI)
  $ Entry 'C' 'I' '\x9b'      -- CONTROL SEQUENCE INTRODUCER (CSI)
  $ Entry 'S' 'T' '\x9c'      -- STRING TERMINATOR (ST)
  $ Entry 'O' 'C' '\x9d'      -- OPERATING SYSTEM COMMAND (OSC)
  $ Entry 'P' 'M' '\x9e'      -- PRIVACY MESSAGE (PM)
  $ Entry 'A' 'C' '\x9f'      -- APPLICATION PROGRAM COMMAND (APC)
  $ Entry 'N' 'S' '\xa0'      -- NO-BREAK SPACE
  $ Entry '!' 'I' '\xa1'      -- INVERTED EXCLAMATION MARK
  $ Entry 'C' 't' '\xa2'      -- CENT SIGN
  $ Entry 'P' 'd' '\xa3'      -- POUND SIGN
  $ Entry 'C' 'u' '\xa4'      -- CURRENCY SIGN
  $ Entry 'Y' 'e' '\xa5'      -- YEN SIGN
  $ Entry 'B' 'B' '\xa6'      -- BROKEN BAR
  $ Entry 'S' 'E' '\xa7'      -- SECTION SIGN
  $ Entry '\'' ':' '\xa8'      -- DIAERESIS
  $ Entry 'C' 'o' '\xa9'      -- COPYRIGHT SIGN
  $ Entry '-' 'a' '\xaa'      -- FEMININE ORDINAL INDICATOR
  $ Entry '<' '<' '\xab'      -- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
  $ Entry 'N' 'O' '\xac'      -- NOT SIGN
  $ Entry '-' '-' '\xad'      -- SOFT HYPHEN
  $ Entry 'R' 'g' '\xae'      -- REGISTERED SIGN
  $ Entry '\'' 'm' '\xaf'      -- MACRON
  $ Entry 'D' 'G' '\xb0'      -- DEGREE SIGN
  $ Entry '+' '-' '\xb1'      -- PLUS-MINUS SIGN
  $ Entry '2' 'S' '\xb2'      -- SUPERSCRIPT TWO
  $ Entry '3' 'S' '\xb3'      -- SUPERSCRIPT THREE
  $ Entry '\'' '\'' '\xb4'      -- ACUTE ACCENT
  $ Entry 'M' 'y' '\xb5'      -- MICRO SIGN
  $ Entry 'P' 'I' '\xb6'      -- PILCROW SIGN
  $ Entry '.' 'M' '\xb7'      -- MIDDLE DOT
  $ Entry '\'' ',' '\xb8'      -- CEDILLA
  $ Entry '1' 'S' '\xb9'      -- SUPERSCRIPT ONE
  $ Entry '-' 'o' '\xba'      -- MASCULINE ORDINAL INDICATOR
  $ Entry '>' '>' '\xbb'      -- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
  $ Entry '1' '4' '\xbc'      -- VULGAR FRACTION ONE QUARTER
  $ Entry '1' '2' '\xbd'      -- VULGAR FRACTION ONE HALF
  $ Entry '3' '4' '\xbe'      -- VULGAR FRACTION THREE QUARTERS
  $ Entry '?' 'I' '\xbf'      -- INVERTED QUESTION MARK
  $ Entry 'A' '!' '\xc0'      -- LATIN CAPITAL LETTER A WITH GRAVE
  $ Entry 'A' '\'' '\xc1'      -- LATIN CAPITAL LETTER A WITH ACUTE
  $ Entry 'A' '>' '\xc2'      -- LATIN CAPITAL LETTER A WITH CIRCUMFLEX
  $ Entry 'A' '?' '\xc3'      -- LATIN CAPITAL LETTER A WITH TILDE
  $ Entry 'A' ':' '\xc4'      -- LATIN CAPITAL LETTER A WITH DIAERESIS
  $ Entry 'A' 'A' '\xc5'      -- LATIN CAPITAL LETTER A WITH RING ABOVE
  $ Entry 'A' 'E' '\xc6'      -- LATIN CAPITAL LETTER AE
  $ Entry 'C' ',' '\xc7'      -- LATIN CAPITAL LETTER C WITH CEDILLA
  $ Entry 'E' '!' '\xc8'      -- LATIN CAPITAL LETTER E WITH GRAVE
  $ Entry 'E' '\'' '\xc9'      -- LATIN CAPITAL LETTER E WITH ACUTE
  $ Entry 'E' '>' '\xca'      -- LATIN CAPITAL LETTER E WITH CIRCUMFLEX
  $ Entry 'E' ':' '\xcb'      -- LATIN CAPITAL LETTER E WITH DIAERESIS
  $ Entry 'I' '!' '\xcc'      -- LATIN CAPITAL LETTER I WITH GRAVE
  $ Entry 'I' '\'' '\xcd'      -- LATIN CAPITAL LETTER I WITH ACUTE
  $ Entry 'I' '>' '\xce'      -- LATIN CAPITAL LETTER I WITH CIRCUMFLEX
  $ Entry 'I' ':' '\xcf'      -- LATIN CAPITAL LETTER I WITH DIAERESIS
  $ Entry 'D' '-' '\xd0'      -- LATIN CAPITAL LETTER ETH (Icelandic)
  $ Entry 'N' '?' '\xd1'      -- LATIN CAPITAL LETTER N WITH TILDE
  $ Entry 'O' '!' '\xd2'      -- LATIN CAPITAL LETTER O WITH GRAVE
  $ Entry 'O' '\'' '\xd3'      -- LATIN CAPITAL LETTER O WITH ACUTE
  $ Entry 'O' '>' '\xd4'      -- LATIN CAPITAL LETTER O WITH CIRCUMFLEX
  $ Entry 'O' '?' '\xd5'      -- LATIN CAPITAL LETTER O WITH TILDE
  $ Entry 'O' ':' '\xd6'      -- LATIN CAPITAL LETTER O WITH DIAERESIS
  $ Entry '*' 'X' '\xd7'      -- MULTIPLICATION SIGN
  $ Entry 'O' '/' '\xd8'      -- LATIN CAPITAL LETTER O WITH STROKE
  $ Entry 'U' '!' '\xd9'      -- LATIN CAPITAL LETTER U WITH GRAVE
  $ Entry 'U' '\'' '\xda'      -- LATIN CAPITAL LETTER U WITH ACUTE
  $ Entry 'U' '>' '\xdb'      -- LATIN CAPITAL LETTER U WITH CIRCUMFLEX
  $ Entry 'U' ':' '\xdc'      -- LATIN CAPITAL LETTER U WITH DIAERESIS
  $ Entry 'Y' '\'' '\xdd'      -- LATIN CAPITAL LETTER Y WITH ACUTE
  $ Entry 'T' 'H' '\xde'      -- LATIN CAPITAL LETTER THORN (Icelandic)
  $ Entry 's' 's' '\xdf'      -- LATIN SMALL LETTER SHARP S (German)
  $ Entry 'a' '!' '\xe0'      -- LATIN SMALL LETTER A WITH GRAVE
  $ Entry 'a' '\'' '\xe1'      -- LATIN SMALL LETTER A WITH ACUTE
  $ Entry 'a' '>' '\xe2'      -- LATIN SMALL LETTER A WITH CIRCUMFLEX
  $ Entry 'a' '?' '\xe3'      -- LATIN SMALL LETTER A WITH TILDE
  $ Entry 'a' ':' '\xe4'      -- LATIN SMALL LETTER A WITH DIAERESIS
  $ Entry 'a' 'a' '\xe5'      -- LATIN SMALL LETTER A WITH RING ABOVE
  $ Entry 'a' 'e' '\xe6'      -- LATIN SMALL LETTER AE
  $ Entry 'c' ',' '\xe7'      -- LATIN SMALL LETTER C WITH CEDILLA
  $ Entry 'e' '!' '\xe8'      -- LATIN SMALL LETTER E WITH GRAVE
  $ Entry 'e' '\'' '\xe9'      -- LATIN SMALL LETTER E WITH ACUTE
  $ Entry 'e' '>' '\xea'      -- LATIN SMALL LETTER E WITH CIRCUMFLEX
  $ Entry 'e' ':' '\xeb'      -- LATIN SMALL LETTER E WITH DIAERESIS
  $ Entry 'i' '!' '\xec'      -- LATIN SMALL LETTER I WITH GRAVE
  $ Entry 'i' '\'' '\xed'      -- LATIN SMALL LETTER I WITH ACUTE
  $ Entry 'i' '>' '\xee'      -- LATIN SMALL LETTER I WITH CIRCUMFLEX
  $ Entry 'i' ':' '\xef'      -- LATIN SMALL LETTER I WITH DIAERESIS
  $ Entry 'd' '-' '\xf0'      -- LATIN SMALL LETTER ETH (Icelandic)
  $ Entry 'n' '?' '\xf1'      -- LATIN SMALL LETTER N WITH TILDE
  $ Entry 'o' '!' '\xf2'      -- LATIN SMALL LETTER O WITH GRAVE
  $ Entry 'o' '\'' '\xf3'      -- LATIN SMALL LETTER O WITH ACUTE
  $ Entry 'o' '>' '\xf4'      -- LATIN SMALL LETTER O WITH CIRCUMFLEX
  $ Entry 'o' '?' '\xf5'      -- LATIN SMALL LETTER O WITH TILDE
  $ Entry 'o' ':' '\xf6'      -- LATIN SMALL LETTER O WITH DIAERESIS
  $ Entry '-' ':' '\xf7'      -- DIVISION SIGN
  $ Entry 'o' '/' '\xf8'      -- LATIN SMALL LETTER O WITH STROKE
  $ Entry 'u' '!' '\xf9'      -- LATIN SMALL LETTER U WITH GRAVE
  $ Entry 'u' '\'' '\xfa'      -- LATIN SMALL LETTER U WITH ACUTE
  $ Entry 'u' '>' '\xfb'      -- LATIN SMALL LETTER U WITH CIRCUMFLEX
  $ Entry 'u' ':' '\xfc'      -- LATIN SMALL LETTER U WITH DIAERESIS
  $ Entry 'y' '\'' '\xfd'      -- LATIN SMALL LETTER Y WITH ACUTE
  $ Entry 't' 'h' '\xfe'      -- LATIN SMALL LETTER THORN (Icelandic)
  $ Entry 'y' ':' '\xff'      -- LATIN SMALL LETTER Y WITH DIAERESIS
  $ Entry 'A' '-' '\x0100'    -- LATIN CAPITAL LETTER A WITH MACRON
  $ Entry 'a' '-' '\x0101'    -- LATIN SMALL LETTER A WITH MACRON
  $ Entry 'A' '(' '\x0102'    -- LATIN CAPITAL LETTER A WITH BREVE
  $ Entry 'a' '(' '\x0103'    -- LATIN SMALL LETTER A WITH BREVE
  $ Entry 'A' ';' '\x0104'    -- LATIN CAPITAL LETTER A WITH OGONEK
  $ Entry 'a' ';' '\x0105'    -- LATIN SMALL LETTER A WITH OGONEK
  $ Entry 'C' '\'' '\x0106'    -- LATIN CAPITAL LETTER C WITH ACUTE
  $ Entry 'c' '\'' '\x0107'    -- LATIN SMALL LETTER C WITH ACUTE
  $ Entry 'C' '>' '\x0108'    -- LATIN CAPITAL LETTER C WITH CIRCUMFLEX
  $ Entry 'c' '>' '\x0109'    -- LATIN SMALL LETTER C WITH CIRCUMFLEX
  $ Entry 'C' '.' '\x010A'    -- LATIN CAPITAL LETTER C WITH DOT ABOVE
  $ Entry 'c' '.' '\x010B'    -- LATIN SMALL LETTER C WITH DOT ABOVE
  $ Entry 'C' '<' '\x010C'    -- LATIN CAPITAL LETTER C WITH CARON
  $ Entry 'c' '<' '\x010D'    -- LATIN SMALL LETTER C WITH CARON
  $ Entry 'D' '<' '\x010E'    -- LATIN CAPITAL LETTER D WITH CARON
  $ Entry 'd' '<' '\x010F'    -- LATIN SMALL LETTER D WITH CARON
  $ Entry 'D' '/' '\x0110'    -- LATIN CAPITAL LETTER D WITH STROKE
  $ Entry 'd' '/' '\x0111'    -- LATIN SMALL LETTER D WITH STROKE
  $ Entry 'E' '-' '\x0112'    -- LATIN CAPITAL LETTER E WITH MACRON
  $ Entry 'e' '-' '\x0113'    -- LATIN SMALL LETTER E WITH MACRON
  $ Entry 'E' '(' '\x0114'    -- LATIN CAPITAL LETTER E WITH BREVE
  $ Entry 'e' '(' '\x0115'    -- LATIN SMALL LETTER E WITH BREVE
  $ Entry 'E' '.' '\x0116'    -- LATIN CAPITAL LETTER E WITH DOT ABOVE
  $ Entry 'e' '.' '\x0117'    -- LATIN SMALL LETTER E WITH DOT ABOVE
  $ Entry 'E' ';' '\x0118'    -- LATIN CAPITAL LETTER E WITH OGONEK
  $ Entry 'e' ';' '\x0119'    -- LATIN SMALL LETTER E WITH OGONEK
  $ Entry 'E' '<' '\x011A'    -- LATIN CAPITAL LETTER E WITH CARON
  $ Entry 'e' '<' '\x011B'    -- LATIN SMALL LETTER E WITH CARON
  $ Entry 'G' '>' '\x011C'    -- LATIN CAPITAL LETTER G WITH CIRCUMFLEX
  $ Entry 'g' '>' '\x011D'    -- LATIN SMALL LETTER G WITH CIRCUMFLEX
  $ Entry 'G' '(' '\x011E'    -- LATIN CAPITAL LETTER G WITH BREVE
  $ Entry 'g' '(' '\x011F'    -- LATIN SMALL LETTER G WITH BREVE
  $ Entry 'G' '.' '\x0120'    -- LATIN CAPITAL LETTER G WITH DOT ABOVE
  $ Entry 'g' '.' '\x0121'    -- LATIN SMALL LETTER G WITH DOT ABOVE
  $ Entry 'G' ',' '\x0122'    -- LATIN CAPITAL LETTER G WITH CEDILLA
  $ Entry 'g' ',' '\x0123'    -- LATIN SMALL LETTER G WITH CEDILLA
  $ Entry 'H' '>' '\x0124'    -- LATIN CAPITAL LETTER H WITH CIRCUMFLEX
  $ Entry 'h' '>' '\x0125'    -- LATIN SMALL LETTER H WITH CIRCUMFLEX
  $ Entry 'H' '/' '\x0126'    -- LATIN CAPITAL LETTER H WITH STROKE
  $ Entry 'h' '/' '\x0127'    -- LATIN SMALL LETTER H WITH STROKE
  $ Entry 'I' '?' '\x0128'    -- LATIN CAPITAL LETTER I WITH TILDE
  $ Entry 'i' '?' '\x0129'    -- LATIN SMALL LETTER I WITH TILDE
  $ Entry 'I' '-' '\x012A'    -- LATIN CAPITAL LETTER I WITH MACRON
  $ Entry 'i' '-' '\x012B'    -- LATIN SMALL LETTER I WITH MACRON
  $ Entry 'I' '(' '\x012C'    -- LATIN CAPITAL LETTER I WITH BREVE
  $ Entry 'i' '(' '\x012D'    -- LATIN SMALL LETTER I WITH BREVE
  $ Entry 'I' ';' '\x012E'    -- LATIN CAPITAL LETTER I WITH OGONEK
  $ Entry 'i' ';' '\x012F'    -- LATIN SMALL LETTER I WITH OGONEK
  $ Entry 'I' '.' '\x0130'    -- LATIN CAPITAL LETTER I WITH DOT ABOVE
  $ Entry 'i' '.' '\x0131'    -- LATIN SMALL LETTER DOTLESS I
  $ Entry 'I' 'J' '\x0132'    -- LATIN CAPITAL LIGATURE IJ
  $ Entry 'i' 'j' '\x0133'    -- LATIN SMALL LIGATURE IJ
  $ Entry 'J' '>' '\x0134'    -- LATIN CAPITAL LETTER J WITH CIRCUMFLEX
  $ Entry 'j' '>' '\x0135'    -- LATIN SMALL LETTER J WITH CIRCUMFLEX
  $ Entry 'K' ',' '\x0136'    -- LATIN CAPITAL LETTER K WITH CEDILLA
  $ Entry 'k' ',' '\x0137'    -- LATIN SMALL LETTER K WITH CEDILLA
  $ Entry 'k' 'k' '\x0138'    -- LATIN SMALL LETTER KRA
  $ Entry 'L' '\'' '\x0139'    -- LATIN CAPITAL LETTER L WITH ACUTE
  $ Entry 'l' '\'' '\x013A'    -- LATIN SMALL LETTER L WITH ACUTE
  $ Entry 'L' ',' '\x013B'    -- LATIN CAPITAL LETTER L WITH CEDILLA
  $ Entry 'l' ',' '\x013C'    -- LATIN SMALL LETTER L WITH CEDILLA
  $ Entry 'L' '<' '\x013D'    -- LATIN CAPITAL LETTER L WITH CARON
  $ Entry 'l' '<' '\x013E'    -- LATIN SMALL LETTER L WITH CARON
  $ Entry 'L' '.' '\x013F'    -- LATIN CAPITAL LETTER L WITH MIDDLE DOT
  $ Entry 'l' '.' '\x0140'    -- LATIN SMALL LETTER L WITH MIDDLE DOT
  $ Entry 'L' '/' '\x0141'    -- LATIN CAPITAL LETTER L WITH STROKE
  $ Entry 'l' '/' '\x0142'    -- LATIN SMALL LETTER L WITH STROKE
  $ Entry 'N' '\'' '\x0143'    -- LATIN CAPITAL LETTER N WITH ACUTE `
  $ Entry 'n' '\'' '\x0144'    -- LATIN SMALL LETTER N WITH ACUTE `
  $ Entry 'N' ',' '\x0145'    -- LATIN CAPITAL LETTER N WITH CEDILLA `
  $ Entry 'n' ',' '\x0146'    -- LATIN SMALL LETTER N WITH CEDILLA `
  $ Entry 'N' '<' '\x0147'    -- LATIN CAPITAL LETTER N WITH CARON `
  $ Entry 'n' '<' '\x0148'    -- LATIN SMALL LETTER N WITH CARON `
  $ Entry '\'' 'n' '\x0149'    -- LATIN SMALL LETTER N PRECEDED BY APOSTROPHE `
  $ Entry 'N' 'G' '\x014A'    -- LATIN CAPITAL LETTER ENG
  $ Entry 'n' 'g' '\x014B'    -- LATIN SMALL LETTER ENG
  $ Entry 'O' '-' '\x014C'    -- LATIN CAPITAL LETTER O WITH MACRON
  $ Entry 'o' '-' '\x014D'    -- LATIN SMALL LETTER O WITH MACRON
  $ Entry 'O' '(' '\x014E'    -- LATIN CAPITAL LETTER O WITH BREVE
  $ Entry 'o' '(' '\x014F'    -- LATIN SMALL LETTER O WITH BREVE
  $ Entry 'O' '"' '\x0150'   -- LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
  $ Entry 'o' '"' '\x0151'   -- LATIN SMALL LETTER O WITH DOUBLE ACUTE
  $ Entry 'O' 'E' '\x0152'    -- LATIN CAPITAL LIGATURE OE
  $ Entry 'o' 'e' '\x0153'    -- LATIN SMALL LIGATURE OE
  $ Entry 'R' '\'' '\x0154'    -- LATIN CAPITAL LETTER R WITH ACUTE
  $ Entry 'r' '\'' '\x0155'    -- LATIN SMALL LETTER R WITH ACUTE
  $ Entry 'R' ',' '\x0156'    -- LATIN CAPITAL LETTER R WITH CEDILLA
  $ Entry 'r' ',' '\x0157'    -- LATIN SMALL LETTER R WITH CEDILLA
  $ Entry 'R' '<' '\x0158'    -- LATIN CAPITAL LETTER R WITH CARON
  $ Entry 'r' '<' '\x0159'    -- LATIN SMALL LETTER R WITH CARON
  $ Entry 'S' '\'' '\x015A'    -- LATIN CAPITAL LETTER S WITH ACUTE
  $ Entry 's' '\'' '\x015B'    -- LATIN SMALL LETTER S WITH ACUTE
  $ Entry 'S' '>' '\x015C'    -- LATIN CAPITAL LETTER S WITH CIRCUMFLEX
  $ Entry 's' '>' '\x015D'    -- LATIN SMALL LETTER S WITH CIRCUMFLEX
  $ Entry 'S' ',' '\x015E'    -- LATIN CAPITAL LETTER S WITH CEDILLA
  $ Entry 's' ',' '\x015F'    -- LATIN SMALL LETTER S WITH CEDILLA
  $ Entry 'S' '<' '\x0160'    -- LATIN CAPITAL LETTER S WITH CARON
  $ Entry 's' '<' '\x0161'    -- LATIN SMALL LETTER S WITH CARON
  $ Entry 'T' ',' '\x0162'    -- LATIN CAPITAL LETTER T WITH CEDILLA
  $ Entry 't' ',' '\x0163'    -- LATIN SMALL LETTER T WITH CEDILLA
  $ Entry 'T' '<' '\x0164'    -- LATIN CAPITAL LETTER T WITH CARON
  $ Entry 't' '<' '\x0165'    -- LATIN SMALL LETTER T WITH CARON
  $ Entry 'T' '/' '\x0166'    -- LATIN CAPITAL LETTER T WITH STROKE
  $ Entry 't' '/' '\x0167'    -- LATIN SMALL LETTER T WITH STROKE
  $ Entry 'U' '?' '\x0168'    -- LATIN CAPITAL LETTER U WITH TILDE
  $ Entry 'u' '?' '\x0169'    -- LATIN SMALL LETTER U WITH TILDE
  $ Entry 'U' '-' '\x016A'    -- LATIN CAPITAL LETTER U WITH MACRON
  $ Entry 'u' '-' '\x016B'    -- LATIN SMALL LETTER U WITH MACRON
  $ Entry 'U' '(' '\x016C'    -- LATIN CAPITAL LETTER U WITH BREVE
  $ Entry 'u' '(' '\x016D'    -- LATIN SMALL LETTER U WITH BREVE
  $ Entry 'U' '0' '\x016E'    -- LATIN CAPITAL LETTER U WITH RING ABOVE
  $ Entry 'u' '0' '\x016F'    -- LATIN SMALL LETTER U WITH RING ABOVE
  $ Entry 'U' '"' '\x0170'   -- LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
  $ Entry 'u' '"' '\x0171'   -- LATIN SMALL LETTER U WITH DOUBLE ACUTE
  $ Entry 'U' ';' '\x0172'    -- LATIN CAPITAL LETTER U WITH OGONEK
  $ Entry 'u' ';' '\x0173'    -- LATIN SMALL LETTER U WITH OGONEK
  $ Entry 'W' '>' '\x0174'    -- LATIN CAPITAL LETTER W WITH CIRCUMFLEX
  $ Entry 'w' '>' '\x0175'    -- LATIN SMALL LETTER W WITH CIRCUMFLEX
  $ Entry 'Y' '>' '\x0176'    -- LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
  $ Entry 'y' '>' '\x0177'    -- LATIN SMALL LETTER Y WITH CIRCUMFLEX
  $ Entry 'Y' ':' '\x0178'    -- LATIN CAPITAL LETTER Y WITH DIAERESIS
  $ Entry 'Z' '\'' '\x0179'    -- LATIN CAPITAL LETTER Z WITH ACUTE
  $ Entry 'z' '\'' '\x017A'    -- LATIN SMALL LETTER Z WITH ACUTE
  $ Entry 'Z' '.' '\x017B'    -- LATIN CAPITAL LETTER Z WITH DOT ABOVE
  $ Entry 'z' '.' '\x017C'    -- LATIN SMALL LETTER Z WITH DOT ABOVE
  $ Entry 'Z' '<' '\x017D'    -- LATIN CAPITAL LETTER Z WITH CARON
  $ Entry 'z' '<' '\x017E'    -- LATIN SMALL LETTER Z WITH CARON
  $ Entry 'O' '9' '\x01A0'    -- LATIN CAPITAL LETTER O WITH HORN
  $ Entry 'o' '9' '\x01A1'    -- LATIN SMALL LETTER O WITH HORN
  $ Entry 'O' 'I' '\x01A2'    -- LATIN CAPITAL LETTER OI
  $ Entry 'o' 'i' '\x01A3'    -- LATIN SMALL LETTER OI
  $ Entry 'y' 'r' '\x01A6'    -- LATIN LETTER YR
  $ Entry 'U' '9' '\x01AF'    -- LATIN CAPITAL LETTER U WITH HORN
  $ Entry 'u' '9' '\x01B0'    -- LATIN SMALL LETTER U WITH HORN
  $ Entry 'Z' '/' '\x01B5'    -- LATIN CAPITAL LETTER Z WITH STROKE
  $ Entry 'z' '/' '\x01B6'    -- LATIN SMALL LETTER Z WITH STROKE
  $ Entry 'E' 'D' '\x01B7'    -- LATIN CAPITAL LETTER EZH
  $ Entry 'A' '<' '\x01CD'    -- LATIN CAPITAL LETTER A WITH CARON
  $ Entry 'a' '<' '\x01CE'    -- LATIN SMALL LETTER A WITH CARON
  $ Entry 'I' '<' '\x01CF'    -- LATIN CAPITAL LETTER I WITH CARON
  $ Entry 'i' '<' '\x01D0'    -- LATIN SMALL LETTER I WITH CARON
  $ Entry 'O' '<' '\x01D1'    -- LATIN CAPITAL LETTER O WITH CARON
  $ Entry 'o' '<' '\x01D2'    -- LATIN SMALL LETTER O WITH CARON
  $ Entry 'U' '<' '\x01D3'    -- LATIN CAPITAL LETTER U WITH CARON
  $ Entry 'u' '<' '\x01D4'    -- LATIN SMALL LETTER U WITH CARON
  $ Entry 'A' '1' '\x01DE'    -- LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
  $ Entry 'a' '1' '\x01DF'    -- LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
  $ Entry 'A' '7' '\x01E0'    -- LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
  $ Entry 'a' '7' '\x01E1'    -- LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
  $ Entry 'A' '3' '\x01E2'    -- LATIN CAPITAL LETTER AE WITH MACRON
  $ Entry 'a' '3' '\x01E3'    -- LATIN SMALL LETTER AE WITH MACRON
  $ Entry 'G' '/' '\x01E4'    -- LATIN CAPITAL LETTER G WITH STROKE
  $ Entry 'g' '/' '\x01E5'    -- LATIN SMALL LETTER G WITH STROKE
  $ Entry 'G' '<' '\x01E6'    -- LATIN CAPITAL LETTER G WITH CARON
  $ Entry 'g' '<' '\x01E7'    -- LATIN SMALL LETTER G WITH CARON
  $ Entry 'K' '<' '\x01E8'    -- LATIN CAPITAL LETTER K WITH CARON
  $ Entry 'k' '<' '\x01E9'    -- LATIN SMALL LETTER K WITH CARON
  $ Entry 'O' ';' '\x01EA'    -- LATIN CAPITAL LETTER O WITH OGONEK
  $ Entry 'o' ';' '\x01EB'    -- LATIN SMALL LETTER O WITH OGONEK
  $ Entry 'O' '1' '\x01EC'    -- LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
  $ Entry 'o' '1' '\x01ED'    -- LATIN SMALL LETTER O WITH OGONEK AND MACRON
  $ Entry 'E' 'Z' '\x01EE'    -- LATIN CAPITAL LETTER EZH WITH CARON
  $ Entry 'e' 'z' '\x01EF'    -- LATIN SMALL LETTER EZH WITH CARON
  $ Entry 'j' '<' '\x01F0'    -- LATIN SMALL LETTER J WITH CARON
  $ Entry 'G' '\'' '\x01F4'    -- LATIN CAPITAL LETTER G WITH ACUTE
  $ Entry 'g' '\'' '\x01F5'    -- LATIN SMALL LETTER G WITH ACUTE
  $ Entry ';' 'S' '\x02BF'    -- MODIFIER LETTER LEFT HALF RING
  $ Entry '\'' '<' '\x02C7'    -- CARON
  $ Entry '\'' '(' '\x02D8'    -- BREVE
  $ Entry '\'' '.' '\x02D9'    -- DOT ABOVE
  $ Entry '\'' '0' '\x02DA'    -- RING ABOVE
  $ Entry '\'' ';' '\x02DB'    -- OGONEK
  $ Entry '\'' '"' '\x02DD'    -- DOUBLE ACUTE ACCENT
  $ Entry 'A' '%' '\x0386'    -- GREEK CAPITAL LETTER ALPHA WITH TONOS
  $ Entry 'E' '%' '\x0388'    -- GREEK CAPITAL LETTER EPSILON WITH TONOS
  $ Entry 'Y' '%' '\x0389'    -- GREEK CAPITAL LETTER ETA WITH TONOS
  $ Entry 'I' '%' '\x038A'    -- GREEK CAPITAL LETTER IOTA WITH TONOS
  $ Entry 'O' '%' '\x038C'    -- GREEK CAPITAL LETTER OMICRON WITH TONOS
  $ Entry 'U' '%' '\x038E'    -- GREEK CAPITAL LETTER UPSILON WITH TONOS
  $ Entry 'W' '%' '\x038F'    -- GREEK CAPITAL LETTER OMEGA WITH TONOS
  $ Entry 'i' '3' '\x0390'    -- GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
  $ Entry 'A' '*' '\x0391'    -- GREEK CAPITAL LETTER ALPHA
  $ Entry 'B' '*' '\x0392'    -- GREEK CAPITAL LETTER BETA
  $ Entry 'G' '*' '\x0393'    -- GREEK CAPITAL LETTER GAMMA
  $ Entry 'D' '*' '\x0394'    -- GREEK CAPITAL LETTER DELTA
  $ Entry 'E' '*' '\x0395'    -- GREEK CAPITAL LETTER EPSILON
  $ Entry 'Z' '*' '\x0396'    -- GREEK CAPITAL LETTER ZETA
  $ Entry 'Y' '*' '\x0397'    -- GREEK CAPITAL LETTER ETA
  $ Entry 'H' '*' '\x0398'    -- GREEK CAPITAL LETTER THETA
  $ Entry 'I' '*' '\x0399'    -- GREEK CAPITAL LETTER IOTA
  $ Entry 'K' '*' '\x039A'    -- GREEK CAPITAL LETTER KAPPA
  $ Entry 'L' '*' '\x039B'    -- GREEK CAPITAL LETTER LAMDA
  $ Entry 'M' '*' '\x039C'    -- GREEK CAPITAL LETTER MU
  $ Entry 'N' '*' '\x039D'    -- GREEK CAPITAL LETTER NU
  $ Entry 'C' '*' '\x039E'    -- GREEK CAPITAL LETTER XI
  $ Entry 'O' '*' '\x039F'    -- GREEK CAPITAL LETTER OMICRON
  $ Entry 'P' '*' '\x03A0'    -- GREEK CAPITAL LETTER PI
  $ Entry 'R' '*' '\x03A1'    -- GREEK CAPITAL LETTER RHO
  $ Entry 'S' '*' '\x03A3'    -- GREEK CAPITAL LETTER SIGMA
  $ Entry 'T' '*' '\x03A4'    -- GREEK CAPITAL LETTER TAU
  $ Entry 'U' '*' '\x03A5'    -- GREEK CAPITAL LETTER UPSILON
  $ Entry 'F' '*' '\x03A6'    -- GREEK CAPITAL LETTER PHI
  $ Entry 'X' '*' '\x03A7'    -- GREEK CAPITAL LETTER CHI
  $ Entry 'Q' '*' '\x03A8'    -- GREEK CAPITAL LETTER PSI
  $ Entry 'W' '*' '\x03A9'    -- GREEK CAPITAL LETTER OMEGA
  $ Entry 'J' '*' '\x03AA'    -- GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
  $ Entry 'V' '*' '\x03AB'    -- GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
  $ Entry 'a' '%' '\x03AC'    -- GREEK SMALL LETTER ALPHA WITH TONOS
  $ Entry 'e' '%' '\x03AD'    -- GREEK SMALL LETTER EPSILON WITH TONOS
  $ Entry 'y' '%' '\x03AE'    -- GREEK SMALL LETTER ETA WITH TONOS
  $ Entry 'i' '%' '\x03AF'    -- GREEK SMALL LETTER IOTA WITH TONOS
  $ Entry 'u' '3' '\x03B0'    -- GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
  $ Entry 'a' '*' '\x03B1'    -- GREEK SMALL LETTER ALPHA
  $ Entry 'b' '*' '\x03B2'    -- GREEK SMALL LETTER BETA
  $ Entry 'g' '*' '\x03B3'    -- GREEK SMALL LETTER GAMMA
  $ Entry 'd' '*' '\x03B4'    -- GREEK SMALL LETTER DELTA
  $ Entry 'e' '*' '\x03B5'    -- GREEK SMALL LETTER EPSILON
  $ Entry 'z' '*' '\x03B6'    -- GREEK SMALL LETTER ZETA
  $ Entry 'y' '*' '\x03B7'    -- GREEK SMALL LETTER ETA
  $ Entry 'h' '*' '\x03B8'    -- GREEK SMALL LETTER THETA
  $ Entry 'i' '*' '\x03B9'    -- GREEK SMALL LETTER IOTA
  $ Entry 'k' '*' '\x03BA'    -- GREEK SMALL LETTER KAPPA
  $ Entry 'l' '*' '\x03BB'    -- GREEK SMALL LETTER LAMDA
  $ Entry 'm' '*' '\x03BC'    -- GREEK SMALL LETTER MU
  $ Entry 'n' '*' '\x03BD'    -- GREEK SMALL LETTER NU
  $ Entry 'c' '*' '\x03BE'    -- GREEK SMALL LETTER XI
  $ Entry 'o' '*' '\x03BF'    -- GREEK SMALL LETTER OMICRON
  $ Entry 'p' '*' '\x03C0'    -- GREEK SMALL LETTER PI
  $ Entry 'r' '*' '\x03C1'    -- GREEK SMALL LETTER RHO
  $ Entry '*' 's' '\x03C2'    -- GREEK SMALL LETTER FINAL SIGMA
  $ Entry 's' '*' '\x03C3'    -- GREEK SMALL LETTER SIGMA
  $ Entry 't' '*' '\x03C4'    -- GREEK SMALL LETTER TAU
  $ Entry 'u' '*' '\x03C5'    -- GREEK SMALL LETTER UPSILON
  $ Entry 'f' '*' '\x03C6'    -- GREEK SMALL LETTER PHI
  $ Entry 'x' '*' '\x03C7'    -- GREEK SMALL LETTER CHI
  $ Entry 'q' '*' '\x03C8'    -- GREEK SMALL LETTER PSI
  $ Entry 'w' '*' '\x03C9'    -- GREEK SMALL LETTER OMEGA
  $ Entry 'j' '*' '\x03CA'    -- GREEK SMALL LETTER IOTA WITH DIALYTIKA
  $ Entry 'v' '*' '\x03CB'    -- GREEK SMALL LETTER UPSILON WITH DIALYTIKA
  $ Entry 'o' '%' '\x03CC'    -- GREEK SMALL LETTER OMICRON WITH TONOS
  $ Entry 'u' '%' '\x03CD'    -- GREEK SMALL LETTER UPSILON WITH TONOS
  $ Entry 'w' '%' '\x03CE'    -- GREEK SMALL LETTER OMEGA WITH TONOS
  $ Entry '\'' 'G' '\x03D8'    -- GREEK LETTER ARCHAIC KOPPA
  $ Entry ',' 'G' '\x03D9'    -- GREEK SMALL LETTER ARCHAIC KOPPA
  $ Entry 'T' '3' '\x03DA'    -- GREEK LETTER STIGMA
  $ Entry 't' '3' '\x03DB'    -- GREEK SMALL LETTER STIGMA
  $ Entry 'M' '3' '\x03DC'    -- GREEK LETTER DIGAMMA
  $ Entry 'm' '3' '\x03DD'    -- GREEK SMALL LETTER DIGAMMA
  $ Entry 'K' '3' '\x03DE'    -- GREEK LETTER KOPPA
  $ Entry 'k' '3' '\x03DF'    -- GREEK SMALL LETTER KOPPA
  $ Entry 'P' '3' '\x03E0'    -- GREEK LETTER SAMPI
  $ Entry 'p' '3' '\x03E1'    -- GREEK SMALL LETTER SAMPI
  $ Entry '\'' '%' '\x03F4'    -- GREEK CAPITAL THETA SYMBOL
  $ Entry 'j' '3' '\x03F5'    -- GREEK LUNATE EPSILON SYMBOL
  $ Entry 'I' 'O' '\x0401'    -- CYRILLIC CAPITAL LETTER IO
  $ Entry 'D' '%' '\x0402'    -- CYRILLIC CAPITAL LETTER DJE
  $ Entry 'G' '%' '\x0403'    -- CYRILLIC CAPITAL LETTER GJE
  $ Entry 'I' 'E' '\x0404'    -- CYRILLIC CAPITAL LETTER UKRAINIAN IE
  $ Entry 'D' 'S' '\x0405'    -- CYRILLIC CAPITAL LETTER DZE
  $ Entry 'I' 'I' '\x0406'    -- CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
  $ Entry 'Y' 'I' '\x0407'    -- CYRILLIC CAPITAL LETTER YI
  $ Entry 'J' '%' '\x0408'    -- CYRILLIC CAPITAL LETTER JE
  $ Entry 'L' 'J' '\x0409'    -- CYRILLIC CAPITAL LETTER LJE
  $ Entry 'N' 'J' '\x040A'    -- CYRILLIC CAPITAL LETTER NJE
  $ Entry 'T' 's' '\x040B'    -- CYRILLIC CAPITAL LETTER TSHE
  $ Entry 'K' 'J' '\x040C'    -- CYRILLIC CAPITAL LETTER KJE
  $ Entry 'V' '%' '\x040E'    -- CYRILLIC CAPITAL LETTER SHORT U
  $ Entry 'D' 'Z' '\x040F'    -- CYRILLIC CAPITAL LETTER DZHE
  $ Entry 'A' '=' '\x0410'    -- CYRILLIC CAPITAL LETTER A
  $ Entry 'B' '=' '\x0411'    -- CYRILLIC CAPITAL LETTER BE
  $ Entry 'V' '=' '\x0412'    -- CYRILLIC CAPITAL LETTER VE
  $ Entry 'G' '=' '\x0413'    -- CYRILLIC CAPITAL LETTER GHE
  $ Entry 'D' '=' '\x0414'    -- CYRILLIC CAPITAL LETTER DE
  $ Entry 'E' '=' '\x0415'    -- CYRILLIC CAPITAL LETTER IE
  $ Entry 'Z' '%' '\x0416'    -- CYRILLIC CAPITAL LETTER ZHE
  $ Entry 'Z' '=' '\x0417'    -- CYRILLIC CAPITAL LETTER ZE
  $ Entry 'I' '=' '\x0418'    -- CYRILLIC CAPITAL LETTER I
  $ Entry 'J' '=' '\x0419'    -- CYRILLIC CAPITAL LETTER SHORT I
  $ Entry 'K' '=' '\x041A'    -- CYRILLIC CAPITAL LETTER KA
  $ Entry 'L' '=' '\x041B'    -- CYRILLIC CAPITAL LETTER EL
  $ Entry 'M' '=' '\x041C'    -- CYRILLIC CAPITAL LETTER EM
  $ Entry 'N' '=' '\x041D'    -- CYRILLIC CAPITAL LETTER EN
  $ Entry 'O' '=' '\x041E'    -- CYRILLIC CAPITAL LETTER O
  $ Entry 'P' '=' '\x041F'    -- CYRILLIC CAPITAL LETTER PE
  $ Entry 'R' '=' '\x0420'    -- CYRILLIC CAPITAL LETTER ER
  $ Entry 'S' '=' '\x0421'    -- CYRILLIC CAPITAL LETTER ES
  $ Entry 'T' '=' '\x0422'    -- CYRILLIC CAPITAL LETTER TE
  $ Entry 'U' '=' '\x0423'    -- CYRILLIC CAPITAL LETTER U
  $ Entry 'F' '=' '\x0424'    -- CYRILLIC CAPITAL LETTER EF
  $ Entry 'H' '=' '\x0425'    -- CYRILLIC CAPITAL LETTER HA
  $ Entry 'C' '=' '\x0426'    -- CYRILLIC CAPITAL LETTER TSE
  $ Entry 'C' '%' '\x0427'    -- CYRILLIC CAPITAL LETTER CHE
  $ Entry 'S' '%' '\x0428'    -- CYRILLIC CAPITAL LETTER SHA
  $ Entry 'S' 'c' '\x0429'    -- CYRILLIC CAPITAL LETTER SHCHA
  $ Entry '=' '\'' '\x042A'   -- CYRILLIC CAPITAL LETTER HARD SIGN
  $ Entry 'Y' '=' '\x042B'    -- CYRILLIC CAPITAL LETTER YERU
  $ Entry '%' '\'' '\x042C'   -- CYRILLIC CAPITAL LETTER SOFT SIGN
  $ Entry 'J' 'E' '\x042D'    -- CYRILLIC CAPITAL LETTER E
  $ Entry 'J' 'U' '\x042E'    -- CYRILLIC CAPITAL LETTER YU
  $ Entry 'J' 'A' '\x042F'    -- CYRILLIC CAPITAL LETTER YA
  $ Entry 'a' '=' '\x0430'    -- CYRILLIC SMALL LETTER A
  $ Entry 'b' '=' '\x0431'    -- CYRILLIC SMALL LETTER BE
  $ Entry 'v' '=' '\x0432'    -- CYRILLIC SMALL LETTER VE
  $ Entry 'g' '=' '\x0433'    -- CYRILLIC SMALL LETTER GHE
  $ Entry 'd' '=' '\x0434'    -- CYRILLIC SMALL LETTER DE
  $ Entry 'e' '=' '\x0435'    -- CYRILLIC SMALL LETTER IE
  $ Entry 'z' '%' '\x0436'    -- CYRILLIC SMALL LETTER ZHE
  $ Entry 'z' '=' '\x0437'    -- CYRILLIC SMALL LETTER ZE
  $ Entry 'i' '=' '\x0438'    -- CYRILLIC SMALL LETTER I
  $ Entry 'j' '=' '\x0439'    -- CYRILLIC SMALL LETTER SHORT I
  $ Entry 'k' '=' '\x043A'    -- CYRILLIC SMALL LETTER KA
  $ Entry 'l' '=' '\x043B'    -- CYRILLIC SMALL LETTER EL
  $ Entry 'm' '=' '\x043C'    -- CYRILLIC SMALL LETTER EM
  $ Entry 'n' '=' '\x043D'    -- CYRILLIC SMALL LETTER EN
  $ Entry 'o' '=' '\x043E'    -- CYRILLIC SMALL LETTER O
  $ Entry 'p' '=' '\x043F'    -- CYRILLIC SMALL LETTER PE
  $ Entry 'r' '=' '\x0440'    -- CYRILLIC SMALL LETTER ER
  $ Entry 's' '=' '\x0441'    -- CYRILLIC SMALL LETTER ES
  $ Entry 't' '=' '\x0442'    -- CYRILLIC SMALL LETTER TE
  $ Entry 'u' '=' '\x0443'    -- CYRILLIC SMALL LETTER U
  $ Entry 'f' '=' '\x0444'    -- CYRILLIC SMALL LETTER EF
  $ Entry 'h' '=' '\x0445'    -- CYRILLIC SMALL LETTER HA
  $ Entry 'c' '=' '\x0446'    -- CYRILLIC SMALL LETTER TSE
  $ Entry 'c' '%' '\x0447'    -- CYRILLIC SMALL LETTER CHE
  $ Entry 's' '%' '\x0448'    -- CYRILLIC SMALL LETTER SHA
  $ Entry 's' 'c' '\x0449'    -- CYRILLIC SMALL LETTER SHCHA
  $ Entry '=' '\'' '\x044A'    -- CYRILLIC SMALL LETTER HARD SIGN
  $ Entry 'y' '=' '\x044B'    -- CYRILLIC SMALL LETTER YERU
  $ Entry '%' '\'' '\x044C'    -- CYRILLIC SMALL LETTER SOFT SIGN
  $ Entry 'j' 'e' '\x044D'    -- CYRILLIC SMALL LETTER E
  $ Entry 'j' 'u' '\x044E'    -- CYRILLIC SMALL LETTER YU
  $ Entry 'j' 'a' '\x044F'    -- CYRILLIC SMALL LETTER YA
  $ Entry 'i' 'o' '\x0451'    -- CYRILLIC SMALL LETTER IO
  $ Entry 'd' '%' '\x0452'    -- CYRILLIC SMALL LETTER DJE
  $ Entry 'g' '%' '\x0453'    -- CYRILLIC SMALL LETTER GJE
  $ Entry 'i' 'e' '\x0454'    -- CYRILLIC SMALL LETTER UKRAINIAN IE
  $ Entry 'd' 's' '\x0455'    -- CYRILLIC SMALL LETTER DZE
  $ Entry 'i' 'i' '\x0456'    -- CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
  $ Entry 'y' 'i' '\x0457'    -- CYRILLIC SMALL LETTER YI
  $ Entry 'j' '%' '\x0458'    -- CYRILLIC SMALL LETTER JE
  $ Entry 'l' 'j' '\x0459'    -- CYRILLIC SMALL LETTER LJE
  $ Entry 'n' 'j' '\x045A'    -- CYRILLIC SMALL LETTER NJE
  $ Entry 't' 's' '\x045B'    -- CYRILLIC SMALL LETTER TSHE
  $ Entry 'k' 'j' '\x045C'    -- CYRILLIC SMALL LETTER KJE
  $ Entry 'v' '%' '\x045E'    -- CYRILLIC SMALL LETTER SHORT U
  $ Entry 'd' 'z' '\x045F'    -- CYRILLIC SMALL LETTER DZHE
  $ Entry 'Y' '3' '\x0462'    -- CYRILLIC CAPITAL LETTER YAT
  $ Entry 'y' '3' '\x0463'    -- CYRILLIC SMALL LETTER YAT
  $ Entry 'O' '3' '\x046A'    -- CYRILLIC CAPITAL LETTER BIG YUS
  $ Entry 'o' '3' '\x046B'    -- CYRILLIC SMALL LETTER BIG YUS
  $ Entry 'F' '3' '\x0472'    -- CYRILLIC CAPITAL LETTER FITA
  $ Entry 'f' '3' '\x0473'    -- CYRILLIC SMALL LETTER FITA
  $ Entry 'V' '3' '\x0474'    -- CYRILLIC CAPITAL LETTER IZHITSA
  $ Entry 'v' '3' '\x0475'    -- CYRILLIC SMALL LETTER IZHITSA
  $ Entry 'C' '3' '\x0480'    -- CYRILLIC CAPITAL LETTER KOPPA
  $ Entry 'c' '3' '\x0481'    -- CYRILLIC SMALL LETTER KOPPA
  $ Entry 'G' '3' '\x0490'    -- CYRILLIC CAPITAL LETTER GHE WITH UPTURN
  $ Entry 'g' '3' '\x0491'    -- CYRILLIC SMALL LETTER GHE WITH UPTURN
  $ Entry 'A' '+' '\x05D0'    -- HEBREW LETTER ALEF
  $ Entry 'B' '+' '\x05D1'    -- HEBREW LETTER BET
  $ Entry 'G' '+' '\x05D2'    -- HEBREW LETTER GIMEL
  $ Entry 'D' '+' '\x05D3'    -- HEBREW LETTER DALET
  $ Entry 'H' '+' '\x05D4'    -- HEBREW LETTER HE
  $ Entry 'W' '+' '\x05D5'    -- HEBREW LETTER VAV
  $ Entry 'Z' '+' '\x05D6'    -- HEBREW LETTER ZAYIN
  $ Entry 'X' '+' '\x05D7'    -- HEBREW LETTER HET
  $ Entry 'T' 'j' '\x05D8'    -- HEBREW LETTER TET
  $ Entry 'J' '+' '\x05D9'    -- HEBREW LETTER YOD
  $ Entry 'K' '%' '\x05DA'    -- HEBREW LETTER FINAL KAF
  $ Entry 'K' '+' '\x05DB'    -- HEBREW LETTER KAF
  $ Entry 'L' '+' '\x05DC'    -- HEBREW LETTER LAMED
  $ Entry 'M' '%' '\x05DD'    -- HEBREW LETTER FINAL MEM
  $ Entry 'M' '+' '\x05DE'    -- HEBREW LETTER MEM
  $ Entry 'N' '%' '\x05DF'    -- HEBREW LETTER FINAL NUN `
  $ Entry 'N' '+' '\x05E0'    -- HEBREW LETTER NUN `
  $ Entry 'S' '+' '\x05E1'    -- HEBREW LETTER SAMEKH
  $ Entry 'E' '+' '\x05E2'    -- HEBREW LETTER AYIN
  $ Entry 'P' '%' '\x05E3'    -- HEBREW LETTER FINAL PE
  $ Entry 'P' '+' '\x05E4'    -- HEBREW LETTER PE
  $ Entry 'Z' 'j' '\x05E5'    -- HEBREW LETTER FINAL TSADI
  $ Entry 'Z' 'J' '\x05E6'    -- HEBREW LETTER TSADI
  $ Entry 'Q' '+' '\x05E7'    -- HEBREW LETTER QOF
  $ Entry 'R' '+' '\x05E8'    -- HEBREW LETTER RESH
  $ Entry 'S' 'h' '\x05E9'    -- HEBREW LETTER SHIN
  $ Entry 'T' '+' '\x05EA'    -- HEBREW LETTER TAV
  $ Entry ',' '+' '\x060C'    -- ARABIC COMMA
  $ Entry ';' '+' '\x061B'    -- ARABIC SEMICOLON
  $ Entry '?' '+' '\x061F'    -- ARABIC QUESTION MARK
  $ Entry 'H' '\'' '\x0621'    -- ARABIC LETTER HAMZA
  $ Entry 'a' 'M' '\x0622'    -- ARABIC LETTER ALEF WITH MADDA ABOVE
  $ Entry 'a' 'H' '\x0623'    -- ARABIC LETTER ALEF WITH HAMZA ABOVE
  $ Entry 'w' 'H' '\x0624'    -- ARABIC LETTER WAW WITH HAMZA ABOVE
  $ Entry 'a' 'h' '\x0625'    -- ARABIC LETTER ALEF WITH HAMZA BELOW
  $ Entry 'y' 'H' '\x0626'    -- ARABIC LETTER YEH WITH HAMZA ABOVE
  $ Entry 'a' '+' '\x0627'    -- ARABIC LETTER ALEF
  $ Entry 'b' '+' '\x0628'    -- ARABIC LETTER BEH
  $ Entry 't' 'm' '\x0629'    -- ARABIC LETTER TEH MARBUTA
  $ Entry 't' '+' '\x062A'    -- ARABIC LETTER TEH
  $ Entry 't' 'k' '\x062B'    -- ARABIC LETTER THEH
  $ Entry 'g' '+' '\x062C'    -- ARABIC LETTER JEEM
  $ Entry 'h' 'k' '\x062D'    -- ARABIC LETTER HAH
  $ Entry 'x' '+' '\x062E'    -- ARABIC LETTER KHAH
  $ Entry 'd' '+' '\x062F'    -- ARABIC LETTER DAL
  $ Entry 'd' 'k' '\x0630'    -- ARABIC LETTER THAL
  $ Entry 'r' '+' '\x0631'    -- ARABIC LETTER REH
  $ Entry 'z' '+' '\x0632'    -- ARABIC LETTER ZAIN
  $ Entry 's' '+' '\x0633'    -- ARABIC LETTER SEEN
  $ Entry 's' 'n' '\x0634'    -- ARABIC LETTER SHEEN
  $ Entry 'c' '+' '\x0635'    -- ARABIC LETTER SAD
  $ Entry 'd' 'd' '\x0636'    -- ARABIC LETTER DAD
  $ Entry 't' 'j' '\x0637'    -- ARABIC LETTER TAH
  $ Entry 'z' 'H' '\x0638'    -- ARABIC LETTER ZAH
  $ Entry 'e' '+' '\x0639'    -- ARABIC LETTER AIN
  $ Entry 'i' '+' '\x063A'    -- ARABIC LETTER GHAIN
  $ Entry '+' '+' '\x0640'    -- ARABIC TATWEEL
  $ Entry 'f' '+' '\x0641'    -- ARABIC LETTER FEH
  $ Entry 'q' '+' '\x0642'    -- ARABIC LETTER QAF
  $ Entry 'k' '+' '\x0643'    -- ARABIC LETTER KAF
  $ Entry 'l' '+' '\x0644'    -- ARABIC LETTER LAM
  $ Entry 'm' '+' '\x0645'    -- ARABIC LETTER MEEM
  $ Entry 'n' '+' '\x0646'    -- ARABIC LETTER NOON
  $ Entry 'h' '+' '\x0647'    -- ARABIC LETTER HEH
  $ Entry 'w' '+' '\x0648'    -- ARABIC LETTER WAW
  $ Entry 'j' '+' '\x0649'    -- ARABIC LETTER ALEF MAKSURA
  $ Entry 'y' '+' '\x064A'    -- ARABIC LETTER YEH
  $ Entry ':' '+' '\x064B'    -- ARABIC FATHATAN
  $ Entry '"' '+' '\x064C'   -- ARABIC DAMMATAN
  $ Entry '=' '+' '\x064D'    -- ARABIC KASRATAN
  $ Entry '/' '+' '\x064E'    -- ARABIC FATHA
  $ Entry '\'' '+' '\x064F'    -- ARABIC DAMMA
  $ Entry '1' '+' '\x0650'    -- ARABIC KASRA
  $ Entry '3' '+' '\x0651'    -- ARABIC SHADDA
  $ Entry '0' '+' '\x0652'    -- ARABIC SUKUN
  $ Entry 'a' 'S' '\x0670'    -- ARABIC LETTER SUPERSCRIPT ALEF
  $ Entry 'p' '+' '\x067E'    -- ARABIC LETTER PEH
  $ Entry 'v' '+' '\x06A4'    -- ARABIC LETTER VEH
  $ Entry 'g' 'f' '\x06AF'    -- ARABIC LETTER GAF
  $ Entry '0' 'a' '\x06F0'    -- EXTENDED ARABIC-INDIC DIGIT ZERO
  $ Entry '1' 'a' '\x06F1'    -- EXTENDED ARABIC-INDIC DIGIT ONE
  $ Entry '2' 'a' '\x06F2'    -- EXTENDED ARABIC-INDIC DIGIT TWO
  $ Entry '3' 'a' '\x06F3'    -- EXTENDED ARABIC-INDIC DIGIT THREE
  $ Entry '4' 'a' '\x06F4'    -- EXTENDED ARABIC-INDIC DIGIT FOUR
  $ Entry '5' 'a' '\x06F5'    -- EXTENDED ARABIC-INDIC DIGIT FIVE
  $ Entry '6' 'a' '\x06F6'    -- EXTENDED ARABIC-INDIC DIGIT SIX
  $ Entry '7' 'a' '\x06F7'    -- EXTENDED ARABIC-INDIC DIGIT SEVEN
  $ Entry '8' 'a' '\x06F8'    -- EXTENDED ARABIC-INDIC DIGIT EIGHT
  $ Entry '9' 'a' '\x06F9'    -- EXTENDED ARABIC-INDIC DIGIT NINE
  $ Entry 'B' '.' '\x1E02'    -- LATIN CAPITAL LETTER B WITH DOT ABOVE
  $ Entry 'b' '.' '\x1E03'    -- LATIN SMALL LETTER B WITH DOT ABOVE
  $ Entry 'B' '_' '\x1E06'    -- LATIN CAPITAL LETTER B WITH LINE BELOW
  $ Entry 'b' '_' '\x1E07'    -- LATIN SMALL LETTER B WITH LINE BELOW
  $ Entry 'D' '.' '\x1E0A'    -- LATIN CAPITAL LETTER D WITH DOT ABOVE
  $ Entry 'd' '.' '\x1E0B'    -- LATIN SMALL LETTER D WITH DOT ABOVE
  $ Entry 'D' '_' '\x1E0E'    -- LATIN CAPITAL LETTER D WITH LINE BELOW
  $ Entry 'd' '_' '\x1E0F'    -- LATIN SMALL LETTER D WITH LINE BELOW
  $ Entry 'D' ',' '\x1E10'    -- LATIN CAPITAL LETTER D WITH CEDILLA
  $ Entry 'd' ',' '\x1E11'    -- LATIN SMALL LETTER D WITH CEDILLA
  $ Entry 'F' '.' '\x1E1E'    -- LATIN CAPITAL LETTER F WITH DOT ABOVE
  $ Entry 'f' '.' '\x1E1F'    -- LATIN SMALL LETTER F WITH DOT ABOVE
  $ Entry 'G' '-' '\x1E20'    -- LATIN CAPITAL LETTER G WITH MACRON
  $ Entry 'g' '-' '\x1E21'    -- LATIN SMALL LETTER G WITH MACRON
  $ Entry 'H' '.' '\x1E22'    -- LATIN CAPITAL LETTER H WITH DOT ABOVE
  $ Entry 'h' '.' '\x1E23'    -- LATIN SMALL LETTER H WITH DOT ABOVE
  $ Entry 'H' ':' '\x1E26'    -- LATIN CAPITAL LETTER H WITH DIAERESIS
  $ Entry 'h' ':' '\x1E27'    -- LATIN SMALL LETTER H WITH DIAERESIS
  $ Entry 'H' ',' '\x1E28'    -- LATIN CAPITAL LETTER H WITH CEDILLA
  $ Entry 'h' ',' '\x1E29'    -- LATIN SMALL LETTER H WITH CEDILLA
  $ Entry 'K' '\'' '\x1E30'    -- LATIN CAPITAL LETTER K WITH ACUTE
  $ Entry 'k' '\'' '\x1E31'    -- LATIN SMALL LETTER K WITH ACUTE
  $ Entry 'K' '_' '\x1E34'    -- LATIN CAPITAL LETTER K WITH LINE BELOW
  $ Entry 'k' '_' '\x1E35'    -- LATIN SMALL LETTER K WITH LINE BELOW
  $ Entry 'L' '_' '\x1E3A'    -- LATIN CAPITAL LETTER L WITH LINE BELOW
  $ Entry 'l' '_' '\x1E3B'    -- LATIN SMALL LETTER L WITH LINE BELOW
  $ Entry 'M' '\'' '\x1E3E'    -- LATIN CAPITAL LETTER M WITH ACUTE
  $ Entry 'm' '\'' '\x1E3F'    -- LATIN SMALL LETTER M WITH ACUTE
  $ Entry 'M' '.' '\x1E40'    -- LATIN CAPITAL LETTER M WITH DOT ABOVE
  $ Entry 'm' '.' '\x1E41'    -- LATIN SMALL LETTER M WITH DOT ABOVE
  $ Entry 'N' '.' '\x1E44'    -- LATIN CAPITAL LETTER N WITH DOT ABOVE `
  $ Entry 'n' '.' '\x1E45'    -- LATIN SMALL LETTER N WITH DOT ABOVE `
  $ Entry 'N' '_' '\x1E48'    -- LATIN CAPITAL LETTER N WITH LINE BELOW `
  $ Entry 'n' '_' '\x1E49'    -- LATIN SMALL LETTER N WITH LINE BELOW `
  $ Entry 'P' '\'' '\x1E54'    -- LATIN CAPITAL LETTER P WITH ACUTE
  $ Entry 'p' '\'' '\x1E55'    -- LATIN SMALL LETTER P WITH ACUTE
  $ Entry 'P' '.' '\x1E56'    -- LATIN CAPITAL LETTER P WITH DOT ABOVE
  $ Entry 'p' '.' '\x1E57'    -- LATIN SMALL LETTER P WITH DOT ABOVE
  $ Entry 'R' '.' '\x1E58'    -- LATIN CAPITAL LETTER R WITH DOT ABOVE
  $ Entry 'r' '.' '\x1E59'    -- LATIN SMALL LETTER R WITH DOT ABOVE
  $ Entry 'R' '_' '\x1E5E'    -- LATIN CAPITAL LETTER R WITH LINE BELOW
  $ Entry 'r' '_' '\x1E5F'    -- LATIN SMALL LETTER R WITH LINE BELOW
  $ Entry 'S' '.' '\x1E60'    -- LATIN CAPITAL LETTER S WITH DOT ABOVE
  $ Entry 's' '.' '\x1E61'    -- LATIN SMALL LETTER S WITH DOT ABOVE
  $ Entry 'T' '.' '\x1E6A'    -- LATIN CAPITAL LETTER T WITH DOT ABOVE
  $ Entry 't' '.' '\x1E6B'    -- LATIN SMALL LETTER T WITH DOT ABOVE
  $ Entry 'T' '_' '\x1E6E'    -- LATIN CAPITAL LETTER T WITH LINE BELOW
  $ Entry 't' '_' '\x1E6F'    -- LATIN SMALL LETTER T WITH LINE BELOW
  $ Entry 'V' '?' '\x1E7C'    -- LATIN CAPITAL LETTER V WITH TILDE
  $ Entry 'v' '?' '\x1E7D'    -- LATIN SMALL LETTER V WITH TILDE
  $ Entry 'W' '!' '\x1E80'    -- LATIN CAPITAL LETTER W WITH GRAVE
  $ Entry 'w' '!' '\x1E81'    -- LATIN SMALL LETTER W WITH GRAVE
  $ Entry 'W' '\'' '\x1E82'    -- LATIN CAPITAL LETTER W WITH ACUTE
  $ Entry 'w' '\'' '\x1E83'    -- LATIN SMALL LETTER W WITH ACUTE
  $ Entry 'W' ':' '\x1E84'    -- LATIN CAPITAL LETTER W WITH DIAERESIS
  $ Entry 'w' ':' '\x1E85'    -- LATIN SMALL LETTER W WITH DIAERESIS
  $ Entry 'W' '.' '\x1E86'    -- LATIN CAPITAL LETTER W WITH DOT ABOVE
  $ Entry 'w' '.' '\x1E87'    -- LATIN SMALL LETTER W WITH DOT ABOVE
  $ Entry 'X' '.' '\x1E8A'    -- LATIN CAPITAL LETTER X WITH DOT ABOVE
  $ Entry 'x' '.' '\x1E8B'    -- LATIN SMALL LETTER X WITH DOT ABOVE
  $ Entry 'X' ':' '\x1E8C'    -- LATIN CAPITAL LETTER X WITH DIAERESIS
  $ Entry 'x' ':' '\x1E8D'    -- LATIN SMALL LETTER X WITH DIAERESIS
  $ Entry 'Y' '.' '\x1E8E'    -- LATIN CAPITAL LETTER Y WITH DOT ABOVE
  $ Entry 'y' '.' '\x1E8F'    -- LATIN SMALL LETTER Y WITH DOT ABOVE
  $ Entry 'Z' '>' '\x1E90'    -- LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
  $ Entry 'z' '>' '\x1E91'    -- LATIN SMALL LETTER Z WITH CIRCUMFLEX
  $ Entry 'Z' '_' '\x1E94'    -- LATIN CAPITAL LETTER Z WITH LINE BELOW
  $ Entry 'z' '_' '\x1E95'    -- LATIN SMALL LETTER Z WITH LINE BELOW
  $ Entry 'h' '_' '\x1E96'    -- LATIN SMALL LETTER H WITH LINE BELOW
  $ Entry 't' ':' '\x1E97'    -- LATIN SMALL LETTER T WITH DIAERESIS
  $ Entry 'w' '0' '\x1E98'    -- LATIN SMALL LETTER W WITH RING ABOVE
  $ Entry 'y' '0' '\x1E99'    -- LATIN SMALL LETTER Y WITH RING ABOVE
  $ Entry 'A' '2' '\x1EA2'    -- LATIN CAPITAL LETTER A WITH HOOK ABOVE
  $ Entry 'a' '2' '\x1EA3'    -- LATIN SMALL LETTER A WITH HOOK ABOVE
  $ Entry 'E' '2' '\x1EBA'    -- LATIN CAPITAL LETTER E WITH HOOK ABOVE
  $ Entry 'e' '2' '\x1EBB'    -- LATIN SMALL LETTER E WITH HOOK ABOVE
  $ Entry 'E' '?' '\x1EBC'    -- LATIN CAPITAL LETTER E WITH TILDE
  $ Entry 'e' '?' '\x1EBD'    -- LATIN SMALL LETTER E WITH TILDE
  $ Entry 'I' '2' '\x1EC8'    -- LATIN CAPITAL LETTER I WITH HOOK ABOVE
  $ Entry 'i' '2' '\x1EC9'    -- LATIN SMALL LETTER I WITH HOOK ABOVE
  $ Entry 'O' '2' '\x1ECE'    -- LATIN CAPITAL LETTER O WITH HOOK ABOVE
  $ Entry 'o' '2' '\x1ECF'    -- LATIN SMALL LETTER O WITH HOOK ABOVE
  $ Entry 'U' '2' '\x1EE6'    -- LATIN CAPITAL LETTER U WITH HOOK ABOVE
  $ Entry 'u' '2' '\x1EE7'    -- LATIN SMALL LETTER U WITH HOOK ABOVE
  $ Entry 'Y' '!' '\x1EF2'    -- LATIN CAPITAL LETTER Y WITH GRAVE
  $ Entry 'y' '!' '\x1EF3'    -- LATIN SMALL LETTER Y WITH GRAVE
  $ Entry 'Y' '2' '\x1EF6'    -- LATIN CAPITAL LETTER Y WITH HOOK ABOVE
  $ Entry 'y' '2' '\x1EF7'    -- LATIN SMALL LETTER Y WITH HOOK ABOVE
  $ Entry 'Y' '?' '\x1EF8'    -- LATIN CAPITAL LETTER Y WITH TILDE
  $ Entry 'y' '?' '\x1EF9'    -- LATIN SMALL LETTER Y WITH TILDE
  $ Entry ';' '\'' '\x1F00'    -- GREEK SMALL LETTER ALPHA WITH PSILI
  $ Entry ',' '\'' '\x1F01'    -- GREEK SMALL LETTER ALPHA WITH DASIA
  $ Entry ';' '!' '\x1F02'    -- GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
  $ Entry ',' '!' '\x1F03'    -- GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
  $ Entry '?' ';' '\x1F04'    -- GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
  $ Entry '?' ',' '\x1F05'    -- GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
  $ Entry '!' ':' '\x1F06'    -- GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
  $ Entry '?' ':' '\x1F07'    -- GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
  $ Entry '1' 'N' '\x2002'    -- EN SPACE
  $ Entry '1' 'M' '\x2003'    -- EM SPACE
  $ Entry '3' 'M' '\x2004'    -- THREE-PER-EM SPACE
  $ Entry '4' 'M' '\x2005'    -- FOUR-PER-EM SPACE
  $ Entry '6' 'M' '\x2006'    -- SIX-PER-EM SPACE
  $ Entry '1' 'T' '\x2009'    -- THIN SPACE
  $ Entry '1' 'H' '\x200A'    -- HAIR SPACE
  $ Entry '-' '1' '\x2010'    -- HYPHEN
  $ Entry '-' 'N' '\x2013'    -- EN DASH `
  $ Entry '-' 'M' '\x2014'    -- EM DASH
  $ Entry '-' '3' '\x2015'    -- HORIZONTAL BAR
  $ Entry '!' '2' '\x2016'    -- DOUBLE VERTICAL LINE
  $ Entry '=' '2' '\x2017'    -- DOUBLE LOW LINE
  $ Entry '\'' '6' '\x2018'    -- LEFT SINGLE QUOTATION MARK
  $ Entry '\'' '9' '\x2019'    -- RIGHT SINGLE QUOTATION MARK
  $ Entry '.' '9' '\x201A'    -- SINGLE LOW-9 QUOTATION MARK
  $ Entry '9' '\'' '\x201B'    -- SINGLE HIGH-REVERSED-9 QUOTATION MARK
  $ Entry '"' '6' '\x201C'   -- LEFT DOUBLE QUOTATION MARK
  $ Entry '"' '9' '\x201D'   -- RIGHT DOUBLE QUOTATION MARK
  $ Entry ':' '9' '\x201E'    -- DOUBLE LOW-9 QUOTATION MARK
  $ Entry '9' '"' '\x201F'   -- DOUBLE HIGH-REVERSED-9 QUOTATION MARK
  $ Entry '/' '-' '\x2020'    -- DAGGER
  $ Entry '/' '=' '\x2021'    -- DOUBLE DAGGER
  $ Entry '.' '.' '\x2025'    -- TWO DOT LEADER
  $ Entry '%' '0' '\x2030'    -- PER MILLE SIGN
  $ Entry '1' '\'' '\x2032'    -- PRIME
  $ Entry '2' '\'' '\x2033'    -- DOUBLE PRIME
  $ Entry '3' '\'' '\x2034'    -- TRIPLE PRIME
  $ Entry '1' '"' '\x2035'   -- REVERSED PRIME
  $ Entry '2' '"' '\x2036'   -- REVERSED DOUBLE PRIME
  $ Entry '3' '"' '\x2037'   -- REVERSED TRIPLE PRIME
  $ Entry 'C' 'a' '\x2038'    -- CARET
  $ Entry '<' '1' '\x2039'    -- SINGLE LEFT-POINTING ANGLE QUOTATION MARK
  $ Entry '>' '1' '\x203A'    -- SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
  $ Entry ':' 'X' '\x203B'    -- REFERENCE MARK
  $ Entry '\'' '-' '\x203E'    -- OVERLINE
  $ Entry '/' 'f' '\x2044'    -- FRACTION SLASH
  $ Entry '0' 'S' '\x2070'    -- SUPERSCRIPT ZERO
  $ Entry '4' 'S' '\x2074'    -- SUPERSCRIPT FOUR
  $ Entry '5' 'S' '\x2075'    -- SUPERSCRIPT FIVE
  $ Entry '6' 'S' '\x2076'    -- SUPERSCRIPT SIX
  $ Entry '7' 'S' '\x2077'    -- SUPERSCRIPT SEVEN
  $ Entry '8' 'S' '\x2078'    -- SUPERSCRIPT EIGHT
  $ Entry '9' 'S' '\x2079'    -- SUPERSCRIPT NINE
  $ Entry '+' 'S' '\x207A'    -- SUPERSCRIPT PLUS SIGN
  $ Entry '-' 'S' '\x207B'    -- SUPERSCRIPT MINUS
  $ Entry '=' 'S' '\x207C'    -- SUPERSCRIPT EQUALS SIGN
  $ Entry '(' 'S' '\x207D'    -- SUPERSCRIPT LEFT PARENTHESIS
  $ Entry ')' 'S' '\x207E'    -- SUPERSCRIPT RIGHT PARENTHESIS
  $ Entry 'n' 'S' '\x207F'    -- SUPERSCRIPT LATIN SMALL LETTER N `
  $ Entry '0' 's' '\x2080'    -- SUBSCRIPT ZERO
  $ Entry '1' 's' '\x2081'    -- SUBSCRIPT ONE
  $ Entry '2' 's' '\x2082'    -- SUBSCRIPT TWO
  $ Entry '3' 's' '\x2083'    -- SUBSCRIPT THREE
  $ Entry '4' 's' '\x2084'    -- SUBSCRIPT FOUR
  $ Entry '5' 's' '\x2085'    -- SUBSCRIPT FIVE
  $ Entry '6' 's' '\x2086'    -- SUBSCRIPT SIX
  $ Entry '7' 's' '\x2087'    -- SUBSCRIPT SEVEN
  $ Entry '8' 's' '\x2088'    -- SUBSCRIPT EIGHT
  $ Entry '9' 's' '\x2089'    -- SUBSCRIPT NINE
  $ Entry '+' 's' '\x208A'    -- SUBSCRIPT PLUS SIGN
  $ Entry '-' 's' '\x208B'    -- SUBSCRIPT MINUS
  $ Entry '=' 's' '\x208C'    -- SUBSCRIPT EQUALS SIGN
  $ Entry '(' 's' '\x208D'    -- SUBSCRIPT LEFT PARENTHESIS
  $ Entry ')' 's' '\x208E'    -- SUBSCRIPT RIGHT PARENTHESIS
  $ Entry 'L' 'i' '\x20A4'    -- LIRA SIGN
  $ Entry 'P' 't' '\x20A7'    -- PESETA SIGN
  $ Entry 'W' '=' '\x20A9'    -- WON SIGN
  $ Entry 'E' 'u' '\x20AC'    -- EURO SIGN
  $ Entry 'o' 'C' '\x2103'    -- DEGREE CELSIUS
  $ Entry 'c' 'o' '\x2105'    -- CARE OF
  $ Entry 'o' 'F' '\x2109'    -- DEGREE FAHRENHEIT
  $ Entry 'N' '0' '\x2116'    -- NUMERO SIGN
  $ Entry 'P' 'O' '\x2117'    -- SOUND RECORDING COPYRIGHT
  $ Entry 'R' 'x' '\x211E'    -- PRESCRIPTION TAKE
  $ Entry 'S' 'M' '\x2120'    -- SERVICE MARK
  $ Entry 'T' 'M' '\x2122'    -- TRADE MARK SIGN
  $ Entry 'O' 'm' '\x2126'    -- OHM SIGN
  $ Entry 'A' 'O' '\x212B'    -- ANGSTROM SIGN
  $ Entry '1' '3' '\x2153'    -- VULGAR FRACTION ONE THIRD
  $ Entry '2' '3' '\x2154'    -- VULGAR FRACTION TWO THIRDS
  $ Entry '1' '5' '\x2155'    -- VULGAR FRACTION ONE FIFTH
  $ Entry '2' '5' '\x2156'    -- VULGAR FRACTION TWO FIFTHS
  $ Entry '3' '5' '\x2157'    -- VULGAR FRACTION THREE FIFTHS
  $ Entry '4' '5' '\x2158'    -- VULGAR FRACTION FOUR FIFTHS
  $ Entry '1' '6' '\x2159'    -- VULGAR FRACTION ONE SIXTH
  $ Entry '5' '6' '\x215A'    -- VULGAR FRACTION FIVE SIXTHS
  $ Entry '1' '8' '\x215B'    -- VULGAR FRACTION ONE EIGHTH
  $ Entry '3' '8' '\x215C'    -- VULGAR FRACTION THREE EIGHTHS
  $ Entry '5' '8' '\x215D'    -- VULGAR FRACTION FIVE EIGHTHS
  $ Entry '7' '8' '\x215E'    -- VULGAR FRACTION SEVEN EIGHTHS
  $ Entry '1' 'R' '\x2160'    -- ROMAN NUMERAL ONE
  $ Entry '2' 'R' '\x2161'    -- ROMAN NUMERAL TWO
  $ Entry '3' 'R' '\x2162'    -- ROMAN NUMERAL THREE
  $ Entry '4' 'R' '\x2163'    -- ROMAN NUMERAL FOUR
  $ Entry '5' 'R' '\x2164'    -- ROMAN NUMERAL FIVE
  $ Entry '6' 'R' '\x2165'    -- ROMAN NUMERAL SIX
  $ Entry '7' 'R' '\x2166'    -- ROMAN NUMERAL SEVEN
  $ Entry '8' 'R' '\x2167'    -- ROMAN NUMERAL EIGHT
  $ Entry '9' 'R' '\x2168'    -- ROMAN NUMERAL NINE
  $ Entry 'a' 'R' '\x2169'    -- ROMAN NUMERAL TEN
  $ Entry 'b' 'R' '\x216A'    -- ROMAN NUMERAL ELEVEN
  $ Entry 'c' 'R' '\x216B'    -- ROMAN NUMERAL TWELVE
  $ Entry '1' 'r' '\x2170'    -- SMALL ROMAN NUMERAL ONE
  $ Entry '2' 'r' '\x2171'    -- SMALL ROMAN NUMERAL TWO
  $ Entry '3' 'r' '\x2172'    -- SMALL ROMAN NUMERAL THREE
  $ Entry '4' 'r' '\x2173'    -- SMALL ROMAN NUMERAL FOUR
  $ Entry '5' 'r' '\x2174'    -- SMALL ROMAN NUMERAL FIVE
  $ Entry '6' 'r' '\x2175'    -- SMALL ROMAN NUMERAL SIX
  $ Entry '7' 'r' '\x2176'    -- SMALL ROMAN NUMERAL SEVEN
  $ Entry '8' 'r' '\x2177'    -- SMALL ROMAN NUMERAL EIGHT
  $ Entry '9' 'r' '\x2178'    -- SMALL ROMAN NUMERAL NINE
  $ Entry 'a' 'r' '\x2179'    -- SMALL ROMAN NUMERAL TEN
  $ Entry 'b' 'r' '\x217A'    -- SMALL ROMAN NUMERAL ELEVEN
  $ Entry 'c' 'r' '\x217B'    -- SMALL ROMAN NUMERAL TWELVE
  $ Entry '<' '-' '\x2190'    -- LEFTWARDS ARROW
  $ Entry '-' '!' '\x2191'    -- UPWARDS ARROW
  $ Entry '-' '>' '\x2192'    -- RIGHTWARDS ARROW
  $ Entry '-' 'v' '\x2193'    -- DOWNWARDS ARROW
  $ Entry '<' '>' '\x2194'    -- LEFT RIGHT ARROW
  $ Entry 'U' 'D' '\x2195'    -- UP DOWN ARROW
  $ Entry '<' '=' '\x21D0'    -- LEFTWARDS DOUBLE ARROW
  $ Entry '=' '>' '\x21D2'    -- RIGHTWARDS DOUBLE ARROW
  $ Entry '=' '=' '\x21D4'    -- LEFT RIGHT DOUBLE ARROW
  $ Entry 'F' 'A' '\x2200'    -- FOR ALL
  $ Entry 'd' 'P' '\x2202'    -- PARTIAL DIFFERENTIAL
  $ Entry 'T' 'E' '\x2203'    -- THERE EXISTS
  $ Entry '/' '0' '\x2205'    -- EMPTY SET
  $ Entry 'D' 'E' '\x2206'    -- INCREMENT
  $ Entry 'N' 'B' '\x2207'    -- NABLA
  $ Entry '(' '-' '\x2208'    -- ELEMENT OF
  $ Entry '-' ')' '\x220B'    -- CONTAINS AS MEMBER
  $ Entry '*' 'P' '\x220F'    -- N-ARY PRODUCT `
  $ Entry '+' 'Z' '\x2211'    -- N-ARY SUMMATION `
  $ Entry '-' '2' '\x2212'    -- MINUS SIGN
  $ Entry '-' '+' '\x2213'    -- MINUS-OR-PLUS SIGN
  $ Entry '*' '-' '\x2217'    -- ASTERISK OPERATOR
  $ Entry 'O' 'b' '\x2218'    -- RING OPERATOR
  $ Entry 'S' 'b' '\x2219'    -- BULLET OPERATOR
  $ Entry 'R' 'T' '\x221A'    -- SQUARE ROOT
  $ Entry '0' '(' '\x221D'    -- PROPORTIONAL TO
  $ Entry '0' '0' '\x221E'    -- INFINITY
  $ Entry '-' 'L' '\x221F'    -- RIGHT ANGLE
  $ Entry '-' 'V' '\x2220'    -- ANGLE
  $ Entry 'P' 'P' '\x2225'    -- PARALLEL TO
  $ Entry 'A' 'N' '\x2227'    -- LOGICAL AND
  $ Entry 'O' 'R' '\x2228'    -- LOGICAL OR
  $ Entry '(' 'U' '\x2229'    -- INTERSECTION
  $ Entry ')' 'U' '\x222A'    -- UNION
  $ Entry 'I' 'n' '\x222B'    -- INTEGRAL
  $ Entry 'D' 'I' '\x222C'    -- DOUBLE INTEGRAL
  $ Entry 'I' 'o' '\x222E'    -- CONTOUR INTEGRAL
  $ Entry '.' ':' '\x2234'    -- THEREFORE
  $ Entry ':' '.' '\x2235'    -- BECAUSE
  $ Entry ':' 'R' '\x2236'    -- RATIO
  $ Entry ':' ':' '\x2237'    -- PROPORTION
  $ Entry '?' '1' '\x223C'    -- TILDE OPERATOR
  $ Entry 'C' 'G' '\x223E'    -- INVERTED LAZY S
  $ Entry '?' '-' '\x2243'    -- ASYMPTOTICALLY EQUAL TO
  $ Entry '?' '=' '\x2245'    -- APPROXIMATELY EQUAL TO
  $ Entry '?' '2' '\x2248'    -- ALMOST EQUAL TO
  $ Entry '=' '?' '\x224C'    -- ALL EQUAL TO
  $ Entry 'H' 'I' '\x2253'    -- IMAGE OF OR APPROXIMATELY EQUAL TO
  $ Entry '!' '=' '\x2260'    -- NOT EQUAL TO
  $ Entry '=' '3' '\x2261'    -- IDENTICAL TO
  $ Entry '=' '<' '\x2264'    -- LESS-THAN OR EQUAL TO
  $ Entry '>' '=' '\x2265'    -- GREATER-THAN OR EQUAL TO
  $ Entry '<' '*' '\x226A'    -- MUCH LESS-THAN
  $ Entry '*' '>' '\x226B'    -- MUCH GREATER-THAN
  $ Entry '!' '<' '\x226E'    -- NOT LESS-THAN
  $ Entry '!' '>' '\x226F'    -- NOT GREATER-THAN
  $ Entry '(' 'C' '\x2282'    -- SUBSET OF
  $ Entry ')' 'C' '\x2283'    -- SUPERSET OF
  $ Entry '(' '_' '\x2286'    -- SUBSET OF OR EQUAL TO
  $ Entry ')' '_' '\x2287'    -- SUPERSET OF OR EQUAL TO
  $ Entry '0' '.' '\x2299'    -- CIRCLED DOT OPERATOR
  $ Entry '0' '2' '\x229A'    -- CIRCLED RING OPERATOR
  $ Entry '-' 'T' '\x22A5'    -- UP TACK
  $ Entry '.' 'P' '\x22C5'    -- DOT OPERATOR
  $ Entry ':' '3' '\x22EE'    -- VERTICAL ELLIPSIS
  $ Entry '.' '3' '\x22EF'    -- MIDLINE HORIZONTAL ELLIPSIS
  $ Entry 'E' 'h' '\x2302'    -- HOUSE
  $ Entry '<' '7' '\x2308'    -- LEFT CEILING
  $ Entry '>' '7' '\x2309'    -- RIGHT CEILING
  $ Entry '7' '<' '\x230A'    -- LEFT FLOOR
  $ Entry '7' '>' '\x230B'    -- RIGHT FLOOR
  $ Entry 'N' 'I' '\x2310'    -- REVERSED NOT SIGN
  $ Entry '(' 'A' '\x2312'    -- ARC
  $ Entry 'T' 'R' '\x2315'    -- TELEPHONE RECORDER
  $ Entry 'I' 'u' '\x2320'    -- TOP HALF INTEGRAL
  $ Entry 'I' 'l' '\x2321'    -- BOTTOM HALF INTEGRAL
  $ Entry '<' '/' '\x2329'    -- LEFT-POINTING ANGLE BRACKET
  $ Entry '/' '>' '\x232A'    -- RIGHT-POINTING ANGLE BRACKET
  $ Entry 'V' 's' '\x2423'    -- OPEN BOX
  $ Entry '1' 'h' '\x2440'    -- OCR HOOK
  $ Entry '3' 'h' '\x2441'    -- OCR CHAIR
  $ Entry '2' 'h' '\x2442'    -- OCR FORK
  $ Entry '4' 'h' '\x2443'    -- OCR INVERTED FORK
  $ Entry '1' 'j' '\x2446'    -- OCR BRANCH BANK IDENTIFICATION
  $ Entry '2' 'j' '\x2447'    -- OCR AMOUNT OF CHECK
  $ Entry '3' 'j' '\x2448'    -- OCR DASH
  $ Entry '4' 'j' '\x2449'    -- OCR CUSTOMER ACCOUNT NUMBER
  $ Entry '1' '.' '\x2488'    -- DIGIT ONE FULL STOP
  $ Entry '2' '.' '\x2489'    -- DIGIT TWO FULL STOP
  $ Entry '3' '.' '\x248A'    -- DIGIT THREE FULL STOP
  $ Entry '4' '.' '\x248B'    -- DIGIT FOUR FULL STOP
  $ Entry '5' '.' '\x248C'    -- DIGIT FIVE FULL STOP
  $ Entry '6' '.' '\x248D'    -- DIGIT SIX FULL STOP
  $ Entry '7' '.' '\x248E'    -- DIGIT SEVEN FULL STOP
  $ Entry '8' '.' '\x248F'    -- DIGIT EIGHT FULL STOP
  $ Entry '9' '.' '\x2490'    -- DIGIT NINE FULL STOP
  $ Entry 'h' 'h' '\x2500'    -- BOX DRAWINGS LIGHT HORIZONTAL
  $ Entry 'H' 'H' '\x2501'    -- BOX DRAWINGS HEAVY HORIZONTAL
  $ Entry 'v' 'v' '\x2502'    -- BOX DRAWINGS LIGHT VERTICAL
  $ Entry 'V' 'V' '\x2503'    -- BOX DRAWINGS HEAVY VERTICAL
  $ Entry '3' '-' '\x2504'    -- BOX DRAWINGS LIGHT TRIPLE DASH HORIZONTAL
  $ Entry '3' '_' '\x2505'    -- BOX DRAWINGS HEAVY TRIPLE DASH HORIZONTAL
  $ Entry '3' '!' '\x2506'    -- BOX DRAWINGS LIGHT TRIPLE DASH VERTICAL
  $ Entry '3' '/' '\x2507'    -- BOX DRAWINGS HEAVY TRIPLE DASH VERTICAL
  $ Entry '4' '-' '\x2508'    -- BOX DRAWINGS LIGHT QUADRUPLE DASH HORIZONTAL
  $ Entry '4' '_' '\x2509'    -- BOX DRAWINGS HEAVY QUADRUPLE DASH HORIZONTAL
  $ Entry '4' '!' '\x250A'    -- BOX DRAWINGS LIGHT QUADRUPLE DASH VERTICAL
  $ Entry '4' '/' '\x250B'    -- BOX DRAWINGS HEAVY QUADRUPLE DASH VERTICAL
  $ Entry 'd' 'r' '\x250C'    -- BOX DRAWINGS LIGHT DOWN AND RIGHT
  $ Entry 'd' 'R' '\x250D'    -- BOX DRAWINGS DOWN LIGHT AND RIGHT HEAVY
  $ Entry 'D' 'r' '\x250E'    -- BOX DRAWINGS DOWN HEAVY AND RIGHT LIGHT
  $ Entry 'D' 'R' '\x250F'    -- BOX DRAWINGS HEAVY DOWN AND RIGHT
  $ Entry 'd' 'l' '\x2510'    -- BOX DRAWINGS LIGHT DOWN AND LEFT
  $ Entry 'd' 'L' '\x2511'    -- BOX DRAWINGS DOWN LIGHT AND LEFT HEAVY
  $ Entry 'D' 'l' '\x2512'    -- BOX DRAWINGS DOWN HEAVY AND LEFT LIGHT
  $ Entry 'L' 'D' '\x2513'    -- BOX DRAWINGS HEAVY DOWN AND LEFT
  $ Entry 'u' 'r' '\x2514'    -- BOX DRAWINGS LIGHT UP AND RIGHT
  $ Entry 'u' 'R' '\x2515'    -- BOX DRAWINGS UP LIGHT AND RIGHT HEAVY
  $ Entry 'U' 'r' '\x2516'    -- BOX DRAWINGS UP HEAVY AND RIGHT LIGHT
  $ Entry 'U' 'R' '\x2517'    -- BOX DRAWINGS HEAVY UP AND RIGHT
  $ Entry 'u' 'l' '\x2518'    -- BOX VOICED SOUND MARKDRAWINGS LIGHT UP AND LEFT
  $ Entry 'u' 'L' '\x2519'    -- BOX DRAWINGS UP LIGHT AND LEFT HEAVY
  $ Entry 'U' 'l' '\x251A'    -- BOX DRAWINGS UP HEAVY AND LEFT LIGHT
  $ Entry 'U' 'L' '\x251B'    -- BOX DRAWINGS HEAVY UP AND LEFT
  $ Entry 'v' 'r' '\x251C'    -- BOX DRAWINGS LIGHT VERTICAL AND RIGHT
  $ Entry 'v' 'R' '\x251D'    -- BOX DRAWINGS VERTICAL LIGHT AND RIGHT HEAVY
  $ Entry 'V' 'r' '\x2520'    -- BOX DRAWINGS VERTICAL HEAVY AND RIGHT LIGHT
  $ Entry 'V' 'R' '\x2523'    -- BOX DRAWINGS HEAVY VERTICAL AND RIGHT
  $ Entry 'v' 'l' '\x2524'    -- BOX DRAWINGS LIGHT VERTICAL AND LEFT
  $ Entry 'v' 'L' '\x2525'    -- BOX DRAWINGS VERTICAL LIGHT AND LEFT HEAVY
  $ Entry 'V' 'l' '\x2528'    -- BOX DRAWINGS VERTICAL HEAVY AND LEFT LIGHT
  $ Entry 'V' 'L' '\x252B'    -- BOX DRAWINGS HEAVY VERTICAL AND LEFT
  $ Entry 'd' 'h' '\x252C'    -- BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
  $ Entry 'd' 'H' '\x252F'    -- BOX DRAWINGS DOWN LIGHT AND HORIZONTAL HEAVY
  $ Entry 'D' 'h' '\x2530'    -- BOX DRAWINGS DOWN HEAVY AND HORIZONTAL LIGHT
  $ Entry 'D' 'H' '\x2533'    -- BOX DRAWINGS HEAVY DOWN AND HORIZONTAL
  $ Entry 'u' 'h' '\x2534'    -- BOX DRAWINGS LIGHT UP AND HORIZONTAL
  $ Entry 'u' 'H' '\x2537'    -- BOX DRAWINGS UP LIGHT AND HORIZONTAL HEAVY
  $ Entry 'U' 'h' '\x2538'    -- BOX DRAWINGS UP HEAVY AND HORIZONTAL LIGHT
  $ Entry 'U' 'H' '\x253B'    -- BOX DRAWINGS HEAVY UP AND HORIZONTAL
  $ Entry 'v' 'h' '\x253C'    -- BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
  $ Entry 'v' 'H' '\x253F'    -- BOX DRAWINGS VERTICAL LIGHT AND HORIZONTAL HEAVY
  $ Entry 'V' 'h' '\x2542'    -- BOX DRAWINGS VERTICAL HEAVY AND HORIZONTAL LIGHT
  $ Entry 'V' 'H' '\x254B'    -- BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
  $ Entry 'F' 'D' '\x2571'    -- BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
  $ Entry 'B' 'D' '\x2572'    -- BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
  $ Entry 'T' 'B' '\x2580'    -- UPPER HALF BLOCK
  $ Entry 'L' 'B' '\x2584'    -- LOWER HALF BLOCK
  $ Entry 'F' 'B' '\x2588'    -- FULL BLOCK
  $ Entry 'l' 'B' '\x258C'    -- LEFT HALF BLOCK
  $ Entry 'R' 'B' '\x2590'    -- RIGHT HALF BLOCK
  $ Entry '.' 'S' '\x2591'    -- LIGHT SHADE
  $ Entry ':' 'S' '\x2592'    -- MEDIUM SHADE
  $ Entry '?' 'S' '\x2593'    -- DARK SHADE
  $ Entry 'f' 'S' '\x25A0'    -- BLACK SQUARE
  $ Entry 'O' 'S' '\x25A1'    -- WHITE SQUARE
  $ Entry 'R' 'O' '\x25A2'    -- WHITE SQUARE WITH ROUNDED CORNERS
  $ Entry 'R' 'r' '\x25A3'    -- WHITE SQUARE CONTAINING BLACK SMALL SQUARE
  $ Entry 'R' 'F' '\x25A4'    -- SQUARE WITH HORIZONTAL FILL
  $ Entry 'R' 'Y' '\x25A5'    -- SQUARE WITH VERTICAL FILL
  $ Entry 'R' 'H' '\x25A6'    -- SQUARE WITH ORTHOGONAL CROSSHATCH FILL
  $ Entry 'R' 'Z' '\x25A7'    -- SQUARE WITH UPPER LEFT TO LOWER RIGHT FILL
  $ Entry 'R' 'K' '\x25A8'    -- SQUARE WITH UPPER RIGHT TO LOWER LEFT FILL
  $ Entry 'R' 'X' '\x25A9'    -- SQUARE WITH DIAGONAL CROSSHATCH FILL
  $ Entry 's' 'B' '\x25AA'    -- BLACK SMALL SQUARE
  $ Entry 'S' 'R' '\x25AC'    -- BLACK RECTANGLE
  $ Entry 'O' 'r' '\x25AD'    -- WHITE RECTANGLE
  $ Entry 'U' 'T' '\x25B2'    -- BLACK UP-POINTING TRIANGLE
  $ Entry 'u' 'T' '\x25B3'    -- WHITE UP-POINTING TRIANGLE
  $ Entry 'P' 'R' '\x25B6'    -- BLACK RIGHT-POINTING TRIANGLE
  $ Entry 'T' 'r' '\x25B7'    -- WHITE RIGHT-POINTING TRIANGLE
  $ Entry 'D' 't' '\x25BC'    -- BLACK DOWN-POINTING TRIANGLE
  $ Entry 'd' 'T' '\x25BD'    -- WHITE DOWN-POINTING TRIANGLE
  $ Entry 'P' 'L' '\x25C0'    -- BLACK LEFT-POINTING TRIANGLE
  $ Entry 'T' 'l' '\x25C1'    -- WHITE LEFT-POINTING TRIANGLE
  $ Entry 'D' 'b' '\x25C6'    -- BLACK DIAMOND
  $ Entry 'D' 'w' '\x25C7'    -- WHITE DIAMOND
  $ Entry 'L' 'Z' '\x25CA'    -- LOZENGE
  $ Entry '0' 'm' '\x25CB'    -- WHITE CIRCLE
  $ Entry '0' 'o' '\x25CE'    -- BULLSEYE
  $ Entry '0' 'M' '\x25CF'    -- BLACK CIRCLE
  $ Entry '0' 'L' '\x25D0'    -- CIRCLE WITH LEFT HALF BLACK
  $ Entry '0' 'R' '\x25D1'    -- CIRCLE WITH RIGHT HALF BLACK
  $ Entry 'S' 'n' '\x25D8'    -- INVERSE BULLET
  $ Entry 'I' 'c' '\x25D9'    -- INVERSE WHITE CIRCLE
  $ Entry 'F' 'd' '\x25E2'    -- BLACK LOWER RIGHT TRIANGLE
  $ Entry 'B' 'd' '\x25E3'    -- BLACK LOWER LEFT TRIANGLE
  $ Entry '*' '2' '\x2605'    -- BLACK STAR
  $ Entry '*' '1' '\x2606'    -- WHITE STAR
  $ Entry '<' 'H' '\x261C'    -- WHITE LEFT POINTING INDEX
  $ Entry '>' 'H' '\x261E'    -- WHITE RIGHT POINTING INDEX
  $ Entry '0' 'u' '\x263A'    -- WHITE SMILING FACE
  $ Entry '0' 'U' '\x263B'    -- BLACK SMILING FACE
  $ Entry 'S' 'U' '\x263C'    -- WHITE SUN WITH RAYS
  $ Entry 'F' 'm' '\x2640'    -- FEMALE SIGN
  $ Entry 'M' 'l' '\x2642'    -- MALE SIGN
  $ Entry 'c' 'S' '\x2660'    -- BLACK SPADE SUIT
  $ Entry 'c' 'H' '\x2661'    -- WHITE HEART SUIT
  $ Entry 'c' 'D' '\x2662'    -- WHITE DIAMOND SUIT
  $ Entry 'c' 'C' '\x2663'    -- BLACK CLUB SUIT
  $ Entry 'M' 'd' '\x2669'    -- QUARTER NOTE `
  $ Entry 'M' '8' '\x266A'    -- EIGHTH NOTE `
  $ Entry 'M' '2' '\x266B'    -- BEAMED EIGHTH NOTES
  $ Entry 'M' 'b' '\x266D'    -- MUSIC FLAT SIGN
  $ Entry 'M' 'x' '\x266E'    -- MUSIC NATURAL SIGN
  $ Entry 'M' 'X' '\x266F'    -- MUSIC SHARP SIGN
  $ Entry 'O' 'K' '\x2713'    -- CHECK MARK
  $ Entry 'X' 'X' '\x2717'    -- BALLOT X
  $ Entry '-' 'X' '\x2720'    -- MALTESE CROSS
  $ Entry 'I' 'S' '\x3000'    -- IDEOGRAPHIC SPACE
  $ Entry ',' '_' '\x3001'    -- IDEOGRAPHIC COMMA
  $ Entry '.' '_' '\x3002'    -- IDEOGRAPHIC FULL STOP
  $ Entry '+' '"' '\x3003'   -- DITTO MARK
  $ Entry '+' '_' '\x3004'    -- JAPANESE INDUSTRIAL STANDARD SYMBOL
  $ Entry '*' '_' '\x3005'    -- IDEOGRAPHIC ITERATION MARK
  $ Entry ';' '_' '\x3006'    -- IDEOGRAPHIC CLOSING MARK
  $ Entry '0' '_' '\x3007'    -- IDEOGRAPHIC NUMBER ZERO
  $ Entry '<' '+' '\x300A'    -- LEFT DOUBLE ANGLE BRACKET
  $ Entry '>' '+' '\x300B'    -- RIGHT DOUBLE ANGLE BRACKET
  $ Entry '<' '\'' '\x300C'    -- LEFT CORNER BRACKET
  $ Entry '>' '\'' '\x300D'    -- RIGHT CORNER BRACKET
  $ Entry '<' '"' '\x300E'   -- LEFT WHITE CORNER BRACKET
  $ Entry '>' '"' '\x300F'   -- RIGHT WHITE CORNER BRACKET
  $ Entry '(' '"' '\x3010'   -- LEFT BLACK LENTICULAR BRACKET
  $ Entry ')' '"' '\x3011'   -- RIGHT BLACK LENTICULAR BRACKET
  $ Entry '=' 'T' '\x3012'    -- POSTAL MARK
  $ Entry '=' '_' '\x3013'    -- GETA MARK
  $ Entry '(' '\'' '\x3014'    -- LEFT TORTOISE SHELL BRACKET
  $ Entry ')' '\'' '\x3015'    -- RIGHT TORTOISE SHELL BRACKET
  $ Entry '(' 'I' '\x3016'    -- LEFT WHITE LENTICULAR BRACKET
  $ Entry ')' 'I' '\x3017'    -- RIGHT WHITE LENTICULAR BRACKET
  $ Entry '-' '?' '\x301C'    -- WAVE DASH
  $ Entry 'A' '5' '\x3041'    -- HIRAGANA LETTER SMALL A
  $ Entry 'a' '5' '\x3042'    -- HIRAGANA LETTER A
  $ Entry 'I' '5' '\x3043'    -- HIRAGANA LETTER SMALL I
  $ Entry 'i' '5' '\x3044'    -- HIRAGANA LETTER I
  $ Entry 'U' '5' '\x3045'    -- HIRAGANA LETTER SMALL U
  $ Entry 'u' '5' '\x3046'    -- HIRAGANA LETTER U
  $ Entry 'E' '5' '\x3047'    -- HIRAGANA LETTER SMALL E
  $ Entry 'e' '5' '\x3048'    -- HIRAGANA LETTER E
  $ Entry 'O' '5' '\x3049'    -- HIRAGANA LETTER SMALL O
  $ Entry 'o' '5' '\x304A'    -- HIRAGANA LETTER O
  $ Entry 'k' 'a' '\x304B'    -- HIRAGANA LETTER KA
  $ Entry 'g' 'a' '\x304C'    -- HIRAGANA LETTER GA
  $ Entry 'k' 'i' '\x304D'    -- HIRAGANA LETTER KI
  $ Entry 'g' 'i' '\x304E'    -- HIRAGANA LETTER GI
  $ Entry 'k' 'u' '\x304F'    -- HIRAGANA LETTER KU
  $ Entry 'g' 'u' '\x3050'    -- HIRAGANA LETTER GU
  $ Entry 'k' 'e' '\x3051'    -- HIRAGANA LETTER KE
  $ Entry 'g' 'e' '\x3052'    -- HIRAGANA LETTER GE
  $ Entry 'k' 'o' '\x3053'    -- HIRAGANA LETTER KO
  $ Entry 'g' 'o' '\x3054'    -- HIRAGANA LETTER GO
  $ Entry 's' 'a' '\x3055'    -- HIRAGANA LETTER SA
  $ Entry 'z' 'a' '\x3056'    -- HIRAGANA LETTER ZA
  $ Entry 's' 'i' '\x3057'    -- HIRAGANA LETTER SI
  $ Entry 'z' 'i' '\x3058'    -- HIRAGANA LETTER ZI
  $ Entry 's' 'u' '\x3059'    -- HIRAGANA LETTER SU
  $ Entry 'z' 'u' '\x305A'    -- HIRAGANA LETTER ZU
  $ Entry 's' 'e' '\x305B'    -- HIRAGANA LETTER SE
  $ Entry 'z' 'e' '\x305C'    -- HIRAGANA LETTER ZE
  $ Entry 's' 'o' '\x305D'    -- HIRAGANA LETTER SO
  $ Entry 'z' 'o' '\x305E'    -- HIRAGANA LETTER ZO
  $ Entry 't' 'a' '\x305F'    -- HIRAGANA LETTER TA
  $ Entry 'd' 'a' '\x3060'    -- HIRAGANA LETTER DA
  $ Entry 't' 'i' '\x3061'    -- HIRAGANA LETTER TI
  $ Entry 'd' 'i' '\x3062'    -- HIRAGANA LETTER DI
  $ Entry 't' 'U' '\x3063'    -- HIRAGANA LETTER SMALL TU
  $ Entry 't' 'u' '\x3064'    -- HIRAGANA LETTER TU
  $ Entry 'd' 'u' '\x3065'    -- HIRAGANA LETTER DU
  $ Entry 't' 'e' '\x3066'    -- HIRAGANA LETTER TE
  $ Entry 'd' 'e' '\x3067'    -- HIRAGANA LETTER DE
  $ Entry 't' 'o' '\x3068'    -- HIRAGANA LETTER TO
  $ Entry 'd' 'o' '\x3069'    -- HIRAGANA LETTER DO
  $ Entry 'n' 'a' '\x306A'    -- HIRAGANA LETTER NA
  $ Entry 'n' 'i' '\x306B'    -- HIRAGANA LETTER NI
  $ Entry 'n' 'u' '\x306C'    -- HIRAGANA LETTER NU
  $ Entry 'n' 'e' '\x306D'    -- HIRAGANA LETTER NE
  $ Entry 'n' 'o' '\x306E'    -- HIRAGANA LETTER NO
  $ Entry 'h' 'a' '\x306F'    -- HIRAGANA LETTER HA
  $ Entry 'b' 'a' '\x3070'    -- HIRAGANA LETTER BA
  $ Entry 'p' 'a' '\x3071'    -- HIRAGANA LETTER PA
  $ Entry 'h' 'i' '\x3072'    -- HIRAGANA LETTER HI
  $ Entry 'b' 'i' '\x3073'    -- HIRAGANA LETTER BI
  $ Entry 'p' 'i' '\x3074'    -- HIRAGANA LETTER PI
  $ Entry 'h' 'u' '\x3075'    -- HIRAGANA LETTER HU
  $ Entry 'b' 'u' '\x3076'    -- HIRAGANA LETTER BU
  $ Entry 'p' 'u' '\x3077'    -- HIRAGANA LETTER PU
  $ Entry 'h' 'e' '\x3078'    -- HIRAGANA LETTER HE
  $ Entry 'b' 'e' '\x3079'    -- HIRAGANA LETTER BE
  $ Entry 'p' 'e' '\x307A'    -- HIRAGANA LETTER PE
  $ Entry 'h' 'o' '\x307B'    -- HIRAGANA LETTER HO
  $ Entry 'b' 'o' '\x307C'    -- HIRAGANA LETTER BO
  $ Entry 'p' 'o' '\x307D'    -- HIRAGANA LETTER PO
  $ Entry 'm' 'a' '\x307E'    -- HIRAGANA LETTER MA
  $ Entry 'm' 'i' '\x307F'    -- HIRAGANA LETTER MI
  $ Entry 'm' 'u' '\x3080'    -- HIRAGANA LETTER MU
  $ Entry 'm' 'e' '\x3081'    -- HIRAGANA LETTER ME
  $ Entry 'm' 'o' '\x3082'    -- HIRAGANA LETTER MO
  $ Entry 'y' 'A' '\x3083'    -- HIRAGANA LETTER SMALL YA
  $ Entry 'y' 'a' '\x3084'    -- HIRAGANA LETTER YA
  $ Entry 'y' 'U' '\x3085'    -- HIRAGANA LETTER SMALL YU
  $ Entry 'y' 'u' '\x3086'    -- HIRAGANA LETTER YU
  $ Entry 'y' 'O' '\x3087'    -- HIRAGANA LETTER SMALL YO
  $ Entry 'y' 'o' '\x3088'    -- HIRAGANA LETTER YO
  $ Entry 'r' 'a' '\x3089'    -- HIRAGANA LETTER RA
  $ Entry 'r' 'i' '\x308A'    -- HIRAGANA LETTER RI
  $ Entry 'r' 'u' '\x308B'    -- HIRAGANA LETTER RU
  $ Entry 'r' 'e' '\x308C'    -- HIRAGANA LETTER RE
  $ Entry 'r' 'o' '\x308D'    -- HIRAGANA LETTER RO
  $ Entry 'w' 'A' '\x308E'    -- HIRAGANA LETTER SMALL WA
  $ Entry 'w' 'a' '\x308F'    -- HIRAGANA LETTER WA
  $ Entry 'w' 'i' '\x3090'    -- HIRAGANA LETTER WI
  $ Entry 'w' 'e' '\x3091'    -- HIRAGANA LETTER WE
  $ Entry 'w' 'o' '\x3092'    -- HIRAGANA LETTER WO
  $ Entry 'n' '5' '\x3093'    -- HIRAGANA LETTER N `
  $ Entry 'v' 'u' '\x3094'    -- HIRAGANA LETTER VU
  $ Entry '"' '5' '\x309B'   -- KATAKANA-HIRAGANA VOICED SOUND MARK
  $ Entry '0' '5' '\x309C'    -- KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
  $ Entry '*' '5' '\x309D'    -- HIRAGANA ITERATION MARK
  $ Entry '+' '5' '\x309E'    -- HIRAGANA VOICED ITERATION MARK
  $ Entry 'a' '6' '\x30A1'    -- KATAKANA LETTER SMALL A
  $ Entry 'A' '6' '\x30A2'    -- KATAKANA LETTER A
  $ Entry 'i' '6' '\x30A3'    -- KATAKANA LETTER SMALL I
  $ Entry 'I' '6' '\x30A4'    -- KATAKANA LETTER I
  $ Entry 'u' '6' '\x30A5'    -- KATAKANA LETTER SMALL U
  $ Entry 'U' '6' '\x30A6'    -- KATAKANA LETTER U
  $ Entry 'e' '6' '\x30A7'    -- KATAKANA LETTER SMALL E
  $ Entry 'E' '6' '\x30A8'    -- KATAKANA LETTER E
  $ Entry 'o' '6' '\x30A9'    -- KATAKANA LETTER SMALL O
  $ Entry 'O' '6' '\x30AA'    -- KATAKANA LETTER O
  $ Entry 'K' 'a' '\x30AB'    -- KATAKANA LETTER KA
  $ Entry 'G' 'a' '\x30AC'    -- KATAKANA LETTER GA
  $ Entry 'K' 'i' '\x30AD'    -- KATAKANA LETTER KI
  $ Entry 'G' 'i' '\x30AE'    -- KATAKANA LETTER GI
  $ Entry 'K' 'u' '\x30AF'    -- KATAKANA LETTER KU
  $ Entry 'G' 'u' '\x30B0'    -- KATAKANA LETTER GU
  $ Entry 'K' 'e' '\x30B1'    -- KATAKANA LETTER KE
  $ Entry 'G' 'e' '\x30B2'    -- KATAKANA LETTER GE
  $ Entry 'K' 'o' '\x30B3'    -- KATAKANA LETTER KO
  $ Entry 'G' 'o' '\x30B4'    -- KATAKANA LETTER GO
  $ Entry 'S' 'a' '\x30B5'    -- KATAKANA LETTER SA
  $ Entry 'Z' 'a' '\x30B6'    -- KATAKANA LETTER ZA
  $ Entry 'S' 'i' '\x30B7'    -- KATAKANA LETTER SI
  $ Entry 'Z' 'i' '\x30B8'    -- KATAKANA LETTER ZI
  $ Entry 'S' 'u' '\x30B9'    -- KATAKANA LETTER SU
  $ Entry 'Z' 'u' '\x30BA'    -- KATAKANA LETTER ZU
  $ Entry 'S' 'e' '\x30BB'    -- KATAKANA LETTER SE
  $ Entry 'Z' 'e' '\x30BC'    -- KATAKANA LETTER ZE
  $ Entry 'S' 'o' '\x30BD'    -- KATAKANA LETTER SO
  $ Entry 'Z' 'o' '\x30BE'    -- KATAKANA LETTER ZO
  $ Entry 'T' 'a' '\x30BF'    -- KATAKANA LETTER TA
  $ Entry 'D' 'a' '\x30C0'    -- KATAKANA LETTER DA
  $ Entry 'T' 'i' '\x30C1'    -- KATAKANA LETTER TI
  $ Entry 'D' 'i' '\x30C2'    -- KATAKANA LETTER DI
  $ Entry 'T' 'U' '\x30C3'    -- KATAKANA LETTER SMALL TU
  $ Entry 'T' 'u' '\x30C4'    -- KATAKANA LETTER TU
  $ Entry 'D' 'u' '\x30C5'    -- KATAKANA LETTER DU
  $ Entry 'T' 'e' '\x30C6'    -- KATAKANA LETTER TE
  $ Entry 'D' 'e' '\x30C7'    -- KATAKANA LETTER DE
  $ Entry 'T' 'o' '\x30C8'    -- KATAKANA LETTER TO
  $ Entry 'D' 'o' '\x30C9'    -- KATAKANA LETTER DO
  $ Entry 'N' 'a' '\x30CA'    -- KATAKANA LETTER NA
  $ Entry 'N' 'i' '\x30CB'    -- KATAKANA LETTER NI
  $ Entry 'N' 'u' '\x30CC'    -- KATAKANA LETTER NU
  $ Entry 'N' 'e' '\x30CD'    -- KATAKANA LETTER NE
  $ Entry 'N' 'o' '\x30CE'    -- KATAKANA LETTER NO
  $ Entry 'H' 'a' '\x30CF'    -- KATAKANA LETTER HA
  $ Entry 'B' 'a' '\x30D0'    -- KATAKANA LETTER BA
  $ Entry 'P' 'a' '\x30D1'    -- KATAKANA LETTER PA
  $ Entry 'H' 'i' '\x30D2'    -- KATAKANA LETTER HI
  $ Entry 'B' 'i' '\x30D3'    -- KATAKANA LETTER BI
  $ Entry 'P' 'i' '\x30D4'    -- KATAKANA LETTER PI
  $ Entry 'H' 'u' '\x30D5'    -- KATAKANA LETTER HU
  $ Entry 'B' 'u' '\x30D6'    -- KATAKANA LETTER BU
  $ Entry 'P' 'u' '\x30D7'    -- KATAKANA LETTER PU
  $ Entry 'H' 'e' '\x30D8'    -- KATAKANA LETTER HE
  $ Entry 'B' 'e' '\x30D9'    -- KATAKANA LETTER BE
  $ Entry 'P' 'e' '\x30DA'    -- KATAKANA LETTER PE
  $ Entry 'H' 'o' '\x30DB'    -- KATAKANA LETTER HO
  $ Entry 'B' 'o' '\x30DC'    -- KATAKANA LETTER BO
  $ Entry 'P' 'o' '\x30DD'    -- KATAKANA LETTER PO
  $ Entry 'M' 'a' '\x30DE'    -- KATAKANA LETTER MA
  $ Entry 'M' 'i' '\x30DF'    -- KATAKANA LETTER MI
  $ Entry 'M' 'u' '\x30E0'    -- KATAKANA LETTER MU
  $ Entry 'M' 'e' '\x30E1'    -- KATAKANA LETTER ME
  $ Entry 'M' 'o' '\x30E2'    -- KATAKANA LETTER MO
  $ Entry 'Y' 'A' '\x30E3'    -- KATAKANA LETTER SMALL YA
  $ Entry 'Y' 'a' '\x30E4'    -- KATAKANA LETTER YA
  $ Entry 'Y' 'U' '\x30E5'    -- KATAKANA LETTER SMALL YU
  $ Entry 'Y' 'u' '\x30E6'    -- KATAKANA LETTER YU
  $ Entry 'Y' 'O' '\x30E7'    -- KATAKANA LETTER SMALL YO
  $ Entry 'Y' 'o' '\x30E8'    -- KATAKANA LETTER YO
  $ Entry 'R' 'a' '\x30E9'    -- KATAKANA LETTER RA
  $ Entry 'R' 'i' '\x30EA'    -- KATAKANA LETTER RI
  $ Entry 'R' 'u' '\x30EB'    -- KATAKANA LETTER RU
  $ Entry 'R' 'e' '\x30EC'    -- KATAKANA LETTER RE
  $ Entry 'R' 'o' '\x30ED'    -- KATAKANA LETTER RO
  $ Entry 'W' 'A' '\x30EE'    -- KATAKANA LETTER SMALL WA
  $ Entry 'W' 'a' '\x30EF'    -- KATAKANA LETTER WA
  $ Entry 'W' 'i' '\x30F0'    -- KATAKANA LETTER WI
  $ Entry 'W' 'e' '\x30F1'    -- KATAKANA LETTER WE
  $ Entry 'W' 'o' '\x30F2'    -- KATAKANA LETTER WO
  $ Entry 'N' '6' '\x30F3'    -- KATAKANA LETTER N `
  $ Entry 'V' 'u' '\x30F4'    -- KATAKANA LETTER VU
  $ Entry 'K' 'A' '\x30F5'    -- KATAKANA LETTER SMALL KA
  $ Entry 'K' 'E' '\x30F6'    -- KATAKANA LETTER SMALL KE
  $ Entry 'V' 'a' '\x30F7'    -- KATAKANA LETTER VA
  $ Entry 'V' 'i' '\x30F8'    -- KATAKANA LETTER VI
  $ Entry 'V' 'e' '\x30F9'    -- KATAKANA LETTER VE
  $ Entry 'V' 'o' '\x30FA'    -- KATAKANA LETTER VO
  $ Entry '.' '6' '\x30FB'    -- KATAKANA MIDDLE DOT
  $ Entry '-' '6' '\x30FC'    -- KATAKANA-HIRAGANA PROLONGED SOUND MARK
  $ Entry '*' '6' '\x30FD'    -- KATAKANA ITERATION MARK
  $ Entry '+' '6' '\x30FE'    -- KATAKANA VOICED ITERATION MARK
  $ Entry 'b' '4' '\x3105'    -- BOPOMOFO LETTER B
  $ Entry 'p' '4' '\x3106'    -- BOPOMOFO LETTER P
  $ Entry 'm' '4' '\x3107'    -- BOPOMOFO LETTER M
  $ Entry 'f' '4' '\x3108'    -- BOPOMOFO LETTER F
  $ Entry 'd' '4' '\x3109'    -- BOPOMOFO LETTER D
  $ Entry 't' '4' '\x310A'    -- BOPOMOFO LETTER T
  $ Entry 'n' '4' '\x310B'    -- BOPOMOFO LETTER N `
  $ Entry 'l' '4' '\x310C'    -- BOPOMOFO LETTER L
  $ Entry 'g' '4' '\x310D'    -- BOPOMOFO LETTER G
  $ Entry 'k' '4' '\x310E'    -- BOPOMOFO LETTER K
  $ Entry 'h' '4' '\x310F'    -- BOPOMOFO LETTER H
  $ Entry 'j' '4' '\x3110'    -- BOPOMOFO LETTER J
  $ Entry 'q' '4' '\x3111'    -- BOPOMOFO LETTER Q
  $ Entry 'x' '4' '\x3112'    -- BOPOMOFO LETTER X
  $ Entry 'z' 'h' '\x3113'    -- BOPOMOFO LETTER ZH
  $ Entry 'c' 'h' '\x3114'    -- BOPOMOFO LETTER CH
  $ Entry 's' 'h' '\x3115'    -- BOPOMOFO LETTER SH
  $ Entry 'r' '4' '\x3116'    -- BOPOMOFO LETTER R
  $ Entry 'z' '4' '\x3117'    -- BOPOMOFO LETTER Z
  $ Entry 'c' '4' '\x3118'    -- BOPOMOFO LETTER C
  $ Entry 's' '4' '\x3119'    -- BOPOMOFO LETTER S
  $ Entry 'a' '4' '\x311A'    -- BOPOMOFO LETTER A
  $ Entry 'o' '4' '\x311B'    -- BOPOMOFO LETTER O
  $ Entry 'e' '4' '\x311C'    -- BOPOMOFO LETTER E
  $ Entry 'a' 'i' '\x311E'    -- BOPOMOFO LETTER AI
  $ Entry 'e' 'i' '\x311F'    -- BOPOMOFO LETTER EI
  $ Entry 'a' 'u' '\x3120'    -- BOPOMOFO LETTER AU
  $ Entry 'o' 'u' '\x3121'    -- BOPOMOFO LETTER OU
  $ Entry 'a' 'n' '\x3122'    -- BOPOMOFO LETTER AN
  $ Entry 'e' 'n' '\x3123'    -- BOPOMOFO LETTER EN
  $ Entry 'a' 'N' '\x3124'    -- BOPOMOFO LETTER ANG
  $ Entry 'e' 'N' '\x3125'    -- BOPOMOFO LETTER ENG
  $ Entry 'e' 'r' '\x3126'    -- BOPOMOFO LETTER ER
  $ Entry 'i' '4' '\x3127'    -- BOPOMOFO LETTER I
  $ Entry 'u' '4' '\x3128'    -- BOPOMOFO LETTER U
  $ Entry 'i' 'u' '\x3129'    -- BOPOMOFO LETTER IU
  $ Entry 'v' '4' '\x312A'    -- BOPOMOFO LETTER V
  $ Entry 'n' 'G' '\x312B'    -- BOPOMOFO LETTER NG
  $ Entry 'g' 'n' '\x312C'    -- BOPOMOFO LETTER GN
  $ Entry '1' 'c' '\x3220'    -- PARENTHESIZED IDEOGRAPH ONE
  $ Entry '2' 'c' '\x3221'    -- PARENTHESIZED IDEOGRAPH TWO
  $ Entry '3' 'c' '\x3222'    -- PARENTHESIZED IDEOGRAPH THREE
  $ Entry '4' 'c' '\x3223'    -- PARENTHESIZED IDEOGRAPH FOUR
  $ Entry '5' 'c' '\x3224'    -- PARENTHESIZED IDEOGRAPH FIVE
  $ Entry '6' 'c' '\x3225'    -- PARENTHESIZED IDEOGRAPH SIX
  $ Entry '7' 'c' '\x3226'    -- PARENTHESIZED IDEOGRAPH SEVEN
  $ Entry '8' 'c' '\x3227'    -- PARENTHESIZED IDEOGRAPH EIGHT
  $ Entry '9' 'c' '\x3228'    -- PARENTHESIZED IDEOGRAPH NINE
  $ Entry 'f' 'f' '\xFB00'    -- LATIN SMALL LIGATURE FF
  $ Entry 'f' 'i' '\xFB01'    -- LATIN SMALL LIGATURE FI
  $ Entry 'f' 'l' '\xFB02'    -- LATIN SMALL LIGATURE FL
  $ Entry 'f' 't' '\xFB05'    -- LATIN SMALL LIGATURE LONG S T
  $ Entry 's' 't' '\xFB06'    -- LATIN SMALL LIGATURE ST
  $ Nil
