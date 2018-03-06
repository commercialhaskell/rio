-- | Unicode @Char@. Import as:
--
-- > import qualified RIO.Char as C
module RIO.Char
  (
    Data.Char.Char

  -- * Character classification
  -- | Unicode characters are divided into letters, Data.Char.numbers, marks,
  -- punctuation, Data.Char.symbols, separators (including spaces) and others
  -- (including control characters).
  , Data.Char.isControl
  , Data.Char.isSpace
  , Data.Char.isLower
  , Data.Char.isUpper
  , Data.Char.isAlpha
  , Data.Char.isAlphaNum
  , Data.Char.isPrint
  , Data.Char.isDigit
  , Data.Char.isOctDigit
  , Data.Char.isHexDigit
  , Data.Char.isLetter
  , Data.Char.isMark
  , Data.Char.isNumber
  , Data.Char.isPunctuation
  , Data.Char.isSymbol
  , Data.Char.isSeparator

  -- ** Subranges
  , Data.Char.isAscii
  , Data.Char.isLatin1
  , Data.Char.isAsciiUpper
  , Data.Char.isAsciiLower

  -- ** Unicode general categories
  , Data.Char.GeneralCategory(..)
  , Data.Char.generalCategory

  -- * Case conversion
  , Data.Char.toUpper
  , Data.Char.toLower
  , Data.Char.toTitle

  -- * Numeric representations
  , Data.Char.ord

  -- * String representations
  , Data.Char.showLitChar
  , Data.Char.lexLitChar
  , Data.Char.readLitChar
  ) where

import qualified Data.Char
