{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Text.Utf8 
(
  Utf8String,
  byteWidth, Data.Text.Utf8.isPrefixOf, 
  Data.Text.Utf8.cons
) where

import Data.String (IsString)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bool (bool)
import Data.Bifunctor (bimap)
import Text.Megaparsec (PosState(..), SourcePos(..), Stream(..), Pos, 
                        unPos, pos1, mkPos)
import Data.Text.Short (ShortText)

import Data.ByteString.Short as BSS
import Data.List.NonEmpty as NE
import Data.Text.Short as TS

newtype Utf8String = Utf8String { expose :: ShortText }
  deriving (Eq, Ord, Show, Hashable, IsString)

instance Stream Utf8String where
  type Token Utf8String = Char
  type Tokens Utf8String = Utf8String
  {-# INLINE tokenToChunk #-}
  tokenToChunk _ = Utf8String . TS.singleton
  {-# INLINE tokensToChunk #-}
  tokensToChunk _ = Utf8String . TS.pack
  {-# INLINE chunkToTokens #-}
  chunkToTokens _ = TS.unpack . expose
  {-# INLINE chunkLength #-}
  chunkLength _ = TS.length . expose
  {-# INLINE take1_ #-}
  take1_ = fmap (fmap Utf8String) . TS.uncons . expose
  {-# INLINE takeN_ #-}
  takeN_ n str@(Utf8String st) = 
    if TS.null st
    then bool (Just (Utf8String TS.empty, str)) Nothing (n > 0)
    else Just . bimap Utf8String Utf8String . TS.splitAt n $ st
  {-# INLINE takeWhile_ #-}
  takeWhile_ p (Utf8String st) = bimap Utf8String Utf8String . TS.span p $ st
  {-# INLINE showTokens #-}
  showTokens _ = stringPretty
  {-# INLINE reachOffset #-}
  reachOffset = reachOffset' splitter folder unpacker ('\n', '\t')
    where splitter pos = bimap Utf8String Utf8String . TS.splitAt pos . expose
          folder f x = TS.foldl' f x . expose
          unpacker = TS.toString . expose

-- Interface functions

{-# INLINE byteWidth #-}
byteWidth :: Utf8String -> Int
byteWidth = BSS.length . TS.toShortByteString . expose

{-# INLINE isPrefixOf #-}
isPrefixOf :: Utf8String -> Utf8String -> Bool
isPrefixOf (Utf8String s) (Utf8String t) = s `TS.isPrefixOf` t

{-# INLINE cons #-}
cons :: Char -> Utf8String -> Utf8String
cons c = Utf8String . TS.cons c . expose

-- Helpers

-- Adapted from Megaparsec
data St = St SourcePos ShowS

{-# INLINE reachOffset' #-}
reachOffset' :: (Int -> Utf8String -> (Utf8String, Utf8String)) -- how to split
  -> (forall b . (b -> Char -> b) -> b -> Utf8String -> b) -- how to fold
  -> (Utf8String -> String)
  -> (Char, Char) -- newline, tab
  -> Int -- offset
  -> PosState Utf8String
  -> (String, PosState Utf8String)
reachOffset' splitAt' foldl'' fromToks (newlineTok, tabTok) o PosState{..} = 
  (output, newPosState)
  where newPosState = PosState { pstateInput = post, 
                                 pstateOffset = max pstateOffset o,
                                 pstateSourcePos = spos,
                                 pstateTabWidth = pstateTabWidth,
                                 pstateLinePrefix = if sameLine
                                                    then pstateLinePrefix ++ f ""
                                                    else f "" }
        (pre, post) = splitAt' (o - pstateOffset) pstateInput
        St spos f = foldl'' go (St pstateSourcePos id) pre
        sameLine = sourceLine spos == sourceLine pstateSourcePos
        go (St apos g) ch = 
          let SourcePos n l c = apos
              c' = unPos c
              w = unPos pstateTabWidth in
            if | ch == newlineTok -> St (SourcePos n (l <> pos1) pos1) id
               | ch == tabTok -> St (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
                                    (g . (ch :))
               | otherwise -> St (SourcePos n l (c <> pos1)) (g . (ch :))
        output = case expandTab pstateTabWidth . addPrefix . f . fromToks . fst $ takeWhile_ (/= newlineTok) post of
                  "" -> "<empty line>"
                  xs -> xs
        addPrefix xs = if sameLine
                       then pstateLinePrefix ++ xs
                       else xs

{-# INLINE expandTab #-}
expandTab :: Pos -> String -> String
expandTab w' = go 0
  where go 0 [] = []
        go 0 ('\t' : xs) = go w xs
        go 0 (x : xs) = x : go 0 xs
        go n xs = ' ' : go (n - 1) xs
        w = unPos w'

{-# INLINE stringPretty #-}
stringPretty :: NonEmpty Char -> String
stringPretty (x :| []) = charPretty x
stringPretty ('\r' :| "\n") = "crlf newline"
stringPretty xs = "\"" <> foldMap f (NE.toList xs) <> "\""
  where f ch = case charPretty' ch of
          Nothing -> [ch]
          Just pretty -> "<" <> pretty <> ">"

{-# INLINE charPretty #-}
charPretty :: Char -> String
charPretty ' ' = "space"
charPretty ch = fromMaybe ("'" <> [ch] <> "'") (charPretty' ch)

{-# INLINE charPretty' #-}
charPretty' :: Char -> Maybe String
charPretty' = \case
  '\NUL' -> Just "null"
  '\SOH' -> Just "start of heading"
  '\STX' -> Just "start of text"
  '\ETX' -> Just "end of text"
  '\EOT' -> Just "end of transmission"
  '\ENQ' -> Just "enquiry"
  '\ACK' -> Just "acknowledge"
  '\BEL' -> Just "bell"
  '\BS'  -> Just "backspace"
  '\t'   -> Just "tab"
  '\n'   -> Just "newline"
  '\v'   -> Just "vertical tab"
  '\f'   -> Just "form feed"
  '\r'   -> Just "carriage return"
  '\SO'  -> Just "shift out"
  '\SI'  -> Just "shift in"
  '\DLE' -> Just "data link escape"
  '\DC1' -> Just "device control one"
  '\DC2' -> Just "device control two"
  '\DC3' -> Just "device control three"
  '\DC4' -> Just "device control four"
  '\NAK' -> Just "negative acknowledge"
  '\SYN' -> Just "synchronous idle"
  '\ETB' -> Just "end of transmission block"
  '\CAN' -> Just "cancel"
  '\EM'  -> Just "end of medium"
  '\SUB' -> Just "substitute"
  '\ESC' -> Just "escape"
  '\FS'  -> Just "file separator"
  '\GS'  -> Just "group separator"
  '\RS'  -> Just "record separator"
  '\US'  -> Just "unit separator"
  '\DEL' -> Just "delete"
  '\160' -> Just "non-breaking space"
  _      -> Nothing
