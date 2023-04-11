{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language LambdaCase #-}

module Main where

import System.Environment (getArgs)
import Data.Builder.Catenable.Text (Builder)
import Data.Word (Word8,Word32,Word64)
import System.IO (Handle)
import Data.Bits (testBit)

import qualified Data.Builder.Catenable.Text as Builder
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes as Bytes
import qualified Asn.Ber as Ber
import qualified Asn.Oid as Oid
import qualified System.IO as IO
import qualified Data.Text.Short as TS
import qualified Chronos

main :: IO ()
main = getArgs >>= \case
  [] -> action IO.stdin
  [name] -> action =<< IO.openFile name IO.ReadMode
  _ -> fail "Expected at most one argument"

action :: Handle -> IO ()
action !h = do
  c <- Chunks.hGetContents h
  let b = Chunks.concat c
  case Ber.decode b of
    Left e -> fail ("Could not decode, failed with: " ++ e)
    Right v -> Chunks.hPut IO.stdout (Builder.run (encodeValue 0 v))

encodeValue :: Int -> Ber.Value -> Builder
encodeValue !pad Ber.Value{tagClass,tagNumber,contents} =
  Builder.shortText (TS.replicate pad (TS.singleton ' '))
  <>
  encodeClass tagClass
  <>
  Builder.char ':'
  <>
  Builder.word32Dec tagNumber
  <>
  Builder.char ':'
  <>
  Builder.char ' '
  <>
  encodeContents (pad + 2) tagNumber contents
  -- ( case tagNumber of
  --     Ber.Sequence | Ber.decode
  --     _ -> 
  -- )

encodeClass :: Ber.Class -> Builder
encodeClass = \case
  Ber.Universal -> "universal"
  Ber.Application -> "application"
  Ber.ContextSpecific -> "context"
  Ber.Private -> "private"

encodeContents :: Int -> Word32 -> Ber.Contents -> Builder
encodeContents !pad !tagNumber v0 = case v0 of
  Ber.Integer i -> Builder.int64Dec i <> "\n"
  Ber.Null -> Builder.shortText "null\n"
  Ber.ObjectIdentifier oid -> "[oid] " <> Builder.shortText (Oid.toShortText oid) <> "\n"
  Ber.Boolean b -> case b of
    True -> "true\n"
    False -> "false\n"
  Ber.Utf8String t -> "[utf8-string] " <> Builder.shortText t <> "\n"
  Ber.PrintableString t -> "[printable-string] " <> Builder.shortText t <> "\n"
  Ber.UtcTime epochSeconds -> "[utc-time] "
    <> Builder.shortText
       ( Chronos.encodeShortTextIso8601Zulu
         ( Chronos.timeToDatetime (Chronos.Time (1_000_000_000 * epochSeconds))
         )
       )
    <> "\n"
  Ber.OctetString b -> "[octet-string] "
    <> ( if | Bytes.all (\w -> w >= 0x20 && w < 127) b
            , Just t <- TS.fromShortByteString (Bytes.toShortByteString b)
              -> Builder.shortText t
            | otherwise -> Builder.shortText (TS.pack (show b))
       )
    <> "\n"
  Ber.Constructed vals ->
    ( case tagNumber of
        Ber.Sequence -> "sequence"
        Ber.Set -> "set"
        _ -> "constructed"
    ) <> "\n" <> foldMap (encodeValue pad) vals
  Ber.BitString padding payload ->
    "[bit-string, length "
    <>
    Builder.word64Dec (fromIntegral @Int @Word64 (Bytes.length payload * 8 - fromIntegral @Word8 @Int padding))
    <>
    "] "
    <>
    ( case padding of
        0 -> Builder.shortText (TS.pack (show payload))
        _ -> case Bytes.length payload of
          0 -> "{empty}"
          _ ->
            let finalByte = Bytes.unsafeIndex payload (Bytes.length payload - 1)
                leadingBytes = Bytes.unsafeTake (Bytes.length payload - 1) payload
             in Bytes.foldr
                  (\w acc -> encodeBitsFrom 0 w <> acc)
                  (encodeBitsFrom (fromIntegral @Word8 @Int padding) finalByte)
                  leadingBytes
    )
    <>
    "\n"
  Ber.Unresolved{} -> "[unresolved]\n"

encodeBitsFrom ::
     Int -- lowest position (0-7) 
  -> Word8
  -> Builder
encodeBitsFrom !ix !w = if ix >= 8
  then mempty
  else encodeBitsFrom (ix + 1) w <> (if testBit w ix then "1," else "0,")

