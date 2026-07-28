module MyFramework.TrustBase.Digest
  ( sha256
  , sha256VectorsValid
  ) where

import Data.Array
  ( Array
  , (!)
  , listArray
  )
import Data.Bits
  ( complement
  , rotateR
  , shiftR
  , xor
  , (.&.)
  )
import Data.Char
  ( ord )
import Data.List
  ( foldl'
  )
import Data.Word
  ( Word32
  , Word64
  , Word8
  )
import Numeric
  ( showHex )

sha256 :: String -> String
sha256 currentText =
  concatMap renderWord32 finalState
  where
    finalState =
      foldl'
        compressChunk
        initialState
        (chunksOf 64 (padMessage (utf8Bytes currentText)))

sha256VectorsValid :: Bool
sha256VectorsValid =
  and
    [ sha256 ""
        == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    , sha256 "abc"
        == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    ]

initialState :: [Word32]
initialState =
  [ 0x6a09e667
  , 0xbb67ae85
  , 0x3c6ef372
  , 0xa54ff53a
  , 0x510e527f
  , 0x9b05688c
  , 0x1f83d9ab
  , 0x5be0cd19
  ]

roundConstants :: Array Int Word32
roundConstants =
  listArray
    (0, 63)
    [ 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5
    , 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
    , 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3
    , 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
    , 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc
    , 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
    , 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7
    , 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
    , 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13
    , 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
    , 0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3
    , 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
    , 0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5
    , 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
    , 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208
    , 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
    ]

compressChunk :: [Word32] -> [Word8] -> [Word32]
compressChunk currentHash currentChunk =
  zipWith (+) currentHash finalWorking
  where
    schedule :: Array Int Word32
    schedule =
      listArray
        (0, 63)
        [ scheduleWord currentIndex
        | currentIndex <- [0 .. 63]
        ]
    scheduleWord currentIndex
      | currentIndex < 16 =
          word32At (currentIndex * 4) currentChunk
      | otherwise =
          smallSigma1 (schedule ! (currentIndex - 2))
            + schedule ! (currentIndex - 7)
            + smallSigma0 (schedule ! (currentIndex - 15))
            + schedule ! (currentIndex - 16)
    finalWorking =
      foldl'
        (roundStep schedule)
        currentHash
        [0 .. 63]

roundStep ::
  Array Int Word32 ->
  [Word32] ->
  Int ->
  [Word32]
roundStep schedule working currentIndex =
  case working of
    [a, b, c, d, e, f, g, h] ->
      [ temporary1 + temporary2
      , a
      , b
      , c
      , d + temporary1
      , e
      , f
      , g
      ]
      where
        temporary1 =
          h
            + bigSigma1 e
            + choose e f g
            + roundConstants ! currentIndex
            + schedule ! currentIndex
        temporary2 =
          bigSigma0 a + majority a b c
    _ ->
      error "sha256 internal state width"

choose :: Word32 -> Word32 -> Word32 -> Word32
choose x y z =
  (x .&. y) `xor` (complement x .&. z)

majority :: Word32 -> Word32 -> Word32 -> Word32
majority x y z =
  (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

bigSigma0 :: Word32 -> Word32
bigSigma0 value =
  rotateR value 2 `xor` rotateR value 13 `xor` rotateR value 22

bigSigma1 :: Word32 -> Word32
bigSigma1 value =
  rotateR value 6 `xor` rotateR value 11 `xor` rotateR value 25

smallSigma0 :: Word32 -> Word32
smallSigma0 value =
  rotateR value 7 `xor` rotateR value 18 `xor` shiftR value 3

smallSigma1 :: Word32 -> Word32
smallSigma1 value =
  rotateR value 17 `xor` rotateR value 19 `xor` shiftR value 10

word32At :: Int -> [Word8] -> Word32
word32At currentIndex currentBytes =
  foldl'
    (\currentWord currentByte -> currentWord * 256 + fromIntegral currentByte)
    0
    (take 4 (drop currentIndex currentBytes))

padMessage :: [Word8] -> [Word8]
padMessage currentBytes =
  currentBytes
    ++ [0x80]
    ++ replicate zeroCount 0
    ++ word64Bytes bitLength
  where
    bitLength =
      fromIntegral (length currentBytes) * 8 :: Word64
    zeroCount =
      (56 - ((length currentBytes + 1) `mod` 64)) `mod` 64

word64Bytes :: Word64 -> [Word8]
word64Bytes currentWord =
  [ fromIntegral (shiftR currentWord currentShift .&. 0xff)
  | currentShift <- [56, 48 .. 0]
  ]

utf8Bytes :: String -> [Word8]
utf8Bytes =
  concatMap encodeChar

encodeChar :: Char -> [Word8]
encodeChar currentChar
  | currentCode <= 0x7f =
      [fromIntegral currentCode]
  | currentCode <= 0x7ff =
      [ fromIntegral (0xc0 + shiftR currentCode 6)
      , fromIntegral (0x80 + (currentCode .&. 0x3f))
      ]
  | currentCode <= 0xffff =
      [ fromIntegral (0xe0 + shiftR currentCode 12)
      , fromIntegral (0x80 + (shiftR currentCode 6 .&. 0x3f))
      , fromIntegral (0x80 + (currentCode .&. 0x3f))
      ]
  | otherwise =
      [ fromIntegral (0xf0 + shiftR currentCode 18)
      , fromIntegral (0x80 + (shiftR currentCode 12 .&. 0x3f))
      , fromIntegral (0x80 + (shiftR currentCode 6 .&. 0x3f))
      , fromIntegral (0x80 + (currentCode .&. 0x3f))
      ]
  where
    currentCode =
      ord currentChar

renderWord32 :: Word32 -> String
renderWord32 currentWord =
  replicate (8 - length currentDigits) '0' ++ currentDigits
  where
    currentDigits =
      showHex currentWord ""

chunksOf :: Int -> [item] -> [[item]]
chunksOf _ [] =
  []
chunksOf currentSize currentItems =
  take currentSize currentItems
    : chunksOf currentSize (drop currentSize currentItems)
