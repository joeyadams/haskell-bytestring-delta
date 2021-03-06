{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- |
-- Module:       Data.ByteString.Delta
-- Copyright:    (c) Joseph Adams 2011
-- License:      MIT
--
-- Maintainer:   joeyadams3.14159@gmail.com
-- Stability:    experimental
-- Portability:  unknown
--
-- Binary diff/patch for 'ByteString's.
--
-- The 'diff' function takes two 'ByteString's, and produces a \"patch\" that
-- can later be applied with the 'patch' function to the first string to produce
-- the second string.  It exploits common subsequences between the two strings,
-- and can be used to save bandwidth and disk space when many strings differing
-- by a small number of bytes need to be transmitted or stored.
--
-- Deltas produced by this version of the library can be applied using
-- current or future versions, but may not be compatible with past versions.
--
-- bytestring-delta implements the algorithm described in
-- /An O(ND) Difference Algorithm and Its Variations/ by Eugene W. Myers.
-- Because its memory usage and expected running time are O(N + D^2),
-- it works well only when the strings differ by a small number of bytes.
-- This implementation stops trying when the strings differ by more than
-- 1000 bytes, and falls back to producing a patch that simply emits the new
-- string.
--
-- Thus, bytestring-delta does not save any space when given two strings that
-- differ by more than 1000 bytes.  This may be improved in a future version of
-- the library.

module Data.ByteString.Delta (
    diff,
    patch,
) where

#include "bdelta.h"

import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int
import Data.Word
import Foreign (Ptr, alloca, peek)
import Foreign.C.String (CString, peekCAString)
import Foreign.C.Types

##if MIN_VERSION_base(4,4,0)
import Foreign.Marshal.Unsafe (unsafeLocalState)
##else
import System.IO.Unsafe (unsafePerformIO)

unsafeLocalState :: IO a -> a
unsafeLocalState = unsafePerformIO
##endif

type BDELTAcode = #{type BDELTAcode}

type BDeltaFunc = Ptr CChar       -> CSize
               -> Ptr CChar       -> CSize
               -> Ptr (Ptr CChar) -> Ptr CSize
               -> IO BDELTAcode

foreign import ccall safe "bdelta.h bdelta_diff"
    bdelta_diff :: BDeltaFunc

foreign import ccall safe "bdelta.h bdelta_patch"
    bdelta_patch :: BDeltaFunc

foreign import ccall unsafe "bdelta.h bdelta_strerror"
    bdelta_strerror :: BDELTAcode -> IO CString

-- I don't know if Foreign.Marshal.Alloc.free is guaranteed
-- to be compatible with stdlib's free or not.
foreign import ccall unsafe "stdlib.h free"
    free :: Ptr a -> IO ()

callBDeltaFunc :: BDeltaFunc -> ByteString -> ByteString -> Either BDELTAcode ByteString
callBDeltaFunc func old new =
    unsafeLocalState $
    unsafeUseAsCStringLen old $ \(oldPtr, oldSize) ->
    unsafeUseAsCStringLen new $ \(newPtr, newSize) ->
    alloca $ \diffPtrPtr ->
    alloca $ \diffSizePtr ->
    do
        rc <- func oldPtr     (fromIntegral oldSize)
                   newPtr     (fromIntegral newSize)
                   diffPtrPtr diffSizePtr
        case rc of
            #{const BDELTA_OK} -> do
                diffPtr <- peek diffPtrPtr
                diffSize <- peek diffSizePtr
                result <- packCStringLen (diffPtr, fromIntegral diffSize)
                free diffPtr
                return $ Right result
            _ -> return $ Left rc

strerror :: BDELTAcode -> String
strerror code = unsafeLocalState $ bdelta_strerror code >>= peekCAString

-- | Compute a delta between two 'ByteString's.
--
-- > patch old (diff old new) == Right new
diff :: ByteString -> ByteString -> ByteString
diff old new =
    case callBDeltaFunc bdelta_diff old new of
         Right result  -> result
         Left  errcode -> error $ "Data.ByteString.Delta.diff: " ++ strerror errcode

-- | Apply a delta produced by 'diff'.
--
-- If the patch cannot be applied, this function returns @Left errmsg@,
-- where @errmsg@ is a string describing the error.
patch :: ByteString -> ByteString -> Either String ByteString
patch old patch_ =
    case callBDeltaFunc bdelta_patch old patch_ of
         Right result                               -> Right result
         Left (rc @ #{const BDELTA_PATCH_INVALID})  -> Left $ strerror rc
         Left (rc @ #{const BDELTA_PATCH_MISMATCH}) -> Left $ strerror rc
         Left rc                                    -> error $ "Data.ByteString.Delta.patch: " ++ strerror rc
