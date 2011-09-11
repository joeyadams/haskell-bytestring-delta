module BDelta where

import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign (Ptr, alloca, peek, unsafePerformIO)
import Foreign.C.Types (CSize, CInt, CChar)

type BDeltaFunc = Ptr CChar       -> CSize
               -> Ptr CChar       -> CSize
               -> Ptr (Ptr CChar) -> Ptr CSize
               -> IO CInt

data BDeltaResult = BDeltaOK ByteString
                  | BDeltaMemory
                  | BDeltaPatchInvalid
                  | BDeltaPatchMismatch
                  | BDeltaInternal

foreign import ccall unsafe "bdelta.h bdelta_diff"
    bdelta_diff :: BDeltaFunc

foreign import ccall unsafe "bdelta.h bdelta_patch"
    bdelta_patch :: BDeltaFunc

-- I don't know if Foreign.Marshal.Alloc.free is guaranteed
-- to be compatible with stdlib's free or not.
foreign import ccall unsafe "stdlib.h free"
    free :: Ptr a -> IO ()

callBDeltaFunc :: BDeltaFunc -> ByteString -> ByteString -> BDeltaResult
callBDeltaFunc func old new =
    unsafePerformIO $
    unsafeUseAsCStringLen old $ \(oldPtr, oldSize) ->
    unsafeUseAsCStringLen new $ \(newPtr, newSize) ->
    alloca $ \diffPtrPtr ->
    alloca $ \diffSizePtr ->
    do
        rc <- func oldPtr     (fromIntegral oldSize)
                   newPtr     (fromIntegral newSize)
                   diffPtrPtr diffSizePtr
        case rc of
            0 -> do
                diffPtr <- peek diffPtrPtr
                diffSize <- peek diffSizePtr
                result <- packCStringLen (diffPtr, fromIntegral diffSize)
                free diffPtr
                return (BDeltaOK result)
            1 -> return BDeltaMemory
            2 -> return BDeltaPatchInvalid
            3 -> return BDeltaPatchMismatch
            _ -> return BDeltaInternal

diff :: ByteString -> ByteString -> ByteString
diff old new =
    case callBDeltaFunc bdelta_diff old new of
         BDeltaOK result -> result
         BDeltaMemory    -> error "BDelta.diff: out of memory"
         _               -> error "BDelta.diff: internal error"

patch :: ByteString -> ByteString -> Maybe ByteString
patch old new =
    case callBDeltaFunc bdelta_patch old new of
         BDeltaOK result     -> Just result
         BDeltaMemory        -> error "BDelta.patch: out of memory"
         BDeltaPatchInvalid  -> Nothing
         BDeltaPatchMismatch -> Nothing
         _                   -> error "BDelta.patch: internal error"
