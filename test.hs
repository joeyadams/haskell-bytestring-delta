import BDelta (diff, patch)

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Word (Word8)
import Test.QuickCheck (Arbitrary(arbitrary), CoArbitrary(coarbitrary),
                        Testable, quickCheckWith, stdArgs, Args(maxSize))

import qualified Data.ByteString as B

newtype ByteString = Wrap {unwrap :: B.ByteString}

pack :: [Word8] -> ByteString
pack = Wrap . B.pack

unpack :: ByteString -> [Word8]
unpack = B.unpack . unwrap

instance Show ByteString where
    show = show . unwrap

instance Arbitrary ByteString where
    arbitrary = pack <$> arbitrary
instance CoArbitrary ByteString where
    coarbitrary = coarbitrary . unpack

data Edit = Insert Int Word8
          | Delete Int

type EditString = [Edit]

applyEditString :: EditString -> ByteString -> ByteString
applyEditString editString (Wrap string) =
    let loop _   str []                   = [str]
        loop pos str (Insert i c : edits) =
            let (a, b) = B.splitAt (i - pos) str
             in a : B.singleton c : loop i b edits
        loop pos str (Delete i : edits)
            | pos <= i =
                let (a, b) = B.splitAt (i - pos) str
                 in a : loop (i+1) (B.tail b) edits
            | otherwise = loop pos str edits
        editIndex (Insert idx _) = idx
        editIndex (Delete idx)   = idx
     in Wrap $ B.concat $ loop 0 string (sortBy (compare `on` editIndex) editString)

prop_match :: ByteString -> ByteString -> Bool
prop_match (Wrap a) (Wrap b) = patch a (diff a b) == Just b

prop_equal :: ByteString -> Bool
prop_equal (Wrap s) =
    let d = diff s s
    in B.length d < 10 && patch s d == Just s

main :: IO ()
main = do
    let qc :: (Testable prop) => prop -> IO ()
        qc = quickCheckWith stdArgs {maxSize = 1000}
    qc prop_match
    qc prop_equal

