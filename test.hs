import BDelta (diff, patch)

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (sortBy)
import Data.String (IsString(fromString))
import Data.Word (Word8)
import Test.QuickCheck (Arbitrary(arbitrary), CoArbitrary(coarbitrary),
                        Testable, quickCheckWith, stdArgs, Args(maxSize),
                        choose)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

newtype ByteString = Wrap {unwrap :: B.ByteString}

pack :: [Word8] -> ByteString
pack = Wrap . B.pack

unpack :: ByteString -> [Word8]
unpack = B.unpack . unwrap

instance Show ByteString where
    show = show . unwrap

instance IsString ByteString where
    fromString = Wrap . C.pack

instance Arbitrary ByteString where
    arbitrary = pack <$> arbitrary
instance CoArbitrary ByteString where
    coarbitrary = coarbitrary . unpack

prop_match :: ByteString -> ByteString -> Bool
prop_match (Wrap a) (Wrap b) = patch a (diff a b) == Just b

prop_equal :: ByteString -> Bool
prop_equal (Wrap s) =
    let d = diff s s
    in B.length d < 10 && patch s d == Just s

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
        editKey (Insert idx _) = (idx, 0 :: Int)
        editKey (Delete idx)   = (idx, 1 :: Int)
     in Wrap $ B.concat $ loop 0 string (sortBy (compare `on` editKey) editString)

data Similar = Similar ByteString ByteString
    deriving Show

instance Arbitrary Similar where
    arbitrary = do
        old <- arbitrary
        let len = B.length $ unwrap old
        
        -- Choose length of edit string, favoring small sizes.
        c <- choose (0, len)
        c' <- choose (0, c)
        
        editString <- replicateM c' $ do
            -- This is a little tricky.  op = 0 means insert, while op = 1 means delete.
            -- We can insert on indices 0..len, but we can only delete on 0..len-1.
            -- If the string is empty, we can't delete.
            op <- choose (0 :: Int, if len > 0 then 1 else 0)
            pos <- choose (0, len - op)
            case op of
                 0 -> Insert pos <$> arbitrary
                 _ -> return (Delete pos)
        
        return $ Similar old (applyEditString editString old)

prop_match_similar :: Similar -> Bool
prop_match_similar (Similar (Wrap a) (Wrap b)) = patch a (diff a b) == Just b

main :: IO ()
main = do
    let qc :: (Testable prop) => prop -> IO ()
        qc = quickCheckWith stdArgs {maxSize = 1000}
    qc prop_match
    qc prop_equal
    qc prop_match_similar

