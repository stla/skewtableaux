{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module SkewTableaux where
import           Data.Bool                        (bool)
import           Data.Singletons                  (sing)
import qualified Data.Vector.SEXP                 as VS
import           Foreign
import           Foreign.C
import           Foreign.R                        (SEXP, SomeSEXP, cast,
                                                   indexVector)
import qualified Foreign.R.Type                   as R
import           Language.R.Literal               (mkProtectedSEXPVectorIO)
import           Math.Combinat.Partitions.Integer
import           Math.Combinat.Partitions.Skew
import           Math.Combinat.Tableaux.Skew

--- ~# Partitions #~ ---

importPartition :: Ptr (SEXP s R.Int) -> IO (Partition)
importPartition partition = do
  partition <- peek partition
  return $ mkPartition $
            (map fromIntegral ((VS.toList . VS.fromSEXP) partition) :: [Int])

foreign export ccall isPartitionR :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO()
isPartitionR :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO()
isPartitionR partition result = do
  partition <- peek partition
  poke result $
    bool 0 1 $ isPartition $ map fromIntegral ((VS.toList . VS.fromSEXP) partition)

foreign export ccall isSubPartitionOfR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
isSubPartitionOfR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
isSubPartitionOfR partition1 partition2 result = do
  partition1 <- importPartition partition1
  partition2 <- importPartition partition2
  poke result $ bool 0 1 (isSubPartitionOf partition1 partition2)

foreign export ccall mkPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> IO ()
mkPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> IO ()
mkPartitionR partition result = do
  partition <- importPartition partition
  poke result $
    (VS.toSEXP . VS.fromList) (map fromIntegral (fromPartition partition) :: [Int32])

--- ~# Skew Partitions #~ ---

-- we can't apply fromSkewPartition to an invalid SkewPartition
isValidSkewPartition :: SkewPartition -> Bool
isValidSkewPartition (SkewPartition list) =
  isPartition outer && isPartition inner && isSubPartitionOf (toPartition inner) (toPartition outer)
  where
    (outer, inner) = (zipWith (+) as bs , filter (>0) as)
    (as,bs) = unzip list

foreign export ccall isValidSkewPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
isValidSkewPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CInt -> IO ()
isValidSkewPartitionR _outer _inner result = do
  _outer <- peek _outer
  _inner <- peek _inner
  let outer = (map fromIntegral ((VS.toList . VS.fromSEXP) _outer) :: [Int])
  let inner = (map fromIntegral ((VS.toList . VS.fromSEXP) _inner) :: [Int])
  poke result $
    bool 0 1 $
      isPartition outer && isPartition inner && isSubPartitionOf (toPartition inner) (toPartition outer)

rListToSkewPartition :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO (SkewPartition)
rListToSkewPartition rlist l = do
  l <- peek l
  rlist <- peekArray (fromIntegral l :: Int) rlist
  return $ SkewPartition $
            map (\[x,y] -> ((fromIntegral x :: Int), (fromIntegral y :: Int)))
              (map (VS.toList . VS.fromSEXP) rlist)

-- skewPartitionToR :: SkewPartition -> IO (SEXP s R.Vector)
-- skewPartitionToR (SkewPartition skewpartition) = do
--   let rlist = map (\(x,y) -> [(fromIntegral x :: Int32), (fromIntegral y :: Int32)])
--                 skewpartition
--   mkProtectedSEXPVectorIO sing $
--                  (map (VS.toSEXP . VS.fromList) rlist :: [SEXP s R.Int])
skewPartitionToR :: SkewPartition -> IO (SEXP s R.Vector)
skewPartitionToR skewpartition = do
  let (_outer, _inner) = fromSkewPartition skewpartition
  let outer = (VS.toSEXP . VS.fromList) (map fromIntegral (fromPartition _outer) :: [Int32]) :: SEXP s R.Int
  let inner = (VS.toSEXP . VS.fromList) (map fromIntegral (fromPartition _inner) :: [Int32]) :: SEXP s R.Int
  mkProtectedSEXPVectorIO sing [outer, inner]

showSkewPartition :: Partition -> Partition -> String
showSkewPartition outer inner =
  "SkewPartition " ++ (show $ fromPartition outer) ++ " \\ " ++ (show $ fromPartition inner)

foreign export ccall showSkewPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CString -> IO ()
showSkewPartitionR :: Ptr (SEXP s R.Int) -> Ptr (SEXP s R.Int) -> Ptr CString -> IO ()
showSkewPartitionR outer inner result = do
  outer <- importPartition outer
  inner <- importPartition inner
  (>>=) (newCString $ showSkewPartition outer inner) (poke result)

--- ~# Skew Tableaux #~ ---

isValidSkewTableau :: SkewTableau a -> Bool
isValidSkewTableau = isValidSkewPartition . skewTableauShape

foreign export ccall isValidSkewTableauR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr CInt -> IO ()
isValidSkewTableauR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr CInt -> IO ()
isValidSkewTableauR rlist l result = do
  skewtableau <- rListToSkewTableau rlist l
  poke result $ bool 0 1 (isValidSkewTableau skewtableau)

foreign export ccall skewTableauShapeR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
skewTableauShapeR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
skewTableauShapeR rlist l result = do
  skewTableau <- rListToSkewTableau rlist l
  let shape = skewTableauShape skewTableau
  (>>=) (skewPartitionToR shape) (poke result)

someSexpToSint :: SomeSEXP s -> SEXP s R.Int
someSexpToSint someSexpToSint = cast sing someSexpToSint

-- list(list(2L,c(2L,3L)), list(2L,c(2L,3L)))
rListToSkewTableauRow :: SEXP s R.Vector -> IO (Int, [Int])
rListToSkewTableauRow row = do
  _offset <- indexVector row 0
  let offset = fromIntegral ((VS.fromSEXP $ someSexpToSint _offset) VS.! 0) :: Int
  _entries <- indexVector row 1
  let entries = map fromIntegral ((VS.toList . VS.fromSEXP) (someSexpToSint _entries)) :: [Int]
  return (offset, entries)

rListToSkewTableau :: Ptr (SEXP s R.Vector) -> Ptr CInt -> IO (SkewTableau Int)
rListToSkewTableau rlist l = do
  l <- peek l
  rlist <- peekArray (fromIntegral l :: Int) rlist
  fmap SkewTableau (mapM rListToSkewTableauRow rlist)

skewTableauRowToR :: (Int, [Int]) -> IO (SEXP s R.Vector)
skewTableauRowToR (_offset, _entries) = do
  let offset = (VS.toSEXP . VS.fromList) [(fromIntegral _offset :: Int32)] :: SEXP s R.Int
  let entries = (VS.toSEXP . VS.fromList) (map fromIntegral _entries :: [Int32]) :: SEXP s R.Int
  mkProtectedSEXPVectorIO sing [offset, entries]

skewTableautoR :: SkewTableau Int -> IO (SEXP s R.Vector)
skewTableautoR (SkewTableau skewTableau) = do
  rows <- mapM skewTableauRowToR skewTableau
  mkProtectedSEXPVectorIO sing rows

foreign export ccall dualSkewTableauR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
dualSkewTableauR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr (SEXP s R.Vector) -> IO ()
dualSkewTableauR rlist l result = do
  skewTableau <- rListToSkewTableau rlist l
  let dskewTableau = dualSkewTableau skewTableau
  (>>=) (skewTableautoR dskewTableau) (poke result)

foreign export ccall showSkewTableauR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr CString -> IO ()
showSkewTableauR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr CString -> IO ()
showSkewTableauR rlist l result = do
  skewTableau <- rListToSkewTableau rlist l
  (>>=) (newCString $ show skewTableau) (poke result)

foreign export ccall asciiSkewTableauR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr CString -> IO ()
asciiSkewTableauR :: Ptr (SEXP s R.Vector) -> Ptr CInt -> Ptr CString -> IO ()
asciiSkewTableauR rlist l result = do
  skewTableau <- rListToSkewTableau rlist l
  (>>=) (newCString $ show (asciiSkewTableau skewTableau)) (poke result)
