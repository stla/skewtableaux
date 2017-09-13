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

--- ~# Skew Partitions #~ ---

rListToSkewPartiton :: Ptr (SEXP s R.Int) -> Ptr CInt -> IO (SkewPartition)
rListToSkewPartiton rlist l = do
  l <- peek l
  rlist <- peekArray (fromIntegral l :: Int) rlist
  return $ SkewPartition $
            map (\[x,y] -> ((fromIntegral x :: Int), (fromIntegral y :: Int)))
              (map (VS.toList . VS.fromSEXP) rlist)

skewPartitionToR :: SkewPartition -> IO (SEXP s R.Vector)
skewPartitionToR (SkewPartition skewpartition) = do
  let rlist = map (\(x,y) -> [(fromIntegral x :: Int32), (fromIntegral y :: Int32)])
                skewpartition
  mkProtectedSEXPVectorIO sing $
                 (map (VS.toSEXP . VS.fromList) rlist :: [SEXP s R.Int])

--- ~# Skew Tableaux #~ ---

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
