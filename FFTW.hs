module FFTW (
    fftwMalloc,
    fftwFree,
    fftwAllocReal,
    fftwAllocComplex,
    FFTWPlan,
    planDFT1d,
    planDFTR2C1d,
    execute
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Data.Word
import Data.Complex
import Control.Monad

foreign import ccall unsafe "fftw_malloc" 
    c_fftwMalloc :: CUInt -> IO (Ptr a)

fftwMalloc :: Word32 -> IO (Ptr a)
fftwMalloc = c_fftwMalloc . fromIntegral 

foreign import ccall unsafe "fftw_free"
    fftwFree :: Ptr a -> IO ()

foreign import ccall unsafe "fftw_alloc_real"
    c_fftwAllocReal :: CUInt -> IO (Ptr CDouble)

fftwAllocReal :: Word32 -> IO (Ptr CDouble)
fftwAllocReal = c_fftwAllocReal . fromIntegral

foreign import ccall unsafe "fftw_alloc_complex"
    c_fftwAllocComplex :: CUInt -> IO (Ptr (Complex CDouble))

fftwAllocComplex :: Word32 -> IO (Ptr (Complex CDouble))
fftwAllocComplex = c_fftwAllocComplex . fromIntegral

data CFFTWPlan
newtype FFTWPlan = FFTWPlan (Ptr CFFTWPlan)

foreign import ccall unsafe "fftw_plan_dft_1d"
    c_planDFT1d :: CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> CInt -> CUInt -> IO (Ptr CFFTWPlan)

planDFT1d :: Int -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> Int -> Word32 -> IO FFTWPlan
planDFT1d n inp out sign flags = liftM FFTWPlan $ c_planDFT1d (fromIntegral n) inp out (fromIntegral sign) (fromIntegral flags)

foreign import ccall unsafe "fftw_plan_dft_r2c_1d"
    c_planDFTR2C1d :: CInt -> Ptr CDouble -> Ptr (Complex CDouble) -> CUInt -> IO (Ptr CFFTWPlan)

planDFTR2C1d :: Int -> Ptr CDouble -> Ptr (Complex CDouble) -> Word32 -> IO FFTWPlan
planDFTR2C1d n inp out flags = liftM FFTWPlan $ c_planDFTR2C1d (fromIntegral n) inp out (fromIntegral flags)

foreign import ccall unsafe "fftw_execute"
    c_execute :: Ptr CFFTWPlan -> IO ()

execute :: FFTWPlan -> IO ()
execute (FFTWPlan p) = c_execute p

