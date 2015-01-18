{-| Bindings to FFTW.

Example usage:

> import Foreign.Marshal.Array
> import Data.Complex
> import Foreign.Storable.Complex
> import FFTW
> 
> main = do
>     inA  <- fftwAllocComplex 1024
>     outA <- fftwAllocComplex 1024
> 
>     plan <- planDFT1d 1024 inA outA Forward fftwEstimate
> 
>     pokeArray inA $ map (:+ 0) [0..1023]
>     execute plan
>     res <- peekArray 1024 outA
> 
>     fftwFree inA
>     fftwFree outA
> 
>     print res
-}

module Numeric.FFTW (
    fftwMalloc,
    fftwFree,
    fftwFreePtr,
    fftwAllocReal,
    fftwAllocComplex,

    Direction(..),
    Flag(),

    fftwMeasure,
    fftwDestroyInput,
    fftwUnaligned,
    fftwExhaustive,
    fftwPreserveInput,
    fftwPatient,
    fftwEstimate,
    fftwWisdomOnly,

    FFTWPlan,
    planDFT1d,
    planDFTR2C1d,
    execute,
    executeDFT,
    executeDFTR2C
    ) where

import Foreign.C.Types
import Foreign.Ptr
import Data.Word
import Data.Complex
import Control.Monad
import Data.Bits
import Data.Monoid

#include <fftw3.h>

foreign import ccall unsafe "fftw_malloc" 
    c_fftwMalloc :: CUInt -> IO (Ptr a)

-- | Like malloc, but ensures that the pointer obeys the alignment restrictions of FFTW (e.g. for SIMD acceleration). You probably want to use 'fftwAllocReal' or 'fftwAllocComplex' instead.
fftwMalloc :: Word32 -> IO (Ptr a)
fftwMalloc = c_fftwMalloc . fromIntegral 

-- | Free a pointer returned by 'fftwMalloc', 'fftwAllocReal', or 'fftwAllocComplex'
foreign import ccall unsafe "fftw_free"
    fftwFree :: Ptr a -> IO ()

-- | A function pointer to @fftwFree@.
foreign import ccall unsafe "&fftw_free"
    fftwFreePtr :: FunPtr (Ptr a -> IO ())

foreign import ccall unsafe "fftw_alloc_real"
    c_fftwAllocReal :: CUInt -> IO (Ptr CDouble)

-- | @fftwAllocReal n@ allocates an array of n Doubles. It ensures that the pointer obeys the alignment restrictions of FFTW (e.g. for SIMD acceleration).
fftwAllocReal :: Word32 -> IO (Ptr CDouble)
fftwAllocReal = c_fftwAllocReal . fromIntegral

foreign import ccall unsafe "fftw_alloc_complex"
    c_fftwAllocComplex :: CUInt -> IO (Ptr (Complex CDouble))

-- | @fftwAllocComplex n@ allocates an array of n complex Doubles (i.e. the c type "double complex"). It ensures that the pointer obeys the alignment restrictions of FFTW (e.g. for SIMD acceleration).
fftwAllocComplex :: Word32 -> IO (Ptr (Complex CDouble))
fftwAllocComplex = c_fftwAllocComplex . fromIntegral

-- | The direction of the transform: Forward for a normal transform, Backward for an inverse transform
data Direction = Forward
               | Backward

dirToInt :: Direction -> CInt
dirToInt Forward  = #const FFTW_FORWARD
dirToInt Backward = #const FFTW_BACKWARD

-- | FFTW planner flags. These flags affect the planning process. They can be combined using the 'Monoid' instance. See the FFTW flag documentation: <http://www.fftw.org/doc/Planner-Flags.html>.
newtype Flag = Flag {unFlag :: CUInt}

instance Monoid Flag where
    mempty                    = Flag 0
    mappend (Flag x) (Flag y) = Flag (x .|. y)

--Planning rigor flags
fftwMeasure, fftwExhaustive, fftwPatient, fftwEstimate, fftwWisdomOnly :: Flag
fftwEstimate       = Flag #const FFTW_ESTIMATE
fftwMeasure        = Flag #const FFTW_MEASURE
fftwPatient        = Flag #const FFTW_PATIENT
fftwExhaustive     = Flag #const FFTW_EXHAUSTIVE
fftwWisdomOnly     = Flag #const FFTW_WISDOM_ONLY

--Algorithm restriction flags
fftwDestroyInput, fftwUnaligned, fftwPreserveInput :: Flag
fftwDestroyInput   = Flag #const FFTW_DESTROY_INPUT
fftwUnaligned      = Flag #const FFTW_UNALIGNED
fftwPreserveInput  = Flag #const FFTW_PRESERVE_INPUT

data CFFTWPlan

-- | A @FFTWPlan i o@ contains all of the information necessary to perform a transform from an input array of type @i@ to an output array of type @o@, including pointers to the input and output arrays.
newtype FFTWPlan i o = FFTWPlan (Ptr CFFTWPlan)

foreign import ccall unsafe "fftw_plan_dft_1d"
    c_planDFT1d :: CInt -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> CInt -> CUInt -> IO (Ptr CFFTWPlan)

--This appears to be missing from the fft package on Hackage
-- | Create a plan for a 1 dimensional complex to complex DFT. The plan stores pointers to the input and output arrays, and these will be used if you 'execute' the plan in the future. They are required even if you intend to specify different input and output arrays in the future (i.e. using 'executeDFT')
planDFT1d :: Int -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> Direction -> Flag -> IO (FFTWPlan (Complex CDouble) (Complex CDouble))
planDFT1d n inp out sign flags = liftM FFTWPlan $ c_planDFT1d (fromIntegral n) inp out (dirToInt sign) (unFlag flags)

foreign import ccall unsafe "fftw_plan_dft_r2c_1d"
    c_planDFTR2C1d :: CInt -> Ptr CDouble -> Ptr (Complex CDouble) -> CUInt -> IO (Ptr CFFTWPlan)

--This appears to be missing from the fft package on Hackage
-- | Create a plan for a 1 dimensional real to complex DFT. The plan stores pointers to the input and output arrays, and these will be used if you 'execute' the plan in the future. They are required even if you intend to specify different input and output arrays in the future (i.e. using 'executeDFTR2C')
planDFTR2C1d :: Int -> Ptr CDouble -> Ptr (Complex CDouble) -> Flag -> IO (FFTWPlan CDouble (Complex CDouble))
planDFTR2C1d n inp out flags = liftM FFTWPlan $ c_planDFTR2C1d (fromIntegral n) inp out (unFlag flags)

foreign import ccall unsafe "fftw_execute"
    c_execute :: Ptr CFFTWPlan -> IO ()

-- | Execute a plan. Performs an FFT. The input and output arrays are stored within the plan so do not need to be given.
execute :: FFTWPlan i o -> IO ()
execute (FFTWPlan p) = c_execute p

foreign import ccall unsafe "fftw_execute_dft"
    c_executeDFT :: Ptr CFFTWPlan -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> IO ()

-- | Execute a complex to complex DFT but on different input and output arrays to those specified when the plan was created.
executeDFT :: FFTWPlan (Complex CDouble) (Complex CDouble) -> Ptr (Complex CDouble) -> Ptr (Complex CDouble) -> IO ()
executeDFT (FFTWPlan p) inp out = c_executeDFT p inp out

foreign import ccall unsafe "fftw_execute_dft_r2c"
    c_executeDFTR2C :: Ptr CFFTWPlan -> Ptr CDouble -> Ptr (Complex CDouble) -> IO ()

-- | Execute a real to complex DFT but on different input and output arrays to those specified when the plan was created.
executeDFTR2C :: FFTWPlan CDouble (Complex CDouble) -> Ptr CDouble -> Ptr (Complex CDouble) -> IO ()
executeDFTR2C (FFTWPlan p) inp out = c_executeDFTR2C p inp out

