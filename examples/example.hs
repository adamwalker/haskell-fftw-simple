import Foreign.Marshal.Array
import Data.Complex
import Foreign.Storable.Complex
import FFTW

main = do
    inA  <- fftwAllocComplex 1024
    outA <- fftwAllocComplex 1024

    plan <- planDFT1d 1024 inA outA Forward fftwEstimate

    pokeArray inA $ map (:+ 0) [0..1023]
    execute plan
    res <- peekArray 1024 outA

    fftwFree inA
    fftwFree outA

    print res
