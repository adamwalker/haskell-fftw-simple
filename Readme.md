# Haskell-FFTW-Simple

Yet another set of Haskell bindings to FFTW, the Fastest Fourier Transform in the West.

These are low level bindings for a small subset of FFTW's functionality. Raise an Issue on Github if you need something I haven't implemented.

## Comparison to other FFTW packages on Hackage
Unlike the [fft](https://hackage.haskell.org/package/fft) package, this package provides low level manipulation of FFTW plans (such as `fftw_plan_dft_1d`).

Unlike the [vector-fftw](https://hackage.haskell.org/package/vector-fftw) package, this package is based on pointers instead of the Vector datatype and it avoids copying the input arrays by assuming that the pointers are aligned as FFTW expects.

# Usage
```haskell

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
```
