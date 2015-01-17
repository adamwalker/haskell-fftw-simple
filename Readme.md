# Haskell-FFTW-Simple

Yet another set of Haskell bindings to FFTW, the Fastest Fourier Transform in the West.

These are low level bindings for a small subset of FFTW's functionality. Raise an Issue on Github if you need something I haven't implemented.

## Comparison to other FFTW packages on Hackage
Unlike the [fft](https://hackage.haskell.org/package/fft) package, this package provides low level manipulation of FFTW plans (such as `fftw_plan_dft_1d`).

Unlike the [vector-fftw](https://hackage.haskell.org/package/vector-fftw) package, this package is based on pointers instead of the Vector datatype and it avoids copying the input arrays by assuming that the pointers are aligned as FFTW expects.
