# byteslice

## Purpose

Types for dealing with slices of 'ByteArray' and 'MutableByteArray'.
These are never supposed to introduce overhead. Rather, they exist
to clarify intent in type signatures.

    receive ::
         Resource -- ^ Some scarce resource
      -> MutableByteArray RealWorld -- ^ Buffer
      -> Int -- ^ Offset
      -> Int -- ^ Length
      -> IO ()

With this library, we instead write

    receive ::
         Resource -- ^ Some scarce resource
      -> MutableBytes RealWorld -- ^ Buffer
      -> IO ()

The combination of the worker-wrapper transformation and inlining means
that we can expect these two to end up generating the same code in most
situations.
