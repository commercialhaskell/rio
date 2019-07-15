{-|

== Rationale

This module offers functions to handle files that offer better durability and/or
atomicity.

See "UnliftIO.IO.File" for the rationale behind this module, since all of the functions
were moved upstream and are now simply re-exported from here.

@since 0.1.6
-}
module RIO.File
  ( -- * Regular
    withBinaryFile
  , writeBinaryFile
    -- * Atomic
  , withBinaryFileAtomic
  , writeBinaryFileAtomic
    -- * Durable
  , withBinaryFileDurable
  , writeBinaryFileDurable
  , ensureFileDurable
    -- * Durable and Atomic
  , withBinaryFileDurableAtomic
  , writeBinaryFileDurableAtomic
  ) where

import UnliftIO.IO.File
