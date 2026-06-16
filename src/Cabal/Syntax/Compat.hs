{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

-- | Compatibility shims over the @Cabal@\/@Cabal-syntax@ API differences across the
-- supported version range (@3.10 || 3.12 || 3.14@).
--
-- Path types and the @readGenericPackageDescription@ signature changed across these
-- versions; the CPP branches below provide a uniform surface so the rest of the
-- package needs no version-specific code. Exactly one branch is in scope per build.
module Cabal.Syntax.Compat
    ( Cabal.Syntax.Compat.readGenericPackageDescription
    , makeSymbolicPath
    , SymbolicPath
    , RelativePath
    , SymbolicPathNT (..)
    , RelativePathNT (..)
    , CompatFilePath (..)
    , CompatDataDir (..)
    , CWD
    , FileOrDir (..)
    , Pkg
    , foldl'
    )
where

#if !MIN_VERSION_base(4, 20, 0)
import Data.List (foldl')
#endif

#if MIN_VERSION_Cabal_syntax(3,14,0)
import Data.String
import Distribution.FieldGrammar.Newtypes
#endif
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.FieldGrammar
{- FOURMOLU_DISABLE -}
import Distribution.Pretty
  ( Pretty (..)
#if !MIN_VERSION_Cabal_syntax(3,14,0) && MIN_VERSION_Cabal_syntax(3,10,0)
  , showFilePath
#endif
  )
{- FOURMOLU_ENABLE -}
import Distribution.Simple.PackageDescription
import Distribution.Utils.Path
import Distribution.Verbosity (Verbosity)
{- FOURMOLU_DISABLE -}
#if !MIN_VERSION_Cabal_syntax(3,14,0) && MIN_VERSION_Cabal_syntax(3,10,0)
{- FOURMOLU_ENABLE -}
import Data.Kind (Type)
import GHC.Stack (HasCallStack)
{- FOURMOLU_DISABLE -}
#endif
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
#if MIN_VERSION_Cabal_syntax(3,14,0)
{- FOURMOLU_ENABLE -}

-- | A 'FilePath' newtype carrying the version-appropriate 'Pretty' instance.
newtype CompatFilePath = CompatFilePath FilePath

instance Pretty CompatFilePath where
  pretty (CompatFilePath fpath) = fromString fpath

-- | Parse a @.cabal@ file into a 'GenericPackageDescription', wrapping over the
-- argument-type changes between Cabal versions.
readGenericPackageDescription
   :: Verbosity
   -> Maybe (SymbolicPath CWD (Dir Pkg))
   -> SymbolicPath Pkg File
   -> IO GenericPackageDescription
readGenericPackageDescription = Distribution.Simple.PackageDescription.readGenericPackageDescription

{- FOURMOLU_DISABLE -}
#elif MIN_VERSION_Cabal_syntax(3,10,0)
{- FOURMOLU_ENABLE -}

-- | A data-directory 'FilePath' newtype with the version-appropriate 'Pretty' instance.
newtype CompatDataDir = CompatDataDir FilePath

instance Pretty CompatDataDir where
  pretty (CompatDataDir fpath) = showFilePath fpath

-- | A relative-path newtype, standing in for the @SymbolicPath@ machinery absent in
-- these older Cabal versions.
newtype RelativePath from to = RelativePath FilePath

instance Pretty (RelativePath from to) where
  pretty (RelativePath fpath)= showFilePath fpath

-- | A 'SymbolicPath' newtype wrapper (for deriving instances).
newtype SymbolicPathNT from to = SymbolicPathNT (SymbolicPath from to)

deriving via (SymbolicPath from to) instance Pretty (SymbolicPathNT from to)

-- | A relative 'SymbolicPath' newtype wrapper (for deriving instances).
newtype RelativePathNT from to = RelativePathNT (SymbolicPath from to)

deriving via (SymbolicPathNT from to) instance Pretty (RelativePathNT from to)

-- | Phantom tag for the current working directory in a 'SymbolicPath'.
data CWD

-- | Phantom tag for the package root in a 'SymbolicPath'.
data Pkg

-- | Phantom tag distinguishing a file from a directory in a 'SymbolicPath'.
data FileOrDir = File | Dir Type

-- | Parse a @.cabal@ file into a 'GenericPackageDescription' (older-Cabal signature).
readGenericPackageDescription
  :: HasCallStack
  => Verbosity
  -> Maybe (SymbolicPath from to)
  -> SymbolicPath from to
  -> IO GenericPackageDescription
readGenericPackageDescription normal _mbWorkDir fpath =
  Distribution.Simple.PackageDescription.readGenericPackageDescription normal (getSymbolicPath fpath)

-- | Build a 'SymbolicPath' from a plain 'FilePath'.
makeSymbolicPath :: FilePath -> SymbolicPath from to
makeSymbolicPath fpath = unsafeMakeSymbolicPath fpath

{- FOURMOLU_DISABLE -}
#endif
{- FOURMOLU_ENABLE -}
