{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

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

newtype CompatFilePath = CompatFilePath FilePath

instance Pretty CompatFilePath where
  pretty (CompatFilePath fpath) = fromString fpath

readGenericPackageDescription
   :: Verbosity
   -> Maybe (SymbolicPath CWD (Dir Pkg))
   -> SymbolicPath Pkg File
   -> IO GenericPackageDescription
readGenericPackageDescription = Distribution.Simple.PackageDescription.readGenericPackageDescription

{- FOURMOLU_DISABLE -}
#elif MIN_VERSION_Cabal_syntax(3,10,0)
{- FOURMOLU_ENABLE -}

newtype CompatDataDir = CompatDataDir FilePath

instance Pretty CompatDataDir where
  pretty (CompatDataDir fpath) = showFilePath fpath

newtype RelativePath from to = RelativePath FilePath

instance Pretty (RelativePath from to) where
  pretty (RelativePath fpath)= showFilePath fpath

newtype SymbolicPathNT from to = SymbolicPathNT (SymbolicPath from to)

deriving via (SymbolicPath from to) instance Pretty (SymbolicPathNT from to)

newtype RelativePathNT from to = RelativePathNT (SymbolicPath from to)

deriving via (SymbolicPathNT from to) instance Pretty (RelativePathNT from to)

data CWD
data Pkg
data FileOrDir = File | Dir Type

readGenericPackageDescription
  :: HasCallStack
  => Verbosity
  -> Maybe (SymbolicPath from to)
  -> SymbolicPath from to
  -> IO GenericPackageDescription
readGenericPackageDescription normal _mbWorkDir fpath =
  Distribution.Simple.PackageDescription.readGenericPackageDescription normal (getSymbolicPath fpath)

makeSymbolicPath :: FilePath -> SymbolicPath from to 
makeSymbolicPath fpath = unsafeMakeSymbolicPath fpath

{- FOURMOLU_DISABLE -}
#endif
{- FOURMOLU_ENABLE -}
