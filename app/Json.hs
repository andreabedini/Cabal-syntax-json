{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Json where

import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)

import Distribution.Compat.Newtype (Newtype (unpack))
import Distribution.Compat.Prelude qualified as NE
import Distribution.Compiler (CompilerFlavor)
import Distribution.FieldGrammar
    ( FilePathNT (..)
    , List
    , MQuoted (..)
    , SpecLicense (..)
    , SpecVersion (..)
    , TestedWith (..)
    , Token (..)
    , Token' (..)
    )
import Distribution.ModuleName (ModuleName)
import Distribution.PackageDescription.FieldGrammar
    ( CompatLicenseFile (..)
    )
import Distribution.Pretty (Pretty, prettyShow)
import Distribution.System (Arch, OS)
import Distribution.Types.BenchmarkType (BenchmarkType)
import Distribution.Types.BuildType (BuildType)
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.ExecutableScope (ExecutableScope)
import Distribution.Types.Flag (FlagName)
import Distribution.Types.ForeignLib (LibVersionInfo)
import Distribution.Types.ForeignLibOption (ForeignLibOption)
import Distribution.Types.ForeignLibType (ForeignLibType)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.LibraryName (libraryNameString)
import Distribution.Types.LibraryVisibility (LibraryVisibility)
import Distribution.Types.Mixin (Mixin)
import Distribution.Types.ModuleReexport (ModuleReexport)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
import Distribution.Types.PkgconfigName (PkgconfigName)
import Distribution.Types.PkgconfigVersionRange (PkgconfigVersionRange)
import Distribution.Types.SourceRepo (KnownRepoType, RepoType)
import Distribution.Types.TestType (TestType)
import Distribution.Types.UnqualComponentName
    ( UnqualComponentName
    , unUnqualComponentName
    )
import Distribution.Types.Version (Version)
import Distribution.Types.VersionRange (VersionRange)
import Distribution.Utils.Json (Json (..), (.=))
import Language.Haskell.Extension (Extension, Language)

import Data.Map (Map)
import Data.Map qualified as Map

import Compat
    ( CompatDataDir (CompatDataDir)
    , CompatFilePath (..)
    , RelativePath
    , RelativePathNT (RelativePathNT)
    , SymbolicPath
    , SymbolicPathNT (SymbolicPathNT)
    )

class ToJSON a where
    toJSON :: a -> Json

    toJSONList :: [a] -> Json
    toJSONList = listValue toJSON

-- | Helper function to use with 'liftToJSON', see 'listEncoding'.
listValue :: (a -> Json) -> [a] -> Json
listValue f = JsonArray . map f

type Pair = (String, Json)

newtype ViaPretty a = ViaPretty a

instance Pretty a => ToJSON (ViaPretty a) where
    toJSON (ViaPretty a) = JsonString $ prettyShow a

newtype ViaUnpack a = ViaUnpack a

instance (ToJSON o, Newtype o n) => ToJSON (ViaUnpack n) where
    toJSON (ViaUnpack n) = toJSON $ unpack n

deriving via String instance ToJSON Token

deriving via String instance ToJSON Token'

deriving via String instance ToJSON FilePathNT

deriving via ViaPretty (SymbolicPathNT from to) instance ToJSON (SymbolicPathNT from to)

deriving via ViaPretty (RelativePathNT from to) instance ToJSON (RelativePathNT from to)

deriving via ViaPretty (SymbolicPath from to) instance ToJSON (SymbolicPath from to)

deriving via ViaPretty (RelativePath from to) instance ToJSON (RelativePath from to)

deriving via ViaPretty CompatDataDir instance ToJSON CompatDataDir

deriving via ViaPretty CompatLicenseFile instance ToJSON CompatLicenseFile

deriving via ViaPretty VersionRange instance ToJSON VersionRange

deriving via ViaUnpack TestedWith instance ToJSON TestedWith

deriving via ViaPretty CompilerFlavor instance ToJSON CompilerFlavor

deriving via ViaPretty SpecVersion instance ToJSON SpecVersion

deriving via ViaPretty SpecLicense instance ToJSON SpecLicense

deriving via (ViaUnpack (List sep b a)) instance ToJSON a => ToJSON (List sep b a)

deriving via (ViaPretty CompatFilePath) instance ToJSON CompatFilePath

deriving via (ViaPretty BuildType) instance ToJSON BuildType

deriving via (ViaPretty PackageName) instance ToJSON PackageName

deriving via (ViaPretty PkgconfigName) instance ToJSON PkgconfigName

deriving via (ViaPretty PkgconfigVersionRange) instance ToJSON PkgconfigVersionRange

deriving via (ViaPretty Version) instance ToJSON Version

deriving via (ViaPretty RepoType) instance ToJSON RepoType

deriving via (ViaPretty KnownRepoType) instance ToJSON KnownRepoType

deriving via (ViaPretty Extension) instance ToJSON Extension

deriving via (ViaPretty Language) instance ToJSON Language

deriving via (ViaUnpack (MQuoted a)) instance ToJSON a => ToJSON (MQuoted a)

deriving via (ViaPretty BenchmarkType) instance ToJSON BenchmarkType

deriving via (ViaPretty ForeignLibType) instance ToJSON ForeignLibType

deriving via (ViaPretty TestType) instance ToJSON TestType

deriving via (ViaPretty ExecutableScope) instance ToJSON ExecutableScope

deriving via (ViaPretty ForeignLibOption) instance ToJSON ForeignLibOption

deriving via (ViaPretty LibVersionInfo) instance ToJSON LibVersionInfo

deriving via (ViaPretty ModuleName) instance ToJSON ModuleName

deriving via (ViaPretty ModuleReexport) instance ToJSON ModuleReexport

deriving via (ViaPretty Mixin) instance ToJSON Mixin

deriving via (ViaPretty LegacyExeDependency) instance ToJSON LegacyExeDependency

deriving via (ViaPretty LibraryVisibility) instance ToJSON LibraryVisibility

deriving via (ViaPretty FlagName) instance ToJSON FlagName

deriving via (ViaPretty Arch) instance ToJSON Arch

deriving via (ViaPretty OS) instance ToJSON OS

deriving via (ViaPretty UnqualComponentName) instance ToJSON UnqualComponentName

instance ToJSON ExeDependency where
    toJSON (ExeDependency pn ucn vr) =
        JsonObject $
            [ "package" .= toJSON pn
            , "version" .= toJSON vr
            , "exe" .= toJSON ucn
            ]

instance ToJSON Dependency where
    toJSON (Dependency pn vr libs) =
        JsonObject $
            [ "package" .= toJSON pn
            , "version" .= toJSON vr
            , "libs"
                .= JsonArray
                    [ maybe (toJSON pn) toJSON (libraryNameString l)
                    | l <- NE.toList libs
                    ]
            ]

instance ToJSON PkgconfigDependency where
    toJSON (PkgconfigDependency pn vr) =
        JsonObject $
            [ "name" .= toJSON pn
            , "version" .= toJSON vr
            ]

instance ToJSON ConfVar where
    toJSON (OS os) = JsonObject ["os" .= toJSON os]
    toJSON (Arch arch) = JsonObject ["arch" .= toJSON arch]
    toJSON (PackageFlag flag) = JsonObject ["flag" .= toJSON flag]
    toJSON (Impl flavor range) = JsonObject ["impl" .= toJSON flavor, "range" .= toJSON range]

instance ToJSON c => ToJSON (Condition c) where
    toJSON (Var v) = toJSON v
    toJSON (Lit b) = toJSON b
    toJSON (CNot c) = JsonObject ["not" .= toJSON c]
    toJSON (COr l r) = JsonObject ["or" .= JsonArray [toJSON l, toJSON r]]
    toJSON (CAnd l r) = JsonObject ["and" .= JsonArray [toJSON l, toJSON r]]

instance ToJSON Char where
    toJSON c = JsonString [c]
    toJSONList = JsonString

instance ToJSON Bool where
    toJSON = JsonBool

instance ToJSON a => ToJSON [a] where
    toJSON = toJSONList

instance ToJSON a => ToJSON (NonEmpty a) where
    toJSON = toJSONList . toList

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
    toJSON (a, b) = JsonArray [toJSON a, toJSON b]

instance ToJSON a => ToJSON (Map String a) where
    toJSON = JsonObject . Map.foldMapWithKey (\k v -> [(k, toJSON v)])

instance ToJSON a => ToJSON (Map UnqualComponentName a) where
    toJSON = JsonObject . Map.foldMapWithKey (\k v -> [(unUnqualComponentName k, toJSON v)])

deriving via (a :: Type) instance ToJSON a => ToJSON (Identity a)

instance ToJSON Json where
    toJSON = id
