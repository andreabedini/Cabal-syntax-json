{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Json
    ( module Json
    , module Distribution.Utils.Json
    )
where

import Compat
import Data.Functor.Identity
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import Distribution.Compat.Newtype
import Distribution.Compat.Prelude qualified as NE
import Distribution.Compiler
import Distribution.FieldGrammar
import Distribution.ModuleName
import Distribution.PackageDescription.FieldGrammar
import Distribution.Pretty
import Distribution.System
import Distribution.Types.BenchmarkType
import Distribution.Types.BuildType
import Distribution.Types.Condition
import Distribution.Types.ConfVar
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.ExecutableScope
import Distribution.Types.Flag
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.LegacyExeDependency
import Distribution.Types.LibraryName
import Distribution.Types.LibraryVisibility
import Distribution.Types.Mixin
import Distribution.Types.ModuleReexport
import Distribution.Types.PackageName
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Types.PkgconfigVersionRange
import Distribution.Types.SourceRepo
import Distribution.Types.TestType
import Distribution.Types.UnqualComponentName
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.Utils.Json
import Language.Haskell.Extension

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

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
    toJSON (a, b) = JsonArray [toJSON a, toJSON b]

instance ToJSON a => ToJSON (Map String a) where
    toJSON = JsonObject . Map.foldMapWithKey (\k v -> [(k, toJSON v)])

deriving via (a :: Type) instance ToJSON a => ToJSON (Identity a)

instance ToJSON Json where
    toJSON = id
