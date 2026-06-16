-- | The cabal→JSON transformation, expressed as five named representations.
--
-- Structurally, a @.cabal@ file is a /tree of fields/: groups of fields nested
-- under @if@\/@else@ conditions. What we want to emit instead is, for each field, a
-- /tree of values/ — every value tagged with the condition under which it applies.
-- This module performs that inversion as a chain of five representations, each given
-- a type-alias name (stages 0–4) and a one-step function between consecutive stages.
--
-- The shapes are easiest to follow through a single field. Take a library with a
-- conditional module:
--
-- @
-- library
--   exposed-modules: Core
--   if flag(fast)
--     exposed-modules: Core.Fast
-- @
--
-- Cabal parses that stanza into a @CondTree ConfVar [Dependency] Library@. Looking at
-- only each node's @exposed-modules@ (i.e. @fmap exposedModules@ over the tree), the
-- conditional library is:
--
-- @
-- CondNode
--   { condTreeData = [ModuleName "Core"]
--   , condTreeConstraints = []
--   , condTreeComponents =
--       [ CondBranch
--           { condBranchCondition = Var (PackageFlag (FlagName "fast"))
--           , condBranchIfTrue  = CondNode { condTreeData = [ModuleName "Core.Fast"]
--                                          , condTreeConstraints = []
--                                          , condTreeComponents = [] }
--           , condBranchIfFalse = Nothing
--           }
--       ]
--   }
-- @
--
-- The pipeline rewrites that in five steps. Each representation below is shown limited
-- to its @exposed-modules@ content (a real component carries every other field too).
--
-- === Stage 0 — 'ComponentTreeCabal'
--
-- "Cabal.Syntax.GenericPackageDescription" first renders each field group into a
-- 'FieldMap' of JSON 'Cabal.Syntax.JsonFieldGrammar.Fragment's, but keeps Cabal's tree
-- — conditions still sit /outside/ the fields. This is the shape @--pristine@ renders.
--
-- @
-- CondNode
--   { condTreeData =
--       FieldMap (ListMap [("exposed-modules", ListLikeFragment (JsonString "Core" :| []))])
--   , condTreeConstraints = []
--   , condTreeComponents =
--       [ CondBranch
--           { condBranchCondition = Var (PackageFlag (FlagName "fast"))
--           , condBranchIfTrue  =
--               CondNode { condTreeData =
--                            FieldMap (ListMap [("exposed-modules", ListLikeFragment (JsonString "Core.Fast" :| []))])
--                        , condTreeConstraints = []
--                        , condTreeComponents = [] }
--           , condBranchIfFalse = Nothing
--           }
--       ]
--   }
-- @
--
-- === Stage 1 — 'ComponentTree'
--
-- 'convertCondTree' re-expresses the tree in our uniform
-- 'Cabal.Syntax.CondTree.CondTree' (a non-empty list of nodes) and drops Cabal's
-- constraint set. Still a /tree of fields/.
--
-- @
-- CondTree
--   ( CondNode (FieldMap (ListMap [("exposed-modules", ListLikeFragment (JsonString "Core" :| []))]))
--   :| [ CondIfThen (Var (PackageFlag (FlagName "fast")))
--          (CondTree (CondNode (FieldMap (ListMap [("exposed-modules", ListLikeFragment (JsonString "Core.Fast" :| []))])) :| [])) ] )
-- @
--
-- === Stage 2 — 'FieldTrees'
--
-- /Fields of trees./ 'pushConditionals' inverts the nesting: the outer 'FieldMap' is
-- now keyed by field name, and each field owns its own little
-- 'Cabal.Syntax.CondTree.CondTree'. The @exposed-modules@ entry of the map becomes:
--
-- @
-- ( "exposed-modules"
-- , CondTree
--     ( CondNode (ListLikeFragment (JsonString "Core" :| []))
--     :| [ CondIfThen (Var (PackageFlag (FlagName "fast")))
--            (CondTree (CondNode (ListLikeFragment (JsonString "Core.Fast" :| [])) :| [])) ] ) )
-- @
--
-- === Stage 3 — 'GuardedFields'
--
-- @fmap 'flattenCondTree'@ collapses each field's tree into a flat, non-empty list of
-- 'Cabal.Syntax.CondTree.Guarded' values, each carrying the /cumulative/ condition of
-- the path that reached it (nested @if@s become a conjunction; an @else@ the negation):
--
-- @
-- ( "exposed-modules"
-- ,   Guarded (Lit True) (ListLikeFragment (JsonString "Core" :| []))
--   :| [ Guarded (Var (PackageFlag (FlagName "fast"))) (ListLikeFragment (JsonString "Core.Fast" :| [])) ] )
-- @
--
-- === Stage 4 — 'JsonFields'
--
-- @fmap 'defragment'@ merges each field's guarded values back into one
-- 'Cabal.Syntax.JsonFieldGrammar.Fragment' (see there for the scalar\/list-like
-- distinction). A @Lit True@ guard is emitted bare; any other guard becomes an
-- @{_if, _then}@ object, and the pieces are concatenated:
--
-- @
-- ( "exposed-modules"
-- , ListLikeFragment
--     ( JsonString "Core"
--     :| [ JsonObject [ ("_if",   JsonObject [("flag", JsonString "fast")])
--                     , ("_then", JsonArray [JsonString "Core.Fast"]) ] ] ) )
-- @
--
-- Rendering that fragment to JSON gives the final output:
--
-- @
-- "exposed-modules": [ "Core", { "_if": { "flag": "fast" }, "_then": [ "Core.Fast" ] } ]
-- @
--
-- The default output renders stage 4; @--pristine@ renders stage 0. The README
-- \"Details\" section narrates the same steps for a reader of the JSON.
module Cabal.Syntax.Pipeline
    ( -- * The five representations
      ComponentTreeCabal
    , ComponentTree
    , FieldTrees
    , GuardedFields
    , JsonFields

      -- * The default pipeline
    , processComponent
    , process
    ) where

import Data.List.NonEmpty (NonEmpty)

import Distribution.Types.CondTree qualified as C (CondTree)
import Distribution.Types.ConfVar (ConfVar)
import Distribution.Types.Dependency (Dependency)
import Distribution.Utils.Json (Json)

import Cabal.Syntax.CondTree
    ( CondTree
    , Guarded
    , convertCondTree
    , defragment
    , flattenCondTree
    , pushConditionals
    )
import Cabal.Syntax.GenericPackageDescription (ComponentMap, FieldMap)
import Cabal.Syntax.JsonFieldGrammar (Fragment)

-- | Stage 0 — Cabal's native conditional tree, with a whole 'FieldMap' at each
-- leaf and one condition per branch (conjunctions nest). @--pristine@ renders this.
type ComponentTreeCabal = C.CondTree ConfVar [Dependency] (FieldMap (Fragment Json))

-- | Stage 1 — our uniform conditional tree; still a tree of fields.
type ComponentTree = CondTree ConfVar (FieldMap (Fragment Json))

-- | Stage 2 — fields of trees: the conditional structure pushed inside each field.
type FieldTrees = FieldMap (CondTree ConfVar (Fragment Json))

-- | Stage 3 — each field a flat list of values guarded by their cumulative condition.
type GuardedFields = FieldMap (NonEmpty (Guarded ConfVar (Fragment Json)))

-- | Stage 4 — each field a single merged JSON fragment. The default output renders this.
type JsonFields = FieldMap (Fragment Json)

-- | The default pipeline for one component: stage 0 → stage 4.
--
-- Read bottom-to-top: 'convertCondTree' (0→1) normalises Cabal's tree;
-- 'pushConditionals' (1→2) turns the tree of fields into fields of trees;
-- @fmap 'flattenCondTree'@ (2→3) flattens each field's tree into cumulative guards;
-- @fmap 'defragment'@ (3→4) encodes the guards as @{_if,_then}@ and merges them.
processComponent :: ComponentTreeCabal -> JsonFields
processComponent =
    fmap defragment
        . fmap flattenCondTree
        . pushConditionals
        . convertCondTree

-- | Apply 'processComponent' to every component of a package.
process :: ComponentMap ComponentTreeCabal -> ComponentMap JsonFields
process = fmap processComponent
