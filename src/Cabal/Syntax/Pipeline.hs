-- | The cabal→JSON transformation, expressed as five named representations.
--
-- @--pristine@ renders stage 0 (Cabal's native conditional tree); the default
-- output renders stage 4. The README \"Details\" section narrates these steps.
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
