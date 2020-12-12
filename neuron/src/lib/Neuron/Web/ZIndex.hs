{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.ZIndex
  ( renderZIndex,
    buildZIndex,
    ZIndex (..),
    style,
  )
where

import Clay (Css, em, (?))
import qualified Clay as C
import Data.Foldable (maximum)
import qualified Data.Map.Strict as Map
import Data.TagTree (mkTagPattern)
import qualified Data.Text as T
import Data.Tree (Forest, Tree (..))
import qualified Neuron.Web.Query.View as QueryView
import Neuron.Web.Route (NeuronWebT)
import qualified Neuron.Web.Theme as Theme
import Neuron.Web.Widget (elPreOverflowing)
import Neuron.Web.Zettel.View (renderZettelParseError)
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (ZettelID (..))
import Neuron.Zettelkasten.Query (zettelsByTag)
import Neuron.Zettelkasten.Query.Error (showQueryResultError)
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelError (..),
    ZettelT (zettelTitle),
  )
import Reflex.Dom.Core hiding (mapMaybe, (&))
import Relude hiding ((&))

-- | The value needed to render the z-index
--
-- All heavy graph computations are decoupled from rendering, producing this
-- value, that is in turn used for instant rendering.
data ZIndex = ZIndex
  { -- | Clusters on the folgezettel graph.
    zIndexClusters :: [Forest (Zettel, [Zettel])],
    zIndexOrphans :: [Zettel],
    -- | All zettel errors
    zIndexErrors :: Map ZettelID (NonEmpty ZettelError),
    zIndexStats :: Stats,
    zPinned :: [Zettel]
  }

data Stats = Stats
  { statsZettelCount :: Int,
    statsZettelConnectionCount :: Int
  }
  deriving (Eq, Show)

buildZIndex :: ZettelGraph -> Map ZettelID (NonEmpty ZettelError) -> ZIndex
buildZIndex graph errors =
  let (orphans, clusters) = partitionEithers $
        flip fmap (G.categoryClusters graph) $ \case
          [Node z []] -> Left z -- Orphans (cluster of exactly one)
          x -> Right x
      clustersWithBacklinks :: [Forest (Zettel, [Zettel])] =
        -- Compute backlinks for each node in the tree.
        flip fmap clusters $ \(zs :: [Tree Zettel]) ->
          G.backlinksMulti Folgezettel zs graph
      stats = Stats (length $ G.getZettels graph) (G.connectionCount graph)
      pinnedZettels = zettelsByTag (G.getZettels graph) [mkTagPattern "pinned"]
   in ZIndex (fmap sortCluster clustersWithBacklinks) orphans errors stats pinnedZettels
  where
    -- TODO: Either optimize or get rid of this (or normalize the sorting somehow)
    sortCluster fs =
      sortZettelForest $
        flip fmap fs $ \Node {..} ->
          Node rootLabel $ sortZettelForest subForest
    -- Sort zettel trees so that trees containing the most recent zettel (by ID) come first.
    sortZettelForest = sortOn (Down . maximum)

renderZIndex ::
  DomBuilder t m =>
  Theme.Theme ->
  ZIndex ->
  -- | Search query to filter
  -- TODO: Finalize type of this.
  Maybe Text ->
  NeuronWebT t m ()
renderZIndex (Theme.semanticColor -> themeColor) ZIndex {..} mq = do
  elClass "h1" "header" $ text "Zettel Index"
  divClass "errors" $ do
    renderErrors zIndexErrors
  forM_ mq $ \q ->
    divClass "ui message" $ do
      text $ "Filtering by query: " <> q
  divClass "z-index" $ do
    forM_ (nonEmpty zPinned) $ \zs ->
      divClass "ui pinned raised segment" $ do
        elClass "h3" "ui header" $ text "Pinned"
        el "ul" $
          forM_ zs $ \z ->
            zettelLink mq z (pure False)
    whenNotNull zIndexOrphans $ \(toList -> zs) ->
      divClass "ui piled segment" $ do
        elClass "p" "info" $ do
          text "Notes without any "
          elAttr "a" ("href" =: "https://neuron.zettel.page/linking.html") $ text "folgezettel"
          text " relationships"
        el "ul" $
          forM_ zs $ \z ->
            zettelLink mq z (pure False)
    forM_ zIndexClusters $ \forest ->
      divClass ("ui " <> themeColor <> " segment") $ do
        el "ul" $ renderForest mq forest
    el "p" $ do
      text $
        "The zettelkasten has "
          <> countNounBe "zettel" "zettels" (statsZettelCount zIndexStats)
          <> " and "
          <> countNounBe "link" "links" (statsZettelConnectionCount zIndexStats)
      text $ ". It has " <> countNounBe "cluster" "clusters" (length zIndexClusters) <> " in its folgezettel graph. "
      text "Each cluster's "
      elAttr "a" ("href" =: "https://neuron.zettel.page/folgezettel-heterarchy.html") $ text "folgezettel heterarchy"
      text " is rendered as a forest."
  where
    countNounBe noun nounPlural = \case
      1 -> "1 " <> noun
      n -> show n <> " " <> nounPlural

zettelLink :: DomBuilder t m => Maybe Text -> Zettel -> NeuronWebT t m Bool -> NeuronWebT t m Bool
zettelLink mq z w = do
  let matched = isNothing $ do
        q <- mq
        guard $ not $ T.toLower q `T.isInfixOf` T.toLower (zettelTitle z)
        pure ()
  elClass "span" (bool "q unmatched" "q matched" matched) $ do
    el "li" $ QueryView.renderZettelLink Nothing Nothing def z
    childMatched <- w
    pure $ matched || childMatched

renderErrors :: DomBuilder t m => Map ZettelID (NonEmpty ZettelError) -> NeuronWebT t m ()
renderErrors errors = do
  let severity = \case
        ZettelError_ParseError _ -> "negative"
        ZettelError_QueryResultErrors _ -> "warning"
        ZettelError_AmbiguousID _ -> "negative"
        ZettelError_AmbiguousSlug _ -> "negative"
      errorMessageHeader zid = \case
        ZettelError_ParseError (slug, _) -> do
          text "Zettel "
          QueryView.renderZettelLinkIDOnly zid slug
          text " failed to parse"
        ZettelError_QueryResultErrors (slug, _) -> do
          text "Zettel "
          QueryView.renderZettelLinkIDOnly zid slug
          text " has missing wiki-links"
        ZettelError_AmbiguousID _files -> do
          text $
            "More than one file define the same zettel ID ("
              <> unZettelID zid
              <> "):"
        ZettelError_AmbiguousSlug _slug -> do
          text $ "Zettel '" <> unZettelID zid <> "' ignored; has ambiguous slug"

  forM_ (Map.toList errors) $ \(zid, zErrors) ->
    forM_ zErrors $ \zError -> do
      divClass ("ui tiny message " <> severity zError) $ do
        divClass "header" $ errorMessageHeader zid zError
        el "p" $ do
          case zError of
            ZettelError_ParseError (_slug, parseError) ->
              renderZettelParseError parseError
            ZettelError_QueryResultErrors queryErrors ->
              el "ol" $ do
                forM_ (snd queryErrors) $ \qe ->
                  el "li" $ elPreOverflowing $ text $ showQueryResultError qe
            ZettelError_AmbiguousID filePaths ->
              el "ul" $ do
                forM_ filePaths $ \fp ->
                  el "li" $ el "tt" $ text $ toText fp
            ZettelError_AmbiguousSlug slug ->
              el "p" $ text $ "Slug '" <> slug <> "' is used by another zettel"

renderForest ::
  DomBuilder t m =>
  Maybe Text ->
  [Tree (Zettel, [Zettel])] ->
  NeuronWebT t m Bool
renderForest mq trees = do
  fmap or $
    forM trees $ \(Node (zettel, uplinks) subtrees) ->
      zettelLink mq zettel $ do
        when (length uplinks >= 2) $ do
          elClass "span" "uplinks" $ do
            forM_ uplinks $ \z2 -> do
              el "small" $
                elAttr "i" ("class" =: "linkify icon" <> "title" =: zettelTitle z2) blank
        if null subtrees
          then pure (isNothing mq)
          else el "ul" $ renderForest mq subtrees

style :: Css
style = do
  "div.z-index" ? do
    "p.info" ? do
      C.color C.gray
    C.ul ? do
      C.listStyleType C.square
      C.paddingLeft $ em 1.5
    ".uplinks" ? do
      C.marginLeft $ em 0.3
  -- debug: TODO: fold
  ".errors" ? do
    C.display C.none
  -- Search filtering
  ".q.unmatched" ? do
    -- C.fontSize $ em 0.3
    -- C.display C.none
    C.color C.auto
  ".q.unmatched > li" ? do
    C.fontSize $ em 0.5
