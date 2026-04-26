{-# LANGUAGE OverloadedRecordDot #-}

{- HLINT ignore "Use zipWith" -}
module Card.Decode (
    fromDisk,
    fromDiskWithNameFilter,
) where

import Card (Card (..))
import Card qualified
import Config qualified
import Control.Lens (view, (^.))
import Data.Binary.Get (Get)
import Data.Binary.Get qualified as Get
import Data.ByteString.Lazy qualified as BL
import Util qualified

fromDisk :: IO [Card Card.Effect]
fromDisk = fromDiskWithNameFilter $ const True

fromDiskWithNameFilter :: (Text -> Bool) -> IO [Card Card.Effect]
fromDiskWithNameFilter nameFilter =
    mkCards nameFilter
        <$> Util.decodeFile Config.paths.name
        <*> Util.decodeFile Config.paths.desc
        <*> readFileLBS Config.paths.part
        <*> readFileLBS Config.paths.pidx

mkCards ::
    (Text -> Bool) ->
    [Text] ->
    [Text] ->
    BL.ByteString ->
    BL.ByteString ->
    [Card Card.Effect]
mkCards nameFilter names (map encodeUtf8 -> descs) partSrc pidxSrc =
    mapMaybe fromNameDescAndPidx $ zip3 names descs pidxData
  where
    pidxData :: [Card.PidxData]
    pidxData = Get.runGet (void Get.getWord32le >> many getSinglePidxData) pidxSrc

    allEffectPositions :: [Card.PartData]
    allEffectPositions = Get.runGet (many getSinglePartData) partSrc

    getSinglePidxData :: Get Card.PidxData
    getSinglePidxData = do
        firstEffectIndex <- fromIntegral <$> Get.getWord16le
        void Get.getWord8
        counts <- Get.getWord8
        let effectCount = fromIntegral $ counts `div` 16 -- first digit
            pendulumEffectCount = fromIntegral $ counts `mod` 16 -- second digit
        pure $ Card.PidxData firstEffectIndex effectCount pendulumEffectCount

    getSinglePartData :: Get Card.PartData
    getSinglePartData = do
        start <- fromIntegral <$> Get.getWord16le
        end <- fromIntegral <$> Get.getWord16le
        pure $ Card.PartData start end

    fromNameDescAndPidx :: (Text, BL.ByteString, Card.PidxData) -> Maybe (Card Card.Effect)
    fromNameDescAndPidx (name, desc, pidx) =
        if nameFilter name
            then Just $ Card name leadingText effects pendulumEffects
            else Nothing
      where
        leadingText :: Text
        leadingText = decodeUtf8 $ BL.take (fromIntegral firstEffectStartPosition) desc

        effects :: [Card.Effect]
        effects = takeAmountPlusEmpty pidx.effectCount allEffects

        pendulumEffects :: [Card.Effect]
        pendulumEffects = dropAmountPlusEmpty pidx.effectCount allEffects

        effectPositionsWithOriginalOrder :: [(Card.PartData, Int)]
        effectPositionsWithOriginalOrder =
            sortOn (fst >>> (^. #start))
                . take (pidx.effectCount + pidx.pendulumEffectCount)
                . drop pidx.firstEffectIndex
                $ zip allEffectPositions [0 ..]

        firstEffectStartPosition :: Int
        firstEffectStartPosition =
            maybe 99999999 (\(part, _) -> part.start) $
                find (\(partData, _) -> partData.end /= 0) effectPositionsWithOriginalOrder

        allEffects :: [Card.Effect]
        allEffects = map fromEffectPositions $ zip effectPositionsWithOriginalOrder [0 ..]

        fromEffectPositions :: ((Card.PartData, Int), Int) -> Card.Effect
        fromEffectPositions ((effectPos, originalPosition), i) =
            let
                nextEffectPos currentIndex =
                    let next = effectPositionsWithOriginalOrder !!? (currentIndex + 1)
                    in if (view #start . fst <$> next) == Just 0
                        then nextEffectPos (currentIndex + 1)
                        else fst <$> next
                mainEffect =
                    decodeUtf8
                        . BL.take (fromIntegral (effectPos.end - effectPos.start))
                        $ BL.drop (fromIntegral effectPos.start) desc
            in
                Card.Effect
                    { mainEffect = mainEffect
                    , trailingText =
                        if mainEffect == ""
                            then ""
                            else
                                decodeUtf8
                                    . case nextEffectPos i of
                                        Nothing -> identity
                                        Just next ->
                                            BL.take
                                                (fromIntegral (next.start - effectPos.end))
                                    $ BL.drop (fromIntegral effectPos.end) desc
                    , originalPosition = originalPosition
                    }

        isEmptyEffect :: Card.Effect -> Bool
        isEmptyEffect effect = effect.mainEffect == "" && effect.trailingText == ""

        takeAmountPlusEmpty :: Int -> [Card.Effect] -> [Card.Effect]
        takeAmountPlusEmpty 0 = const []
        takeAmountPlusEmpty n = \case
            [] -> []
            (x : xs)
                | isEmptyEffect x -> x : takeAmountPlusEmpty n xs
                | otherwise -> x : takeAmountPlusEmpty (n - 1) xs

        dropAmountPlusEmpty :: Int -> [Card.Effect] -> [Card.Effect]
        dropAmountPlusEmpty 0 fx = fx
        dropAmountPlusEmpty n fx = case fx of
            [] -> []
            (x : xs)
                | isEmptyEffect x -> dropAmountPlusEmpty n xs
                | otherwise -> dropAmountPlusEmpty (n - 1) xs
