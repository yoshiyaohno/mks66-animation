module DrawMats where

import Line
import Screen
import Solids
import Transform

import Data.Array.Unboxed
import qualified Data.Map as M

data Material = Material
    { kar :: Double, kdr :: Double, ksr :: Double
    , kag :: Double, kdg :: Double, ksg :: Double
    , kab :: Double, kdb :: Double, ksb :: Double
    , ir  :: Double, ig  :: Double, ib  :: Double
    , alph :: Double
    }

defaultMat :: Material
defaultMat = Material
    0.1 0.6 0.4
    0.1 0.6 0.4
    0.1 0.6 0.4
    0   0   0
    10
    -- god tier formatting

type Materials = M.Map String Material
type Args = [String]
data DrawMats = DrawMats
     { getScreen :: Screen
     , getZBuf   :: ZBuf
     , getTStack :: [Transform Double]
     , getMats   :: Materials
     }

emptyDM :: DrawMats
emptyDM = DrawMats
    { getScreen = emptyScreen blk (1000, 1000)
    , getTStack = [ident]
    , getZBuf   = emptyZB (1000, 1000)
    , getMats   = M.empty
    }

addMaterial :: String -> Material -> DrawMats -> DrawMats
addMaterial name mat dm =
    case (M.lookup name $ getMats dm) of
        Nothing -> modMats (M.insert name mat) dm
        Just _  -> error $ "already defined material: " ++ name

findMaterial :: String -> DrawMats -> Material
findMaterial name dm =
    case (M.lookup name $ getMats dm) of
        Nothing -> error $ "no such material: " ++ name
        Just m  -> m

trTris :: DrawMats -> [Triangle Double] -> [Triangle Double]
trTris dm = map (trTriangle $ getTransform dm)

modMats :: (Materials -> Materials) -> DrawMats -> DrawMats
modMats f dm = dm { getMats = f $ getMats dm }

modScreen :: (Screen -> Screen) -> DrawMats -> DrawMats
modScreen f dm = dm { getScreen = f $ getScreen dm }

getTransform :: DrawMats -> Transform Double
getTransform = head . getTStack

popTransform :: DrawMats -> DrawMats
popTransform dm = dm { getTStack = tail $ getTStack dm }

pushTransform :: DrawMats -> DrawMats
pushTransform dm = dm { getTStack = (tf:tf:tfs) }
    where (tf:tfs) = getTStack dm

modTransform :: (Transform Double -> Transform Double) -> DrawMats -> DrawMats
modTransform f dm = dm { getTStack = ((f tf):tfs) }
    where (tf:tfs) = getTStack dm

