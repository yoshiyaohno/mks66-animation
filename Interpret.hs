{-# LANGUAGE FlexibleContexts #-}
module Interpret where

import Parser
import Line
import Screen
import DrawMats
import Lighting
import qualified Solids as S
import qualified Transform as T

import System.IO
import System.Process
import System.Directory
import System.Environment
import Control.Monad.State
import qualified Data.List as L

interpret :: (MonadState DrawMats m, MonadIO m) => [Command] -> m ()
interpret = mapM_ cmd

cmd :: (MonadState DrawMats m, MonadIO m) => Command -> m ()
cmd c = case c of
    CmdPush                 -> push
    CmdPop                  -> pop
    CmdBox      a b c d     -> box      a b c d 
    CmdSphere   a b c d     -> sphere   a b c d 
    CmdTorus    a b c d e   -> torus    a b c d e
    CmdLine     a b c d e   -> line     a b c d e
    CmdMove     a b         -> move     a b
    CmdScale    a b         -> scale    a b
    CmdRotate   a b c       -> rote     a b c
    CmdDisplay              -> display
    CmdSave path            -> save path
    CmdConstants s r g b i a-> constants s r g b i a
    _                       -> return ()

constants :: (MonadState DrawMats m) =>
    String -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> Db -> m ()
constants name (kar,kdr,ksr) (kag,kdg,ksg) (kab,kdb,ksb) (ir,ig,ib) alph =
    modify $ addMaterial name m
        where m = Material kar kdr ksr kag kdg ksg kab kdb ksb ir ig ib alph

save :: (MonadState DrawMats m, MonadIO m) => String -> m ()
save path = do
    dm <- get
    liftIO $ do
        writeFile ".tempimg.ppm" (printPixels . downsample $ getScreen dm)
        callProcess "convert" [".tempimg.ppm", path]
        removeFile ".tempimg.ppm"

display :: (MonadState DrawMats m, MonadIO m) => m ()
display = do
    dm <- get
    liftIO $ do
        writeFile ".tempimg.ppm" (printPixels . downsample $ getScreen dm)
        callProcess "eog" [".tempimg.ppm"]
        removeFile ".tempimg.ppm"

rote :: (MonadState DrawMats m) => Axis -> Db -> MS -> m ()
rote ax theta _ = modify . modTransform $ (mappend $ roti ax theta)
    where roti ax theta
            | ax == AxisX = T.rotX (-theta)
            | ax == AxisY = T.rotY (-theta)
            | ax == AxisZ = T.rotZ (-theta)

scale :: (MonadState DrawMats m) => Vec3 -> MS -> m ()
scale (x,y,z) _ = modify . modTransform $ (mappend $ T.scale x y z)

move :: (MonadState DrawMats m) => Vec3 -> MS -> m ()
move (x,y,z) _ = modify . modTransform $ (mappend $ T.trans x y z)

line :: (MonadState DrawMats m) => MS -> Vec3 -> MS -> Vec3 -> MS -> m ()
line _ (x0,y0,z0) _ (x1,y1,z1) _ = do
    dm <- get
    let ln = Line (T.pmult (getTransform dm) (Vect x0 y0 z0 1))
                  (T.pmult (getTransform dm) (Vect x1 y1 z1 1))
    modify . modScreen $ drawLine red (fmap round ln)

box :: (MonadState DrawMats m) => MS -> Vec3 -> Vec3 -> MS -> m ()
box mat (cx,cy,cz) (w,h,d) _ = do
    dm <- get
    let tris = S.box cx cy cz w h d
        m = case mat of
            Nothing -> defaultMat
            Just s  -> findMaterial s dm
    drawTriangles m $ trTris dm tris

sphere :: (MonadState DrawMats m) => MS -> Vec3 -> Db -> MS -> m ()
sphere mat (cx,cy,cz) r _ = do
    dm <- get
    let tris = S.sphere cx cy cz r
        m = case mat of
            Nothing -> defaultMat
            Just s  -> findMaterial s dm
    drawTriangles m $ trTris dm tris
    
torus :: (MonadState DrawMats m) => MS -> Vec3 -> Db -> Db -> MS -> m ()
torus mat (cx,cy,cz) r0 r1 _ = do
    dm <- get
    let tris = S.torus cx cy cz r0 r1
        m = case mat of
            Nothing -> defaultMat
            Just s  -> findMaterial s dm
    drawTriangles m $ trTris dm tris

push :: (MonadState DrawMats m) => m ()
push = modify pushTransform

pop :: (MonadState DrawMats m) => m ()
pop = modify popTransform

