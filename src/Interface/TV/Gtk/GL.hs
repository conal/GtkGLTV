{-# LANGUAGE RecursiveDo, MultiParamTypeClasses, ScopedTypeVariables
           , TypeFamilies
  #-}
{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}   -- TEMP
----------------------------------------------------------------------
-- |
-- Module      :  Interface.TV.Gtk.GL
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Gtk-based GUIs in the TV (tangible value) framework
----------------------------------------------------------------------

module Interface.TV.Gtk.GL
  ( module Interface.TV.Gtk
  , renderOut, emptyTexture, textureIsEmpty, textureIn
  ) where


import Control.Applicative ((<$>))
import Data.IORef

import Graphics.UI.Gtk hiding (Action)

import Graphics.UI.Gtk.OpenGL
import qualified Graphics.Rendering.OpenGL as G
import Graphics.Rendering.OpenGL hiding (Sink,get)
-- For textures
import Data.Bitmap.OpenGL
import Codec.Image.STB

import Interface.TV.Gtk


mkCanvas :: IO GLDrawingArea
mkCanvas =
 glConfigNew [ GLModeRGBA, GLModeDepth , GLModeDouble, GLModeAlpha ]
  >>= glDrawingAreaNew

-- | Render output, given a rendering action.  Handles all set-up.
-- Intended as an implementation substrate for functional graphics. 
renderOut :: Out Action
renderOut = primMkO $
  do forget $ initGL
     canvas <- mkCanvas
     widgetSetSizeRequest canvas 300 300
     -- Initialise some GL setting just before the canvas first gets shown
     -- (We can't initialise these things earlier since the GL resources that
     -- we are using wouldn't have been set up yet)
     -- TODO experiment with moving some of these steps.
     forget $ onRealize canvas $ withGLDrawingArea canvas $ const $
       do -- setupMatrices  -- do elsewhere, e.g., runSurface
          depthFunc  $= Just Less
          drawBuffer $= BackBuffers
          clearColor $= Color4 0 0 0.2 1
     -- Stash the latest draw action for use in onExpose
     drawRef <- newIORef (return ())
     let display draw =
           -- Draw in context
           withGLDrawingArea canvas $ \ glwindow ->
              do clear [DepthBuffer, ColorBuffer]
                 flipY
                 draw
                 flipY
                 -- glWaitVSync
                 finish
                 glDrawableSwapBuffers glwindow
                 writeIORef drawRef draw
     -- Sync canvas size with and use draw action
     forget $ onExpose canvas $ \_ -> 
       do (w',h') <- widgetGetSize canvas
          let w = fromIntegral w' :: GLsizei
              h = fromIntegral h'
              maxWH = w `max` h
              start s = fromIntegral ((s - maxWH) `div` 2)
          viewport $= (Position (start w) (start h), Size maxWH maxWH)  -- square
          readIORef drawRef >>= display
          return True
     return (toWidget canvas, display, return ())

flipY :: Action
flipY = scale 1 (-1 :: GLfloat) 1

-- Is there another way to flip Y?

-- | An empty texture.  Test with 'textureIsEmpty'
emptyTexture :: TextureObject
emptyTexture = TextureObject bogusTO

bogusTO :: G.GLuint
bogusTO = -1

-- | Is a texture empty?
textureIsEmpty :: TextureObject -> Bool
textureIsEmpty (TextureObject i) = i == bogusTO

loadTexture :: FilePath -> IO (Either String TextureObject)
loadTexture path =
  do e  <- loadImage path
     case e of
       Left err -> return (Left err)
       Right im -> Right <$> makeSimpleBitmapTexture im


-- Is there a more elegant formulation of loadTex?  It's close to
-- being fmap on Either.  I can almost get there as follows:
-- 
--   foo :: FilePath -> IO (Either String (IO TextureObject))
--   foo = (result.fmap.fmap) makeSimpleBitmapTexture loadImage

-- loadImage :: FilePath -> IO (Either String Image)
-- makeSimpleBitmapTexture :: Image -> IO TextureObject

textureIn :: In TextureObject
textureIn = fileMungeIn loadTexture deleteTexture emptyTexture

deleteTexture :: Sink TextureObject
deleteTexture tex | textureIsEmpty tex = return ()
                  | otherwise          =
                      do -- putStrLn $ "deleteTexture " ++ show tex
                         deleteObjectNames [tex]

fileMungeIn :: -- Show a =>   -- for debugging
               (FilePath -> IO (Either String a)) -> Sink a -> a -> In a
fileMungeIn munge free start = primMkI $ \ refresh ->
  do w <- fileChooserButtonNew "Select file" FileChooserActionOpen
     current <- newIORef start
     -- onCurrentFolderChanged w $ putStrLn "onCurrentFolderChanged"
     -- onFileActivated w $ putStrLn "onFileActivated"
     -- I'm changing the value on preview.  TODO: change back if the
     -- user cancels.
     forget $ onUpdatePreview w $
       do -- putStrLn "onUpdatePreview"
          mb <- fileChooserGetFilename w
          case mb of
            Nothing -> return ()
            Just path ->
              do e <- munge path
                 case e of
                   Left   _ -> return ()
                   -- Left err -> putStrLn $ "fileMungeIn error: " ++ err
                   Right a  -> do readIORef current >>= free
                                  writeIORef current a
                                  -- putStrLn $ "fileMungeIn: new value " ++ show a
                                  refresh
     return (toWidget w, readIORef current, return ())

-- TODO: Replace the error message with a GUI version.

-- We're freeing the old thingie before saving the new thingie.  In a
-- multi-threaded setting, there could be dire consequences.

-- I'd like to move to a consistently GC'd setting, in which textures,
-- shaders, etc are GC'd.  In that case, what keeps GPU resources alive?
