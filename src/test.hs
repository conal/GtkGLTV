-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  test
-- Copyright   :  (c) Conal Elliott 2009-2011
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Test GtkGLTV
----------------------------------------------------------------------

import Control.Arrow ((&&&))

import Interface.TV.Gtk.GL2     -- or Gtk.GL
import Control.Arrow.DeepArrow ((->|),result,dupA)
import Data.FunArr (($$))

import Data.Lambda (lambda) -- or use oLambda
import Data.Pair   (pair)   -- or use iPair, oPair
import Data.Title  (title)  -- or use iTitle, oTitle

-- import Interface.TV (tv,runTV,boolIn,stringOut,oLambda)
-- import Interface.TV.Gtk (In,Out,gtv,R,sliderRI,sliderII)

import Graphics.Rendering.OpenGL hiding (Sink,get)


{--------------------------------------------------------------------
    Rendering examples
--------------------------------------------------------------------}

-- Test renderer.

renderGray :: Sink Float
renderGray x' = do -- putStrLn "renderGray"
                   color (Color4 x x x x)
                   renderSquare
 where
   x = realToFrac x' :: GLfloat

-- Render a square with vertex coordinates ranging from -1 to 1 and
-- texture coordinates ranging from 0 to 1
renderSquare :: Action
renderSquare =
  do renderPrimitive Quads $  -- start drawing a polygon (4 sided)
       do vert 0 1 -- top left
          vert 1 1 -- top right
          vert 1 0 -- bottom right
          vert 0 0 -- bottom left
 where
   vert :: GLfloat -> GLfloat -> Action
   vert u v = do texCoord (TexCoord2 u v)
                 vertex (Vertex2 (q u) (q v))
     where q w = 2 * w - 1


renderTexture :: Sink TextureObject
renderTexture tex | textureIsEmpty tex = return ()
                  | otherwise          = do useTexture tex
                                            renderSquare

useTexture :: Sink TextureObject
useTexture obj =
  do texture Texture2D $= Enabled
     activeTexture $= TextureUnit 0
     textureBinding Texture2D $= Just obj


iGray :: In R
iGray = title "gray level" $ sliderRIn (0,1) 0.5

tv7 :: GTV (R -> Action)
tv7 = tv (lambda iGray renderOut) renderGray

-- Oscillate between 0 & 1
osc :: Floating n => n -> n
osc x = (sin x + 1) / 2

oscTV :: GTV (R -> R)
oscTV = tv (lambda (sliderRIn (0,10) 0) (title "osc" defaultOut)) osc

clockTV :: GTV (R -> R)
clockTV = tv (lambda clockIn defaultOut) id

clockOscTV :: GTV (R -> R)
clockOscTV = clockTV ->| oscTV

tv8 :: GTV (R -> Action)
tv8 = tv (lambda clockIn renderOut) (renderGray . osc)

tv8' :: GTV (R -> Action)
-- tv8' = clockOscTV ->| tv7
tv8' = clockTV ->| oscTV ->| tv7

tv9 :: GTV (R -> (R,Action))
tv9 = tv (lambda clockIn (title "osc" defaultOut `pair` renderOut)) ((id &&& renderGray) . osc)

-- TODO: refactor tv9

tv10 :: GTV (R -> (Action,Action))
tv10 = result dupA $$ tv7

tv11 :: GTV (R -> Action, R -> Action)
tv11 = dupA $$ tv7

tv12 :: GTV (R -> (Action,Action))
tv12 = result dupA $$ tv8

tv13 :: GTV (TextureObject -> Action)
tv13 = tv (lambda textureIn renderOut) renderTexture

tv14 :: GTV (R -> R)
tv14 = tv (lambda (title "rate" $ integralIn $ sliderRIn (-10,10) 0) defaultOut) id

