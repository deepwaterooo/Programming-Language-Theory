module Display (idle, display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Points

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback

display angle pos = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  (x', y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    --rotate a $ Vector3 0 0 1
    rotate a $ Vector3 0 0.3 0 -- 0 .3 1 changed y-component a bit to show off cube corners
    scale 0.3 0.3 (0.3::GLfloat)
    forM_ (points 27) $ \(x, y, z) -> preservingMatrix $ do -- do a cube at a time, glppm-like
      color $ Color3 ((x + 1)/2) ((y + 1)/2) ((z + 1)/2) -- x y z [-1 1], normalized
      --translate $ Vector3 x y z
      translate $ Vector3 (case x of -- all 27 the same, becuase of same x y z
        m | (m-1)/9 == 0 -> -1
        m | (m-1)/9 == 1 -> 0
        _ -> 1) (case y of
        m | (m-1)/9 == 0 -> -1
        m | (m-1)/9 == 1 -> 0
        _ -> 1)  z 
      cube 0.2
      color $ Color3 (0::GLfloat) 0 0 
      cubeFrame 0.2 
  swapBuffers
 
idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing
