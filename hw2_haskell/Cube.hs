module Cube where
 
import Graphics.UI.GLUT
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w), -- 右 2156
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w), -- 上 2103
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w), -- 前 2673
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w), -- 左 3047
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w), -- 下 6547
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]-- 后 1540
  
cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f -- draw the same points with diff color
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]
