-- To do:
--  - Wallpaper
--  - All those fromIntegrals

import Control.Monad
import Data.Char
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M

data KeyColor = White | Black

type PC = Int     -- pitch class
type Finger = Int -- fingering

keyWidth w = (fromIntegral w) / 12.0

keyColors = [White, Black, White, Black, White, White, Black, White, Black, White, Black, White]
fingerColors = [(0.3, 0.0, 0.7),
                (0.1, 0.1, 1.0),
                (0.0, 0.5, 0.4),
                (0.0, 0.6, 0.0)]

parseFingering :: String -> [(PC, Finger)]
parseFingering s =
    map (\(i,c) -> (i, digitToInt c)) $ filter (\(i,c) -> c /= ' ') $ zip [0..] s

fingerings = map parseFingering
     ["1 2 31 2 3 4",
      "1 2 3 41 2 3",
      " 41 2 31 2 3",
      " 31 2 3 41 2",
      " 3 41 2 31 2",
      " 2 31 2 3 41",
      " 2 3 12 3 41",
      "12 3 12 3 4 ",
      "12 3 1 23 4 ",
      "1 23 1 23 4 ",
      "1 23 1 2 34 ",
      "1 2 31 2 34 "]

drawFinger :: Int -> Int -> PC -> Int -> Finger -> C.Render ()
drawFinger w h pc lineNo finger = do
  C.save

  C.setOperator C.OperatorOver
  C.setSourceRGB red green blue
  C.arc x y r 0 (2 * pi)
  C.fill

  C.setSourceRGB 1 1 1
  C.selectFontFace "Arial" C.FontSlantNormal C.FontWeightBold
  C.setFontSize ((fromIntegral w) / 25.0)
  C.setLineWidth 1.0
  (C.TextExtents txb tyb tw th txa tya) <- C.textExtents text
  C.moveTo (x - txa/2) (y + th/2)
  C.textPath text
  C.fill

  C.restore
      where (red,green,blue) = fingerColors !! (finger - 1)
            x = (keyWidth w) * (0.5 + fromIntegral pc)
            y = (keyWidth w) * (0.5 + fromIntegral lineNo)
            r = (keyWidth w) * 0.4
            text = show finger

drawScaleFingering :: Int -> Int -> Int -> [(PC, Finger)] -> C.Render()
drawScaleFingering w h lineNo fs = do
  C.save
  forM_ fs $ \(pc, f) -> drawFinger w h pc lineNo f
  C.restore
    
renderKey :: Int -> Int -> PC -> KeyColor -> C.Render ()
renderKey w h idx color = do
  C.save
  C.rectangle ((fromIntegral idx) * (keyWidth w)) 0 (keyWidth w) (fromIntegral h)
  C.setFillRule C.FillRuleWinding
  case color of
    White -> C.stroke
    Black -> C.fill
  C.restore

renderFingering :: Int -> Int -> C.Render ()
renderFingering w h = do
  C.save
  forM_ (zip [0..] keyColors) $ \(i, c) -> renderKey w h i c
  drawFinger w h 0 0 1
  forM_ (zip [0..] fingerings) $ \(i, f) -> drawScaleFingering w h i f
  C.restore

main :: IO ()
main = do
  C.withImageSurface C.FormatARGB32 width height $ \surf -> do
    C.renderWith surf $ renderFingering width height
    C.surfaceWriteToPNG surf "fingering.png"
    where width  = 400
          height = 400

