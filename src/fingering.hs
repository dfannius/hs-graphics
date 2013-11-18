-- To do:
--  - Wallpaper
--  - All those fromIntegrals

import Control.Monad
import Data.Char
import qualified Graphics.Rendering.Cairo as C

data KeyColor = White | Black

type PC = Int     -- pitch class
type Finger = Int -- fingering

keyColors = [White, Black, White, Black, White, White, Black, White, Black, White, Black, White]
fingerColors = [(0.3, 0.0, 0.7),
                (0.1, 0.1, 1.0),
                (0.0, 0.5, 0.4),
                (0.0, 0.6, 0.0)]

parseFingering :: String -> [(PC, Finger)]
parseFingering s =
    map (\(i,c) -> (i, digitToInt c)) $ filter (\(i,c) -> c /= ' ') $ zip [0..] s

fingerings = map (parseFingering . cycle)
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

drawFinger :: Double -> Double -> PC -> Int -> Finger -> C.Render ()
drawFinger xScale yScale pc lineNo finger = do
  C.save

  C.setOperator C.OperatorOver
  C.setSourceRGB red green blue
  C.arc x y r 0 (2 * pi)
  C.fill

  C.setSourceRGB 1 1 1
  C.selectFontFace "Arial" C.FontSlantNormal C.FontWeightBold
  C.setFontSize (r * 1.2)
  C.setLineWidth 1.0
  (C.TextExtents txb tyb tw th txa tya) <- C.textExtents text
  C.moveTo (x - txa/2) (y + th/2)
  C.textPath text
  C.fill

  C.restore
      where (red,green,blue) = fingerColors !! (finger - 1)
            x = xScale * (0.5 + fromIntegral pc)
            y = yScale * (0.5 + fromIntegral lineNo)
            r = minimum [xScale, yScale] * 0.4
            text = show finger

drawScaleFingering :: Int -> Double -> Double -> Int -> [(PC, Finger)] -> C.Render()
drawScaleFingering numKeys xScale yScale lineNo fs = do
  C.save
  forM_ fs $ \(pc, f) -> drawFinger xScale yScale pc lineNo f
  C.restore
    
renderKey :: Double -> Double -> PC -> KeyColor -> C.Render ()
renderKey xScale h idx color = do
  C.save
  C.rectangle ((fromIntegral idx) * xScale) 0 xScale h
  C.setFillRule C.FillRuleWinding
  case color of
    White -> C.stroke
    Black -> C.fill
  C.restore

renderFingering :: Double -> Double -> C.Render ()
renderFingering w h = do
  C.save
  forM_ (take numKeys $ zip [0..] (cycle keyColors)) $
            \(i, c) -> renderKey xScale h i c
  forM_ (take numScales $ zip [0..] (cycle (map (take numKeys) fingerings))) $
            \(i, f) -> drawScaleFingering numKeys xScale yScale i f
  C.restore
    where numKeys = 24
          numScales = 24
          xScale  = w / (fromIntegral numKeys)
          yScale  = h / (fromIntegral numKeys)

main :: IO ()
main = do
  C.withImageSurface C.FormatARGB32 width height $ \surf -> do
    C.renderWith surf $ renderFingering (fromIntegral width) (fromIntegral height)
    C.surfaceWriteToPNG surf "fingering.png"
    where width  = 800
          height = 800

