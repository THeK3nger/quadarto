{-# OPTIONS_GHC -Wall #-}

module Main where

import Codec.Picture
import Data.Word
import System.Environment
import System.Exit

-- import           Debug.Trace

-- LOAD IMAGE ------------------------------------------------------------------

-- | extractImage assume an RGB8 image.
-- | Parse the result of Codec.Picture.readImage to extract the raw image data.
extractImage :: Either String DynamicImage -> Image PixelRGB8
extractImage (Left e) = error ("Unable to load the image!" ++ e)
extractImage (Right (ImageRGB8 img)) = img
extractImage _ = error "Invalid Format"

--------------------------------------------------------------------------------

-- QUADTREE --------------------------------------------------------------------

type Coord = (Int, Int)

type Rect = (Coord, Coord)

-- | Return all the pixels in the image inside the Rect
pixelsInSquare :: Image PixelRGB8 -> Rect -> [PixelRGB8]
-- pixelsInSquare _ rect | trace ("pixelsInSquare:: " ++ show rect) False = undefined
pixelsInSquare img ((x1, y1), (x2, y2)) = [pixelAt img x y | x <- [x1 .. x2], y <- [y1 .. y2]]

-- | How many pixels are inside Rect?
rectArea :: Rect -> Int
rectArea ((x1, y1), (x2, y2)) = (x2 - x1 + 1) * (y2 - y1 + 1)

rectWidth :: Rect -> Int
rectWidth ((x1, _), (x2, _)) = x2 - x1 + 1

-- | Return the average color in the given square.
avgColor ::
  -- | The input image.
  Image PixelRGB8 ->
  -- | The targeet rectangle.
  Rect ->
  -- | The return average pixel color.
  -- avgColor _ rect | trace ("avgColor:: " ++ show rect) False = undefined
  PixelRGB8
avgColor img rect =
  PixelRGB8 (fromIntegral avgRed) (fromIntegral avgGreen) (fromIntegral avgBlue)
  where
    (avgRed, avgGreen, avgBlue) = avgTuple (rectArea rect) $ foldr sumPixels (0, 0, 0) (pixelsInSquare img rect)
    avgTuple :: Int -> (Int, Int, Int) -> (Int, Int, Int)
    avgTuple n (x, y, z) = (div x n, div y n, div z n)
    -- \| Sum a pixel in an accumulator tuple.
    sumPixels :: PixelRGB8 -> (Int, Int, Int) -> (Int, Int, Int)
    sumPixels (PixelRGB8 pr pg pb) (r, g, b) = (r + fromIntegral pr, g + fromIntegral pg, b + fromIntegral pb)

-- | Return true iff the coordinate is in the rect.
inRect :: Coord -> Rect -> Bool
-- inRect cord rect | trace ("inRect:: " ++ show rect ++ " " ++ show cord) False = undefined
inRect (x, y) ((x1, y1), (x2, y2))
  | x <= x2 && x >= x1 && y <= y2 && y >= y1 = True
  | otherwise = False

-- | Return true iff the coordinate is on the rect border.
onRectBorder :: Coord -> Rect -> Bool
onRectBorder c@(x, y) r@((x1, y1), (x2, y2))
  | inRect c r && (x == x2 || x == x1 || y == y1 || y == y2) = True
  | otherwise = False

wordIsNear :: Word8 -> Word8 -> Int -> Bool
wordIsNear a b threshold =
  let ai = fromIntegral a :: Int
      bi = fromIntegral b :: Int
   in abs (ai - bi) <= threshold

-- | Check if colors in rect are are between a threshold respect to the average.
-- | If true, the rect is "indivisible".
isIndivisible :: Image PixelRGB8 -> Int -> Rect -> Bool
-- isIndivisible _ t rect | trace ("isIndivisible:: " ++ show rect ++ " th: " ++ show t) False = undefined
isIndivisible img threshold rect =
  let isPixelInThreshold (PixelRGB8 r g b)
        | wordIsNear ar r threshold && wordIsNear ag g threshold || wordIsNear ab b threshold = True
        | otherwise = False
        where
          (PixelRGB8 ar ag ab) = avgColor img rect
   in -- any isPixelInThreshold $ pixelsInSquare img rect
      all isPixelInThreshold (pixelsInSquare img rect)

-- | Split the rect in 4 non overlapping rects.
-- | Assumption: Square, multiple of 2.
quadSplit :: Rect -> [Rect]
-- quadSplit rect | trace ("quadSplit:: " ++ show rect) False = undefined
quadSplit rect@((x1, y1), (x2, y2)) =
  [((x1, y1), (x2', y2')), ((x1', y1), (x2, y2')), ((x1, y1'), (x2', y2)), ((x1', y1'), (x2, y2))]
  where
    size = rectWidth rect
    newsize = div size 2
    x1' = x1 + newsize
    x2' = x2 - newsize
    y1' = y1 + newsize
    y2' = y2 - newsize

-- | Return a list of rect obtained by the quaddecomposition of the image over the given rect.
quadDecomposition :: Image PixelRGB8 -> Int -> Rect -> [Rect]
-- quadDecomposition _ _ rect |trace ("quadDecomposition:: " ++ show rect) False = undefined
quadDecomposition img threshold rect
  | rectWidth rect <= 8 = [rect]
  | isIndivisible img threshold rect = [rect]
  | otherwise = concatMap (quadDecomposition img threshold) (quadSplit rect)

-- | Zip rectancles with their color average.
zipRectsWithColors :: Image PixelRGB8 -> [Rect] -> [(Rect, PixelRGB8)]
zipRectsWithColors img rects = [(r, avgColor img r) | r <- rects]

--------------------------------------------------------------------------------

pixelRenderer :: Image PixelRGB8 -> [(Rect, PixelRGB8)] -> Int -> Int -> PixelRGB8
-- pixelRenderer _ rects x y | trace ("pixelRenderer:: " ++ show rects ++ " " ++ show (x,y)) False = undefined
pixelRenderer img rects x y =
  case theRect of
    [] -> pixelAt img x y
    ((r, c) : _) -> if onRectBorder (x, y) r then PixelRGB8 0 0 0 else c
  where
    theRect = filter (inRect (x, y) . fst) rects

-- | Test. Write a gradient image. Just for learning.
imageCreator :: Image PixelRGB8 -> [Rect] -> String -> IO ()
-- imageCreator _ rects _ | trace ("imageCreator:: " ++ show rects) False = undefined
imageCreator img rects path = writePng path $ generateImage (pixelRenderer img (zipRectsWithColors img rects)) (imageWidth img) (imageHeight img)

-- MAIN --- --------------------------------------------------------------------

parseArguments :: IO [String] -> IO (String, Int, Maybe String)
parseArguments argsIO = do
  args <- argsIO
  case args of
    [path, th] -> return (path, read th, Nothing)
    [path, th, "-o", outPath] -> return (path, read th, Just outPath)
    _ -> usage

usage :: IO a
usage = do
  progName <- getProgName
  putStrLn ("Usage " ++ progName ++ " <input-file> <color_threshold> [-o <output_file>]")
  exitSuccess

outPathParsing :: Maybe String -> String
outPathParsing Nothing = "default.png"
outPathParsing (Just out) = out

main :: IO ()
main = do
  -- Parsing command line arguments
  (inputPath, threshold, maybeOut) <- parseArguments getArgs
  let outPath = outPathParsing maybeOut
  print "Loading Image..."
  raw_image <- readImage inputPath
  let image = extractImage raw_image
  print "OK"
  let rects = quadDecomposition image threshold ((0, 0), (imageWidth image - 1, imageHeight image - 1))
  imageCreator image rects outPath

--------------------------------------------------------------------------------
