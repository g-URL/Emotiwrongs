--
-- Starting code for CPSC 449 Assignment 1
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML 
-- document.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- The width and height of the image being generated.
--
width :: Int
width = 1024

height :: Int
height = 768

--
-- Generate and return a list of 20000 random floating point numbers between 
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
-- 
randomList :: Int -> [Float]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)





-- ==============================================================================================
-- ==============================================================================================
--
-- R. Apperley's Code 
--



--
-- Recursively generate rectangles using random numbers to create an emoticon.
-- 
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:t:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:u:rs)
  | (w > half_width) && (h > half_height) = (lst_4, (str_0 ++ str_1 ++ str_2 ++ str_3 ++ str_4))
  | (w > half_width) && (h <= half_height) = (lst_6, (str_0 ++ str_5 ++ str_6))
  | (w <= half_width) && (h > half_height) = (lst_8, (str_0 ++ str_7 ++ str_8))

  | (rand_w_split < w) && (rand_h_split < h) = (lst_4, (str_0 ++ str_1 ++ str_2 ++ str_3 ++ str_4))
  | (rand_w_split < w) && (rand_h_split >= h) = (lst_6, (str_0 ++ str_5 ++ str_6))
  | (rand_w_split >= w) && (rand_h_split < h) = (lst_8, (str_0 ++ str_7 ++ str_8))

  | otherwise = (lst_o, str_o)

  where 
        -- =================================================
        -- CALCULATIONS OF WIDTH 
        -- =================================================

        --
        -- Calculation of half of the original canvas width.
        --
        half_width = width `div` 2

        --
        -- Calculation of new width and remaining width for the new regions. 
        -- The new width is randomly selected between 50% and 84% of the original region width.
        --
        new_width = round(fromIntegral w * (0.5 + (r * 0.34)))
        rem_width = w - new_width

        --
        -- Calculation of a random number used to determine if the width of a region should be split.
        --
        rand_w_split = round((fromIntegral w * s) + 20)


        -- =================================================
        -- CALCULATIONS OF HEIGHT
        -- =================================================

        --
        -- Calculation of half of the original canvas height.
        --
        half_height = height `div` 2

        --
        -- Calculation of new height and remaining height for the new regions.
        -- The new height is randomly selected between 50% and 84% of the original region height.
        --
        new_height = round(fromIntegral h * (0.5 + (t * 0.34)))
        rem_height = h - new_height

        --
        -- Calculation of a random number used to determine if the height of a region should be split.
        --
        rand_h_split = round((fromIntegral h * u) + 20)


        -- =================================================
        -- CALCULATIONS OF NEW COORDINATES
        -- =================================================

        --
        -- The new coordinates will be used for the new regions.
        --
        new_x = x + new_width
        new_y = y + new_height


        -- =================================================
        -- DIFFERENT CASES FOR RECURSIVE CALLS
        -- =================================================

        --
        -- Drawing this rectangle.
        -- 
        my_white_rect = myRectangle x y w h [1,1,1]
        lst_0 = rs
        str_0 = snd (my_white_rect)

        --
        -- Case 1 (4 recursive calls)
        --
        (lst_1, str_1) = mondrian x y new_width new_height lst_0
        (lst_2, str_2) = mondrian new_x y rem_width new_height lst_1
        (lst_3, str_3) = mondrian x new_y new_width rem_height lst_2
        (lst_4, str_4) = mondrian new_x new_y rem_width rem_height lst_3

        --
        -- Case 2 (2 recursive calls)
        --
        (lst_5, str_5) = mondrian x y new_width h lst_0
        (lst_6, str_6) = mondrian new_x y rem_width h lst_5

        --
        -- Case 3 (2 recursive calls)
        --
        (lst_7, str_7) = mondrian x y w new_height lst_0
        (lst_8, str_8) = mondrian x new_y w rem_height lst_7

        --
        -- Case otherwise (1 recursive call)
        --
        (lst_o, str_o) = myRectangle x y w h rs



--
-- Generates the tag for a rectangle with a randomly selected color.
-- 
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:t:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
myRectangle :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
myRectangle x y w h (r:s:t:rs)

  --
  -- Smile lines
  --
  | ((per_width >= 0.20 && per_width <= 0.30) || (per_width >= 0.70 && per_width <= 0.80)) && (per_height >= 0.36 && per_height <= 0.64) && (r < 0.20)  = (rs, (str ++ wht))
  | ((per_width >= 0.20 && per_width <= 0.30) || (per_width >= 0.70 && per_width <= 0.80)) && (per_height >= 0.36 && per_height <= 0.64) && (r < 0.50)  = (rs, (str ++ ylw))
  | ((per_width >= 0.20 && per_width <= 0.30) || (per_width >= 0.70 && per_width <= 0.80)) && (per_height >= 0.36 && per_height <= 0.64) && (r < 1.00)  = (rs, (str ++ blk))

  --
  -- Mouth
  --
  | (per_width >= 0.20 && per_width <= 0.80) && (per_height >= 0.45 && per_height <= 0.54) && (r < 0.20)  = (rs, (str ++ wht))
  | (per_width >= 0.20 && per_width <= 0.80) && (per_height >= 0.45 && per_height <= 0.54) && (r < 0.30)  = (rs, (str ++ ylw))
  | (per_width >= 0.20 && per_width <= 0.80) && (per_height >= 0.45 && per_height <= 0.54) && (r < 1.00)  = (rs, (str ++ blk))

  --
  -- Eyes
  --
  | ((per_width >= 0.35 && per_width <= 0.45) || (per_width >= 0.55 && per_width <= 0.65)) && (per_height >= 0.22 && per_height <= 0.32) && (r < 0.20)  = (rs, (str ++ wht))
  | ((per_width >= 0.35 && per_width <= 0.45) || (per_width >= 0.55 && per_width <= 0.65)) && (per_height >= 0.22 && per_height <= 0.32) && (r < 0.30)  = (rs, (str ++ ylw))
  | ((per_width >= 0.35 && per_width <= 0.45) || (per_width >= 0.55 && per_width <= 0.65)) && (per_height >= 0.22 && per_height <= 0.32) && (r < 1.00)  = (rs, (str ++ blk))

  --
  -- Face
  --
  | (per_width >= 0.18 && per_width <= 0.82) && (per_height >= 0.18 && per_height <= 0.82) && (r < 0.25)  = (rs, (str ++ wht))
  | (per_width >= 0.18 && per_width <= 0.82) && (per_height >= 0.18 && per_height <= 0.82) && (r < 1.00)  = (rs, (str ++ ylw))

  --
  -- More face
  --
  | ((per_width >= 0.09 && per_width <= 0.91) && (per_height >= 0.40 && per_height <= 0.60)) || ((per_width >= 0.40 && per_width <= 0.60) && (per_height >= 0.09 && per_height <= 0.91)) && (r < 0.15)  = (rs, (str ++ wht))
  | ((per_width >= 0.09 && per_width <= 0.91) && (per_height >= 0.40 && per_height <= 0.60)) || ((per_width >= 0.40 && per_width <= 0.60) && (per_height >= 0.09 && per_height <= 0.91)) &&  (r < 1.00) = (rs, (str ++ ylw))

  | otherwise  = (rs, (str ++ wht))


  where 
        --
        -- HTML code that describes the rectangle's position and size and stroke.
        --
        str = "<rect x=" ++ (show x) ++ " y=" ++ (show y) ++ " width=" ++ (show w) ++
              " height=" ++ (show h) ++ " stroke=\"rgb(" ++ show 0 ++ "," ++ show 0 ++ "," ++ show 0 ++ ")\""

        --
        -- HTML code that describes the rectangle's colour.
        --
        blk = " fill=\"rgb(" ++ (show 54) ++ "," ++ (show 53) ++ "," ++ (show 49) ++ ")\" />\n"
        ylw = " fill=\"rgb(" ++ (show 255) ++ "," ++ (show 233) ++ "," ++ (show 124) ++ ")\" />\n"
        wht = " fill=\"rgb(" ++ (show 255) ++ "," ++ (show 255) ++ "," ++ (show 255) ++ ")\" />\n"

        --
        -- Calculations using the x,y coordinates to determine the location in the canvas as a percentage of original width or height.
        --
        per_width = (fromIntegral x) / (fromIntegral width)
        per_height = (fromIntegral y) / (fromIntegral height)


--
-- ==============================================================================================
-- ==============================================================================================
--



--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  --  Right now, the program will generate a different sequence of random
  --  numbers each time it is run.  If you want the same sequence each time
  --  use "let seed = 0" instead of "seed <- randomRIO (0, 100000 :: Int)"

  --let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)
