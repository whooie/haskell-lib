-- | Preferred defaults for plotting with @matplotlib@.
module Whooie.Plotting
  ( ColorSet (..)
  , csetStrings
  , ColorPt
  , RgbFloat
  , ColorMap (..)
  , cmapName
  , cmapPts
  , cmapf
  , cmapfS
  , defplot
  ) where

import Data.List (find)
import Graphics.Matplotlib

colorsMatlab :: [String]
colorsMatlab =
  [ "#0072bd"
  , "#d95319"
  , "#edb120"
  , "#7e2f8e"
  , "#77ac30"
  , "#4dbeee"
  , "#a2142f"
  ]

colorsPython :: [String]
colorsPython =
  [ "#1f77b4"
  , "#ff7f0e"
  , "#2ca02c"
  , "#d62728"
  , "#9467bd"
  , "#8c564b"
  , "#e377c2"
  , "#7f7f7f"
  , "#bcbd22"
  , "#17becf"
  ]

colorsPlotly :: [String]
colorsPlotly =
  [ "#636efa"
  , "#ef553b"
  , "#00cc96"
  , "#ab63fa"
  , "#ffa15a"
  , "#19d3f3"
  , "#ff6692"
  , "#b6e880"
  , "#ff97ff"
  , "#fecb52"
  ]

colorsWhooie :: [String]
colorsWhooie =
  [ "#1f77b4" -- blue
  , "#d95319" -- auburn
  , "#edb120" -- canary
  , "#7e2f8e" -- purple
  , "#46add9" -- cyan
  , "#ff7f0e" -- tangerine
  , "#3d786e" -- dark seafoam
  , "#505050" -- gray
  , "#a2142f" -- burgundy
  , "#bf7878" -- dark rose
  ]

-- | Color set selector.
data ColorSet = Matlab | Python | Plotly | Whooie deriving (Show, Eq, Enum)

-- | Get the bare hex color strings for a particular color set.
--
-- Each color string comes with a leading @"#"@ character.
csetStrings :: ColorSet -> [String]
csetStrings Matlab = colorsMatlab
csetStrings Python = colorsPython
csetStrings Plotly = colorsPlotly
csetStrings Whooie = colorsWhooie

pyStrList :: [String] -> String
pyStrList strList = "[" ++ joined ++ "]"
  where joined = foldl (\acc s -> acc ++ "\"" ++ s ++ "\",") "" strList

-- | A point in a linear segmented color map, identified by a number and a hex
-- string (which includes a leading @"#"@).
type ColorPt = (Float, String)

cmapHotCold :: [ColorPt]
cmapHotCold =
  [ (0.000, "#101010")
  , (0.100, "#3f119d")
  , (0.350, "#3967d0")
  , (0.500, "#f0f0f0")
  , (0.625, "#f1b931")
  , (1.000, "#dd0000")
  ]

cmapFireIce :: [ColorPt]
cmapFireIce =
  [ (0.000, "#2165ac")
  , (0.167, "#68a9cf")
  , (0.333, "#d2e6f1")
  , (0.500, "#f8f8f8")
  , (0.667, "#ffdbc8")
  , (0.833, "#f08a62")
  , (1.000, "#b0172b")
  ]

cmapPowerade :: [ColorPt]
cmapPowerade =
  [ (0.000, "#542689")
  , (0.167, "#9a8dc2")
  , (0.333, "#d9daec")
  , (0.500, "#f8f8f8")
  , (0.667, "#d2e6f1")
  , (0.833, "#68a9cf")
  , (1.000, "#2165ac")
  ]

cmapFloral :: [ColorPt]
cmapFloral =
  [ (0.000, "#35c9a5")
  , (0.167, "#5cbea7")
  , (0.333, "#80b4a8")
  , (0.500, "#a8a8a8")
  , (0.667, "#c2a1a8")
  , (0.833, "#e099a9")
  , (1.000, "#fd8fa8")
  ]

cmapBlueHot :: [ColorPt]
cmapBlueHot =
  [ (0.000, "#000000")
  , (0.450, "#3b4568")
  , (0.600, "#586186")
  , (0.700, "#939cc4")
  , (1.000, "#f8f8f8")
  ]

cmapCyborg :: [ColorPt]
cmapCyborg =
  [ (0.000, "#101010")
  , (0.100, "#3967d0")
  , (1.000, "#dd0000")
  ]

cmapSport :: [ColorPt]
cmapSport =
  [ (0.000, "#0c5ab3")
  , (0.125, "#0099e6")
  , (0.250, "#23acf7")
  , (0.500, "#9b74be")
  , (0.750, "#fd6810")
  , (0.875, "#e62600")
  , (1.000, "#b30003")
  ]

cmapVibrant :: [ColorPt]
cmapVibrant =
  [ (0.000, "#101010")
  , (0.050, "#012d5e")
  , (0.125, "#0039a7")
  , (0.250, "#1647cf")
  , (0.375, "#6646ff")
  , (0.500, "#bc27ff")
  , (0.600, "#dc47af")
  , (0.800, "#f57548")
  , (0.900, "#f19e00")
  , (0.950, "#fbb800")
  , (1.000, "#fec800")
  ]

cmapArtsy :: [ColorPt]
cmapArtsy =
  [ (0.000, "#1f0109")
  , (0.034, "#1f0110")
  , (0.069, "#230211")
  , (0.103, "#250816")
  , (0.138, "#270b1b")
  , (0.172, "#250f1d")
  , (0.207, "#251521")
  , (0.241, "#251a25")
  , (0.276, "#2c1b28")
  , (0.310, "#271d2b")
  , (0.345, "#24202d")
  , (0.379, "#232632")
  , (0.414, "#212d32")
  , (0.448, "#1e343c")
  , (0.483, "#173e44")
  , (0.517, "#17464a")
  , (0.552, "#104a49")
  , (0.586, "#0e5553")
  , (0.621, "#00635f")
  , (0.655, "#007065")
  , (0.690, "#007a6d")
  , (0.724, "#0e8476")
  , (0.759, "#1c8c7d")
  , (0.793, "#219581")
  , (0.828, "#2f9f8a")
  , (0.862, "#49a890")
  , (0.897, "#60b89d")
  , (0.931, "#7ec8a9")
  , (0.966, "#9ad6b4")
  , (1.000, "#bce6bf")
  ]

cmapPix :: [ColorPt]
cmapPix =
  [ (0.000, "#0d2b45")
  , (0.143, "#16334d")
  , (0.286, "#544e68")
  , (0.429, "#8d697a")
  , (0.571, "#d08159")
  , (0.714, "#ffaa5e")
  , (0.857, "#ffd4a3")
  , (1.000, "#ffecd6")
  ]

cmapSunset :: [ColorPt]
cmapSunset =
  [ (0.000, "#0d0887")
  , (0.111, "#46039f")
  , (0.222, "#7201a8")
  , (0.333, "#9c179e")
  , (0.444, "#bd3786")
  , (0.555, "#d8576b")
  , (0.666, "#ed7953")
  , (0.777, "#fb9f3a")
  , (0.888, "#fdca26")
  , (1.000, "#f0f921")
  ]

cmapTopography :: [ColorPt]
cmapTopography =
  [ (0.000, "#173363")
  , (0.125, "#1d417f")
  , (0.250, "#3266a7")
  , (0.375, "#4194b8")
  , (0.500, "#63bcc2")
  , (0.625, "#9bd8c2")
  , (0.750, "#d2edc7")
  , (0.875, "#f3fad8")
  , (1.000, "#ffffff")
  ]

hexCharInt :: Char -> Int
hexCharInt '0' = 0
hexCharInt '1' = 1
hexCharInt '2' = 2
hexCharInt '3' = 3
hexCharInt '4' = 4
hexCharInt '5' = 5
hexCharInt '6' = 6
hexCharInt '7' = 7
hexCharInt '8' = 8
hexCharInt '9' = 9
hexCharInt 'a' = 10
hexCharInt 'A' = 10
hexCharInt 'b' = 11
hexCharInt 'B' = 11
hexCharInt 'c' = 12
hexCharInt 'C' = 12
hexCharInt 'd' = 13
hexCharInt 'D' = 13
hexCharInt 'e' = 14
hexCharInt 'E' = 14
hexCharInt 'f' = 15
hexCharInt 'F' = 15
hexCharInt  _  = 0

-- assumes the input is two characters
hexToFloat :: String -> Float
hexToFloat [a, b] = fromIntegral $ 16 * (hexCharInt a) + (hexCharInt b)
hexToFloat _      = 0.0

-- | A triple of numbers in the range @[0, 1]@ identifying a color in RGB
-- format.
type RgbFloat = (Float, Float, Float)

-- assumes seven-character hex string (with leading "#")
hexToRgb :: String -> RgbFloat
hexToRgb ['#', r0, r1, g0, g1, b0, b1] = (r, g, b)
  where r = hexToFloat [r0, r1]
        g = hexToFloat [g0, g1]
        b = hexToFloat [b0, b1]
hexToRgb _                             = (0.0, 0.0, 0.0)

colorInterp :: ColorPt -> ColorPt -> Float -> RgbFloat
colorInterp (a, ca) (b, cb) x = (r', g', b')
  where (ra, ga, ba) = hexToRgb ca
        (rb, gb, bb) = hexToRgb cb
        d = b - a
        dx = x - a
        r' = ra + (rb - ra) / d * dx
        g' = ga + (gb - ga) / d * dx
        b' = ba + (bb - ba) / d * dx

findBounding :: [ColorPt] -> Float -> Maybe (ColorPt, ColorPt)
findBounding []       _ = Nothing
findBounding (_ : []) _ = Nothing
findBounding (h : t)  x =
  find (\((a, _), (b, _)) -> a <= x && x <= b) (zip (h : t) t)

-- | Color map selector.
data ColorMap
  = HotCold
  | FireIce
  | Powerade
  | Floral
  | BlueHot
  | Cyborg
  | Sport
  | Vibrant
  | Artsy
  | Pix
  | Sunset
  | Topography
  deriving (Show, Eq, Enum)

allCmaps :: [ColorMap]
allCmaps = map toEnum [0..11]

-- | Get the name of a 'ColorMap' registered with matplotlib.
cmapName :: ColorMap -> String
cmapName HotCold    = "\"hot-cold\""
cmapName FireIce    = "\"fire-ice\""
cmapName Powerade   = "\"powerade\""
cmapName Floral     = "\"floral\""
cmapName BlueHot    = "\"blue-hot\""
cmapName Cyborg     = "\"cyborg\""
cmapName Sport      = "\"sport\""
cmapName Vibrant    = "\"vibrant\""
cmapName Artsy      = "\"artsy\""
cmapName Pix        = "\"pix\""
cmapName Sunset     = "\"sunset\""
cmapName Topography = "\"topography\""

-- | Get the bare thresholds and hex color strings for a particular color map.
--
-- Each color string comes with a leading @"#"@ character.
cmapPts :: ColorMap -> [ColorPt]
cmapPts HotCold    = cmapHotCold
cmapPts FireIce    = cmapFireIce
cmapPts Powerade   = cmapPowerade
cmapPts Floral     = cmapFloral
cmapPts BlueHot    = cmapBlueHot
cmapPts Cyborg     = cmapCyborg
cmapPts Sport      = cmapSport
cmapPts Vibrant    = cmapVibrant
cmapPts Artsy      = cmapArtsy
cmapPts Pix        = cmapPix
cmapPts Sunset     = cmapSunset
cmapPts Topography = cmapTopography

-- assumes the colormap is defined over [0, 1]
doCmapf :: [ColorPt] -> Float -> RgbFloat
doCmapf cmap x =
  if x <= 0.0
    then hexToRgb $ snd $ head cmap
  else if x >= 1.0
    then hexToRgb $ snd $ last cmap
  else
    case findBounding cmap x of
      Just (a, b) -> colorInterp a b x
      Nothing     -> error "impossible"

-- | Convert a number in the @[0, 1]@ range to a color under a given 'ColorMap'.
cmapf :: ColorMap -> Float -> RgbFloat
cmapf cmap x = doCmapf (cmapPts cmap) x

-- | Like 'cmapf', but immediately render the color tuple to a
-- matplotlib-compatible string.
cmapfS :: ColorMap -> Float -> String
cmapfS cmap x = show $ cmapf cmap x

pyLinSegCmap :: String -> [ColorPt] -> String
pyLinSegCmap name cmap =
  "mcolors.LinearSegmentedColormap.from_list(" ++ name ++ ", " ++ joined ++ ")"
    where showTuple (x, c) = "(" ++ (show x) ++ "," ++ "\"" ++ c ++ "\")"
          items = foldl (\acc xc -> acc ++ (showTuple xc) ++ ",") "" cmap
          joined = "[" ++ items ++ "]"

pyRegisterCmap :: Matplotlib -> ColorMap -> Matplotlib
pyRegisterCmap plot cmap =
  plot # ("\nmatplotlib.colormaps.register(" ++ linsegcmap ++ ")")
    where linsegcmap = pyLinSegCmap (cmapName cmap) (cmapPts cmap)

-- | An empty plot with a bunch of @rcParams@ and color settings.
defplot :: Matplotlib
defplot =
  (foldl pyRegisterCmap mp allCmaps)
  # ("\ncolors = " ++ (pyStrList $ colorsWhooie))
  # "\nfrom cycler import cycler"
  % setParameter "axes.grid" True
  % setParameter "axes.grid.which" "\"both\""
  % setParameter "axes.linewidth" (0.65 :: Double)
  % setParameter "axes.prop_cycle" "cycler(color=colors)"
  % setParameter "axes.titlesize" "\"medium\""
  % setParameter "errorbar.capsize" (1.25 :: Double)
  % setParameter "figure.dpi" (500.0 :: Double)
  % setParameter "figure.figsize" ([3.375, 2.225] :: [Double])
  % setParameter "figure.labelsize" "\"medium\""
  % setParameter "font.size" (8.0 :: Double)
  % setParameter "grid.color" "\"#d8d8d8\""
  % setParameter "grid.linewidth" (0.5 :: Double)
  % setParameter "image.cmap" "\"jet\""
  % setParameter "image.composite_image" False
  % setParameter "legend.borderaxespad" (0.25 :: Double)
  % setParameter "legend.borderpad" (0.2 :: Double)
  % setParameter "legend.fancybox" False
  % setParameter "legend.fontsize" "\"x-small\""
  % setParameter "legend.framealpha" (0.8 :: Double)
  % setParameter "legend.handlelength" (1.2 :: Double)
  % setParameter "legend.handletextpad" (0.4 :: Double)
  % setParameter "legend.labelspacing" (0.25 :: Double)
  % setParameter "lines.linewidth" (0.8 :: Double)
  % setParameter "lines.markeredgewidth" (0.8 :: Double)
  % setParameter "lines.markerfacecolor" "\"white\""
  % setParameter "lines.markersize" (2.0 :: Double)
  % setParameter "markers.fillstyle" "\"full\""
  % setParameter "savefig.bbox" "\"tight\""
  % setParameter "savefig.pad_inches" (0.05 :: Double)
  % setParameter "xtick.direction" "\"in\""
  % setParameter "xtick.major.size" (2.0 :: Double)
  % setParameter "xtick.minor.size" (1.5 :: Double)
  % setParameter "ytick.direction" "\"in\""
  % setParameter "ytick.major.size" (2.0 :: Double)
  % setParameter "ytick.minor.size" (1.5 :: Double)
  # "\nplot.close(fig)"
  # "\nmatplotlib.use(\"QtAgg\")"
  # "\nfig, ax = plot.subplots()"

