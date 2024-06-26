-- | Quantities related to Rabi oscillations.
module Whooie.Phys.Rabi
  ( freqToSaturation
  , saturationToFreq
  , saturationIntensity
  ) where

import Whooie.Phys.Consts (h, c)

-- | @freqToSaturation linewidth rabiFreq@ returns the saturation parameter
-- needed for a particular Rabi frequency.
freqToSaturation :: Float -> Float -> Float
freqToSaturation linewidth rabiFreq = 2.0 * (rabiFreq / linewidth) ** 2.0

-- | @saturationToFreq linewidth saturation@ returns the Rabi frequency
-- produced by a particular saturation parameter.
saturationToFreq :: Float -> Float -> Float
saturationToFreq linewidth saturation = linewidth * (sqrt $ saturation / 2.0)

-- | @saturationIntensity wavelength linewidth@ returns the saturation
-- intensity of a transition.
saturationIntensity :: Float -> Float -> Float
saturationIntensity wavelength linewidth =
  pi / 2.0 * h * c / wavelength ** 3.0 * linewidth

