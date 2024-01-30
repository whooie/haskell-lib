-- | Quantities related to Rabi oscillations.
module Whooie.Phys.Rabi
  (
    freq_to_saturation,
    saturation_to_freq,
    saturation_intensity,
  ) where

import Whooie.Phys.Consts (h, c)

-- | @freq_to_saturation linewidth rabi_freq@ returns the saturation parameter
-- needed for a particular Rabi frequency.
freq_to_saturation :: Float -> Float -> Float
freq_to_saturation linewidth rabi_freq = 2.0 * (rabi_freq / linewidth) ** 2.0

-- | @saturation_to_freq linewidth saturation@ returns the Rabi frequency
-- produced by a particular saturation parameter.
saturation_to_freq :: Float -> Float -> Float
saturation_to_freq linewidth saturation = linewidth * (sqrt $ saturation / 2.0)

-- | @saturation_intensity wavelength linewidth@ returns the saturation
-- intensity of a transition.
saturation_intensity :: Float -> Float -> Float
saturation_intensity wavelength linewidth =
  pi / 2.0 * h * c / wavelength ** 3.0 * linewidth

