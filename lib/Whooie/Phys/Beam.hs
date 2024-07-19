-- | Basic properties of a Gaussian beam.
module Whooie.Phys.Beam
  ( rayleigh
  , radius
  , peakIntensity
  , power
  , radialWeight
  , axialWeight
  , dist
  ) where

-- | @rayleigh wavelength waistRadius@ returns the Rayleigh range.
rayleigh :: Double -> Double -> Double
rayleigh wavelength waistRadius = pi * waistRadius ** 2.0 / wavelength

-- | @radius wavelength waistRadius z@ returns the local 1/e^2 radius of the
-- beam at axial position @z@, where @z = 0@ corresponds to the waist of the
-- beam.
radius :: Double -> Double -> Double -> Double
radius wavelength waistRadius z =
  waistRadius * (sqrt $ 1.0 + ((z / zR) ** 2.0))
    where zR = rayleigh wavelength waistRadius

-- | @peakIntensity radius power@ returns the maximum intensity of the beam
-- over an axial cross section of 1/e^2 radius @radius@.
peakIntensity :: Double -> Double -> Double
peakIntensity radius power = 2.0 * power / (pi * radius ** 2.0)

-- | @power radius peakIntensity@ returns the total power through a plane at
-- fixed axial position for a given peak intensity over the plane.
power :: Double -> Double -> Double
power radius peakIntensity = pi * radius ** 2.0 * peakIntensity / 2.0

-- | @radialWeight radius r@ returns the zero-to-one weighting factor on the
-- local intensity due to only the radial (Gaussian) power distribution in the
-- beam.
radialWeight :: Double -> Double -> Double
radialWeight radius r = exp $ (-2.0) * (r / radius) ** 2.0

-- | @axialWeight rayl z@ returns the zero-to-one weighting factor on the local
-- intensity due to to only the axial (Lorentzian) power distribution in the
-- beam.
axialWeight :: Double -> Double -> Double
axialWeight rayl z = 1.0 / (1.0 + (z / rayl) ** 2.0)

-- | @dist wavelength waistRadius z r@ is the local zero-to-one power
-- distribution in the beam, normalized to a maximum value of 1.
dist :: Double -> Double -> Double -> Double -> Double
dist wavelength waistRadius z r = wRad * wAx
  where zR = rayleigh wavelength waistRadius
        rad = radius wavelength waistRadius z
        wRad = radialWeight rad r
        wAx = axialWeight zR z

