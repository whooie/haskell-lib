{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Basic properties of a Gaussian beam.
module Whooie.Phys.Beam
  (
    rayleigh,
    radius,
    peak_intensity,
    power,
    radial_weight,
    axial_weight,
    dist,
  ) where

-- | @rayleigh wavelength waist_radius@ returns the Rayleigh range.
rayleigh :: Float -> Float -> Float
rayleigh wavelength waist_radius = pi * waist_radius ** 2.0 / wavelength

-- | @radius wavelength waist_radius z@ returns the local 1/e^2 radius of the
-- beam at axial position @z@, where @z = 0@ corresponds to the waist of the
-- beam.
radius :: Float -> Float -> Float -> Float
radius wavelength waist_radius z =
  waist_radius * (sqrt $ 1.0 + ((z / zR) ** 2.0))
    where zR = rayleigh wavelength waist_radius

-- | @peak_intensity radius power@ returns the maximum intensity of the beam
-- over an axial cross section of 1/e^2 radius @radius@.
peak_intensity :: Float -> Float -> Float
peak_intensity radius power = 2.0 * power / (pi * radius ** 2.0)

-- | @power radius peak_intensity@ returns the total power through a plane at
-- fixed axial position for a given peak intensity over the plane.
power :: Float -> Float -> Float
power radius peak_intensity = pi * radius ** 2.0 * peak_intensity / 2.0

-- | @radial_weight radius r@ returns the zero-to-one weighting factor on the
-- local intensity due to only the radial (Gaussian) power distribution in the
-- beam.
radial_weight :: Float -> Float -> Float
radial_weight radius r = exp $ (-2.0) * (r / radius) ** 2.0

-- | @axial_weight rayl z@ returns the zero-to-one weighting factor on the local
-- intensity due to to only the axial (Lorentzian) power distribution in the
-- beam.
axial_weight :: Float -> Float -> Float
axial_weight rayl z = 1.0 / (1.0 + (z / rayl) ** 2.0)

-- | @dist wavelength waist_radius z r@ is the local zero-to-one power
-- distribution in the beam, normalized to a maximum value of 1.
dist :: Float -> Float -> Float -> Float -> Float
dist wavelength waist_radius z r = w_rad * w_ax
  where zR = rayleigh wavelength waist_radius
        rad = radius wavelength waist_radius z
        w_rad = radial_weight rad r
        w_ax = axial_weight zR z

