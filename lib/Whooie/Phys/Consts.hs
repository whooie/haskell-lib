-- | Physical constants.
module Whooie.Phys.Consts
  ( h
  , hbar
  , c
  , nA
  , kB
  , e0
  , u0
  , gG
  , g
  , e
  , me
  , mp
  , mu
  , rinf
  , alpha
  , r
  , sB
  , a0
  , uB
  , eH
  ) where

-- | Planck constant (kg m^2 s^-1)
h :: Double
h = 6.62607015e-34
-- +/- 0 (exact)

-- | reduced Planck constant (kg m^2 s^-1)
hbar :: Double
hbar = 1.05457180013911271e-34
-- +/- 0 (exact)

-- | speed of light in vacuum (m s^-1)
c :: Double
c = 2.99792458e8
-- +/- 0 (exact)

-- | Avogadro's number
nA :: Double
nA = 6.02214076e23
-- +/- 0 (exact)

-- | Boltzmann's constant (J K^-1)
kB :: Double
kB = 1.380649e-23
-- +/- 0 (exact)

-- | electric permittivity in vacuum (F m^-1)
e0 :: Double
e0 = 8.8541878128e-12
-- +/- 0.0000000013e-12

-- | magnetic permeability in vacuum (N A^-2)
u0 :: Double
u0 = 1.25663706212e-6
-- +/- 0.00000000019e-6

-- | Newtonian gravitational constant (m^3 kg^-1 s^-2)
gG :: Double
gG = 6.67430e-11
-- +/- 0.00015e-11

-- | gravitational acceleration near Earth's surface (m s^-2)
g :: Double
g = 9.80665
-- +/- 0 (exact)

-- | elementary charge (C)
e :: Double
e = 1.602176634e-19
-- +/- 0 (exact)

-- | electron mass (kg)
me :: Double
me = 9.1093837015e-31
-- +/- 0.0000000028e-31

-- | proton mass (kg)
mp :: Double
mp = 1.67262192369e-27
-- +/- 0.00000000051e-27

-- | unified atomic mass unit (kg)
mu :: Double
mu = 1.66053906660e-27
-- +/- 0.00000000050e-27

-- | Rydberg constant for an infinite-mass nucleus (m^-1)
rinf :: Double
rinf = 10973731.568160
-- +/- 0.000021

-- | fine structure constant
alpha :: Double
alpha = 7.2973525693e-3
-- +/- 0.0000000011e-3

-- | molar gas constant
r :: Double
r = 8.314462618
-- +/- 0 (exact)

-- | Stefan-Boltzmann constant (W m^-2 K^-4)
sB :: Double
sB = 5.67036681608326799e-08
-- +/- 0 (exact)

-- | Bohr radius (m)
a0 :: Double
a0 = 5.29177210903e-11
-- +/- 0.00000000080e-11

-- | Bohr magneton (J T^-1)
uB :: Double
uB = 9.2740100783e-24
-- +/- 0.0000000028e-24

-- | Hartree energy (J) = 2\*Rinf\*h\*c
eH :: Double
eH = 4.3597447222071e-18
-- +/- 0.0000000000085e-18

