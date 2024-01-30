-- | Physical constants.
module Whooie.Phys.Consts
  (
    h,
    hbar,
    c,
    n_a,
    k_b,
    e0,
    u0,
    g_g,
    g,
    e,
    me,
    mp,
    mu,
    rinf,
    alpha,
    r,
    s_b,
    a0,
    u_b,
    e_h,
  )where

-- | Planck constant (kg m^2 s^-1)
h :: Float
h = 6.62607015e-34
-- +/- 0 (exact)

-- | reduced Planck constant (kg m^2 s^-1)
hbar :: Float
hbar = 1.05457180013911271e-34
-- +/- 0 (exact)

-- | speed of light in vacuum (m s^-1)
c :: Float
c = 2.99792458e8
-- +/- 0 (exact)

-- | Avogadro's number
n_a :: Float
n_a = 6.02214076e23
-- +/- 0 (exact)

-- | Boltzmann's constant (J K^-1)
k_b :: Float
k_b = 1.380649e-23
-- +/- 0 (exact)

-- | electric permittivity in vacuum (F m^-1)
e0 :: Float
e0 = 8.8541878128e-12
-- +/- 0.0000000013e-12

-- | magnetic permeability in vacuum (N A^-2)
u0 :: Float
u0 = 1.25663706212e-6
-- +/- 0.00000000019e-6

-- | Newtonian gravitational constant (m^3 kg^-1 s^-2)
g_g :: Float
g_g = 6.67430e-11
-- +/- 0.00015e-11

-- | gravitational acceleration near Earth's surface (m s^-2)
g :: Float
g = 9.80665
-- +/- 0 (exact)

-- | elementary charge (C)
e :: Float
e = 1.602176634e-19
-- +/- 0 (exact)

-- | electron mass (kg)
me :: Float
me = 9.1093837015e-31
-- +/- 0.0000000028e-31

-- | proton mass (kg)
mp :: Float
mp = 1.67262192369e-27
-- +/- 0.00000000051e-27

-- | unified atomic mass unit (kg)
mu :: Float
mu = 1.66053906660e-27
-- +/- 0.00000000050e-27

-- | Rydberg constant for an infinite-mass nucleus (m^-1)
rinf :: Float
rinf = 10973731.568160
-- +/- 0.000021

-- | fine structure constant
alpha :: Float
alpha = 7.2973525693e-3
-- +/- 0.0000000011e-3

-- | molar gas constant
r :: Float
r = 8.314462618
-- +/- 0 (exact)

-- | Stefan-Boltzmann constant (W m^-2 K^-4)
s_b :: Float
s_b = 5.67036681608326799e-08
-- +/- 0 (exact)

-- | Bohr radius (m)
a0 :: Float
a0 = 5.29177210903e-11
-- +/- 0.00000000080e-11

-- | Bohr magneton (J T^-1)
u_b :: Float
u_b = 9.2740100783e-24
-- +/- 0.0000000028e-24

-- | Hartree energy (J) = 2\*Rinf\*h\*c
e_h :: Float
e_h = 4.3597447222071e-18
-- +/- 0.0000000000085e-18

