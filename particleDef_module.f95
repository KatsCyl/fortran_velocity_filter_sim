MODULE particleDef
  type particle
    INTEGER :: id
    REAL :: m, q ! Mass in kg, charge in elemental charges
    REAL, DIMENSION(3) :: pos, vel ! Position in meters, velocity in m/s
  end type particle
  REAL :: e_to_coulomb = 1.602176565E-19
END MODULE particleDef
