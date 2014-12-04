MODULE particleDef
  type particle
    INTEGER :: id
    REAL :: m, q ! Mass in kg, charge in elemental charges
    REAL, DIMENSION(3) :: pos = 1, vel, acc = 0 ! Position in meters, velocity in m/s, acceleration in m/s^2
  end type particle
  REAL :: e_to_coulomb = 1.602176565E-19
END MODULE particleDef
