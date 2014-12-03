MODULE particleDef
  type particle
    INTEGER :: id
    REAL :: m, q
    REAL, DIMENSION(3) :: pos, vel
  end type particle
END MODULE particleDef
