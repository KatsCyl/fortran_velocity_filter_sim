MODULE particleDef
  INTEGER, PARAMETER :: real_kind = selected_real_kind(30, 100)
  type particle
    INTEGER :: id
    REAL(real_kind) :: m, q ! Mass in atomic mass units, charge in elemental charges
    REAL(real_kind), DIMENSION(3) :: pos = 1, vel, acc = 0 ! Position in meters, velocity in m/s, acceleration in m/s^2
  end type particle
  type resParticle
    type (particle) :: part
    LOGICAL :: succ
  end type resParticle
  REAL :: e_to_coulomb = 1.602176565E-19
  REAL :: u_to_kg = 1.660538921E-27
END MODULE particleDef
