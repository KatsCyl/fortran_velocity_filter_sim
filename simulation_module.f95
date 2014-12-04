MODULE simulator
  USE particledef
  USE io
  IMPLICIT NONE
  PRIVATE
  REAL, PARAMETER :: boxx = 0.019, boxy = 0.076, boxz = 0.019 ! Box dimensions in meters
  REAL, PARAMETER, DIMENSION(3) :: startpos = [boxx/2., 0., boxz/2.]

  REAL :: dt ! Time step in seconds
  REAL, DIMENSION(3) :: b, e ! Magnetic field (in Teslas), and electric field (in kV/m)
  type (particle), ALLOCATABLE, DIMENSION(:) :: particles ! Particles to be simulated
  type (particle), ALLOCATABLE, DIMENSION(:) :: p_particles ! Particles which passed the simulation
  
  contains
    SUBROUTINE init_env(*)
      IMPLICIT NONE
      CALL init_all(particles,  dt, b, e, *10)
      RETURN
      10 RETURN 1
    END SUBROUTINE init_env
   PURE  FUNCTION lorenz_force(p)
      IMPLICIT NONE
      REAL, DIMENSION(3) :: lorenz_force
      type (particle), INTENT(IN) :: p

      lorenz_force(1) = p%q * (e(1) + (p%vel(2)*b(3) - p%vel(3)*b(2))) 
      lorenz_force(2) = p%q * (e(2) + (p%vel(3)*b(1) - p%vel(1)*b(3)))
      lorenz_force(3) = p%q * (e(3) + (p%vel(1)*b(2) - p%vel(2)*b(1)))
    END FUNCTION lorenz_force
    PURE SUBROUTINE verlet(p)
      IMPLICIT NONE
      type (particle), INTENT(INOUT) :: p

      REAL, DIMENSION(3) :: tempacc
   
      p%pos = p%pos + p%vel*dt + 1/2 * p%acc*dt**2
      
      tempacc = p%acc
      p%acc = lorenz_force(p)/p%m

      p%vel = p%vel + 1/2 * (tempacc + p%acc)*dt
    END SUBROUTINE verlet
    SUBROUTINE simulate()
      IMPLICIT NONE

     
    END SUBROUTINE simulate()

      
END MODULE simulator
