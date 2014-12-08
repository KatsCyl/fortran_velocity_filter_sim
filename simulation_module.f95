MODULE simulator
  USE particledef
  USE io
  IMPLICIT NONE
  PRIVATE
  REAL, PARAMETER :: boxx = 0.019, boxy = 0.076, boxz = 0.019 ! Box dimensions in meters
  REAL, PARAMETER, DIMENSION(3) :: startpos = [boxx/2., 0., boxz/2.]

  REAL(real_kind) :: dt ! Time step in seconds
  REAL(real_kind), DIMENSION(3) :: b, e ! Magnetic field (in Teslas), and electric field (in kV/m)
  type (particle), ALLOCATABLE, DIMENSION(:) :: particles ! Particles to be simulated
  type (particle), ALLOCATABLE, DIMENSION(:) :: p_particles ! Particles which passed the simulation

  PUBLIC :: run_sim
  
  contains
    SUBROUTINE init_env(*)
      IMPLICIT NONE
      CALL init_all(particles,  dt, b, e, *10)
      RETURN
      10 RETURN 1
    END SUBROUTINE init_env
   PURE FUNCTION lorenz_force(p)
      IMPLICIT NONE
      REAL(real_kind), DIMENSION(3) :: lorenz_force
      type (particle), INTENT(IN) :: p

      lorenz_force(1) = p%q * e_to_coulomb * (e(1) + (p%vel(2)*b(3) - p%vel(3)*b(2))) 
      lorenz_force(2) = p%q * e_to_coulomb * (e(2) + (p%vel(3)*b(1) - p%vel(1)*b(3)))
      lorenz_force(3) = p%q * e_to_coulomb * (e(3) + (p%vel(1)*b(2) - p%vel(2)*b(1)))
    END FUNCTION lorenz_force
    PURE SUBROUTINE verlet(p)
      IMPLICIT NONE
      type (particle), INTENT(INOUT) :: p

      REAL(real_kind), DIMENSION(3) :: tempacc

      p%pos = p%pos + p%vel*dt + 0.5 * p%acc*dt**2
      tempacc = p%acc
      p%acc = lorenz_force(p)/(p%m*u_to_kg)

      p%vel = p%vel + 0.5 * (tempacc + p%acc)*dt
    END SUBROUTINE verlet
    SUBROUTINE push_to_array(arr, val)
      IMPLICIT NONE
      type (particle), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: arr
      type (particle), INTENT(IN) :: val

      type (particle), DIMENSION(:), ALLOCATABLE :: temparr
      INTEGER :: i, arrsize

      IF(.NOT. ALLOCATED(arr)) THEN
        ALLOCATE(arr(1))
        arr(1) = val
      ELSE
        arrsize = SIZE(arr)
        ALLOCATE(temparr(arrsize + 1))
        DO i=1, arrsize
          temparr(i) = arr(i)
        END DO
        temparr(arrsize+1) = val
        DEALLOCATE(arr)
        ALLOCATE(arr(arrsize + 1))
        arr = temparr
        DEALLOCATE(temparr)
      END IF
    END SUBROUTINE push_to_array
    FUNCTION simulate_particle(p)
      IMPLICIT NONE
      type (particle), INTENT(IN) :: p
      type (resParticle) :: simulate_particle

      type (particle) :: temp
      temp = p
      temp%pos = startpos
      
      DO WHILE ((0 < temp%pos(1)) .AND. (boxx > temp%pos(1)) .AND. (0 < temp%pos(3)) .AND. (temp%pos(3) < boxz))
        CALL verlet(temp)
!        write(*,*) temp%pos
        IF (temp%pos(2) > boxy) THEN
          simulate_particle%part = temp
          simulate_particle%succ = .TRUE.
          RETURN
        END IF
      END DO
      
      simulate_particle%part = temp
      simulate_particle%succ = .FALSE.
    END FUNCTION simulate_particle
    SUBROUTINE run_sim(*)
      IMPLICIT NONE
      INTEGER :: i
      type (resParticle) :: temp

      CALL init_env(*10)
      
      IF(ALLOCATED(p_particles)) THEN
        DEALLOCATE(p_particles)
      END IF
      DO i=1, size(particles)
        temp = simulate_particle(particles(i))
        IF(temp%succ) THEN
          WRITE(*,*) "particle id: ", temp%part%id, "succeeded"
          CALL push_to_array(p_particles, temp%part)
        END IF
      END DO
      WRITE(*,*) ALLOCATED(p_particles)
      CALL write_file(p_particles, *10)

      RETURN
      10 RETURN 1
    END SUBROUTINE run_sim
      
END MODULE simulator
