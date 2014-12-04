PROGRAM tester
  USE particleDef
  USE io
  IMPLICIT NONE
  type (particle), ALLOCATABLE, DIMENSION(:) :: p_list
  REAL :: dt = -1
  REAL, DIMENSION(3) :: e = -1, b = -1

  CALL init_all(p_list, dt, b, e, *100)

  WRITE(*,*) dt
  WRITE(*,*) e
  WRITE(*,*) b

  WRITE(*,*) "Particle 1 ID:", p_list(1)%id, "mass: ", p_list(1)%m

  CALL write_file(p_list, *100)  

  RETURN
  100 WRITE(*,*) "FAIL"
  110 FORMAT(F10.5)

ENDPROGRAM tester
