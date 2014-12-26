PROGRAM tester
  use simulator
  IMPLICIT NONE
  CALL run_sim(*100)
  RETURN
  100 WRITE(*,*) "FAIL"
  110 FORMAT(F10.5)

ENDPROGRAM tester
