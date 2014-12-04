MODULE IO
  USE particleDef
  IMPLICIT NONE
  PRIVATE

  CHARACTER(len=20) :: filename

  PUBLIC :: init_all
  PUBLIC :: write_file
  contains
    SUBROUTINE read_file(p_list, *)
      IMPLICIT NONE
      type (particle), ALLOCATABLE, DIMENSION(:) :: p_list

      type (particle) :: temp
      INTEGER :: n, tempid, stat, i
      REAL :: tempvx, tempvy, tempvz, tempmass, tempq

      100 FORMAT(I3, 5F10.0)

      OPEN(UNIT = 9, FILE=filename, IOSTAT=stat, STATUS='OLD', ACTION='READ')

      IF(stat /= 0) THEN
        WRITE(*,*) "Error opening file"
        RETURN 1
      END IF

      READ(9,'(I3)', IOSTAT = stat) n
      IF(stat /= 0 .OR. n <= 0) THEN
        WRITE(*,*) "Error on reading amount of particles, see example file"
        CLOSE(UNIT=9)
        RETURN 1
      END IF
      
      WRITE(*,*) "Number of particles expected: ", n
            
      IF(ALLOCATED(p_list)) THEN
        DEALLOCATE(p_list)
      END IF
      
      ALLOCATE(p_list(n))
      DO i=1, n
        READ(9, 100, IOSTAT=stat) tempid, tempmass, tempq, tempvx, tempvy, tempvz
        IF(stat /= 0 .OR. tempmass <= 0) THEN
          WRITE(*,*) "Error on particle on line ", i+1,", see example file"
          DEALLOCATE(p_list)
          CLOSE(UNIT=9)
          RETURN 1
        END IF
        temp%id = tempid
        temp%m = tempmass
        temp%q = tempq
        temp%vel(1) = tempvx
        temp%vel(2) = tempvy
        temp%vel(3) = tempvz
        p_list(i) = temp
      END DO
      CLOSE(UNIT=9)
    END SUBROUTINE read_file
    SUBROUTINE write_file(p_list, *)
      IMPLICIT NONE
      type (particle), DIMENSION(:), ALLOCATABLE, INTENT(IN) :: p_list

      INTEGER :: stat, n, i

      90 FORMAT(1X, "This is the output of the program, syntax is:", /, &
                1X, "<number of passed particles>", /, &
                1X, "<id> , <mass> , <charge> , <vx> , <vy> , <vz> , <px> , <py> , <pz>", //)

      100 FORMAT(I3, 8(" , ", F10.0), /)

      IF(.NOT. ALLOCATED(p_list)) THEN
        WRITE(*,*) "Error: Passed particle list not allocated in output writing!"
        RETURN 1
      END IF

      OPEN(UNIT = 10, FILE="output.dat", STATUS='REPLACE', ACTION='WRITE', IOSTAT=stat)

      IF(stat /= 0) THEN
        WRITE(*,*) "Error on creating output file, error code: ", stat
        RETURN 1
      END IF

      n = SIZE(p_list)

      WRITE(10, 90)
      
      WRITE(10, '(I3, /)') n

      DO i=1, n
        WRITE(10, 100, IOSTAT = stat) p_list(i)%id, p_list(i)%m, p_list(i)%q &
                                       , p_list(i)%vel(1), p_list(i)%vel(2), p_list(i)%vel(3) &
                                        , p_list(i)%pos(1), p_list(i)%pos(2), p_list(i)%pos(3)
        IF(stat /= 0) THEN
          WRITE(*,*) "Write of particle number: ", i, " failed."
          CLOSE(UNIT = 10)
          RETURN 1
        END IF
      END DO
      CLOSE(UNIT=10)
    END SUBROUTINE write_file


    SUBROUTINE parse_real(n, target, *)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n
      REAL, INTENT(OUT) :: target

      CHARACTER(len=20) :: argument
      REAL :: temp
      INTEGER :: len, stat, readstat

      110 FORMAT(1X, "Usage: <program name> dt bx by bz ex ey ez filename", //)
      120 FORMAT(1X, A, I2, A)
      CALL GET_COMMAND_ARGUMENT(n, argument, len, stat)

      IF(n == 1) THEN
        IF(TRIM(argument) == "-help") THEN
          WRITE(*, 110)
          RETURN 1
        END IF
      END IF

      READ(argument, *, IOSTAT=readstat) temp

      IF(stat == 0 .AND. readstat == 0) THEN
        target = temp
      ELSE
        WRITE(*, 120) "Parse error on ", n,":th cmd argument" 
        RETURN 1
      END IF
    END SUBROUTINE parse_real
    SUBROUTINE parse_string(n, target, *)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n
      CHARACTER(len=20), INTENT(OUT) :: target

      INTEGER :: len, stat
      120 FORMAT(1X, A, I2, A)
      CALL GET_COMMAND_ARGUMENT(n, target, len, stat)
      
      IF(len > 20 .OR. stat /= 0) THEN
        WRITE(*,120) "Parse error on", n,"th cdm argument, is filename too long? Max 20"
        RETURN 1
      END IF
    END SUBROUTINE parse_string
    SUBROUTINE get_cmd_args(dt, b, e, *)
      REAL, INTENT(OUT) :: dt
      REAL, INTENT(OUT), DIMENSION(3) :: b, e

      CALL parse_real(1, dt, *555)
      CALL parse_real(2, b(1), *555)
      CALL parse_real(3, b(2), *555)
      CALL parse_real(4, b(3), *555)
      CALL parse_real(5, e(1), *555)
      CALL parse_real(6, e(2), *555)
      CALL parse_real(7, e(3), *555)
      CALL parse_string(8, filename, *555)
      RETURN
      555 RETURN 1
    END SUBROUTINE
    SUBROUTINE init_all(p_list, dt, b, e, *)
      type (particle), ALLOCATABLE, DIMENSION(:) :: p_list
      REAL, INTENT(OUT) :: dt
      REAL, INTENT(OUT), DIMENSION(3) :: b, e
      
      CALL get_cmd_args(dt, b, e, *100)
      CALL read_file(p_list, *100)
      RETURN
      100 RETURN 1

    END SUBROUTINE

END MODULE IO
