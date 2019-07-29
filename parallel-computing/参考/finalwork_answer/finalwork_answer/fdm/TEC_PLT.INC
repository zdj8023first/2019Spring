
!----------------------------------------------------------------

!  # include file use to put data to read by tecplot 8.0d0
!  # last chnage date: nov. 27, 2003
!  # all right reserved by reui-kuo, lin
!========================================================
! 
        SUBROUTINE WRITE_TEC_NC(NX, NY, NZ, X, Y, Z, U, V, W, P, T, AMP)
!
!  # assign [x], [y], [fi] to a file which be named by its iteration
!    time.
!  # the file is formated is tecplot
!-----------------------------------
!
!  x: the x position of fi.
!  y: the y position of fi.
!  fi: the value of p.
!  numa:
!  numb:
!  m:.....
!---------------------------------
! 
        INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(P=15) 
        CHARACTER(30):: FILENAME
        CHARACTER(2) :: CHAR
        CHARACTER(4) :: NAME   
        INTEGER, INTENT(IN) :: NX, NY, NZ
        REAL(KIND=DP), INTENT(IN), DIMENSION(:,:,:) :: X, Y, Z
        REAL(KIND=DP), INTENT(IN), DIMENSION(:,:,:) :: U, V, W, P, T
        REAL(KIND=DP), INTENT(IN) :: AMP
        INTEGER :: I, J, K

!   [2] start assign....

        WRITE(CHAR,'(I2.2)') INT(AMP)
        WRITE(NAME,'(I4.4)') NX

        FILENAME = '3DNC.RA=10^'//CHAR//'.'//NAME//'.TEC'

        OPEN(UNIT=11, FILE=FILENAME, STATUS= 'UNKNOWN' )
        WRITE(11,*) 'TITLE = "NATURAL CONVECTION" '
        WRITE(11,*) 'VARIABLES = "X" "Y" "Z" "U" "V" "W" "P" "T"'
        WRITE(11,*) 'ZONE T= NUMERICAL, I=', NX, ', J=', NY, ', K=', NZ, ', F= POINT'

        DO K = 1, NZ
        DO J = 1, NY
        DO I = 1, NX
           WRITE(11,'(8E15.6)') X(I,J,K), Y(I,J,K), Z(I,J,K), U(I,J,K), V(I,J,K), &
                                W(I,J,K), P(I,J,K), T(I,J,K)
        END DO
        END DO
        END DO

        CLOSE(UNIT=11)

        END SUBROUTINE WRITE_TEC_NC

!---------------------------------



        SUBROUTINE WRITE_TEC_NSEQ(NX, NY, NZ, X, Y, Z, U, V, W, P, AMP)
!
!  # assign [x], [y], [fi] to a file which be named by its iteration
!    time.
!  # the file is formated is tecplot
!-----------------------------------
!
!  x: the x position of fi.
!  y: the y position of fi.
!  fi: the value of p.
!  numa:
!  numb:
!  m:.....
!---------------------------------
! 
        INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(P=15) 
        CHARACTER(30):: FILENAME
        CHARACTER(2) :: CHAR
        CHARACTER(4) :: NAME   
        INTEGER, INTENT(IN) :: NX, NY, NZ
        REAL(KIND=DP), INTENT(IN), DIMENSION(:,:,:) :: X, Y, Z
        REAL(KIND=DP), INTENT(IN), DIMENSION(:,:,:) :: U, V, W, P
        REAL(KIND=DP), INTENT(IN) :: AMP
        INTEGER :: I, J, K

!   [2] start assign....

        WRITE(CHAR,'(I2.2)') INT(AMP)
        WRITE(NAME,'(I4.4)') NX

        FILENAME = '3DNSEQ.RE=10^'//CHAR//'.'//NAME//'.TEC'

        OPEN(UNIT=11, FILE=FILENAME, STATUS= 'UNKNOWN' )
        WRITE(11,*) 'TITLE = "DRIVEN CAVITY" '
        WRITE(11,*) 'VARIABLES = "X" "Y" "Z" "U" "V" "W" "P" '
        WRITE(11,*) 'ZONE T= NUMERICAL, I=', NX, ', J=', NY, ', K=', NZ, ', F= POINT'

        DO K = 1, NZ
        DO J = 1, NY
        DO I = 1, NX
           WRITE(11,'(7E15.6)') X(I,J,K), Y(I,J,K), Z(I,J,K), U(I,J,K), V(I,J,K), &
                                W(I,J,K), P(I,J,K)
        END DO
        END DO
        END DO

        CLOSE(UNIT=11)

        END SUBROUTINE WRITE_TEC_NSEQ

!---------------------------------