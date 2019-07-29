

!------------------
!  3DNC.F90
!  CDS.INC
!  R_SOLVER.INC
!-------------------
        PROGRAM SOLVER_THREE_DIM_NAVIER_STOKES_EQUATION
!
!  SOLVE 3D NAVIER-STOKES EQUATIONS BY TVDRK2 DRP-DFC SCHEME
!
!---------------------------------------------------------
!  SOLVE POISSON EQUATION...(SOURRCE = 0.0D0 LAPLACE)
!
!    TXX + T YY = S
!------------------------------------
!  F: SOLUTION. (UPDATE..)
!  # THE BOUNDARY CONDITION OF U MUST BE PUT IN THE [U] MATRIX.
!  SOURCE: SOURCE TERRM.
!  DX, DY:  GRID SPACE IN X-AXIS, Y-AXIS.
!  NUMY, NU	MX: GRID POINT NUMBER IN X-AXIS, YAXIS.
!  WORK(N,23): A WORKING ARRAY USED TO SOLVE MATRIX.(USED BY STRONG IMPLICITE)
!  WORK2(M,4): USED TO PUT BOUNDARY..
!---------------------------
!  WORK2(I,2): UB(I).         WORK2(I,1): DB(I)
!  WORK2(I,3): LB(I).         WORK2(I,4): RB(I)
!-------------------------
!  WORK(M,M): BASE ARRAY. (WE PUT THE SYSTEM START AT A(1,1).)
!
!  NUM: NUMBER OF NUMY AMD NUMXUMN.
!      (1,1) (1,2) ........    (1,9)
!      (2,1) (2,2) .........   (2,9)
!        .     .   .........      .                =  A(UN_NUM,9)
!        .     .   .....          .
!        .     .   .......  ..    .
!      (UN_NUM,1) .....    .(UN_NUM, 9)
!----------------------------------------------
!         (1,10)
!         (2,10)
!            .      = B(UN_NUM, 1)
!            .
!            .
!      (UN_NUM, UN_NUM+1)
!
!----------------------------------
!
        IMPLICIT NONE
        INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(P=15)
        REAL(KIND=DP), PARAMETER :: PI = 3.141592653589793_DP
        CHARACTER(80) :: CHARNAME
        CHARACTER(30) :: FILENAME
        CHARACTER(4) :: NAME
        CHARACTER(2) :: CHAR

!  # SETTING THE BOUNDARY CONDITIONS.........

        INTEGER, DIMENSION(6) :: BTYPE_3D
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: X, Y, Z
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: U, V, W, P, T
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: U_OLD, V_OLD, W_OLD, P_OLD, T_OLD
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: U1, V1, W1
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: FU, FV, FW
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: US, VS, WS, PP
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: UU, VV, WW, TS
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, S
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: U_X, V_Y, W_Z, P_X, P_Y, P_Z
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:) :: RHS, DIV, DIV_X, DIV_Y, DIV_Z
        REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:) :: L_BC, R_BC, U_BC, D_BC, F_BC, B_BC
        REAL(KIND=DP) :: DX , DY , DZ
        REAL(KIND=DP) :: DDT , W_U , W_V , W_W , W_P
        REAL(KIND=DP) :: XSTART, YSTART, ZSTART, XEND, YEND, ZEND
        REAL(KIND=DP) :: L2_U , L2_V , L2_W , L2_P , L2_T , L2_DIV
        REAL(KIND=DP) :: DT, LA, PR, RA
        INTEGER :: N, M, Q, NODE_X, NODE_Y, NODE_Z
        INTEGER :: I_DIM_U_MAX(3), I_DIM_V_MAX(3), I_DIM_W_MAX(3)
        INTEGER :: I_DIM_U_MIN(3), I_DIM_V_MIN(3), I_DIM_W_MIN(3)
!  # VARIABLE USED BY ITERATION PROCESS

        INTEGER :: I, J, K
        INTEGER :: IVER, NON_T, ITER, II, II_ITER
        INTEGER :: S_ITER, METHOD, RK_ITER, RK_STAGE, RK_TEMP
        REAL(KIND=DP) :: TMP, AMP, TOLERANCE
        REAL(KIND=DP) :: U_MAX, V_MAX, W_MAX
        REAL(KIND=DP) :: U_MIN, V_MIN, W_MIN
        REAL(KIND=DP) :: CPU, CPU_T0, CPU_T1

!*************************
!***** PROGRAM BEGIN *****
!*************************

        CALL CPU_TIME(CPU_T0)

!*************************
!***** SET GRID SIZE *****
!*************************

        FILENAME = 'GRID.TXT'
        OPEN(UNIT=88, FILE=FILENAME, STATUS='UNKNOWN')
        READ(88,'(A80)') CHARNAME
        READ(88,'(A80)') CHARNAME
        READ(88,'(A80)') CHARNAME
        READ(88,*) NODE_X, RA, METHOD, RK_STAGE
        CLOSE(UNIT=88)

        AMP = LOG10(RA)

!***************************
!***** SET CAL. DOMAIN *****
!***************************

        XSTART = 0.0_DP
        YSTART = 0.0_DP
        ZSTART = 0.0_DP
        XEND   = 1.0_DP
        YEND   = 1.0_DP
        ZEND   = 1.0_DP

        NODE_Y = NODE_X*INT(YEND)
        NODE_Z = NODE_X

        N = NODE_X + 1
        M = NODE_Y + 1
        Q = NODE_Z + 1

!*****************************
!***** THE INDEX NUMBERS *****
!*****************************

        ITER    = 1
        II_ITER = 1

        PR = 0.71_DP
        DT = 1.0_DP / REAL(NODE_X,KIND=DP)

        WRITE(*,'(5X,A,I6,A,F15.2,A,F8.5)') 'GRID=', NODE_X, ', RA=', RA, ', DT=', DT
        WRITE(*,'(5X,3(A,I6),A,I10)')       'NX=', N, ', NY=', M, ', NZ=', Q, ', GRID=', N*M*Q
        WRITE(*,'(5X,A,I6,A,I6)')           'METHOD=', METHOD, ', RK STAGE=', RK_STAGE

!        WRITE(*,*) '***********************************'
!        WRITE(*,*) '*** SELECT TIME_MARCHING METHOD ***'
!        WRITE(*,*) '*** (1) EULER-FORWARD           ***'
!        WRITE(*,*) '*** (2)TVD RK2                  ***'
!        WRITE(*,*) '***********************************'
!        READ (*,*) METHOD

        IF(METHOD == 2) THEN
!           WRITE(*,*) 'INPUT THE STAGE:'
!           READ (*,*) RK_STAGE
           RK_TEMP = RK_STAGE  ! 4
        END IF

!************************************
!***** FIND THE ALLOCATE MATRIX *****
!************************************

        ALLOCATE(X(N,M,Q), Y(N,M,Q), Z(N,M,Q),                                           &
                 C_X(N,M,Q), C_Y(N,M,Q), C_Z(N,M,Q), R(N,M,Q), S(N,M,Q),                 &
                 K_X(N,M,Q), K_Y(N,M,Q), K_Z(N,M,Q),                                     &
!**************************
!***** MAIN VARIABLES *****
!**************************
                 U(N,M,Q), V(N,M,Q), W(N,M,Q), P(N,M,Q), T(N,M,Q),                       &
                 U_OLD(N,M,Q), V_OLD(N,M,Q), W_OLD(N,M,Q), P_OLD(N,M,Q), T_OLD(N,M,Q),   &
!**************************
!***** VARIABLE ERROR *****
!**************************
                 US(N,M,Q), VS(N,M,Q), WS(N,M,Q), TS(N,M,Q),                             &
                 UU(N,M,Q), VV(N,M,Q), WW(N,M,Q), PP(N,M,Q),                             &
                 U1(N,M,Q), V1(N,M,Q), W1(N,M,Q),                                        &
!********************************
!***** DIFFERENCE VARIABLES *****
!********************************
                 U_X(N,M,Q), V_Y(N,M,Q), W_Z(N,M,Q), P_X(N,M,Q), P_Y(N,M,Q), P_Z(N,M,Q), &
!***************************
!***** BOUNDARY MATRIX *****
!***************************
                 L_BC(M,Q), R_BC(M,Q), U_BC(N,Q), D_BC(N,Q), F_BC(N,M), B_BC(N,M),       &
!************************
!***** OTHER MATRIX *****
!************************
                 FU(N,M,Q), FV(N,M,Q), FW(N,M,Q), RHS(N,M,Q),                            &
                 DIV(N,M,Q), DIV_X(N,M,Q), DIV_Y(N,M,Q), DIV_Z(N,M,Q),                   &
                 STAT=IVER)

        IF(IVER /= 0) STOP 'ALLOCATE DIV ERROR!'

!*******************************************************
!***** FIND GRID COORDINATE INI RECTANGULAR DOMAIN *****
!*******************************************************

        CALL UNIFORM_GRID_3D(N, M, Q, XSTART, XEND, YSTART, YEND, ZSTART, ZEND, X , Y, Z, DX, DY, DZ)

!*******************************
!***** INITIALIZE CONSTANT *****
!*******************************

        FU = 0.0_DP
        FV = 0.0_DP
        FW = 0.0_DP

        U = 0.0_DP
        V = 0.0_DP
        W = 0.0_DP
        P = 0.0_DP
        T = 0.0_DP

!****************************************
!***** FIND THE BOUNDARY CONDITIONS *****
!****************************************

        T(1,1:M,1:Q) =  0.5_DP
        T(N,1:M,1:Q) = -0.5_DP

!*******************************
!***** INITIALIZE CONSTANT *****
!*******************************

        U_OLD = 0.0_DP
        V_OLD = 0.0_DP
        W_OLD = 0.0_DP
        P_OLD = 0.0_DP
        T_OLD = 0.0_DP

        DIV   = 0.0_DP
        DIV_X = 0.0_DP
        DIV_Y = 0.0_DP
        DIV_Z = 0.0_DP
  
        US = U
        VS = V
        WS = W
        TS = T

        NON_T = 0
        TOLERANCE = 1.0E-6_DP

        WRITE(CHAR,'(I2.2)') INT(AMP)
        WRITE(NAME,'(I4.4)') N

        FILENAME = 'RHS.RA=10^'//CHAR//'.'//NAME//'.TEC'

        OPEN(UNIT=15, FILE=FILENAME, STATUS='UNKNOWN')
        WRITE(15,*) 'TITLE = "COMPUTED RESULTS"'
        WRITE(15,*) 'VARIABLES = "NONLINEAR ITERATION" "U" "V" "W" "P" "T"'

        WRITE(*,'(2X,A)') '========================================================================='
        WRITE(*,'(3X,A,4X,A,4(7X,A))') 'TIME(S)', 'VEL(U)', 'VEL(V)', 'VEL(W)', 'PRE(P)', 'TEMP(T)'
     	  WRITE(*,'(2X,A)') '========================================================================='

!******************************
!***** MAIN PROGRAM START *****
!******************************

        TEMPORAL_STEPPING: DO

        	  NON_T = NON_T + 1

            FU = 0.0_DP
            FV = PR * T_OLD
            FW = 0.0_DP

            CALL COMPACT_EXPLICIT_3D(N, M, Q, P_OLD, P_X, P_Y, P_Z, DX, DY, DZ)

!***********************************
!***** MOMENTUM EQUATION FOR U *****
!***********************************

            C_X = U_OLD
            C_Y = V_OLD
            C_Z = W_OLD
            K_X = PR / SQRT(RA) 
            K_Y = PR / SQRT(RA) 
            K_Z = PR / SQRT(RA)
            R   = 0.0_DP
            S   = - P_X  + FU

            CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, U_OLD, S, RHS)

            US(2:N-1,2:M-1,2:Q-1) = U_OLD(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1) 

!***********************************
!***** MOMENTUM EQUATION FOR V *****
!***********************************

            S = - P_Y + FV

            CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, V_OLD, S, RHS)

            VS(2:N-1,2:M-1,2:Q-1) = V_OLD(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1)

!***********************************
!***** MOMENTUM EQUATION FOR W *****
!***********************************

            S = - P_Z + FW

            CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, W_OLD, S, RHS)      

            WS(2:N-1,2:M-1,2:Q-1) = W_OLD(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1)

!*********************************
!***** ENERGY EQUATION FOR T *****
!*********************************

            C_X = U
            C_Y = V
            C_Z = W
            K_X = 1.0_DP / SQRT(RA)
            K_Y = 1.0_DP / SQRT(RA)
            K_Z = 1.0_DP / SQRT(RA)
            R   = 0.0_DP
            S   = 0.0_DP

            CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, T_OLD, S, RHS)

            TS(2:N-1,2:M-1,2:Q-1) = T_OLD(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1)

            BTYPE_3D(1)   = 2
            D_BC          = 0.0_DP

            BTYPE_3D(2)   = 2
            U_BC          = 0.0_DP

            BTYPE_3D(3)   = 1
            L_BC(1:M,1:Q) = TS(1,1:M,1:Q)

            BTYPE_3D(4)   = 1
            R_BC(1:M,1:Q) = TS(N,1:M,1:Q)

            BTYPE_3D(5)   = 2
            F_BC          = 0.0_DP

            BTYPE_3D(6)   = 2
            B_BC          = 0.0_DP

            CALL BC_STORE_3D(N, M, Q, TS, DX, DY, DZ, BTYPE_3D, D_BC, U_BC, L_BC, R_BC, F_BC, B_BC)

            FU = 0.0_DP
            FV = PR * TS
            FW = 0.0_DP

!*********************************
!***** THE METHOD I BY EULER *****
!*********************************

            IF(METHOD == 1) THEN      

               METHOD_ONE: DO S_ITER = 1, ITER

                  CALL COMPACT_EXPLICIT_3D(N, M, Q, P, P_X, P_Y, P_Z, DX, DY, DZ)

!***********************************
!***** MOMENTUM EQUATION FOR U *****
!***********************************

                  C_X = U
                  C_Y = V
                  C_Z = W
                  K_X = PR / SQRT(RA)
                  K_Y = PR / SQRT(RA)
                  K_Z = PR / SQRT(RA)
                  R   = 0.0_DP
                  S   = - P_X  + FU + DIV_X

                  CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, U, S, RHS)

                  US(2:N-1,2:M-1,2:Q-1) = U_OLD(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1)

!***********************************
!***** MOMENTUM EQUATION FOR V *****
!***********************************

                  S   = - P_Y + FV + DIV_Y    

                  CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, V, S, RHS)

                  VS(2:N-1,2:M-1,2:Q-1) = V_OLD(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1)

!***********************************
!***** MOMENTUM EQUATION FOR W *****
!***********************************

                  S   = - P_Z + FW + DIV_Z

                  CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, W, S, RHS)

                  WS(2:N-1,2:M-1,2:Q-1) = W_OLD(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1)

!*************************************
!***** PRESSURE CORRECTION FOR P *****
!*************************************

                  U1 = US
                  V1 = VS
                  W1 = WS

                  PRESSURE_CORRECTION_ONE: DO II = 1, II_ITER

                     CALL DFC_CORRECTION_3D(N, M, Q, DX, DY, DZ, DT, U1, V1, W1, P, U, V, W, U_OLD, V_OLD, W_OLD, K_X , FU , FV , FW , PP, DIV_X, DIV_Y, DIV_Z)

                     L2_U = MAXVAL( ABS( U(1:N,1:M,1:Q) - U1(1:N,1:M,1:Q) ) )
                     L2_V = MAXVAL( ABS( V(1:N,1:M,1:Q) - V1(1:N,1:M,1:Q) ) )
                     L2_W = MAXVAL( ABS( W(1:N,1:M,1:Q) - W1(1:N,1:M,1:Q) ) )

                     CALL FIND_DIV_3D(N, M, Q, U_X, V_Y, W_Z, U, V, W, DX, DY, DZ)

                     DIV(2:N-1,2:M-1,2:Q-1) = U_X(2:N-1,2:M-1,2:Q-1) + V_Y(2:N-1,2:M-1,2:Q-1) + W_Z(2:N-1,2:M-1,2:Q-1)

                     BTYPE_3D(1) = 2
                     D_BC        = 0.0_DP

                     BTYPE_3D(2) = 2
                     U_BC        = 0.0_DP

                     BTYPE_3D(3) = 2
                     L_BC        = 0.0_DP

                     BTYPE_3D(4) = 2
                     R_BC        = 0.0_DP

                     BTYPE_3D(5) = 2
                     F_BC        = 0.0_DP

                     BTYPE_3D(6) = 2
                     B_BC        = 0.0_DP

                     CALL BC_STORE_3D(N, M, Q, DIV, DX, DY, DZ, BTYPE_3D, D_BC, U_BC, L_BC, R_BC, F_BC, B_BC)

!                    L2_DIV = MAXVAL(ABS(PP(2:N-1,2:M-1,2:Q-1)))

                     L2_DIV = MAX(L2_U,L2_V,L2_W)

!                    WRITE(*,*) ' THE II=', II, L2_DIV, MAXVAL(ABS(DIV(2:N-1,2:M-1,2:Q-1))) * DX * DY

                     IF(II > 1 .AND. L2_DIV <= 1.0E-4_DP) THEN
                        EXIT
                     ELSE IF(L2_DIV >= 1.0E3_DP) THEN
                     	  WRITE(*,*) '*******************'
                        WRITE(*,*) ' THE DIVERGENCE ***'
                     	  WRITE(*,*) '*******************'
                        STOP         	
                     END IF

                     U1 = U
                     V1 = V
                     W1 = W

                  END DO PRESSURE_CORRECTION_ONE

                  L2_U = MAXVAL( ABS( U(1:N,1:M,1:Q) - US(1:N,1:M,1:Q) ) )
                  L2_V = MAXVAL( ABS( V(1:N,1:M,1:Q) - VS(1:N,1:M,1:Q) ) )
                  L2_W = MAXVAL( ABS( W(1:N,1:M,1:Q) - WS(1:N,1:M,1:Q) ) )

                  L2_DIV = MAX(L2_U,L2_V,L2_W)

                  IF(S_ITER > 1 .AND. L2_DIV <=1.0E-5_DP) THEN
                     EXIT
                  ELSE IF(L2_DIV >= 1.0E3_DP) THEN
                     WRITE(*,*) '*******************'
                     WRITE(*,*) ' THE DIVERGENCE ***'
                     WRITE(*,*) '*******************'
                     STOP
                  END IF

               END DO METHOD_ONE

!*********************************
!***** ENERGY EQUATION FOR T *****
!*********************************

               C_X = U
               C_Y = V
               C_Z = W
               K_X = 1.0_DP / SQRT(RA)
               K_Y = 1.0_DP / SQRT(RA)
               K_Z = 1.0_DP / SQRT(RA)      
               R   = 0.0_DP
               S   = 0.0_DP

               CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, TS, S, RHS)

               T(2:N-1,2:M-1,2:Q-1) = TS(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1)

               BTYPE_3D(1)   = 2
               D_BC          = 0.0_DP

               BTYPE_3D(2)   = 2
               U_BC          = 0.0_DP

               BTYPE_3D(3)   = 1
               L_BC(1:M,1:Q) = T(1,1:M,1:Q)      

               BTYPE_3D(4)   = 1
               R_BC(1:M,1:Q) = T(N,1:M,1:Q)      

               BTYPE_3D(5)   = 2
               F_BC          = 0.0_DP

               BTYPE_3D(6)   = 2
               B_BC          = 0.0_DP

               CALL BC_STORE_3D(N, M, Q, T, DX, DY, DZ, BTYPE_3D, D_BC, U_BC, L_BC, R_BC, F_BC, B_BC)

!**************************************
!***** THE METHOD II BY TVD RK2   *****
!**************************************
!***** TVD RK2 FOR X-MOMENTUM (1) *****
!***** TVD RK2 FOR Y-MOMENTUM (1) *****
!***** TVD RK2 FOR Z-MOMENTUM (1) *****
!**************************************
!CHANGE LIKE: US = U_OLD + DT * RHS/(RK_STAGE-1)

            ELSE IF(METHOD == 2) THEN

               US(2:N-1,2:M-1,2:Q-1) = U_OLD(2:N-1,2:M-1,2:Q-1) + (US(2:N-1,2:M-1,2:Q-1) - U_OLD(2:N-1,2:M-1,2:Q-1)) &
                                     / (REAL(RK_STAGE,KIND=DP) - 1.0_DP)

               VS(2:N-1,2:M-1,2:Q-1) = V_OLD(2:N-1,2:M-1,2:Q-1) + (VS(2:N-1,2:M-1,2:Q-1) - V_OLD(2:N-1,2:M-1,2:Q-1)) &
                                     / (REAL(RK_STAGE,KIND=DP) - 1.0_DP)

               WS(2:N-1,2:M-1,2:Q-1) = W_OLD(2:N-1,2:M-1,2:Q-1) + (WS(2:N-1,2:M-1,2:Q-1) - W_OLD(2:N-1,2:M-1,2:Q-1)) &
                                     / (REAL(RK_STAGE,KIND=DP) - 1.0_DP)

               TS(2:N-1,2:M-1,2:Q-1) = T_OLD(2:N-1,2:M-1,2:Q-1) + (TS(2:N-1,2:M-1,2:Q-1) - T_OLD(2:N-1,2:M-1,2:Q-1)) &
                                     / (REAL(RK_STAGE,KIND=DP) - 1.0_DP)

               FU = 0.0_DP
               FV = PR * TS
               FW = 0.0_DP    
          
               RK_STAGE_ITER: DO RK_ITER = 2, RK_STAGE-1

                  U1 = US
                  V1 = VS
                  W1 = WS
                  U  = US
                  V  = VS
                  W  = WS

                  METHOD_TWO: DO S_ITER = 1 , ITER

                     CALL COMPACT_EXPLICIT_3D(N, M, Q, P, P_X, P_Y, P_Z, DX, DY, DZ)

!************************************************************
!***** N STAGE TVD RK2 FOR X-MOMENTUM (2-RK STAGE-1) *****
!************************************************************

                     C_X = U
                     C_Y = V
                     C_Z = W
                     K_X = PR / SQRT(RA) 
                     K_Y = PR / SQRT(RA) 
                     K_Z = PR / SQRT(RA) 
                     R   = 0.0_DP    
                     S   = - P_X  + FU + DIV_X

                     CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, U, S, RHS)

                     US(2:N-1,2:M-1,2:Q-1) = U1(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1) / (REAL(RK_STAGE,KIND=DP) - 1.0_DP)

!************************************************************
!***** N STAGE TVD RK2 FOR Y-MOMENTUM (2-RK STAGE-1) *****
!************************************************************

                     S   = - P_Y + FV + DIV_Y

                     CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, V, S, RHS) 

                     VS(2:N-1,2:M-1,2:Q-1) = V1(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1) / (REAL(RK_STAGE,KIND=DP) - 1.0_DP)

!************************************************************
!***** N STAGE TVD RK2 FOR Z-MOMENTUM (2-RK STAGE-1) *****
!************************************************************

                     S   = - P_Z + FW + DIV_Z

                     CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, W, S, RHS)

                     WS(2:N-1,2:M-1,2:Q-1) = W1(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1) / (REAL(RK_STAGE,KIND=DP) - 1.0_DP)

!*************************************
!***** PRESSURE CORRECTION FOR P *****
!*************************************

                     UU = US
                     VV = VS
                     WW = WS

                     PRESSURE_CORRECTION_TWO: DO II = 1, II_ITER

                        DDT = DT / (REAL(RK_STAGE,KIND=DP) - 1.0_DP)

                        CALL DFC_CORRECTION_3D(N, M, Q, DX, DY, DZ, DDT, UU, VV, WW, P, U, V, W, U1, V1, W1, K_X , FU , FV , FW , PP, DIV_X, DIV_Y, DIV_Z)

                        L2_U = MAXVAL( ABS( U(1:N,1:M,1:Q) - UU(1:N,1:M,1:Q) ) )    
                        L2_V = MAXVAL( ABS( V(1:N,1:M,1:Q) - VV(1:N,1:M,1:Q) ) ) 
                        L2_W = MAXVAL( ABS( W(1:N,1:M,1:Q) - WW(1:N,1:M,1:Q) ) )

                        CALL FIND_DIV_3D(N, M, Q, U_X, V_Y, W_Z, U, V, W, DX, DY, DZ)

                        DIV(2:N-1,2:M-1,2:Q-1) = U_X(2:N-1,2:M-1,2:Q-1) + V_Y(2:N-1,2:M-1,2:Q-1) + W_Z(2:N-1,2:M-1,2:Q-1)

                        BTYPE_3D(1) = 2
                        D_BC        = 0.0_DP

                        BTYPE_3D(2) = 2
                        U_BC        = 0.0_DP

                        BTYPE_3D(3) = 2
                        L_BC        = 0.0_DP

                        BTYPE_3D(4) = 2
                        R_BC        = 0.0_DP

                        BTYPE_3D(5) = 2
                        F_BC        = 0.0_DP

                        BTYPE_3D(6) = 2
                        B_BC        = 0.0_DP

                        CALL BC_STORE_3D(N, M, Q, DIV, DX, DY, DZ, BTYPE_3D, D_BC, U_BC, L_BC, R_BC, F_BC, B_BC)

!                       L2_DIV = MAXVAL(ABS(PP(2:N-1,2:M-1,2:Q-1)))

                        L2_DIV = MAX(L2_U,L2_V,L2_W)

!                       WRITE(*,*) 'THE II=', II, L2_DIV, MAXVAL(ABS(DIV(2:N-1,2:M-1,2:Q-1))) * DX * DY

                        IF(II > 1 .AND. L2_DIV <= 1.0E-4_DP) THEN
                           EXIT
                        ELSE IF(L2_DIV >= 1.0E3_DP) THEN
                           WRITE(*,*) '*******************'
                           WRITE(*,*) ' THE DIVERGENCE ***'
                           WRITE(*,*) '*******************'
                           STOP
                        END IF

                        UU = U
                        VV = V
                        WW = W

                     END DO PRESSURE_CORRECTION_TWO

                     L2_U = MAXVAL( ABS( U(1:N,1:M,1:Q) - US(1:N,1:M,1:Q) ) )
                     L2_V = MAXVAL( ABS( V(1:N,1:M,1:Q) - VS(1:N,1:M,1:Q) ) )
                     L2_W = MAXVAL( ABS( W(1:N,1:M,1:Q) - WS(1:N,1:M,1:Q) ) )

                     L2_DIV = MAX(L2_U,L2_V,L2_W)

!                    WRITE(*,'(1X,A,I2,A,I3,E13.5)') 'TVD RK2-STAGE(', RK_ITER, ')ITER=', S_ITER, L2_DIV

                     IF(S_ITER > 1 .AND. L2_DIV <= 1.0E-5_DP) THEN
                        EXIT
                     ELSE IF(L2_DIV >= 1.0E3_DP) THEN
                        WRITE(*,*) '*******************'
                        WRITE(*,*) ' THE DIVERGENCE ***'
                        WRITE(*,*) '*******************'
                        STOP
                     END IF

                  END DO METHOD_TWO

!*********************************
!***** ENERGY EQUATION FOR T *****
!*********************************

                  C_X = U
                  C_Y = V
                  C_Z = W
                  K_X = 1.0_DP / SQRT(RA)
                  K_Y = 1.0_DP / SQRT(RA)
                  K_Z = 1.0_DP / SQRT(RA)
                  R   = 0.0_DP
                  S   = 0.0_DP

                  CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, TS, S, RHS)

                  TS(2:N-1,2:M-1,2:Q-1) = TS(2:N-1,2:M-1,2:Q-1) + DT * RHS(2:N-1,2:M-1,2:Q-1) / (REAL(RK_STAGE,KIND=DP) - 1.0_DP)

                  BTYPE_3D(1)   = 2
                  D_BC          = 0.0_DP

                  BTYPE_3D(2)   = 2
                  U_BC          = 0.0_DP

                  BTYPE_3D(3)   = 1
                  L_BC(1:M,1:Q) = TS(1,1:M,1:Q)      

                  BTYPE_3D(4)   = 1
                  R_BC(1:M,1:Q) = TS(N,1:M,1:Q)      

                  BTYPE_3D(5)   = 2
                  F_BC          = 0.0_DP

                  BTYPE_3D(6)   = 2
                  B_BC          = 0.0_DP

                  CALL BC_STORE_3D(N, M, Q, T, DX, DY, DZ, BTYPE_3D, D_BC, U_BC, L_BC, R_BC, F_BC, B_BC)           

               END DO RK_STAGE_ITER

!************************************************************
!***** N STAGE TVD RK2 FOR X-MOMENTUM (RK STAGE) *****
!************************************************************

               U1 = U
               V1 = V
               W1 = W

               FU = 0.0_DP
               FV = PR * TS
               FW = 0.0_DP  

               TVD_RK2_STAGE: DO S_ITER = 1, ITER

                  CALL COMPACT_EXPLICIT_3D(N, M, Q, P, P_X, P_Y, P_Z, DX, DY, DZ)

                  C_X = U
                  C_Y = V
                  C_Z = W
                  K_X = PR / SQRT(RA) 
                  K_Y = PR / SQRT(RA) 
                  K_Z = PR / SQRT(RA) 
                  R   = 0.0_DP
                  S   = - P_X  + FU + DIV_X

                  CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, U, S, RHS)

                  US(2:N-1,2:M-1,2:Q-1) = (U_OLD(2:N-1,2:M-1,2:Q-1) + (REAL(RK_STAGE,KIND=DP) - 1.0_DP) * U1(2:N-1,2:M-1,2:Q-1)  &
                                                                    + DT * RHS(2:N-1,2:M-1,2:Q-1) )/ (REAL(RK_STAGE,KIND=DP) )

!************************************************************
!***** N STAGE TVD RK2 FOR Y-MOMENTUM (RK STAGE) *****
!************************************************************

                  S   = - P_Y + FV + DIV_Y

                  CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, V, S, RHS)          

                  VS(2:N-1,2:M-1,2:Q-1) = (V_OLD(2:N-1,2:M-1,2:Q-1) + (REAL(RK_STAGE,KIND=DP) - 1.0_DP) * V1(2:N-1,2:M-1,2:Q-1) &
                                                                    + DT * RHS(2:N-1,2:M-1,2:Q-1) ) / (REAL(RK_STAGE,KIND=DP) )

!************************************************************
!***** N STAGE TVD RK2 FOR Z-MOMENTUM (RK STAGE) *****
!************************************************************

                  S   = - P_Z + FW + DIV_Z

                  CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, W, S, RHS)

                  WS(2:N-1,2:M-1,2:Q-1) = (W_OLD(2:N-1,2:M-1,2:Q-1) + (REAL(RK_STAGE,KIND=DP) - 1.0_DP) * W1(2:N-1,2:M-1,2:Q-1) &
                                                                    + DT * RHS(2:N-1,2:M-1,2:Q-1) ) / (REAL(RK_STAGE,KIND=DP) )

!*************************************
!***** PRESSURE CORRECTION FOR P *****
!*************************************

                  UU = US
                  VV = VS
                  WW = WS

                  PRESSURE_CORRECTION_THREE: DO II = 1 , II_ITER             

                     DDT = DT / (REAL(RK_STAGE,KIND=DP) )            

                     CALL DFC_CORRECTION_3D(N, M, Q, DX, DY, DZ, DDT, UU, VV, WW, P, U, V, W, U_OLD, V_OLD, W_OLD, K_X, FU, FV, FW, PP, DIV_X, DIV_Y, DIV_Z)

                     L2_U = MAXVAL( ABS( U(1:N,1:M,1:Q) - UU(1:N,1:M,1:Q) ) )    
                     L2_V = MAXVAL( ABS( V(1:N,1:M,1:Q) - VV(1:N,1:M,1:Q) ) )
                     L2_W = MAXVAL( ABS( W(1:N,1:M,1:Q) - WW(1:N,1:M,1:Q) ) ) 

                     CALL FIND_DIV_3D(N, M, Q, U_X, V_Y, W_Z, U, V, W, DX, DY, DZ)

                     DIV(2:N-1,2:M-1,2:Q-1) = U_X(2:N-1,2:M-1,2:Q-1) + V_Y(2:N-1,2:M-1,2:Q-1) + W_Z(2:N-1,2:M-1,2:Q-1)

                     BTYPE_3D(1) = 2
                     D_BC        = 0.0_DP

                     BTYPE_3D(2) = 2
                     U_BC        = 0.0_DP

                     BTYPE_3D(3) = 2
                     L_BC        = 0.0_DP

                     BTYPE_3D(4) = 2
                     R_BC        = 0.0_DP

                     BTYPE_3D(5) = 2
                     F_BC        = 0.0_DP

                     BTYPE_3D(6) = 2
                     B_BC        = 0.0_DP

                     CALL BC_STORE_3D(N, M, Q, DIV, DX, DY, DZ, BTYPE_3D, D_BC, U_BC, L_BC, R_BC, F_BC, B_BC)

!                    L2_DIV = MAXVAL(ABS(PP(2:N-1,2:M-1,2:Q-1)))

                     L2_DIV = MAX(L2_U,L2_V,L2_W)           

!                    WRITE(*,*) "II" , II , L2_DIV , MAXVAL(ABS(DIV(2:N-1,2:M-1,2:Q-1))) * DX * DY

                     IF(II > 1 .AND. L2_DIV <= 1.0E-4_DP) THEN
                        EXIT           
                     ELSE IF(L2_DIV >= 1.0E3_DP) THEN
                        WRITE(*,*) '*******************'
                        WRITE(*,*) ' THE DIVERGENCE ***'
                        WRITE(*,*) '*******************'
                        STOP         	
                     END IF        

                     UU = U
                     VV = V
                     WW = W

                  END DO PRESSURE_CORRECTION_THREE

                  L2_U = MAXVAL( ABS( U(1:N,1:M,1:Q) - US(1:N,1:M,1:Q) ) )
                  L2_V = MAXVAL( ABS( V(1:N,1:M,1:Q) - VS(1:N,1:M,1:Q) ) )
                  L2_W = MAXVAL( ABS( W(1:N,1:M,1:Q) - WS(1:N,1:M,1:Q) ) )

                  L2_DIV = MAX(L2_U,L2_V,L2_W)

!                 WRITE(*,'(1X,A,I2,A,I3,E13.5)') 'TVD RK2-STAGE(', RK_ITER, ')ITER=', S_ITER, L2_DIV

                  IF(S_ITER > 1 .AND. L2_DIV <= 1.0E-5_DP) THEN
                     EXIT
                  ELSE IF(L2_DIV >= 1.0E3_DP) THEN
                     WRITE(*,*) '*******************'
                     WRITE(*,*) ' THE DIVERGENCE ***'
                     WRITE(*,*) '*******************'
                     STOP
                  END IF

               END DO TVD_RK2_STAGE

!*********************************
!***** ENERGY EQUATION FOR T *****
!*********************************

                C_X = U
                C_Y = V
                C_Z = W
                K_X = 1.0_DP / SQRT(RA)
                K_Y = 1.0_DP / SQRT(RA)
                K_Z = 1.0_DP / SQRT(RA)        
                R   = 0.0_DP
                S   = 0.0_DP

                CALL EXPLICIT_DRP_13_3D(N, M, Q, C_X, C_Y, C_Z, K_X, K_Y, K_Z, R, DX, DY, DZ, TS, S, RHS)

                T(2:N-1,2:M-1,2:Q-1) = (T_OLD(2:N-1,2:M-1,2:Q-1) + (REAL(RK_STAGE,KIND=DP) - 1.0_DP) * TS(2:N-1,2:M-1,2:Q-1) &
                                                                 + DT * RHS(2:N-1,2:M-1,2:Q-1) ) / (REAL(RK_STAGE,KIND=DP) )

                BTYPE_3D(1)   = 2
                D_BC          = 0.0_DP

                BTYPE_3D(2)   = 2
                U_BC          = 0.0_DP

                BTYPE_3D(3)   = 1
                L_BC(1:M,1:Q) = T(1,1:M,1:Q)

                BTYPE_3D(4)   = 1
                R_BC(1:M,1:Q) = T(N,1:M,1:Q)

                BTYPE_3D(5)   = 2
                F_BC          = 0.0_DP

                BTYPE_3D(6)   = 2
                B_BC          = 0.0_DP

                CALL BC_STORE_3D(N, M, Q, T, DX, DY, DZ, BTYPE_3D, D_BC, U_BC, L_BC, R_BC, F_BC, B_BC)

            END IF

!******************************
!***** CAL. DIV CONDITION *****
!******************************

            CALL FIND_DIV_3D(N, M, Q, U_X, V_Y, W_Z, U, V, W, DX, DY, DZ)

            DIV(2:N-1,2:M-1,2:Q-1) = U_X(2:N-1,2:M-1,2:Q-1) + V_Y(2:N-1,2:M-1,2:Q-1) + W_Z(2:N-1,2:M-1,2:Q-1)

            BTYPE_3D(1) = 2
            D_BC        = 0.0_DP      
            BTYPE_3D(2) = 2
            U_BC        = 0.0_DP      
            BTYPE_3D(3) = 2
            L_BC        = 0.0_DP
            BTYPE_3D(4) = 2
            R_BC        = 0.0_DP
            BTYPE_3D(5) = 2
            F_BC        = 0.0_DP
            BTYPE_3D(6) = 2
            B_BC        = 0.0_DP

            CALL BC_STORE_3D(N, M, Q, DIV, DX, DY, DZ, BTYPE_3D, D_BC, U_BC, L_BC, R_BC, F_BC, B_BC)

!           L2_DIV = MAXVAL(ABS(DIV))

            L2_DIV = SQRT( SUM( ABS( DIV(1:N,1:M,1:Q) )**2 ) / REAL(N*M*Q,KIND=DP) )

!****************************
!***** CONVERGENCE TEST *****
!****************************

!           L2_U = MAXVAL( ABS( U(1:N,1:M,1:Q) - U_OLD(1:N,1:M,1:Q) ) )
!           L2_V = MAXVAL( ABS( V(1:N,1:M,1:Q) - V_OLD(1:N,1:M,1:Q) ) )
!           L2_W = MAXVAL( ABS( W(1:N,1:M,1:Q) - W_OLD(1:N,1:M,1:Q) ) )
!           L2_P = MAXVAL( ABS( P(1:N,1:M,1:Q) - P_OLD(1:N,1:M,1:Q) ) )
!           L2_T = MAXVAL( ABS( T(1:N,1:M,1:Q) - T_OLD(1:N,1:M,1:Q) ) )

            L2_U = SQRT( SUM( ABS( U(1:N,1:M,1:Q) - U_OLD(1:N,1:M,1:Q) )**2 ) / REAL(N*M*Q,KIND=DP) )
            L2_V = SQRT( SUM( ABS( V(1:N,1:M,1:Q) - V_OLD(1:N,1:M,1:Q) )**2 ) / REAL(N*M*Q,KIND=DP) )
            L2_W = SQRT( SUM( ABS( W(1:N,1:M,1:Q) - W_OLD(1:N,1:M,1:Q) )**2 ) / REAL(N*M*Q,KIND=DP) )
            L2_P = SQRT( SUM( ABS( P(1:N,1:M,1:Q) - P_OLD(1:N,1:M,1:Q) )**2 ) / REAL(N*M*Q,KIND=DP) )
            L2_T = SQRT( SUM( ABS( T(1:N,1:M,1:Q) - T_OLD(1:N,1:M,1:Q) )**2 ) / REAL(N*M*Q,KIND=DP) )

            WRITE(*,'(2X,I8,5E13.5)') NON_T, L2_U, L2_V, L2_W, L2_P, L2_T

            IF(MOD(NON_T,10) == 0) THEN
               WRITE(*,'(2X,A)') '========================================================================='
               WRITE(*,'(3X,A,4X,A,4(7X,A))') 'TIME(S)', 'VEL(U)', 'VEL(V)', 'VEL(W)', 'PRE(P)', 'TEMP(T)'
               WRITE(*,'(2X,A)') '========================================================================='
!               WRITE(*,'(5X,A,E15.5)') 'DIV.=', L2_DIV
            END IF

            WRITE(15,'(2X,I8,5E16.6)') NON_T, L2_U, L2_V, L2_W, L2_P, L2_T

            IF(MOD(NON_T,20) == 0) THEN
               CALL WRITE_TEC_NC(N, M, Q, X, Y, Z, U, V, W, P, T, AMP)
            END IF

            IF(NON_T > 1 .AND. MAX(L2_U,L2_V,L2_W) <= TOLERANCE) THEN
               WRITE(*,*) '***************************'
               WRITE(*,*) '***** THE CONVERGENCE *****'
               WRITE(*,*) '***************************'
               EXIT
            ELSE IF( MAX(L2_U,L2_V,L2_W) >= 1.0E3_DP) THEN
               WRITE(*,*) '**************************'
               WRITE(*,*) '***** THE DIVERGENCE *****'
               WRITE(*,*) '**************************'
               STOP
            END IF

            U_OLD = U
            V_OLD = V
            W_OLD = W
            P_OLD = P
            T_OLD = T

!****************************
!***** MAIN PROGRAM END *****
!****************************

        END DO TEMPORAL_STEPPING
  	    CLOSE(UNIT=15)

!***********************************
!***** CAL. DIV-FREE CONDITION *****
!***********************************

        TMP=SUM( DIV(2:N-1,2:M-1,2:Q-1) )

        WRITE(*,*) 'DIV-FREE CONDITION=', TMP

!*********************************
!***** CAL. MAX & MIN VALUES *****
!*********************************

        U_MAX = MAXVAL(U(1:N,1:M,1:Q))
        V_MAX = MAXVAL(V(1:N,1:M,1:Q))
        W_MAX = MAXVAL(W(1:N,1:M,1:Q))
        U_MIN = MINVAL(U(1:N,1:M,1:Q))
        V_MIN = MINVAL(V(1:N,1:M,1:Q))
        W_MIN = MINVAL(W(1:N,1:M,1:Q))

        I_DIM_U_MAX = MAXLOC(U)
        I_DIM_V_MAX = MAXLOC(V)
        I_DIM_W_MAX = MAXLOC(W)

        I_DIM_U_MIN = MINLOC(U)
        I_DIM_V_MIN = MINLOC(V)
        I_DIM_W_MIN = MINLOC(W)

!**************************
!***** CAL. CPU TIMES *****
!**************************

        CALL CPU_TIME(CPU_T1)

        CPU = CPU_T1 - CPU_T0

        FILENAME='CPU.RA.10^'//CHAR//'.'//NAME//'.TXT'
        OPEN(UNIT=17,FILE=FILENAME, STATUS= 'UNKNOWN' )
        WRITE(17,*) N, CPU
        WRITE(17,*) "X Y Z VALUE"
        WRITE(17,'(1X,A,4E16.6)') "U_MAX", X(I_DIM_U_MAX(1), I_DIM_U_MAX(2), I_DIM_U_MAX(3)), &
                                          &Y(I_DIM_U_MAX(1), I_DIM_U_MAX(2), I_DIM_U_MAX(3)), &
                                          &Z(I_DIM_U_MAX(1), I_DIM_U_MAX(2), I_DIM_U_MAX(3)), &
                                          &U_MAX
                            
        WRITE(17,'(1X,A,4E16.6)') "V_MAX", X(I_DIM_V_MAX(1), I_DIM_V_MAX(2), I_DIM_V_MAX(3)), &
                                          &Y(I_DIM_V_MAX(1), I_DIM_V_MAX(2), I_DIM_V_MAX(3)), &
                                          &Z(I_DIM_V_MAX(1), I_DIM_V_MAX(2), I_DIM_V_MAX(3)), &
                                          &V_MAX
                            
        WRITE(17,'(1X,A,4E16.6)') "W_MAX", X(I_DIM_W_MAX(1), I_DIM_W_MAX(2), I_DIM_W_MAX(3)), &
                                          &Y(I_DIM_W_MAX(1), I_DIM_W_MAX(2), I_DIM_W_MAX(3)), &
                                          &Z(I_DIM_W_MAX(1), I_DIM_W_MAX(2), I_DIM_W_MAX(3)), &
                                          &W_MAX
                            
        WRITE(17,'(1X,A,4E16.6)') "U_MIN", X(I_DIM_U_MIN(1), I_DIM_U_MIN(2), I_DIM_U_MIN(3)), &
                                          &Y(I_DIM_U_MIN(1), I_DIM_U_MIN(2), I_DIM_U_MIN(3)), &
                                          &Z(I_DIM_U_MIN(1), I_DIM_U_MIN(2), I_DIM_U_MIN(3)), &
                                          &U_MIN
                            
        WRITE(17,'(1X,A,4E16.6)') "V_MIN", X(I_DIM_V_MIN(1), I_DIM_V_MIN(2), I_DIM_V_MIN(3)), &
                                          &Y(I_DIM_V_MIN(1), I_DIM_V_MIN(2), I_DIM_V_MIN(3)), &
                                          &Z(I_DIM_V_MIN(1), I_DIM_V_MIN(2), I_DIM_V_MIN(3)), &
                                          &V_MIN
                            
        WRITE(17,'(1X,A,4E16.6)') "W_MIN", X(I_DIM_W_MIN(1), I_DIM_W_MIN(2), I_DIM_W_MIN(3)), &
                                          &Y(I_DIM_W_MIN(1), I_DIM_W_MIN(2), I_DIM_W_MIN(3)), &
                                          &Z(I_DIM_W_MIN(1), I_DIM_W_MIN(2), I_DIM_W_MIN(3)), &
                                          &W_MIN
        CLOSE(UNIT=17)

        CALL WRITE_TEC_NC(N, M, Q, X, Y, Z, U, V, W, P, T, AMP)

!        WRITE(*,'(//,3X,A)') &
!                          '+****************************************************************+'
!        WRITE(*,'(3X,A)') '+ USE 2D CDR DISCRETIZATION AND ADI SPLITTING SOLUTION ALOGRITHM +'
!        WRITE(*,'(3X,A)') '+ STUDENT : REUI-KUO LIN        NUMBERS     : D89525003          +'
!        WRITE(*,'(3X,A)') '+ DATE    : JUNE 30, 2004       CHANGE TIME : 12:26:38           +'
!        WRITE(*,'(3X,A)') '+****************************************************************+'
!        WRITE(*,'(3X,A)') '+ SOLVER THE TWO-DIMENSION STEADY-STATE NAVIER-STOKES EQUATION   +'
!        WRITE(*,'(3X,A,E25.18,A)') '+ VELOCITY U: L2-ERROR NORM = (', RESIDUALU,  ' )       +'
!        WRITE(*,'(3X,A,E25.18,A)') '+ VELOCITY V: L2-ERROR NORM = (', RESIDUALV,  ' )       +'
!        WRITE(*,'(3X,A,E25.18,A)') '+ PRESSURE P: L2-ERROR NORM = (', RESIDUALP,  ' )       +'
!        WRITE(*,'(3X,A,E25.18,A)') '+ THE CALCULATION CPU TIMES  = (', CPU   ,   ' ) SEC.  +'
!        WRITE(*,'(3X,A,E25.18,A)') '+ THE REYNOLDS NUMBER RE     = (', RE   ,    ' )       +'
!        WRITE(*,'(3X,A)') '+            THE PROGRAM IS CONVERGECE !!!                       +'
!        WRITE(*,'(3X,A)') '+****************************************************************+'

!**********************************************************

        CONTAINS

!**********************************************************

        INCLUDE 'CDS.INC'
        INCLUDE 'CDS2.INC'
        INCLUDE 'TEC_PLT.INC'
        INCLUDE 'GRID.INC'
        INCLUDE 'EXPLICIT_3D.INC'

!**********************************************************

        END PROGRAM SOLVER_THREE_DIM_NAVIER_STOKES_EQUATION

!**********************************************************
