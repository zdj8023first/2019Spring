
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Explicit Scheme
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  SUBROUTINE FIND_FxFy_001_3D(N , M , Q , Fx, Fy, Fz , F, Dx , Dy , Dz)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to Get First derivatives with uniform Grids.
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    
    INTEGER      , INTENT(IN)  :: N , M , Q
    REAL(KIND=DP), INTENT(IN)  :: Dx, Dy , Dz
    REAL(KIND=DP), INTENT(OUT) :: Fx(:,:,:), Fy(:,:,:) , Fz(:,:,:)
    REAL(KIND=DP), INTENT(IN)  :: F(:,:,:)
        
    INTEGER                    :: I, J , K

   
    Fx = 0.0_DP
    Fy = 0.0_DP
    Fz = 0.0_DP
    
    !Inner
    DO I = 2 , N-1
    DO J = 2 , M-1
    DO K = 2 , Q-1
      Fx(I,J,K) = (F(I+1,J,K) - F(I-1,J,K)) / (2.0_DP*Dx)
      Fy(I,J,K) = (F(I,J+1,K) - F(I,J-1,K)) / (2.0_DP*Dy)
      Fz(I,J,K) = (F(I,J,K+1) - F(I,J,K-1)) / (2.0_DP*Dz)
    END DO
    END DO
    END DO

    !Fy for Y = 1 AND Y = M
    DO I = 1 , N
    DO K = 1 , Q
      Fy(I,1,K)  = (-1.30124249884758_DP    * F(I,1,K) +  1.40372749654270_DP*F(I,2,K) &
               &  +0.962725034572970e-1_DP* F(I,3,K) - 0.198757501152434_DP*F(I,4,K) ) / Dy
              
      Fy(I,M,K)  =-(-1.30124249884758_DP    * F(I,M,K)   +  1.40372749654270_DP*F(I,M-1,K) &
               &  +0.962725034572970e-1_DP* F(I,M-2,K) - 0.198757501152434_DP*F(I,M-3,K) ) / Dy
    END DO
    END DO

    !Fy for X = 1 AND X = N
    DO J = 2 , M-1
    DO K = 1 , Q
      Fy(1,J,K) = (F(1,J+1,K) - F(1,J-1,K)) / (2.0_DP*Dy)
      Fy(N,J,K) = (F(N,J+1,K) - F(N,J-1,K)) / (2.0_DP*Dy)    
    END DO
    END DO


    !Fy for Z = 1 AND Z = Q
    DO I = 1 , N
    DO J = 2 , M-1
      Fy(I,J,1) = (F(I,J+1,1) - F(I,J-1,1)) / (2.0_DP*Dy)
      Fy(I,J,Q) = (F(I,J+1,Q) - F(I,J-1,Q)) / (2.0_DP*Dy)    
    END DO
    END DO
    
    !Fx for X = 1 AND X = N
    DO J = 1 , M
    DO K = 1 , Q
      Fx(1,J,K) = (-1.30124249884758_DP    * F(1,J,K) +  1.40372749654270_DP*F(2,J,K) &
              &  +0.962725034572970e-1_DP* F(3,J,K) - 0.198757501152434_DP*F(4,J,K) ) / Dx
              
      Fx(N,J,K) =-(-1.30124249884758_DP    * F(N,J,K) +  1.40372749654270_DP*F(N-1,J,K) &
              &  +0.962725034572970e-1_DP* F(N-2,J,K) - 0.198757501152434_DP*F(N-3,J,K) ) / Dx
    END DO
    END DO
    
    !Fx for Y = 1 AND Y = M
    DO I = 2 , N-1
    DO K = 1 , Q
      Fx(I,1,K) = (F(I+1,1,K) - F(I-1,1,K)) / (2.0_DP*Dx)
      Fx(I,M,K) = (F(I+1,M,K) - F(I-1,M,K)) / (2.0_DP*Dx)
    END DO
    END DO
    
    !Fx for Z = 1 AND Z = Q
    DO I = 2 , N-1
    DO J = 1 , M
      Fx(I,J,1) = (F(I+1,J,1) - F(I-1,J,1)) / (2.0_DP*Dx)
      Fx(I,J,Q) = (F(I+1,J,Q) - F(I-1,J,Q)) / (2.0_DP*Dx)
    END DO
    END DO    


    !Fz for Z = 1 AND Z = Q
    DO I = 1 , N
    DO J = 1 , M
      Fz(I,J,1) = (-1.30124249884758_DP    * F(I,J,1) +  1.40372749654270_DP*F(I,J,2) &
              &  +0.962725034572970e-1_DP* F(I,J,3) - 0.198757501152434_DP*F(I,J,4) ) / Dz
              
      Fz(I,J,Q) =-(-1.30124249884758_DP    * F(I,J,Q) +  1.40372749654270_DP*F(I,J,Q-1) &
              &  +0.962725034572970e-1_DP* F(I,J,Q-2) - 0.198757501152434_DP*F(I,J,Q-3) ) / Dz
    END DO
    END DO
    
    !Fz for X = 1 AND X = N
    DO J = 1 , M
    DO K = 2 , Q-1
      Fz(1,J,K) = (F(1,J,K+1) - F(1,J,K-1)) / (2.0_DP*Dz)
      Fz(N,J,K) = (F(N,J,K+1) - F(N,J,K-1)) / (2.0_DP*Dz)
    END DO
    END DO
    
    !Fz for Y = 1 AND Y = M
    DO I = 1 , N
    DO K = 2 , Q-1
      Fz(I,1,K) = (F(I,1,K+1) - F(I,1,K-1)) / (2.0_DP*Dz)
      Fz(I,M,K) = (F(I,M,K+1) - F(I,M,K-1)) / (2.0_DP*Dz)
    END DO
    END DO  

  END SUBROUTINE FIND_FxFy_001_3D


  SUBROUTINE FIND_DIV_3D(N , M , Q , U_X, V_Y , W_Z , U , V , W , Dx , Dy , Dz)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to Get First derivatives with uniform Grids.
  !only (2:N-1,2:M-1,2:Q-1) have value!!
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    
    INTEGER      , INTENT(IN)  :: N , M , Q
    REAL(KIND=DP), INTENT(IN)  :: Dx, Dy , Dz
    REAL(KIND=DP), INTENT(OUT) :: U_X(:,:,:), V_Y(:,:,:) , W_Z(:,:,:)
    REAL(KIND=DP), INTENT(IN)  :: U(:,:,:) , V(:,:,:) , W(:,:,:)
        
    INTEGER                    :: I, J , K

   
   U_X = 0.0_DP
   V_Y = 0.0_DP
   W_Z = 0.0_DP

    DO I = 2 , N-1
    DO J = 2 , M-1 
    DO K = 2 , Q-1
      U_X(I,J,K) = (U(I+1,J,K) - U(I-1,J,K)) / (2.0_DP*Dx)
      V_Y(I,J,K) = (V(I,J+1,K) - V(I,J-1,K)) / (2.0_DP*Dy)
      W_Z(I,J,K) = (W(I,J,K+1) - W(I,J,K-1)) / (2.0_DP*Dz)
    END DO
    END DO
    END DO


  END SUBROUTINE FIND_DIV_3D


  SUBROUTINE FIND_FxFy_002_3D(N , M , Q , Fxx, Fyy, Fzz , F, Dx , Dy , Dz)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to Get First derivatives with uniform Grids.
  !
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    
    INTEGER      , INTENT(IN)  :: N , M , Q
    REAL(KIND=DP), INTENT(IN)  :: Dx, Dy , Dz
    REAL(KIND=DP), INTENT(OUT) :: Fxx(:,:,:), Fyy(:,:,:) , Fzz(:,:,:)
    REAL(KIND=DP), INTENT(IN)  :: F(:,:,:)
        
    INTEGER                    :: I, J , K

   
   Fxx = 0.0_DP
   Fyy = 0.0_DP
   Fzz = 0.0_DP
    
    !Inner
    DO I = 2 , N-1
    DO J = 2 , M-1
    DO K = 2 , Q-1
      Fxx(I,J,K) = (F(I+1,J,K) -2.0_DP * F(I,J,K) + F(I-1,J,K)) / (Dx**2)
      Fyy(I,J,K) = (F(I,J+1,K) -2.0_DP * F(I,J,K) + F(I,J-1,K)) / (Dy**2)
      Fzz(I,J,K) = (F(I,J,K+1) -2.0_DP * F(I,J,K) + F(I,J,K-1)) / (Dz**2)
    END DO
    END DO
    END DO

    
    !Fyy for Y = 1 AND Y = M
    DO I = 1 , N
    DO K = 1 , Q
      Fyy(I,1,K) = ( 2.0_DP*F(I,1,K) - 5.0_DP*F(I,2,K)   + 4.0_DP*F(I,3,K)   - F(I,4,K)   ) / (Dy**2)
      Fyy(I,M,K) = ( 2.0_DP*F(I,M,K) - 5.0_DP*F(I,M-1,K) + 4.0_DP*F(I,M-2,K) - F(I,M-3,K) ) / (Dy**2)
    END DO
    END DO
    
    !Fyy for X = 1 AND X = N
    DO J = 2 , M-1
    DO K = 1 , Q
      Fyy(1,J,K) = (F(1,J+1,K) -2.0_DP*F(1,J,K) + F(1,J-1,K)) / (Dy**2)
      Fyy(N,J,K) = (F(N,J+1,K) -2.0_DP*F(N,J,K) + F(N,J-1,K)) / (Dy**2)
    END DO
    END DO
    
    !Fyy for Z = 1 AND Z = Q
    DO I = 1 , N
    DO J = 2 , M-1
      Fyy(I,J,1) = (F(I,J+1,1) -2.0_DP*F(I,J,1) + F(I,J-1,1)) / (Dy**2)
      Fyy(I,J,Q) = (F(I,J+1,Q) -2.0_DP*F(I,J,Q) + F(I,J-1,Q)) / (Dy**2)
    END DO
    END DO
    
    
    !Fxx for X = 1 AND X = N
    DO J = 1 , M
    DO K = 1 , Q
      Fxx(1,J,K) = ( 2.0_DP*F(1,J,K) - 5.0_DP*F(2,J,K)   + 4.0_DP*F(3,J,K)   - F(4,J,K)   ) / (Dx**2)
      Fxx(N,J,K) = ( 2.0_DP*F(N,J,K) - 5.0_DP*F(N-1,J,K) + 4.0_DP*F(N-2,J,K) - F(N-3,J,K) ) / (Dx**2)
    END DO
    END DO
    
    !Fxx for Y = 1 AND Y = M
    DO I = 2 , N-1
    DO K = 1 , Q
      Fxx(I,1,K) = (F(I+1,1,K) -2.0_DP*F(I,1,K) + F(I-1,1,K)) / (Dx**2)
      Fxx(I,M,K) = (F(I+1,M,K) -2.0_DP*F(I,M,K) + F(I-1,M,K)) / (Dx**2)
    END DO
    END DO
    
    !Fxx for Z = 1 AND Z = Q
    DO I = 2 , N-1
    DO J = 1 , M
      Fxx(I,J,1) = (F(I+1,J,1) -2.0_DP*F(I,J,1) + F(I-1,J,1)) / (Dx**2)
      Fxx(I,J,Q) = (F(I+1,J,Q) -2.0_DP*F(I,J,Q) + F(I-1,J,Q)) / (Dx**2)
    END DO
    END DO


    !Fzz for Z = 1 AND Z = Q
    DO I = 1 , N
    DO J = 1 , M
      Fzz(I,J,1) = ( 2.0_DP*F(I,J,1) - 5.0_DP*F(I,J,2)   + 4.0_DP*F(I,J,3)   - F(I,J,4)   ) / (Dz**2)
      Fzz(I,J,Q) = ( 2.0_DP*F(I,J,Q) - 5.0_DP*F(I,J,Q-1) + 4.0_DP*F(I,J,Q-2) - F(I,J,Q-3) ) / (Dz**2)
    END DO
    END DO
    
    !Fzz for X = 1 AND X = N
    DO J = 1 , M
    DO K = 2 , Q-1
      Fzz(1,J,K) = (F(1,J,K+1) -2.0_DP*F(1,J,K) + F(1,J,K-1)) / (Dz**2)
      Fzz(N,J,K) = (F(N,J,K+1) -2.0_DP*F(N,J,K) + F(N,J,K-1)) / (Dz**2)
    END DO
    END DO
    
    
    !Fzz for Y = 1 AND Y = M
    DO I = 1 , N
    DO K = 2 , Q-1
      Fzz(I,1,K) = (F(I,1,K+1) -2.0_DP*F(I,1,K) + F(I,1,K-1)) / (Dz**2)
      Fzz(I,M,K) = (F(I,M,K+1) -2.0_DP*F(I,M,K) + F(I,M,K-1)) / (Dz**2)
    END DO
    END DO


  END SUBROUTINE FIND_FxFy_002_3D



  SUBROUTINE FIND_Fxy_2nd(N , M , Fxy , F , Dx, Dy)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to Get Mixed derivatives with uniform Grids.
  !
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    
    INTEGER      , INTENT(IN)  :: N , M
    REAL(KIND=DP), INTENT(IN)  :: Dx, Dy
    REAL(KIND=DP), INTENT(OUT) :: Fxy(:,:)
    REAL(KIND=DP), INTENT(IN)  :: F(:,:)
    
    INTEGER                    :: I, J

   
   Fxy = 0.0_DP

    !inner
    DO I = 2 , N-1 
    DO J = 2 , M-1
      Fxy(I,J) = ( (F(I+1,J+1) - F(I+1,J-1)) / (2.0_DP*Dy) &
               &  -(F(I-1,J+1) - F(I-1,J-1)) / (2.0_DP*Dy) ) / (2.0_DP * Dx)
    END DO
    END DO

    !down and up
    DO I=2 , N-1
      Fxy(I,1) = (-3.0_DP * (F(I+1,1) - F(I-1,1) ) / (2.0_DP * Dx) &
               &  +4.0_DP * (F(I+1,2) - F(I-1,2) ) / (2.0_DP * Dx) &
               &  -         (F(I+1,3) - F(I-1,3) ) / (2.0_DP * Dx) ) / (2.0_DP * Dy)

      Fxy(I,M) = ( 3.0_DP * (F(I+1,M) - F(I-1,M) )     / (2.0_DP * Dx)  &
               &  -4.0_DP * (F(I+1,M-1) - F(I-1,M-1) ) / (2.0_DP * Dx)  &
               &  +         (F(I+1,M-2) - F(I-1,M-2) ) / (2.0_DP * Dx)  ) / (2.0_DP * Dy)
               
    END DO

    !left and right

    DO J=2 , M-1
      Fxy(1,J) = (-3.0_DP * (F(1,J+1) - F(1,J-1)) / (2.0_DP * Dy) &
               &  +4.0_DP * (F(2,J+1) - F(2,J-1)) / (2.0_DP * Dy) &
               &  -         (F(3,J+1) - F(3,J-1)) / (2.0_DP * Dy) ) / (2.0_DP * Dx)

      Fxy(N,J) = ( 3.0_DP * (F(N,J+1) - F(N,J-1))     / (2.0_DP * Dy) &
               &  -4.0_DP * (F(N-1,J+1) - F(N-1,J-1)) / (2.0_DP * Dy) &
               &  +         (F(N-2,J+1) - F(N-2,J-1)) / (2.0_DP * Dy) ) / (2.0_DP * Dx)
    END DO
    
    !corner
    Fxy(1,1) = (-3.0_DP * (-3.0_DP * F(1,1) + 4.0_DP * F(1,2) - F(1,3) ) / (2.0_DP * Dy) &
             &  +4.0_DP * (-3.0_DP * F(2,1) + 4.0_DP * F(2,2) - F(2,3) ) / (2.0_DP * Dy) &
             &  -         (-3.0_DP * F(3,1) + 4.0_DP * F(3,2) - F(3,3) ) / (2.0_DP * Dy) &
             & ) / (2.0_DP * Dx)
    
    Fxy(N,1) = ( 3.0_DP * (-3.0_DP * F(N,1) + 4.0_DP * F(N,2) - F(N,3)       ) / (2.0_DP * Dy) &
             &  -4.0_DP * (-3.0_DP * F(N-1,1) + 4.0_DP * F(N-1,2) - F(N-1,3) ) / (2.0_DP * Dy) &
             &  +         (-3.0_DP * F(N-2,1) + 4.0_DP * F(N-2,2) - F(N-2,3) ) / (2.0_DP * Dy) &
             & ) / (2.0_DP * Dx)
    
    Fxy(1,M) = (-3.0_DP * (3.0_DP * F(1,M) - 4.0_DP * F(1,M-1) + F(1,M-2) ) / (2.0_DP * Dy) &
             &  +4.0_DP * (3.0_DP * F(2,M) - 4.0_DP * F(2,M-1) + F(2,M-2) ) / (2.0_DP * Dy) &
             &  -         (3.0_DP * F(3,M) - 4.0_DP * F(3,M-1) + F(3,M-2) ) / (2.0_DP * Dy) &
             & ) / (2.0_DP * Dx)

    Fxy(N,M) = ( 3.0_DP * (3.0_DP * F(N,M) - 4.0_DP * F(N,M-1) + F(N,M-2)       ) / (2.0_DP * Dy) &
             &  -4.0_DP * (3.0_DP * F(N-1,M) - 4.0_DP * F(N-1,M-1) + F(N-1,M-2) ) / (2.0_DP * Dy) &
             &  +         (3.0_DP * F(N-2,M) - 4.0_DP * F(N-2,M-1) + F(N-2,M-2) ) / (2.0_DP * Dy) &
             & ) / (2.0_DP * Dx)

  END SUBROUTINE FIND_Fxy_2nd



  SUBROUTINE FIND_Fxy_3D_2nd(N , M , Q , Fxy , Fyz , Fxz , F , Dx, Dy , Dz)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to Get Mixed derivatives with uniform Grids.
  !
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    
    INTEGER      , INTENT(IN)  :: N , M , Q
    REAL(KIND=DP), INTENT(IN)  :: Dx, Dy , Dz
    REAL(KIND=DP), INTENT(OUT) :: Fxy(:,:,:) , Fyz(:,:,:) , Fxz(:,:,:)
    REAL(KIND=DP), INTENT(IN)  :: F(:,:,:)
    
    REAL(KIND=DP) :: Fxy_2D(N,M) , Fyz_2D(M,Q) , Fxz_2D(N,Q)
    REAL(KIND=DP) :: Fxy_temp(N,M) , Fyz_temp(M,Q) , Fxz_temp(N,Q)
    INTEGER                    :: I, J , K

   
    Fxy = 0.0_DP
    Fyz = 0.0_DP
    Fxz = 0.0_DP
   
    !For Fxy
    DO K = 1 , Q
     
      Fxy_temp(1:N,1:M) = F(1:N,1:M,K)
     
      CALL FIND_Fxy_2nd(N , M , Fxy_2D , Fxy_temp , Dx, Dy)
   
      Fxy(1:N,1:M,K) = Fxy_2D(1:N,1:M)
   
    END DO
   
    !For Fyz
    DO I = 1 , N
   
      Fyz_temp(1:M,1:Q) = F(I,1:M,1:Q)
     
      CALL FIND_Fxy_2nd(M , Q , Fyz_2D , Fyz_temp , Dy, Dz)
     
      Fyz(I,1:M,1:Q) = Fyz_2D(1:M,1:Q)
    
    END DO
   
    !For Fxz
    DO J = 1 , M
   	
      Fxz_Temp(1:N,1:Q) = F(1:N,J,1:Q)
      
      CALL FIND_Fxy_2nd(N , Q , Fxz_2D , Fxz_temp , Dx, Dz)
     
      Fxz(1:N,J,1:Q) = Fxz_2D(1:N,1:Q)

    END DO 

  END SUBROUTINE FIND_Fxy_3D_2nd


  SUBROUTINE Compact_Explicit_3D(N , M , Q , V , V_X , V_Y , V_Z , Dx , Dy , Dz)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to Get First derivatives with uniform Grids.
  !The  B.C. grids use 4th order compact scheme 
  !The inner grids use 4th order compact scheme
  !
  !
  !INPUT  : N , M , Q , V , Dx , Dy , Dz
  !OUTPUT : V_X , V_Y , V_Z
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    
    INTEGER      ,INTENT(IN)  :: N , M , Q
    REAL(KIND=DP),INTENT(IN)  :: V(:,:,:) , Dx , Dy , Dz
    REAL(KIND=DP),INTENT(OUT) :: V_X(:,:,:) , V_Y(:,:,:) , V_Z(:,:,:)
    
    REAL(KIND=DP) , DIMENSION(N)   :: Vx1D , Vx1D_X
    REAL(KIND=DP) , DIMENSION(M)   :: Vy1D , Vy1D_Y
    REAL(KIND=DP) , DIMENSION(Q)   :: Vz1D , Vz1D_Z
    
    INTEGER                                    :: I , J , K


      DO J = 1 , M
      DO K = 1 , Q
        Vx1D(1:N) = V(1:N,J,K)
      
        CALL Compact_Explicit(N , Vx1D_X, Vx1D, Dx)
       
        V_X(1:N,J,K) = Vx1D_X(1:N)
       
      END DO
      END DO

      
      DO I = 1 , N
      DO K = 1 , Q
        Vy1D(1:M) = V(I,1:M,K)
       
        CALL Compact_Explicit(M , Vy1D_Y, Vy1D, Dy)
       
        V_Y(I,1:M,K) = Vy1D_Y(1:M)
       	
      END DO
      END DO
 
      
      DO I = 1 , N
      DO J = 1 , M
        Vz1D(1:Q) = V(I,J,1:Q)
       
        CALL Compact_Explicit(Q , Vz1D_Z, Vz1D, Dz)
       
        V_Z(I,J,1:Q) = Vz1D_Z(1:Q)
       	
      END DO
      END DO

  END SUBROUTINE Compact_Explicit_3D


  
  SUBROUTINE Compact_Explicit(N , P_X, P, Dx)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to Get First derivatives with uniform Grids.
  !
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    
    INTEGER      , INTENT(IN)  :: N 
    REAL(KIND=DP), INTENT(IN)  :: Dx
    REAL(KIND=DP), INTENT(OUT) :: P_X(:)
    REAL(KIND=DP), INTENT(IN)  :: P(:)
        
    INTEGER                    :: I

   
   P_X = 0.0_DP

      P_X(2 ) = (- 41.0_DP * P(1 ) - 171.0_DP * P(2 ) + 315.0_DP * P(3 ) &
               &  -140.0_DP * P(4 ) + 45.0_DP * P(5 ) - 9.0_DP * P(6 ) &
               &  + P(7 ) ) / 180.0_DP / Dx

                 
      P_X(N-1 ) = -(- 41.0_DP * P(N ) - 171.0_DP * P(N-1 ) + 315.0_DP * P(N-2 ) &
               &     -140.0_DP * P(N-3 ) + 45.0_DP * P(N-4 ) - 9.0_DP * P(N-5 ) &
               &     + P(N-6 ) ) / 180.0_DP / Dx
                 

      
      DO I = 3 , N/2
        P_X(I ) = (  11.0_DP * P(I-2 ) - 99.0_DP * P(I-1 ) - 45.0_DP * P(I ) &
                 &  +170.0_DP * P(I+1 ) - 45.0_DP * P(I+2 ) +  9.0_DP * P(I+3 ) &
                 &  - P(I+4 ) ) / 180.0_DP / Dx
      END DO
      
      DO I = N/2 + 1 , N-2
        P_X(I ) = -(  11.0_DP * P(I+2 ) - 99.0_DP * P(I+1 ) - 45.0_DP * P(I ) &
                 &     +170.0_DP * P(I-1 ) - 45.0_DP * P(I-2 ) +  9.0_DP * P(I-3 ) &
                 &     - P(I-4 ) ) / 180.0_DP / Dx
      END DO
     
      
      IF(MOD(N,2) == 1) THEN
        P_X(N/2+1 ) = (-P(N/2+1-3 ) + 9.0_DP * P(N/2+1-2 ) - 45.0_DP * P(N/2+1-1 ) + 45.0_DP * P(N/2+1+1 ) &
                 &  -9.0_DP * P(N/2+1+2 ) + P(N/2+1+3 ) ) / 60.0_DP / Dx        
      END IF


  END SUBROUTINE Compact_Explicit



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Boundary condition Setting
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  SUBROUTINE BC_STORE_3D( N , M , Q , T , Dx , Dy , Dz , Btype_3D &
                       &, D_BC , U_BC , L_BC , R_BC , F_BC , B_BC )
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to store the B.C.
  !
  !
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    
    INTEGER,INTENT(IN) :: N , M , Q , Btype_3D(6)
    
    REAL(KIND=DP),INTENT(IN   ) :: Dx , Dy , Dz , D_BC(:,:) , U_BC(:,:) , L_BC(:,:) , R_BC(:,:) , F_BC(:,:) , B_BC(:,:)
    REAL(KIND=DP),INTENT(INOUT) :: T(:,:,:)
    
    INTEGER :: I , J , K
    
      !Store Down Part(X,1,Z)
      IF(Btype_3D(1) == 1) THEN
        DO K = 2 , Q-1
        DO I = 2 , N-1
          T(I,1,K) = D_BC(I,K)
        END DO
        END DO
        
      ELSE IF(Btype_3D(1) == 2) THEN
        DO I = 2 , N-1
        DO K = 2 , Q-1
          T(I,1,K) =   4.0_DP / 3.0_DP * T(I,2,K) - 1.0_DP / 3.0_DP * T(I,3,K) &
                   & - 2.0_DP / 3.0_DP * Dy * D_BC(I,K)
        END DO
        END DO
      END IF
      
      !Store Up Part(X,M,Z)
      IF(Btype_3D(2) == 1) THEN
        DO K = 2 , Q-1
        DO I = 2 , N-1
          T(I,M,K) = U_BC(I,K)
        END DO
        END DO
        	
      ELSE IF(Btype_3D(2) == 2) THEN
        DO K = 2 , Q-1
        DO I = 2 , N-1
      	  T(I,M,K) =   4.0_DP / 3.0_DP * T(I,M-1,K) - 1.0_DP / 3.0_DP * T(I,M-2,K) &
      	           & + 2.0_DP / 3.0_DP * Dy * U_BC(I,K)
        END DO
        END DO
      END IF
      
      !Store Left Part(1,Y,Z)
      IF(Btype_3D(3) == 1) THEN
        DO K = 2 , Q-1
        DO J = 2 , M-1
          T(1,J,K) = L_BC(J,K)
        END DO
        END DO
      
      ELSE IF(Btype_3D(3) == 2) THEN
        DO K = 2 , Q-1
        DO J = 2 , M-1
          T(1,J,K) =   4.0_DP / 3.0_DP * T(2,J,K) - 1.0_DP / 3.0_DP * T(3,J,K) &
                   & - 2.0_DP / 3.0_DP * Dx * L_BC(J,K)
        END DO
        END DO
      END IF
      
      !Store Right Part(N,Y,Z)
      IF(Btype_3D(4) == 1) THEN
        DO K = 2 , Q-1
        DO J = 2 , M-1
          T(N,J,K) = R_BC(J,K)
        END DO
        END DO
      
      ELSE IF(Btype_3D(4) == 2) THEN
        DO K = 2 , Q-1
        DO J = 2 , M-1
      	  T(N,J,K) =   4.0_DP / 3.0_DP * T(N-1,J,K) - 1.0_DP / 3.0_DP * T(N-2,J,K) &
      	           & + 2.0_DP / 3.0_DP * Dx * R_BC(J,K)
        END DO
        END DO
      END IF

      !Store Forward Part(X,Y,1)
      IF(Btype_3D(5) == 1) THEN
        DO I = 2 , N-1
        DO J = 2 , M-1
          T(I,J,1) = F_BC(I,J)
        END DO
        END DO
      
      ELSE IF(Btype_3D(5) == 2) THEN
        DO I = 2 , N-1
        DO J = 2 , M-1
      	  T(I,J,1) =   4.0_DP / 3.0_DP * T(I,J,2) - 1.0_DP / 3.0_DP * T(I,J,3) &
                   & - 2.0_DP / 3.0_DP * Dz * F_BC(I,J)
        END DO
        END DO
      END IF

      !Store Backward Part(X,Y,Q)
      IF(Btype_3D(6) == 1) THEN
        DO I = 2 , N-1
        DO J = 2 , M-1
          T(I,J,Q) = B_BC(I,J)
        END DO
        END DO
      
      ELSE IF(Btype_3D(6) == 2) THEN
        DO I = 2 , N-1
        DO J = 2 , M-1
      	  T(I,J,Q) =   4.0_DP / 3.0_DP * T(I,J,Q-1) - 1.0_DP / 3.0_DP * T(I,J,Q-2) &
      	           & + 2.0_DP / 3.0_DP * Dz * B_BC(I,J)
        END DO
        END DO
      END IF


      !Store corner Y-line
        !(1,Y,1)
        DO J = 2 , M-1
          T(1,J,1) = ( T(2,J,1) + T(1,J,2) ) / 2.0_DP
        END DO
        !(N,Y,1)
        DO J = 2 , M-1
          T(N,J,1) = ( T(N-1,J,1) + T(N,J,2) ) / 2.0_DP
        END DO
        !(1,Y,Q)
        DO J = 2 , M-1
          T(1,J,Q) = ( T(2,J,Q) + T(1,J,Q-1) ) / 2.0_DP
        END DO        
        !(N,Y,Q)
        DO J = 2 , M-1
          T(N,J,Q) = ( T(N-1,J,Q) + T(N,J,Q-1) ) / 2.0_DP
        END DO
       
       !Store corner X-line
        !(X,1,1)
        DO I = 2 , N-1
          T(I,1,1) = ( T(I,2,1) + T(I,1,2) ) / 2.0_DP
        END DO
        !(X,M,1)
        DO I = 2 , N-1
          T(I,M,1) = ( T(I,M-1,1) + T(I,M,2) ) / 2.0_DP
        END DO
        !(X,1,Q)
        DO I = 2 , N-1
          T(I,1,Q) = ( T(I,2,Q) + T(I,1,Q-1) ) / 2.0_DP
        END DO        
        !(X,M,Q)
        DO I = 2 , N-1
          T(I,M,Q) = ( T(I,M-1,Q) + T(I,M,Q-1) ) / 2.0_DP
        END DO        


       !Store corner Z-line
        !(1,1,Z)
        DO K = 2 , Q-1
          T(1,1,K) = ( T(2,1,K) + T(1,2,K) ) / 2.0_DP
        END DO
        !(N,1,Z)
        DO K = 2 , Q-1
          T(N,1,K) = ( T(N-1,1,K) + T(N,2,K) ) / 2.0_DP
        END DO
        !(1,M,Z)
        DO K = 2 , Q-1
          T(1,M,K) = ( T(2,M,K) + T(1,M-1,K) ) / 2.0_DP
        END DO        
        !(N,M,Z)
        DO K = 2 , Q-1
          T(N,M,K) = ( T(N-1,M,K) + T(N,M-1,K) ) / 2.0_DP
        END DO  

      !Store corner
      T(1,1,1) = ( T(2,1,1)   + T(1,2,1)   + T(1,1,2)   ) / 3.0_DP
      T(N,1,1) = ( T(N-1,1,1) + T(N,2,1)   + T(N,1,2)   ) / 3.0_DP
      T(1,1,Q) = ( T(2,1,Q)   + T(1,2,Q)   + T(1,1,Q-1) ) / 3.0_DP
      T(N,1,Q) = ( T(N-1,1,Q) + T(N,2,Q)   + T(N,1,Q-1) ) / 3.0_DP
      
      T(1,M,1) = ( T(2,M,1)   + T(1,M-1,1) + T(1,M,2)   ) / 3.0_DP
      T(N,M,1) = ( T(N-1,M,1) + T(N,M-1,1) + T(N,M,2)   ) / 3.0_DP        
      T(1,M,Q) = ( T(2,M,Q)   + T(1,M-1,Q) + T(1,M,Q-1) ) / 3.0_DP
      T(N,M,Q) = ( T(N-1,M,Q) + T(N,M-1,Q) + T(N,M,Q-1) ) / 3.0_DP  

      
  END SUBROUTINE BC_STORE_3D  