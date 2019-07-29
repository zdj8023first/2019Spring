



  SUBROUTINE DFC_Correction_3D(N , M , Q , Dx , Dy , Dz , Dt , Us , Vs , Ws , P &
                         &, U , V , W , U_old , V_old , W_old , KK , Fu , Fv , Fw , PP , DIV_X , DIV_Y , DIV_Z)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to do DFC Correction.
  !Note that The Fu , Fv , Fw means that Forcing terms.
  !
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
    INTEGER,PARAMETER  :: DP = SELECTED_REAL_KIND(P=15)  
    
    INTEGER , INTENT(IN) :: N , M , Q
    
    REAL(KIND=DP),INTENT(IN ) :: Us(:,:,:) , Vs(:,:,:) , Ws(:,:,:) , Dx , Dy , Dz , Dt
    REAL(KIND=DP),INTENT(IN ) :: KK(:,:,:) , Fu(:,:,:) , Fv(:,:,:) , Fw(:,:,:)
    REAL(KIND=DP),INTENT(IN ) :: U_old(:,:,:) , V_old(:,:,:) , W_old(:,:,:)    
    REAL(KIND=DP),INTENT(OUT) :: U(:,:,:) , V(:,:,:) , W(:,:,:) , PP(:,:,:)
    REAL(KIND=DP),INTENT(OUT) :: DIV_X(:,:,:) , DIV_Y(:,:,:) , DIV_Z(:,:,:)
    REAL(KIND=DP),INTENT(INOUT) :: P(:,:,:)

    INTEGER :: Btype_3D(6) , Iver
    
    REAL(KIND=DP) :: La
    
    REAL(KIND=DP), ALLOCATABLE , DIMENSION(:,:) :: L_BC , R_BC , U_BC , D_BC, F_BC , B_BC
    REAL(KIND=DP), ALLOCATABLE , DIMENSION(:,:,:) :: P_X , P_Y , P_Z , DIV , U_X , V_Y , W_Z
    
    REAL(KIND=DP), ALLOCATABLE , DIMENSION(:,:,:) :: U_Y , U_Z , V_X , V_Z , W_X , W_Y
    
    REAL(KIND=DP), ALLOCATABLE , DIMENSION(:,:,:) :: U_YY , U_ZZ , V_XX , V_ZZ , W_XX , W_YY
    
    REAL(KIND=DP), ALLOCATABLE , DIMENSION(:,:,:) :: S , U_XY , U_XZ , V_XY , V_YZ , W_XZ , W_YZ
    
    ALLOCATE(L_BC(M,Q) , R_BC(M,Q) , U_BC(N,Q) , D_BC(N,Q), F_BC(N,M) , B_BC(N,M) , STAT=Iver)
    IF(Iver /= 0) STOP 'In DFC_Correction_3D, ALLOCATE BC ERROR!'
    
    ALLOCATE(P_X(N,M,Q) , P_Y(N,M,Q) , P_Z(N,M,Q) , DIV(N,M,Q) , S(N,M,Q) , STAT=Iver)
    IF(Iver /= 0) STOP 'In DFC_Correction_3D, ALLOCATE P_X~DIV ERROR!'
    
    ALLOCATE(U_X(N,M,Q) , V_Y(N,M,Q) , W_Z(N,M,Q) , STAT=Iver)
    IF(Iver /= 0) STOP 'In DFC_Correction_3D, ALLOCATE U_X~W_Z ERROR!'    

    ALLOCATE(U_Y(N,M,Q) , U_Z(N,M,Q) , V_X(N,M,Q) , STAT=Iver)
    IF(Iver /= 0) STOP 'In DFC_Correction_3D, ALLOCATE U_X~W_Z ERROR!'

    ALLOCATE(V_Z(N,M,Q) , W_X(N,M,Q) , W_Y(N,M,Q) , STAT=Iver)
    IF(Iver /= 0) STOP 'In DFC_Correction_3D, ALLOCATE U_X~W_Z ERROR!'    

    ALLOCATE(U_YY(N,M,Q) , U_ZZ(N,M,Q) , V_XX(N,M,Q) , V_ZZ(N,M,Q) , STAT=Iver)
    IF(Iver /= 0) STOP 'In DFC_Correction_3D, ALLOCATE U_X~W_Z ERROR!'      

    ALLOCATE(W_XX(N,M,Q) , W_YY(N,M,Q) , U_XY(N,M,Q) , U_XZ(N,M,Q) , STAT=Iver)
    IF(Iver /= 0) STOP 'In DFC_Correction_3D, ALLOCATE U_X~W_Z ERROR!'  

    ALLOCATE(V_XY(N,M,Q) , V_YZ(N,M,Q) , W_XZ(N,M,Q) , W_YZ(N,M,Q) , STAT=Iver)
    IF(Iver /= 0) STOP 'In DFC_Correction_3D, ALLOCATE U_X~W_Z ERROR!' 
          
      CALL FIND_DIV_3D(N , M , Q , U_X , V_Y , W_Z , Us , Vs , Ws , Dx , Dy , Dz)


      DIV(2:N-1,2:M-1,2:Q-1) =  U_X(2:N-1,2:M-1,2:Q-1) + V_Y(2:N-1,2:M-1,2:Q-1) &
                             &+ W_Z(2:N-1,2:M-1,2:Q-1)


      Btype_3D(1) = 2
      D_BC = 0.0_DP
      
      Btype_3D(2) = 2
      U_BC = 0.0_DP
      
      Btype_3D(3) = 2
      L_BC = 0.0_DP
      
      Btype_3D(4) = 2
      R_BC = 0.0_DP
      
      Btype_3D(5) = 2
      F_BC = 0.0_DP
      
      Btype_3D(6) = 2
      B_BC = 0.0_DP
      
      CALL BC_STORE_3D( N , M , Q , DIV , Dx , Dy , Dz , Btype_3D &
                       &, D_BC , U_BC , L_BC , R_BC , F_BC , B_BC )


      La = 1.0_DP / (2.0_DP / (Dx**2) + 2.0_DP / (Dy**2) + 2.0_DP / (Dz**2)) / Dt 



      PP = -La * DIV 

      Btype_3D(1) = 2
      D_BC = 0.0_DP
      
      Btype_3D(2) = 2
      U_BC = 0.0_DP
      
      Btype_3D(3) = 2
      L_BC = 0.0_DP
      
      Btype_3D(4) = 2
      R_BC = 0.0_DP
      
      Btype_3D(5) = 2
      F_BC = 0.0_DP
      
      Btype_3D(6) = 2
      B_BC = 0.0_DP
      
      CALL BC_STORE_3D( N , M , Q , PP , Dx , Dy , Dz , Btype_3D &
                       &, D_BC , U_BC , L_BC , R_BC , F_BC , B_BC )

      
      FORALL( I = 2 : N-1 , J = 2 : M-1 , K = 2:Q-1)
        PP(I,J,K) = PP(I,J,K) + (  La*Dt / (Dx**2) *(PP(I+1,J,K) + PP(I-1,J,K)) &
                              &  + La*Dt / (Dy**2) *(PP(I,J+1,K) + PP(I,J-1,K)) &
                              &  + La*Dt / (Dz**2) *(PP(I,J,K+1) + PP(I,J,K-1)) )
      END FORALL
      
      

      CALL BC_STORE_3D( N , M , Q , PP , Dx , Dy , Dz , Btype_3D &
                       &, D_BC , U_BC , L_BC , R_BC , F_BC , B_BC )


      CALL FIND_FxFy_001_3D(N , M , Q , P_X, P_Y, P_Z , PP, Dx, Dy , Dz)
      
      
      U(2:N-1,2:M-1,2:Q-1) = Us(2:N-1,2:M-1,2:Q-1) - P_X(2:N-1,2:M-1,2:Q-1) * Dt
      V(2:N-1,2:M-1,2:Q-1) = Vs(2:N-1,2:M-1,2:Q-1) - P_Y(2:N-1,2:M-1,2:Q-1) * Dt
      W(2:N-1,2:M-1,2:Q-1) = Ws(2:N-1,2:M-1,2:Q-1) - P_Z(2:N-1,2:M-1,2:Q-1) * Dt

      
      CALL FIND_FxFy_001_3D(N , M , Q , U_X, U_Y, U_Z , U, Dx, Dy , Dz)
      CALL FIND_FxFy_001_3D(N , M , Q , V_X, V_Y, V_Z , V, Dx, Dy , Dz)
      CALL FIND_FxFy_001_3D(N , M , Q , W_X, W_Y, W_Z , W, Dx, Dy , Dz)

      CALL FIND_FxFy_002_3D(N , M , Q , S, U_YY, U_ZZ , U, Dx, Dy , Dz)
      CALL FIND_FxFy_002_3D(N , M , Q , V_XX, S, V_ZZ , V, Dx, Dy , Dz)
      CALL FIND_FxFy_002_3D(N , M , Q , W_XX, W_YY, S , W, Dx, Dy , Dz)

      CALL FIND_Fxy_3D_2nd(N , M , Q , U_XY , S , U_XZ , U , Dx, Dy , Dz)
      CALL FIND_Fxy_3D_2nd(N , M , Q , V_XY , V_YZ , S , V , Dx, Dy , Dz)
      CALL FIND_Fxy_3D_2nd(N , M , Q , S , W_YZ , W_XZ , W , Dx, Dy , Dz)
      

      P = P + PP

      Btype_3D(1) = 2
      D_BC(1:N,1:Q) = -( (V(1:N,1,1:Q) - V_old(1:N,1,1:Q)) / Dt&
                    &   + U(1:N,1,1:Q) * V_X(1:N,1,1:Q) + V(1:N,1,1:Q) * V_Y(1:N,1,1:Q) &
                    &   + W(1:N,1,1:Q) * V_Z(1:N,1,1:Q) &
                    &   -(  V_XX(1:N,1,1:Q) + V_ZZ(1:N,1,1:Q) &
                    &     - U_XY(1:N,1,1:Q) - W_YZ(1:N,1,1:Q)) * KK(1:N,1,1:Q)  - Fv(1:N,1,1:Q) ) 
      
      Btype_3D(2) = 2
      U_BC(1:N,1:Q) = -( (V(1:N,M,1:Q) - V_old(1:N,M,1:Q)) / Dt&
                    &   + U(1:N,M,1:Q) * V_X(1:N,M,1:Q) + V(1:N,M,1:Q) * V_Y(1:N,M,1:Q) &
                    &   + W(1:N,M,1:Q) * V_Z(1:N,M,1:Q) &
                    &   -(  V_XX(1:N,M,1:Q) + V_ZZ(1:N,M,1:Q) &
                    &     - U_XY(1:N,M,1:Q) - W_YZ(1:N,M,1:Q)) * KK(1:N,M,1:Q)  - Fv(1:N,M,1:Q) ) 
      
      Btype_3D(3) = 2
      L_BC(1:M,1:Q) = -( (U(1,1:M,1:Q) - U_old(1,1:M,1:Q)) / Dt&
                    &   + U(1,1:M,1:Q) * U_X(1,1:M,1:Q) + V(1,1:M,1:Q) * U_Y(1,1:M,1:Q) &
                    &   + W(1,1:M,1:Q) * U_Z(1,1:M,1:Q) &
                    &   -(  U_YY(1,1:M,1:Q) + U_ZZ(1,1:M,1:Q) &
                    &     - V_XY(1,1:M,1:Q) - W_XZ(1,1:M,1:Q)) * KK(1,1:M,1:Q)  - Fu(1,1:M,1:Q) ) 
      
      Btype_3D(4) = 2
      R_BC(1:M,1:Q) = -( (U(N,1:M,1:Q) - U_old(N,1:M,1:Q)) / Dt&
                    &   + U(N,1:M,1:Q) * U_X(N,1:M,1:Q) + V(N,1:M,1:Q) * U_Y(N,1:M,1:Q) &
                    &   + W(N,1:M,1:Q) * U_Z(N,1:M,1:Q) &
                    &   -(  U_YY(N,1:M,1:Q) + U_ZZ(N,1:M,1:Q) &
                    &     - V_XY(N,1:M,1:Q) - W_XZ(N,1:M,1:Q)) * KK(N,1:M,1:Q) - Fu(N,1:M,1:Q) ) 
      
      Btype_3D(5) = 2
      F_BC(1:N,1:M) = -( (W(1:N,1:M,1) - W_old(1:N,1:M,1)) / Dt&
                    &   + U(1:N,1:M,1) * W_X(1:N,1:M,1) + V(1:N,1:M,1) * W_Y(1:N,1:M,1) &
                    &   + W(1:N,1:M,1) * W_Z(1:N,1:M,1) &
                    &   -(  W_XX(1:N,1:M,1) + W_YY(1:N,1:M,1) &
                    &     - U_XZ(1:N,1:M,1) - V_YZ(1:N,1:M,1)) * KK(1:N,1:M,1) - Fw(1:N,1:M,1) ) 
      
      Btype_3D(6) = 2
      B_BC(1:N,1:M) = -( (W(1:N,1:M,Q) - W_old(1:N,1:M,Q)) / Dt&
                    &   + U(1:N,1:M,Q) * W_X(1:N,1:M,Q) + V(1:N,1:M,Q) * W_Y(1:N,1:M,Q) &
                    &   + W(1:N,1:M,Q) * W_Z(1:N,1:M,Q) &
                    &   -(  W_XX(1:N,1:M,Q) + W_YY(1:N,1:M,Q) &
                    &     - U_XZ(1:N,1:M,Q) - V_YZ(1:N,1:M,Q)) * KK(1:N,1:M,Q) - Fw(1:N,1:M,Q) ) 
      
      
      CALL BC_STORE_3D( N , M , Q , P , Dx , Dy , Dz , Btype_3D &
                       &, D_BC , U_BC , L_BC , R_BC , F_BC , B_BC )

      
      DIV_X = - P_X
      DIV_Y = - P_Y
      DIV_Z = - P_Z
      
      
  END SUBROUTINE DFC_Correction_3D