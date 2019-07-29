  SUBROUTINE EXPLICIT_DRP_13_3D( N , M , Q , C_X , C_Y , C_Z , K_X , K_Y , K_Z &
                             &, R , Dx , Dy , Dz&
                             & , T , S , RHS )
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Routine to compute the CDR eqaution:
  !
  !   Tt + C_X * Tx + C_Y * Ty - K_X * Txx + K_Y * Tyy ) + R* T = S
  !
  !by explicit method's RHS using DRP_13 scheme.
  !
  !
  !The result RHS can using as follow form:
  !
  !  dT/dt = RHS , so any explicit method can use to solve the problem.
  !
  !  For Euler-forward method, the result form can use:
  !  
  !  T = T_old + Dt * RHS
  !
  !  to get the new time step solution.
  !
  !
  !  The RHS term also can using as steady state - residual.
  !  by using like
  !  
  !  Res = SQRT( SUM( RHS^2 ) )
  !
  !  to test steady state.
  !
  !
  !
  !
  !
  !INPUT  : C_X , C_Y , K_X , K_Y , R , S , Dx , Dy , S , U
  ! 
  !OUTPUT : RHS , Ue , Uw , Vn , Vs
  !
  !
  !LOCAL use variable :
  !  for compute value using in computing : 
  !  for linear interpolation      : LIN_s , LIN_n , LIN_w , LIN_s
  !      normal curvature term     : CURVN_s , CURVN_n , CURVN_w , CURVN_e
  !      transverse curvature term : CURVT_s , CURVT_n , CURVT_w , CURVT_e
  ! 
  !
  !--------------------------------------------------
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    
    INTEGER       , INTENT(IN   ) :: N , M , Q
    REAL(KIND=DP) , INTENT(IN   ) :: C_X(:,:,:) , C_Y(:,:,:) , C_Z(:,:,:) 
    REAL(KIND=DP) , INTENT(IN   ) :: K_X(:,:,:) , K_Y(:,:,:) , K_Z(:,:,:) , R(:,:,:)
    REAL(KIND=DP) , INTENT(IN   ) :: Dx , Dy , Dz , T(:,:,:) , S(:,:,:)

    REAL(KIND=DP) , INTENT(OUT  ) :: RHS(:,:,:)
    
    REAL(KIND=DP) , ALLOCATABLE , DIMENSION(:,:,:) :: TT
    REAL(KIND=DP) , ALLOCATABLE , DIMENSION(:,:,:) :: U1 , V1 , W1 , U , V , W 
    
    REAL(KIND=DP) :: h , S1 , Temp

    INTEGER , DIMENSION(N,M,Q) :: Mask

    REAL(KIND=DP) :: C4 , C5 , C6 , C12 , C13 , C16 , C18
    REAL(KIND=DP) :: CC4 , CC5 , CC6 , CC12 , CC13 , CC17 , CC19 , SS
    
    INTEGER :: I , J , K , Iver
    
    
    ALLOCATE(U(N,M,Q) , V(N,M,Q) , W(N,M,Q) , U1(N,M,Q) , V1(N,M,Q) , W1(N,M,Q) , STAT=Iver)
    IF(Iver /= 0) STOP 'In EXPLICIT_DRP_13_3D, ALLOCATE U~W1 ERROR!'
    
    ALLOCATE(TT(0:N+1,0:M+1,0:Q+1) , STAT=Iver)
    IF(Iver /= 0) STOP 'In EXPLICIT_DRP_13_3D, ALLOCATE TT ERROR!'
    
    U = 0.0_DP
    V = 0.0_DP
    W = 0.0_DP
    TT = 0.0_DP
    RHS = 0.0_DP
    
    U(1:N,1:M,1:Q) = C_X(1:N,1:M,1:Q)
    V(1:N,1:M,1:Q) = C_Y(1:N,1:M,1:Q)
    W(1:N,1:M,1:Q) = C_Z(1:N,1:M,1:Q)
    
    TT(1:N,1:M,1:Q) = T(1:N,1:M,1:Q)
    
    S1 = 0.352318704775206e-1_DP


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !DRP_19
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                     C4 = -1.22949826221130_DP        !(f[i-1])
                     C5 = 0.452039424915845_DP      !(f[i])
                     C6 = 0.473884605430140_DP        !(f[i+1])
                    C12 = 0.475541301732594_DP        !(f[i-2])
                    C13 = -0.492086079044118e-1_DP      !(f[i+2])
                    C16 = -0.144916896091044_DP        !(f[i-3])
                      
                    C18 = 0.221584341278977e-1_DP       !(f[i-4])


                    CC6 = 1.22949826221130_DP        !(f[i+1])
                    CC5 =-0.452039424915845_DP       !(f[i])
                    CC4 =-0.473884605430140_DP       !(f[i-1])
                   CC13 =-0.475541301732594_DP       !(f[i+2])
                   CC12 = 0.492086079044118e-1_DP      !(f[i-2])
                   CC17 = 0.144916896091044_DP       !(f[i+3])
                       
                   CC19 = -0.221584341278977e-1_DP     !(f[i+4])


                   SS = 0.400212481180128e-2_DP      !weighting

        Mask = 0
        !Mask(4:N-3,4:M-3,4:Q-3) = 1
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Taylor Extrapolation
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    DO J = 1 , M
    DO K = 1 , Q
      TT(0,J,K  ) = Taylor_Extrapolation_X_3D( N , J , K , T , 1)
      TT(N+1,J,K) = Taylor_Extrapolation_X_3D( N , J , K , T , 2)
    END DO
    END DO
    
    DO I = 1 , N
    DO K = 1 , Q
      TT(I,0,K  ) = Taylor_Extrapolation_Y_3D( M , I , K , T , 1)
      TT(I,M+1,K) = Taylor_Extrapolation_Y_3D( M , I , K , T , 2)
    END DO
    END DO

    DO I = 1 , N
    DO J = 1 , M
      TT(I,J,0  ) = Taylor_Extrapolation_Z_3D( Q , I , J , T , 1)
      TT(I,J,Q+1) = Taylor_Extrapolation_Z_3D( Q , I , J , T , 2)
    END DO
    END DO
    	
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !DRP_13
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        h = dx
     


      !compute the convective coeff.
        DO I = 2 , N-1
        DO J = 2 , M-1
        DO K = 2 , Q-1
        
        IF(Mask(I,J,K) == 0) THEN
          !fx
          U1(I,J,K) = (U(I,J,K) * (  TT(I-2,J,K) - 8.0_DP * TT(I-1,J,K) &
                 &            + 8.0_DP * TT(I+1,J,K) - TT(I+2,J,K) ) / 12.0_DP &
                 & + ABS(U(I,J,K)) * (   TT(I-2,J,K) - 4.0_DP * TT(I-1,J,K) + 6.0_DP * TT(I,J,K) &
                 &                  - 4.0_DP * TT(I+1,J,K) + TT(I+2,J,K) ) / 12.0_DP &
                 
                 & + ABS(U(I,J,K)) * S1 * (   TT(I,J-2,K) - 4.0_DP * TT(I,J-1,K) + 6.0_DP * TT(I,J,K) &
                 &                  - 4.0_DP * TT(I,J+1,K) + TT(I,J+2,K) )  &
                 
                 & + ABS(U(I,J,K)) * S1 * (   TT(I,J,K-2) - 4.0_DP * TT(I,J,K-1) + 6.0_DP * TT(I,J,K) &
                 &                  - 4.0_DP * TT(I,J,K+1) + TT(I,J,K+2) )  ) / h


          !fy
          V1(I,J,K) = (V(I,J,K) * (  TT(I,J-2,K) - 8.0_DP * TT(I,J-1,K) &
                 &            + 8.0_DP * TT(I,J+1,K) - TT(I,J+2,K) ) / 12.0_DP &
                 & + ABS(V(I,J,K)) * (   TT(I,J-2,K) - 4.0_DP * TT(I,J-1,K) + 6.0_DP * TT(I,J,K) &
                 &                  - 4.0_DP * TT(I,J+1,K) + TT(I,J+2,K) ) / 12.0_DP &
                 
                 & + ABS(V(I,J,K)) * S1 * (   TT(I-2,J,K) - 4.0_DP * TT(I-1,J,K) + 6.0_DP * TT(I,J,K) &
                 &                  - 4.0_DP * TT(I+1,J,K) + TT(I+2,J,K) )  &
                 
                 & + ABS(V(I,J,K)) * S1 * (   TT(I,J,K-2) - 4.0_DP * TT(I,J,K-1) + 6.0_DP * TT(I,J,K) &
                 &                  - 4.0_DP * TT(I,J,K+1) + TT(I,J,K+2) )  ) / h


          !fx
          W1(I,J,K) = (W(I,J,K) * (  TT(I,J,K-2) - 8.0_DP * TT(I,J,K-1) &
                 &            + 8.0_DP * TT(I,J,K+1) - TT(I,J,K+2) ) / 12.0_DP &
                 & + ABS(W(I,J,K)) * (   TT(I,J,K-2) - 4.0_DP * TT(I,J,K-1) + 6.0_DP * TT(I,J,K) &
                 &                  - 4.0_DP * TT(I,J,K+1) + TT(I,J,K+2) ) / 12.0_DP &
                 
                 & + ABS(W(I,J,K)) * S1 * (   TT(I,J-2,K) - 4.0_DP * TT(I,J-1,K) + 6.0_DP * TT(I,J,K) &
                 &                  - 4.0_DP * TT(I,J+1,K) + TT(I,J+2,K) )  &
                 
                 & + ABS(W(I,J,K)) * S1 * (   TT(I-2,J,K) - 4.0_DP * TT(I-1,J,K) + 6.0_DP * TT(I,J,K) &
                 &                  - 4.0_DP * TT(I+1,J,K) + TT(I+2,J,K) )  ) / h


        
        ELSE
          IF(U(I,J,K) >= 0.0_DP) THEN
            U1(I,J,K) = ( C18 * TT(I-4,J,K) + C16 * TT(I-3,J,K) + C12 * TT(I-2,J,K) &
                    &    +C4  * TT(I-1,J,K) + C5  * TT(I,J,K)   + C6  * TT(I+1,J,K) &
                    &    +C13 * TT(I+2,J,K) ) / h

        	U1(I,J,K) =  U1(I,J,K) &
         &+ SS * ( TT(I,J-3,K) - 6.0_DP * TT(I,J-2,K) + 15.0_DP * TT(I,J-1,K) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I,J+1,K) - 6.0_DP * TT(I,J+2,K) &
         &                +TT(I,J+3,K)) / h

        	U1(I,J,K) =  U1(I,J,K) &
         &+ SS * ( TT(I,J,K-3) - 6.0_DP * TT(I,J,K-2) + 15.0_DP * TT(I,J,K-1) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I,J,K+1) - 6.0_DP * TT(I,J,K+2) &
         &                +TT(I,J,K+3)) / h

          U1(I,J,K) = U(I,J,K) * U1(I,J,K)
          
          
          ELSE
            U1(I,J,K) = ( CC19 * TT(I+4,J,K) + CC17 * TT(I+3,J,K) + CC12 * TT(I-2,J,K) &
                    &  +CC4  * TT(I-1,J,K) + CC5  * TT(I,J,K)   + CC6  * TT(I+1,J,K) &
                    &  +CC13 * TT(I+2,J,K) ) / h 

        	U1(I,J,K) =  U1(I,J,K) &
         &- SS * ( TT(I,J-3,K) - 6.0_DP * TT(I,J-2,K) + 15.0_DP * TT(I,J-1,K) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I,J+1,K) - 6.0_DP * TT(I,J+2,K) &
         &                +TT(I,J+3,K)) / h

        	U1(I,J,K) =  U1(I,J,K) &
         &- SS * ( TT(I,J,K-3) - 6.0_DP * TT(I,J,K-2) + 15.0_DP * TT(I,J,K-1) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I,J,K+1) - 6.0_DP * TT(I,J,K+2) &
         &                +TT(I,J,K+3)) / h   
         
          U1(I,J,K) = U(I,J,K) * U1(I,J,K)                        	
          
          END IF



          IF(V(I,J,K) >= 0.0_DP) THEN
            V1(I,J,K) = ( C18 * TT(I,J-4,K) + C16 * TT(I,J-3,K) + C12 * TT(I,J-2,K) &
                    &  +C4  * TT(I,J-1,K) + C5  * TT(I,J,K)   + C6  * TT(I,J+1,K) &
                    &  +C13 * TT(I,J+2,K) ) / h

        	V1(I,J,K) =  V1(I,J,K) &
         &+ SS * ( TT(I-3,J,K) - 6.0_DP * TT(I-2,J,K) + 15.0_DP * TT(I-1,J,K) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I+1,J,K) - 6.0_DP * TT(I+2,J,K) &
         &                +TT(I+3,J,K)) / h  

        	V1(I,J,K) =  V1(I,J,K) &
         &+ SS * ( TT(I,J,K-3) - 6.0_DP * TT(I,J,K-2) + 15.0_DP * TT(I,J,K-1) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I,J,K+1) - 6.0_DP * TT(I,J,K+2) &
         &                +TT(I,J,K+3)) / h
          
          V1(I,J,K) = V(I,J,K) * V1(I,J,K)
                                     
          ELSE
            V1(I,J,K) = ( CC19 * TT(I,J+4,K) + CC17 * TT(I,J+3,K) + CC12 * TT(I,J-2,K) &
                    &  +CC4  * TT(I,J-1,K) + CC5  * TT(I,J,K)   + CC6  * TT(I,J+1,K) &
                    &  +CC13 * TT(I,J+2,K) ) / h   

        	V1(I,J,K) =  V1(I,J,K) &
         &- SS * ( TT(I-3,J,K) - 6.0_DP * TT(I-2,J,K) + 15.0_DP * TT(I-1,J,K) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I+1,J,K) - 6.0_DP * TT(I+2,J,K) &
         &                +TT(I+3,J,K)) / h   

        	V1(I,J,K) =  V1(I,J,K) &
         &- SS * ( TT(I,J,K-3) - 6.0_DP * TT(I,J,K-2) + 15.0_DP * TT(I,J,K-1) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I,J,K+1) - 6.0_DP * TT(I,J,K+2) &
         &                +TT(I,J,K+3)) / h    

          V1(I,J,K) = V(I,J,K) * V1(I,J,K)
                                                	
          END IF


          IF(W(I,J,K) >= 0.0_DP) THEN
            W1(I,J,K) = ( C18 * TT(I,J,K-4) + C16 * TT(I,J,K-3) + C12 * TT(I,J,K-2) &
                    &  +C4  * TT(I,J,K-1) + C5  * TT(I,J,K)   + C6  * TT(I,J,K+1) &
                    &  +C13 * TT(I,J,K+2) ) / h

        	W1(I,J,K) =  W1(I,J,K) &
         &+ SS * ( TT(I-3,J,K) - 6.0_DP * TT(I-2,J,K) + 15.0_DP * TT(I-1,J,K) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I+1,J,K) - 6.0_DP * TT(I+2,J,K) &
         &                +TT(I+3,J,K)) / h  

        	W1(I,J,K) =  W1(I,J,K) &
         &+ SS * ( TT(I,J-3,K) - 6.0_DP * TT(I,J-2,K) + 15.0_DP * TT(I,J-1,K) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I,J+1,K) - 6.0_DP * TT(I,J+2,K) &
         &                +TT(I,J+3,K)) / h
          
          W1(I,J,K) = W(I,J,K) * W1(I,J,K)
                                     
          ELSE
            W1(I,J,K) = ( CC19 * TT(I,J,K+4) + CC17 * TT(I,J,K+3) + CC12 * TT(I,J,K-2) &
                    &  +CC4  * TT(I,J,K-1) + CC5  * TT(I,J,K)   + CC6  * TT(I,J,K+1) &
                    &  +CC13 * TT(I,J,K+2) ) / h   

        	W1(I,J,K) =  W1(I,J,K) &
         &- SS * ( TT(I-3,J,K) - 6.0_DP * TT(I-2,J,K) + 15.0_DP * TT(I-1,J,K) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I+1,J,K) - 6.0_DP * TT(I+2,J,K) &
         &                +TT(I+3,J,K)) / h  

        	W1(I,J,K) =  W1(I,J,K) &
         &- SS * ( TT(I,J-3,K) - 6.0_DP * TT(I,J-2,K) + 15.0_DP * TT(I,J-1,K) &
         &                -20.0_DP * TT(I,J,K) + 15.0_DP * TT(I,J+1,K) - 6.0_DP * TT(I,J+2,K) &
         &                +TT(I,J+3,K)) / h  

          W1(I,J,K) = W(I,J,K) * W1(I,J,K)
                                                	
          END IF
         
         
        END IF
        
        END DO
        END DO
        END DO
        	
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      
      
      
      !compute RHS:
        !diffusion term using central difference , and the source term
        DO I = 2 , N-1
        DO J = 2 , M-1
        DO K = 2 , Q-1

          IF(Mask(I,J,K) == 0) THEN
        	
          !2nd order     
          !RHS(I,J,K) =  ( K_X(I,J,K) * ( TT(I-1,J,K) - 2.0_DP * TT(I,J,K) + TT(I+1,J,K)) / (Dx**2)   &
          !          &    +K_Y(I,J,K) * ( TT(I,J-1,K) - 2.0_DP * TT(I,J,K) + TT(I,J+1,K)) / (Dy**2)   &
          !          &    +K_Z(I,J,K) * ( TT(I,J,K-1) - 2.0_DP * TT(I,J,K) + TT(I,J,K+1)) / (Dz**2) ) &
          !          &    - R(I,J,K) * TT(I,J,K)  & 
          !          &    + S(I,J,K)
          
          !4th order
          RHS(I,J,K) =  ( K_X(I,J,K) * ( -TT(I-2,J,K) + 16.0_DP * TT(I-1,J,K) - 30.0_DP * TT(I,J,K) &
                                   & + 16.0_DP * TT(I+1,J,K) - TT(I+2,J,K)) / (12.0_DP * Dx**2)     &
                    &    +K_Y(I,J,K) * ( -TT(I,J-2,K) + 16.0_DP * TT(I,J-1,K) - 30.0_DP * TT(I,J,K) &
                                   & + 16.0_DP * TT(I,J+1,K) - TT(I,J+2,K)) / (12.0_DP * Dy**2)     &
                    &    +K_Z(I,J,K) * ( -TT(I,J,K-2) + 16.0_DP * TT(I,J,K-1) - 30.0_DP * TT(I,J,K) &
                                   & + 16.0_DP * TT(I,J,K+1) - TT(I,J,K+2)) / (12.0_DP * DZ**2)   ) &
                    &    - R(I,J,K) * TT(I,J,K)  & 
                    &    + S(I,J,K)
          ELSE
         	
         	!6th order
          RHS(I,J,K) =  ( K_X(I,J,K) * (   2.0_DP * TT(I-3,J,K) - 27.0_DP * TT(I-2,J,K)  &
                                   &    +270.0_DP * TT(I-1,J,K) - 490.0_DP * TT(I,J,K)   &
                                   &    +270.0_DP * TT(I+1,J,K) - 27.0_DP * TT(I+2,J,K)  &
                                   &    +  2.0_DP * TT(I+3,J,K) ) / (180.0_DP * Dx**2)   &
                                   
                    &    +K_Y(I,J,K) * (   2.0_DP * TT(I,J-3,K) - 27.0_DP * TT(I,J-2,K)  &
                                   &    +270.0_DP * TT(I,J-1,K) - 490.0_DP * TT(I,J,K)   &
                                   &    +270.0_DP * TT(I,J+1,K) - 27.0_DP * TT(I,J+2,K)  &
                                   &    +  2.0_DP * TT(I,J+3,K) ) / (180.0_DP * Dy**2)   &
                                   
                    &    +K_Z(I,J,K) * (   2.0_DP * TT(I,J,K-3) - 27.0_DP * TT(I,J,K-2)  &
                                   &    +270.0_DP * TT(I,J,K-1) - 490.0_DP * TT(I,J,K)   &
                                   &    +270.0_DP * TT(I,J,K+1) - 27.0_DP * TT(I,J,K+2)  &
                                   &    +  2.0_DP * TT(I,J,K+3) ) / (180.0_DP * Dz**2)  )&
                                   
                    &    - R(I,J,K) * TT(I,J,K)  & 
                    &    + S(I,J,K)

          END IF
          
        END DO
        END DO
        END DO
        
        
        !convection term, DRP_13
        DO I = 2 , N-1
        DO J = 2 , M-1
        DO K = 2 , Q-1
          RHS(I,J,K) = RHS(I,J,K)-( U1(I,J,K) + V1(I,J,K) + W1(I,J,K) )
        END DO
        END DO
        END DO
        

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
        
  END SUBROUTINE EXPLICIT_DRP_13_3D



  FUNCTION Taylor_Extrapolation_X_3D( N , J , K , U , Flag)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Function to compute the Taylor series extra-polation by X-direction:
  !  IF Flag = 1 , compute :
  !    U(0,J)   = 3 * U(1,J) - 3 * U(2,J)   + U(3,J)   + O(h^3)
  !
  !     Flag = 2 , compute : 
  !    U(N+1,J) = 3 * U(N,J) - 3 * U(N-1,J) + U(N-2,J) + O(h^3)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    REAL(KIND=DP) :: Taylor_Extrapolation_X_3D
    REAL(KIND=DP) :: U(:,:,:)
    INTEGER , INTENT(IN) :: N , J , K , Flag
    
    
    IF(Flag == 1 ) THEN
      !Taylor_Extrapolation_X_3D = 3.0_DP * U(1,J,K) - 3.0_DP * U(2,J,K)   + U(3,J,K)
      Taylor_Extrapolation_X_3D = 2.0_DP * U(1,J,K) - U(2,J,K)
    ELSE IF(Flag == 2) THEN
    	!Taylor_Extrapolation_X_3D = 3.0_DP * U(N,J,K) - 3.0_DP * U(N-1,J,K) + U(N-2,J,K)
    	Taylor_Extrapolation_X_3D = 2.0_DP * U(N,J,K) - U(N-1,J,K)
    ELSE
      WRITE(*,*) "ERROR! , in function Taylor_Extra_Polation_X, Flag must 1 or 2"
      READ (*,*)
    END IF
   
  END FUNCTION  Taylor_Extrapolation_X_3D
   
   
   
   
  FUNCTION Taylor_Extrapolation_Y_3D( M , I , K , V , Flag)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Function to compute the Taylor series extra-polation by Y-direction:
  !IF Flag = 1 , compute :
  !  V(I,0)   = 3 * V(I,1) - 3 * V(I,2)   + V(I,3)   + O(h^3)
  !
  !   Flag = 2 , compute :
  !  V(I,M+1) = 3 * V(I,M) - 3 * V(I,M-1) + V(I,M-2) + O(h^3)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    REAL(KIND=DP) :: Taylor_Extrapolation_Y_3D
    REAL(KIND=DP) :: V(:,:,:)
    INTEGER , INTENT(IN) :: M , I , K , Flag
    
    
    IF(Flag == 1 ) THEN
      !Taylor_Extrapolation_Y_3D = 3.0_DP * V(I,1,K) - 3.0_DP * V(I,2,K)   + V(I,3,K)
      Taylor_Extrapolation_Y_3D = 2.0_DP * V(I,1,K) - V(I,2,K)
    ELSE IF(Flag == 2) THEN
    	!Taylor_Extrapolation_Y_3D = 3.0_DP * V(I,M,K) - 3.0_DP * V(I,M-1,K) + V(I,M-2,K)
    	Taylor_Extrapolation_Y_3D = 2.0_DP * V(I,M,K) - V(I,M-1,K)
    ELSE
      WRITE(*,*) "ERROR! , in function Taylor_Extra_Polation_Y, Flag must 1 or 2"
      READ (*,*)
    END IF
   
  END FUNCTION  Taylor_Extrapolation_Y_3D


  FUNCTION Taylor_Extrapolation_Z_3D( Q , I , J , W , Flag)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Function to compute the Taylor series extra-polation by Y-direction:
  !IF Flag = 1 , compute :
  !  V(I,0)   = 3 * V(I,1) - 3 * V(I,2)   + V(I,3)   + O(h^3)
  !
  !   Flag = 2 , compute :
  !  V(I,M+1) = 3 * V(I,M) - 3 * V(I,M-1) + V(I,M-2) + O(h^3)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    REAL(KIND=DP) :: Taylor_Extrapolation_Z_3D
    REAL(KIND=DP) :: W(:,:,:)
    INTEGER , INTENT(IN) :: Q , I , J , Flag
    
    
    IF(Flag == 1 ) THEN
      !Taylor_Extrapolation_Z_3D = 3.0_DP * W(I,J,1) - 3.0_DP * W(I,J,2)   + W(I,J,3)
      Taylor_Extrapolation_Z_3D = 2.0_DP * W(I,J,1) - W(I,J,2)
    ELSE IF(Flag == 2) THEN
    	!Taylor_Extrapolation_Z_3D = 3.0_DP * W(I,J,Q) - 3.0_DP * W(I,J,Q-1) + W(I,J,Q-2)
    	Taylor_Extrapolation_Z_3D = 2.0_DP * W(I,J,Q) - W(I,J,Q-1)
    ELSE
      WRITE(*,*) "ERROR! , in function Taylor_Extra_Polation_Y, Flag must 1 or 2"
      READ (*,*)
    END IF
   
  END FUNCTION  Taylor_Extrapolation_Z_3D  
 