
  SUBROUTINE UNIFORM_GRID_3D(N , M , Q , Xstart , Xend , Ystart , Yend &
                          &, Zstart , Zend , X , Y , Z , Dx , Dy , Dz)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !PROGRAM to create the uniform GRID.
  !
  !
  !
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IMPLICIT NONE
    INTEGER ,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    INTEGER       , INTENT(IN ) :: N , M , Q
    REAL(KIND=DP) , INTENT(IN ) :: Xstart , Xend , Ystart , Yend , Zstart , Zend
    REAL(KIND=DP) , INTENT(OUT) :: X(:,:,:) , Y(:,:,:) , Z(:,:,:) , Dx , Dy , Dz
    
    INTEGER :: I , J , K
    
      Dx = (Xend - Xstart) / REAL(N-1 ,KIND=DP)
      Dy = (Yend - Ystart) / REAL(M-1 ,KIND=DP)
      Dz = (Zend - Zstart) / REAL(Q-1 ,KIND=DP)

      DO I = 1 , N
      DO J = 1 , M
      DO K = 1 , Q
        X(I,J,K) = Xstart + Dx * REAL(I-1,KIND=DP)
        Y(I,J,K) = Ystart + Dy * REAL(J-1,KIND=DP)
        Z(I,J,K) = Zstart + Dz * REAL(K-1,KIND=DP)
      END DO
      END DO
      END DO

      WRITE(*,*) "    ================================="
      WRITE(*,*) "    UNIFORM_GRID generateing complete"
      WRITE(*,*) "    ================================="
  
  
  END SUBROUTINE UNIFORM_GRID_3D



  SUBROUTINE NON_UNIFORM_GRID_T_B_3D(N , M , Q , Xstart , Xend , Ystart , Yend &
                              & , Zstart , Zend , Beta_X , Beta_Y , Beta_Z , X , Y , Z , Dx_L , Dx_R &
                              & , Dy_L , Dy_R , Dz_L , Dz_R , Xz , Xzz , Ye , Yee , Zg , Zgg)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !PROGRAM to create the non-uniform GRID.
  !Clustering at top and bottom walls type.
  !Transforming to (0,1)x(0,1)x(0,1) (Zeta , Eta) coordnate.
  !
  !INPUT  : N , M , Q , Beta_X , Beta_Y , Beta_Z , Xstart , Xend , Ystart , Yend , Zstart , Zend
  !OUTPUT : X , Y , Z , Dx_L , Dx_R , Dy_L , Dy_R , Dz_L , Dz_R , Xz , Xzz , Ye , Yee , Zg , Zgg
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IMPLICIT NONE
    INTEGER ,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
    INTEGER      ,INTENT(IN ) :: N , M , Q
    REAL(KIND=DP),INTENT(IN ) :: Xstart , Xend , Ystart , Yend , Zstart , Zend , Beta_X , Beta_Y , Beta_Z
    REAL(KIND=DP),INTENT(OUT) :: X(:,:,:) , Y(:,:,:)  , Z(:,:,:)
    REAL(KIND=DP),INTENT(OUT) :: Dx_L(:,:,:) , Dx_R(:,:,:) , Dy_L(:,:,:) , Dy_R(:,:,:) , Dz_L(:,:,:) , Dz_R(:,:,:)
    REAL(KIND=DP),INTENT(OUT) :: Xz(:,:,:) , Xzz(:,:,:) , Ye(:,:,:) , Yee(:,:,:) , Zg(:,:,:) , Zgg(:,:,:)
    REAL(KIND=DP)                            :: Afa , Zeta , Eta , Dzeta , Deta , Gamma , Dgamma
    REAL(KIND=DP)                            :: Hx , Hy , Hz
    REAL(KIND=DP)                            :: Max_Spacing , Max_Grid , Min_Grid
    INTEGER                                  :: I , J , K

    Hx = Xend - Xstart
    Hy = Yend - Ystart
    Hz = Zend - Zstart

    Dzeta  = 1.0_DP *Hx / Hx / REAL(N-1,KIND=DP)
    Deta   = 1.0_DP *Hy / Hx / REAL(M-1,KIND=DP)
    Dgamma = 1.0_DP *Hz / Hx / REAL(Q-1,KIND=DP)
    
    Afa = 0.5_DP
    
    
    DO I = 1 , N
    DO J = 1 , M
    DO K = 1 , Q
      Zeta  = Dzeta  * REAL(I-1,KIND=DP)
      Eta   = Deta   * REAL(J-1,KIND=DP)
      Gamma = Dgamma * REAL(K-1,KIND=DP)
  
      X(I,J,K) = Xstart + Hx * (((beta_X + 1) / (beta_X - 1)) ** (1 / (-1 + afa) * afa) &
     & * beta_X + 2 * ((beta_X + 1) / (beta_X - 1)) ** (1 / (-1 + afa) * afa) * afa  &
     & - beta_X * ((beta_X + 1) / (beta_X - 1)) ** (1 / Hx / (-1 + afa) * zeta)  &
     & + 2 * afa * ((beta_X + 1) / (beta_X - 1)) ** (1 / Hx / (-1 + afa) * zeta)) &
     & / (2 * ((beta_X + 1) / (beta_X - 1)) ** (1 / (-1 + afa) * afa) * afa &
     & + ((beta_X + 1) / (beta_X - 1)) ** (1 / (-1 + afa) * afa) + 2 * afa &
     & * ((beta_X + 1) / (beta_X - 1)) ** (1 / Hx / (-1 + afa) * zeta) &
     & + ((beta_X + 1) / (beta_X - 1)) ** (1 / Hx / (-1 + afa) * zeta))



     
      Y(I,J,K) = Ystart + Hy * (((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa) * afa) &
     & * beta_Y + 2 * ((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa) * afa) * afa  &
     & - beta_Y * ((beta_Y + 1) / (beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta) &
     & + 2 * afa * ((beta_Y + 1) / (beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta) ) &
     & / (2 * ((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa) * afa) * afa &
     & + ((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa) * afa) + 2 * afa &
     & * ((beta_Y + 1) / (beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta) + ((beta_Y  &
     & + 1) / (beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta))




      Z(I,J,K) = Zstart + Hz * (((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa) * afa) &
     & * beta_Z + 2 * ((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa) * afa) * afa  &
     & - beta_Z * ((beta_Z + 1) / (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma) &
     & + 2 * afa * ((beta_Z + 1) / (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma) ) &
     & / (2 * ((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa) * afa) * afa &
     & + ((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa) * afa) + 2 * afa &
     & * ((beta_Z + 1) / (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma) + ((beta_Z & 
     + 1) / (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma))



               
      Xz(I,J,K) = -0.2D1 * dble(((beta_X + 1) / (beta_X - 1)) ** ((zeta + afa * Hx) / &
     & Hx / (-1 + afa))) * dble(beta_X) * log(dble((beta_X + 1) / (beta_X - 1)) &
     &) / dble(((beta_X + 1) / (beta_X - 1)) ** (1 / (-1 + afa) * afa) &
     &+ ((beta_X + 1) / (beta_X - 1)) ** (1 / Hx / (-1 + afa) * zeta)) &
     &/ dble(2 * ((beta_X + 1) / (beta_X - 1)) ** (1 / (-1 + afa) * afa) * afa + ((beta_X &
     & + 1) / (beta_X - 1)) ** (1 / (-1 + afa) * afa) + 2 * afa * ((beta_X + &
     & 1) / (beta_X - 1)) ** (1 / Hx / (-1 + afa) * zeta) + ((beta_X + 1) / ( &
     &beta_X - 1)) ** (1 / Hx / (-1 + afa) * zeta)) / dble(-1 + afa)

      
      Xzz(I,J,K) = -0.2D1 * log(dble((beta_X + 1) / (beta_X - 1))) ** 2 * dble(beta_X) &
     &* dble(-((beta_X + 1) / (beta_X - 1)) ** ((2 * zeta + afa * Hx) / Hx / ( &
     &-1 + afa)) + ((beta_X + 1) / (beta_X - 1)) ** ((zeta + 2 * afa * Hx) /  &
     &Hx / (-1 + afa))) / dble((((beta_X + 1) / (beta_X - 1)) ** (1 / (-1 &
     &+ afa) * afa) + ((beta_X + 1) / (beta_X - 1)) ** (1 / Hx / (-1 + afa) &
     &* zeta)) ** 2) / dble(2 * ((beta_X + 1) / (beta_X - 1)) ** (1 / (-1 + afa) &
     & * afa) * afa + ((beta_X + 1) / (beta_X - 1)) ** (1 / (-1 + afa) * afa &
     &) + 2 * afa * ((beta_X + 1) / (beta_X - 1)) ** (1 / Hx / (-1 + afa) &
     &* zeta) + ((beta_X + 1) / (beta_X - 1)) ** (1 / Hx / (-1 + afa) * zeta)) / &
     & dble((-1 + afa) ** 2) / dble(Hx)
    



      Ye(I,J,K) = -0.2D1 * dble(((beta_Y + 1) / (beta_Y - 1)) ** ((eta + afa * Hy) / &
     &Hy / (-1 + afa))) * dble(beta_Y) * log(dble((beta_Y + 1) / (beta_Y - 1))) &
     & / dble(((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa) * afa) &
     &+ ((beta_Y + 1) / (beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta)) &
     &/ dble(2 * ((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa) * afa) * afa &
     &+ ((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa) * afa) + 2 * afa * ((beta_Y + 1 &
     &) / (beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta) + ((beta_Y + 1) / &
     &(beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta)) / dble(-1 + afa)



      Yee(I,J,K) = 0.2D1 * log(dble((beta_Y + 1) / (beta_Y - 1))) ** 2 * dble(beta_Y) * &
     & dble(((beta_Y + 1) / (beta_Y - 1)) ** ((2 * eta + afa * Hy) / Hy / (-1  &
     &+ afa)) - ((beta_Y + 1) / (beta_Y - 1)) ** ((eta + 2 * afa * Hy) / Hy /  &
     &(-1 + afa))) / dble((((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa)  &
     &* afa) + ((beta_Y + 1) / (beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta))  &
     &** 2) / dble(2 * ((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa) &
     &* afa) * afa + ((beta_Y + 1) / (beta_Y - 1)) ** (1 / (-1 + afa) * afa) &
     &+ 2 * afa * ((beta_Y + 1) / (beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta) + &
     & ((beta_Y + 1) / (beta_Y - 1)) ** (1 / Hy / (-1 + afa) * eta)) &
     &/ dble((-1 + afa) ** 2) / dble(Hy)

       

      Zg(I,J,K) = -0.2D1 * dble(((beta_Z + 1) / (beta_Z - 1)) ** ((gamma + afa * Hz) / &
     &Hz / (-1 + afa))) * dble(beta_Z) * log(dble((beta_Z + 1) / (beta_Z - 1))) &
     & / dble(((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa) * afa) &
     &+ ((beta_Z + 1) / (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma)) &
     &/ dble(2 * ((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa) * afa) * afa &
     &+ ((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa) * afa) + 2 * afa * ((beta_Z + 1 &
     &) / (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma) + ((beta_Z + 1) &
     &/ (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma)) / dble(-1 + afa)



      Zgg(I,J,K) = -0.2D1 * log(dble((beta_Z + 1) / (beta_Z - 1))) ** 2 * dble(beta_Z) & 
     &* dble(-((beta_Z + 1) / (beta_Z - 1)) ** ((2 * gamma + afa * Hz) / Hz / (- &
     &1 + afa)) + ((beta_Z + 1) / (beta_Z - 1)) ** ((gamma + 2 * afa * Hz) / Hz  &
     &/ (-1 + afa))) / dble((((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa &
     &) * afa) + ((beta_Z + 1) / (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma) &
     &) ** 2) / dble(2 * ((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa) *  &
     &afa) * afa + ((beta_Z + 1) / (beta_Z - 1)) ** (1 / (-1 + afa) * afa) + &
     & 2 * afa * ((beta_Z + 1) / (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma) &
     & + ((beta_Z + 1) / (beta_Z - 1)) ** (1 / Hz / (-1 + afa) * gamma)) / dble &
     &((-1 + afa) ** 2) / dble(Hz)


       
       
    END DO
    END DO
    END DO
    
      FORALL(I=2:N,J=1:M,K=1:Q)
        Dx_L(I,J,K) = ABS( X(I,J,K) - X(I-1,J,K) )
      END FORALL
        Dx_L(1,1:M,1:Q) = ABS( X(1,1:M,1:Q) - X(2,1:M,1:Q) )
  
  
      FORALL(I=1:N-1,J=1:M,K=1:Q)
        Dx_R(I,J,K) = ABS( X(I,J,K) - X(I+1,J,K) )
      END FORALL
        Dx_R(N,1:M,1:Q) = ABS( X(N,1:M,1:Q) - X(N-1,1:M,1:Q) )
  
  
      FORALL(I=1:N,J=2:M,K=1:Q)
        Dy_L(I,J,K) = ABS( Y(I,J,K) - Y(I,J-1,K) )
      END FORALL
        Dy_L(1:N,1,1:Q) = ABS( Y(1:N,1,1:Q) - Y(1:N,2,1:Q) )
  
      
      FORALL(I=1:N,J=1:M-1,K=1:Q)  
        Dy_R(I,J,K) = ABS( Y(I,J,K) - Y(I,J+1,K) )
      END FORALL
        Dy_R(1:N,M,1:Q) = ABS( Y(1:N,M,1:Q) - Y(1:N,M-1,1:Q) )


      FORALL(I=1:N,J=1:M,K=2:Q)
        Dz_L(I,J,K) = ABS( Z(I,J,K) - Z(I,J,K-1) )
      END FORALL
        Dz_L(1:N,1:M,1) = ABS( Z(1:N,1:M,1) - Z(1:N,1:M,2) )
  
      
      FORALL(I=1:N,J=1:M,K=1:Q-1)  
        Dz_R(I,J,K) = ABS( Z(I,J,K) - Z(I,J,K+1) )
      END FORALL
        Dz_R(1:N,1:M,Q) = ABS( Z(1:N,1:M,Q) - Z(1:N,1:M,Q-1) )
      
      Max_Spacing = MAX( MAXVAL(Dx_L(1:N-1,1:M,1:Q) / Dx_L(2:N,1:M,1:Q)) , &
                       & MAXVAL(Dx_L(2:N,1:M,1:Q) / Dx_L(1:N-1,1:M,1:Q)) , &
                       & MAXVAL(Dy_L(1:N-1,1:M,1:Q) / Dy_L(2:N,1:M,1:Q)) , &
                       & MAXVAL(Dy_L(2:N,1:M,1:Q) / Dy_L(1:N-1,1:M,1:Q)) , & 
                       & MAXVAL(Dz_L(1:N-1,1:M,1:Q) / Dz_L(2:N,1:M,1:Q)) , &
                       & MAXVAL(Dz_L(2:N,1:M,1:Q) / Dz_L(1:N-1,1:M,1:Q)) )
                       
      Max_Grid = MAX(  MAXVAL(Dx_L(1:N,1:M,1:Q)) , MAXVAL(Dy_L(1:N,1:M,1:Q)) &
               &     , MAXVAL(Dz_L(1:N,1:M,1:Q)) )
      Min_Grid = MIN(  MINVAL(Dx_L(1:N,1:M,1:Q)) , MINVAL(Dy_L(1:N,1:M,1:Q))  &
               &     , MINVAL(Dz_L(1:N,1:M,1:Q)) )
      
      WRITE(*,*) "     ========================================="
      WRITE(*,*) "     NON_UNIFORM_GRID generateing complete"
      WRITE(*,*) "     Clustering at walls."
      WRITE(*,*) "     The Max Grid size is : " , Max_Grid
      WRITE(*,*) "     The Min Grid size is : " , Min_Grid
      WRITE(*,*) "     The Max Grid_Spacing is : " , Max_Spacing
      WRITE(*,*) "     ========================================="


  END SUBROUTINE NON_UNIFORM_GRID_T_B_3D
