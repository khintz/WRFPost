MODULE DIAG
  USE CONSTANTS
  USE DOMAIN

  IMPLICIT NONE

  CONTAINS

    SUBROUTINE SEA_LVL_PRESSURE(PH,PHB,T2m,PSFC,SLP)
      IMPLICIT NONE

      REAL, INTENT(IN)     :: PH(NX,NY,BOTTOM_TOP_STAG,NT), PHB(NX,NY,BOTTOM_TOP_STAG,NT), T2m(NX,NY,NT)  !INPUT
      REAL, INTENT(IN)     :: PSFC(NX,NY,NT)
      REAL, INTENT(OUT)    :: SLP(NX,NY,NT) !OUTPUT

      REAL                 :: H(NX,NY,NT), es(NX,NY,NT) !(DE)ALLOCATABLE

      H = (PH(:,:,1,:)+PHB(:,:,1,:))/gravit
      es = 0.6108*exp(17.27*T2m/(T2m+237.3))

      SLP = PSFC*0.01 * exp(gravit/R * H/((T2m+273.15)+a*H/2.+es*c))

    END SUBROUTINE SEA_LVL_PRESSURE

! - - - - - - - - - - - - - - - !

    SUBROUTINE WINDSPEED(U10,V10,WSPD,WDIR)
      IMPLICIT NONE

      REAL, DIMENSION(:,:,:), INTENT(IN)   :: U10, V10
      REAL, DIMENSION(:,:,:), INTENT(OUT)  :: WSPD, WDIR

      WSPD = sqrt(U10*U10+V10*V10)
      WDIR = atan2(V10,U10)
      WDIR = mod(atan2(U10/WSPD,V10/WSPD)*180./pi + 180.,360.)

    END SUBROUTINE WINDSPEED

! - - - - - - - - - - - - - - - !

    SUBROUTINE WINDGUST(WSPD,TKE_PBL,GUST)
      IMPLICIT NONE

      REAL, INTENT(IN)    :: WSPD(NX,NY,NT), TKE_PBL(NX,NY,BOTTOM_TOP_STAG,NT)
      REAL, INTENT(OUT)   :: GUST(NX,NY,NT)

      GUST = WSPD * (1+4*sqrt(TKE_PBL(:,:,1,:)/(WSPD**2)))

    END SUBROUTINE WINDGUST

! - - - - - - - - - - - - - - - !

    SUBROUTINE PRECIPITATION(RAINC,RAINSH,RAINNC,SNOWNC,PREC,SNOW)
      IMPLICIT NONE

      REAL, INTENT(IN)    :: RAINC(NX,NY,NT), RAINSH(NX,NY,NT)
      REAL, INTENT(IN)    :: RAINNC(NX,NY,NT), SNOWNC(NX,NY,NT)
      REAL, INTENT(OUT)   :: PREC(NX,NY,NT), SNOW(NX,NY,NT)

      REAL, ALLOCATABLE   :: TPREC(:,:,:), TSNOW(:,:,:)
      INTEGER             :: ALLOC_ERR, i, j, k

      ALLOCATE(TPREC(NX,NY,NT))

      TPREC = RAINC+RAINSH+RAINNC+SNOWNC
      TSNOW = SNOWNC

        DO i=1,NX
          DO j=1,NY
           DO k=2,NT
            PREC(i,j,k) = TPREC(i,j,k) - TPREC(i,j,k-1)
            SNOW(i,j,k) = TSNOW(i,j,k) - TSNOW(i,j,k-1)
           END DO
          END DO
         END DO

      DEALLOCATE(TPREC, TSNOW, STAT = ALLOC_ERR)

    END SUBROUTINE PRECIPITATION

! - - - - - - - - - - - - - - - !

  SUBROUTINE CLOUDCOVER(P,PB,T,QVAPOR,CLOUD)
    IMPLICIT NONE

    REAL, INTENT(IN)    :: P(NX,NY,BOTTOM_TOP,NT), PB(NX,NY,BOTTOM_TOP,NT)
    REAL, INTENT(IN)    :: T(NX,NY,BOTTOM_TOP,NT), QVAPOR(NX,NY,BOTTOM_TOP,NT)
    REAL, INTENT(OUT)   :: CLOUD(NX,NY,NT)

    REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: RH, p_lvl, temp_lvl, es_lvl, ws_lvl
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: sigma

    REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: lvl_low, lvl_mid, lvl_high

    INTEGER, ALLOCATABLE, DIMENSION(:)     :: low_idx, mid_idx, high_idx
    INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: low_RH_idx, mid_RH_idx, high_RH_idx

    INTEGER             :: i, j, k, lvl, tmp(1)
    INTEGER, PARAMETER  :: low_threshold=970
    INTEGER, PARAMETER  :: mid_threshold=800
    INTEGER, PARAMETER  :: high_threshold=400

    REAL, PARAMETER     :: RH_threshold_low=0.7
    REAL, PARAMETER     :: RH_threshold_mid=0.7
    REAL, PARAMETER     :: RH_threshold_high=0.5

    ALLOCATE(RH(NX,NY,BOTTOM_TOP,NT),p_lvl(NX,NY,BOTTOM_TOP,NT))
    ALLOCATE(temp_lvl(NX,NY,BOTTOM_TOP,NT))
    ALLOCATE(es_lvl(NX,NY,BOTTOM_TOP,NT), ws_lvl(NX,NY,BOTTOM_TOP,NT))

    p_lvl = (P+PB)*0.01
    temp_lvl = (T+300)*(p_lvl/1000.0)**(2.0/7.0) !converting from pot t to temp
    es_lvl = 6.112*exp(17.62*(temp_lvl-273.15)/(243.12+(temp_lvl-273.15)))
    ws_lvl = 621.97*(es_lvl/(p_lvl-es_lvl))
    RH = (QVAPOR*1000.0)/ws_lvl

    DEALLOCATE(temp_lvl,es_lvl,ws_lvl)

    ALLOCATE(lvl_low(NX,NY,NT), lvl_mid(NX,NY,NT), lvl_high(NX,NY,NT))
    ALLOCATE(low_idx(NT), mid_idx(NT), high_idx(NT))
    ALLOCATE(low_RH_idx(NX,NY,NT), mid_RH_idx(NX,NY,NT), high_RH_idx(NX,NY,NT))

        do k = 1,NT
        do j = 1,NY
        do i = 1,NX
          do lvl = 1,BOTTOM_TOP
                if ( p_lvl(i,j,lvl,k) >= low_threshold ) then
                  lvl_low(i,j,k) = lvl
                elseif (( p_lvl(i,j,lvl,k) >= mid_threshold ) .and. ( p_lvl(i,j,lvl,k) <= low_threshold )) then
                  lvl_mid(i,j,k) = lvl
                elseif (( p_lvl(i,j,lvl,k) >= high_threshold ) .and. ( p_lvl(i,j,lvl,k) <= mid_threshold )) then
                  lvl_high(i,j,k) = lvl
                end if
          enddo
        enddo
        enddo
          low_idx(k) = ceiling(sum(lvl_low(1:NX,1:NY,k))/(max(1,size(lvl_low(1:NX,1:NY,k)))))
          mid_idx(k) = ceiling(sum(lvl_mid(1:NX,1:NY,k))/(max(1,size(lvl_mid(1:NX,1:NY,k)))))  ! sum(x)/max(1,sixe(x)) = The Mean
          high_idx(k) = ceiling(sum(lvl_high(1:NX,1:NY,k))/(max(1,size(lvl_high(1:NX,1:NY,k)))))
        enddo

    ALLOCATE(sigma(NX,NY,3,NT))

      do k = 1,NT
      do j = 1,NY
      do i = 1,NX
           tmp = MAXLOC(RH(i,j,low_idx(k):mid_idx(k),k))
           low_RH_idx(i,j,k) = tmp(1)
           tmp = MAXLOC(RH(i,j,mid_idx(k):high_idx(k),k))
           mid_RH_idx(i,j,k) = tmp(1)
           tmp = MAXLOC(RH(i,j,high_idx(k):BOTTOM_TOP,k))
           high_RH_idx(i,j,k) = tmp(1)

          sigma(i,j,1,k) = 1.0 - sqrt((1-RH(i,j,low_RH_idx(i,j,k),k))/(1.0-RH_threshold_low)) ! 0.25 : if RH=1 then sqrt(1-4)=sqrt(-3)=NaN! (Fully cloudy)
          sigma(i,j,2,k) = 1.0 - sqrt((1-RH(i,j,mid_RH_idx(i,j,k),k))/(1.0-RH_threshold_mid))
          sigma(i,j,3,k) = 1.0 - sqrt((1-RH(i,j,high_RH_idx(i,j,k),k))/(1.0-RH_threshold_high))

           CLOUD(i,j,k) = sigma(i,j,1,k) + sigma(i,j,2,k) + sigma(i,j,3,k)
            if (CLOUD(i,j,k)>1) then
              CLOUD(i,j,k) = 1
            endif
            if (CLOUD(i,j,k)<0) then
              CLOUD(i,j,k) = 0
            end if

            if (CLOUD(i,j,k)/=CLOUD(i,j,k)) then ! NaN "values" are not even equal to them selfes
              CLOUD(i,j,k) = 1
            end if

      enddo
      enddo
      enddo

    DEALLOCATE(RH,p_lvl,sigma)
    DEALLOCATE(lvl_low,lvl_mid,lvl_high)
    DEALLOCATE(low_idx,mid_idx,high_idx)
    DEALLOCATE(low_RH_idx,mid_RH_idx,high_RH_idx)

  END SUBROUTINE CLOUDCOVER


  SUBROUTINE DEWPOINT_PROFILE(P,Q,TD)
    IMPLICIT NONE

    REAL, INTENT(IN)     :: Q(NX,NY,BOTTOM_TOP,NT), P(NX,NY,BOTTOM_TOP,NT)  ! INPUT
    REAL, INTENT(OUT)    :: TD(NX,NY,BOTTOM_TOP,NT)                         ! OUTPUT

    TD = zb/(log(za*e/(Q*P)))
  !  TD = TD

  END SUBROUTINE DEWPOINT_PROFILE


  ! - - - - - - - - - - - - - - - !

    SUBROUTINE CORRECTED_GFS_PRECIPITATION(PR,N,PROUT)
      IMPLICIT NONE

      REAL, INTENT(IN)        :: PR(NXGFS,NYGFS,NTGFS)
      INTEGER, INTENT(IN)     :: N
      REAL, INTENT(OUT)       :: PROUT(NXGFS,NYGFS,NTGFS)

      !Local parameters
      INTEGER                 :: k, i, j
      INTEGER, ALLOCATABLE    :: preccorrection(:)

      !Create array of numbers to skip
      ALLOCATE(preccorrection(N/6+1))

      k=1
      DO i=8,N+6,6  ! file number 8 is the 7th hour
        preccorrection(k) = i
        k=k+1
      END DO

      k=1
      j=0
      DO i=2,N
        IF (i.EQ.preccorrection(k)) THEN

          PROUT(:,:,i) = PR(:,:,i)
          k=k+1
        ELSE
          PROUT(:,:,i) = PR(:,:,i) - PR(:,:,i-1)
        END IF
      j=j+1
      END DO

    END SUBROUTINE CORRECTED_GFS_PRECIPITATION


END MODULE DIAG
