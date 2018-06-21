MODULE T2

USE DOMAIN
USE CONSTANTS

IMPLICIT NONE

CONTAINS

  SUBROUTINE T2K_TO_T2C(T2,T2C)
    IMPLICIT NONE

    REAL, INTENT(IN), DIMENSION(:,:,:)  :: T2  !INPUT
    REAL, INTENT(OUT), DIMENSION(:,:,:) :: T2C !OUTPUT

    T2C = T2 - KELVIN

    RETURN
  END SUBROUTINE T2K_TO_T2C

! - - - - - - - - - - - - - - - !

  SUBROUTINE DEWPOINT(Q2M,PSFC,TD)
    IMPLICIT NONE

    REAL, INTENT(IN)     :: Q2M(NX,NY,NT), PSFC(NX,NY,NT)  !INPUT
    REAL, INTENT(OUT)    :: TD(NX,NY,NT)                   !OUTPUT

    TD = zb/(log(za*e/(Q2M*PSFC)))
    TD = TD - 273.15

  END SUBROUTINE DEWPOINT

! - - - - - - - - - - - - - - - !

  SUBROUTINE DEPRESSION(T2m,T2d,FOG)
    IMPLICIT NONE

    REAL, INTENT(IN)     :: T2m(NX,NY,NT), T2d(NX,NY,NT)  !INPUT
    REAL, INTENT(OUT)    :: FOG(NX,NY,NT)                 !OUTPUT

    INTEGER              :: i,j,k

    FOG = T2m - T2d

    do k = 1,NT
      do j = 1,NY
        do i = 1,NX
           if (FOG(i,j,k) < 0) then
            FOG(i,j,k) = 0
           end if
        end do
      end do
    end do

  END SUBROUTINE DEPRESSION

! - - - - - - - - - - - - - - - !

  SUBROUTINE RELATIVE_HUM(T2m,PSFC,Q2m,RH2)
    IMPLICIT NONE

    REAL, INTENT(IN)     :: T2m(NX,NY,NT), Q2m(NX,NY,NT) ,PSFC(NX,NY,NT)  !INPUT
    REAL, INTENT(OUT)    :: RH2(NX,NY,NT)                                 !OUTPUT

    INTEGER              :: i,j,k
    REAL                 :: es(NX,NY,NT), QS(NX,NY,NT)

    es = 0.6108*exp(17.27*T2m/(T2m+237.3))
    QS = 0.622*es/(PSFC*0.001-es)

  !  RH2 = Q2M/QS

    do k = 1,NT
      do j = 1,NY
        do i = 1,NX
          RH2(i,j,k) = min(1.0,Q2M(i,j,k)/QS(i,j,k))
          RH2(i,j,k) = max(0.0,RH2(i,j,k))
        end do
      end do
    end do

  END SUBROUTINE RELATIVE_HUM

! - - - - - - - - - - - - - - - !

  SUBROUTINE APPARENT_TEMPERATURE(T2m,WSPD,AT)
    IMPLICIT NONE
    !https://www.ncdc.noaa.gov/societal-impacts/apparent-temp/at

    REAL, INTENT(IN)      :: T2m(NX,NY,NT), WSPD(NX,NY,NT)
    REAL, INTENT(OUT)     :: AT(NX,NY,NT)

    REAL                  :: es(NX,NY,NT)

    es = 0.6108*exp(17.27*T2m/(T2m+237.3))

    AT = -2.7 + 1.04*T2m + 2.0*es-0.65*WSPD


  END SUBROUTINE APPARENT_TEMPERATURE


END MODULE T2
