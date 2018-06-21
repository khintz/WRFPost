MODULE SURFACE

  USE DOMAIN
  USE CONSTANTS

  IMPLICIT NONE

  CONTAINS

    SUBROUTINE SOIL_TEMPERATURE(SOILT,LANDMASK,SOILTC)
      IMPLICIT NONE

      REAL, INTENT(IN)     :: SOILT(NX,NY,SOIL_LAYERS_STAG,NT)  !INPUT
      INTEGER, INTENT(IN)  :: LANDMASK(NX,NY,NT)
      REAL, INTENT(OUT)    :: SOILTC(NX,NY,NT)                  !OUTPUT

      INTEGER              :: i, j, k

      do k = 1,NT
        do j = 1,NY
          do i = 1,NX
             if (LANDMASK(i,j,k) == 1) then
              SOILTC(i,j,k) = SOILT(i,j,1,k) - KELVIN
             elseif (LANDMASK(i,j,k) == 0) then
              SOILTC(i,j,k) = -9999.
             end if
          end do
        end do
      end do

    END SUBROUTINE SOIL_TEMPERATURE

! - - - - - - - - - - - - - - - - - - - - !
    SUBROUTINE SURFACE_TEMPERATURE(SURFT,LANDMASK,SURFTC)
      IMPLICIT NONE

      REAL, INTENT(IN)     :: SURFT(NX,NY,NT)     !INPUT
      INTEGER, INTENT(IN)  :: LANDMASK(NX,NY,NT)
      REAL, INTENT(OUT)    :: SURFTC(NX,NY,NT)    !OUTPUT

      INTEGER              :: i, j, k

      do k = 1,NT
        do j = 1,NY
          do i = 1,NX
             if (LANDMASK(i,j,k) == 1) then
              SURFTC(i,j,k) = SURFT(i,j,k) - KELVIN
             elseif (LANDMASK(i,j,k) == 0) then
              SURFTC(i,j,k) = -9999.
             end if
          end do
        end do
      end do

    END SUBROUTINE SURFACE_TEMPERATURE

END MODULE SURFACE
