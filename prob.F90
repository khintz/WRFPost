MODULE PROB
  USE CONSTANTS
  USE DOMAIN

  IMPLICIT NONE

  CONTAINS

    SUBROUTINE PRECIPITATION_PROBABILITY(PREC,PREC_PROB)
      IMPLICIT NONE

      REAL, INTENT(IN)     :: PREC(NX,NY,NT)  !INPUT
      REAL, INTENT(OUT)    :: PREC_PROB(NX,NY,NT) !OUTPUT

      !LOCAL CONSTANTS
      INTEGER, PARAMETER        :: NLAT = NY, NLON = NX, NHOR=NX*NY
      REAL, PARAMETER           :: dtr = 3.1415/180.
      REAL, PARAMETER           :: threshold = 0.1
      !INTEGER, PARAMETER        :: N=16 !Nearest points to consider
      INTEGER, PARAMETER        :: NSPTIME=0 !Hours +/- Current time to consider
      INTEGER                   :: N

      !LOCAL VARIABLES
      INTEGER                       :: i,j,k,t,l ! some counters
      REAL, DIMENSION(:,:)          :: OPREC(NX,NY)
      INTEGER                       :: INDXI,INDXJ ! INDICES
      INTEGER                       :: FACO,FACM   ! INTERMEDIATES

      REAL                          :: BPROB(NX,NY,NT) !OUTPUT
      INTEGER, DIMENSION(NLON,NLAT) :: ON          ! PROV. OBS. FIELD
      REAL, DIMENSION(NLON,NLAT)    :: ONO         ! OBS. FIELD

      !Convert precipitaion field to a binary field
      DO k=1,NT
        DO i=1,NLON
        DO j=1,NLAT
            if (PREC(i,j,k)>=threshold) then
            BPROB(i,j,k) = 1
            end if
        END do
        END do
      END DO


      DO t=1,nt

      N = Neighbours_points_to_consider    
        IF (NSPTIME /= 0) THEN
          OPREC = BPROB(:,:,t-NSPTIME) + BPROB(:,:,t) + BPROB(:,:,t+NSPTIME)
        ELSE
          OPREC = BPROB(:,:,t)
        ENDIF

        ON(:,:) = 0

        DO j=1,NLAT
          DO i=1,NLON

            DO k=1,N
              DO l=1,N

                INDXI = i+k-1.0-(N-1)*0.5
                INDXJ = j+l-1.0-(N-1)*0.5

                IF ((INDXI.GE.1).AND.(INDXI.LE.NLON).AND.(INDXJ.GE.1).AND.(INDXJ.LE.NLAT)) THEN
                  FACO = OPREC(INDXI,INDXJ)
                ELSE
                  FACO = 0
                END IF

                ON(i,j) = ON(i,j) + FACO

              END DO
            END DO

            IF (NSPTIME /= 0) THEN
              ONO(i,j) = ON(i,j)/((N*((NSPTIME*2)+1))**2.)
            ELSE
              ONO(i,j) = ON(i,j)/(N**2.)
            END IF

          END DO
        END DO

        PREC_PROB(:,:,t) = ONO

      END DO

    END SUBROUTINE PRECIPITATION_PROBABILITY


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !


    SUBROUTINE PRECIPITATION_PERCENTILE(PREC,PCTH,PCTL)

      IMPLICIT NONE

      REAL, INTENT(IN)              :: PREC(NX,NY,NT)  !INPUT
      REAL, INTENT(OUT)             :: PCTH(NX,NY,NT), PCTL(NX,NY,NT) !OUTPUT

      !LOCAL VARIABLES
      INTEGER                       :: i,j,k,t,l, N, m, h !Some counters

      INTEGER                       :: FACO
      REAL, DIMENSION(:,:)          :: OPREC(NX,NY)
      INTEGER, DIMENSION(NX,NY)     :: ON ! PROV. OBS. FIELD
      INTEGER                       :: INDXI,INDXJ ! INDICES
      REAL, DIMENSION(:)            :: TO_BE_SORTED_tmp(Neighbours_points_to_consider**2)

      REAL                          :: mean, variance, std


      N = Neighbours_points_to_consider

      DO t=1,NT
        OPREC(:,:) = PREC(:,:,t)
        ON(:,:) = 0 !Reset matrix
        DO i=1,NX
          DO j=1,NY

              TO_BE_SORTED_tmp = 999

              m=0
              DO k=1,N
              DO l=1,N
                INDXI = i+k-1.0-(N-1)*0.5
                INDXJ = j+l-1.0-(N-1)*0.5

                IF ((INDXI.GE.1).AND.(INDXI.LE.NX).AND.(INDXJ.GE.1).AND.(INDXJ.LE.NY)) THEN
                  m=m+1
                  TO_BE_SORTED_tmp(m) =  OPREC(INDXI,INDXJ)
                END IF

                ! To calculate Percentiles we need to sort the data.
                ! Sorting algorithms are either O(N) or O(N*logN) making the
                ! algorithm too insuefficient for our purpose

              END DO
              END DO

             mean = sum(TO_BE_SORTED_tmp(1:m))/(max(1,m))
             variance = 0
             DO h=1,m
               variance = variance + (((TO_BE_SORTED_tmp(h)-mean)**2.0)/m)
             END DO

             std = sqrt(variance)

             PCTH(i,j,t) = mean + 0.5*std
             PCTL(i,j,t) = mean - 0.5*std

             IF (PCTL(i,j,t).LT.0) THEN
               PCTL(i,j,t) = 0.
             END IF

          END DO
        END DO

      END DO

    END SUBROUTINE PRECIPITATION_PERCENTILE

END MODULE PROB
