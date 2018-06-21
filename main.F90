PROGRAM WRF_POST_PROCESSING
  USE netcdf
  USE CONSTANTS
  USE DOMAIN
  USE T2
  USE DIAG
  USE SURFACE
  USE PROB
  USE SOUNDING

  IMPLICIT NONE

  INTEGER                                :: narg,cptArg ! #of arg & counter of arg
  CHARACTER*120                          :: name        ! Arg name

  !MAX DIMENSION IS NX,NY,NT,BOTTOM_TOP_STAG
  INTEGER :: i,j,k
  !DECLARATIONS FROM INPUT FILE
  CHARACTER*120                          :: FILE_NAME

  CHARACTER*5, PARAMETER                 :: LAT_NAME = "XLAT", LON_NAME = "XLONG"
  CHARACTER*4, PARAMETER                 :: TIME_NAME = "Time"
  INTEGER                                :: lat_dimid, lon_dimid, time_dimid, lvl_dimid

  INTEGER                                :: ncid, varid
  INTEGER                                :: timestart_id
  INTEGER                                :: validRangeLength, titleLength ! Attribute lengths
  REAL, DIMENSION(:), ALLOCATABLE        :: validRange
  CHARACTER (LEN = 80)                   :: starttime

  !NOTICE THAT THE ORDER OF DIMENSIONS IS REVERSED
  REAL                                   :: lats(NX,NY,NT), lons(NX,NY,NT)
  INTEGER                                :: lat_varid, lon_varid

  !INPUT TEMPERATURE
  REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: T2m
  CHARACTER*2, PARAMETER                 :: T2m_NAME = "T2"
  INTEGER                                :: t2m_varid, t2m_dimid
  INTEGER                                :: dimids(NDIMS)

  CHARACTER*3, PARAMETER                 :: U10_NAME = "U10", V10_NAME = "V10"
  REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: U10,V10
  INTEGER                                :: u10_varid, u10_dimid, v10_varid, v10_dimid

  CHARACTER*10, PARAMETER                :: RAINC_NAME = "RAINC", RAINSH_NAME = "RAINSH", &
                                            & RAINNC_NAME = "RAINNC", SNOWNC_NAME = "SNOWNC"
  REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: RAINC, RAINSH, RAINNC, SNOWNC
  INTEGER                                :: rainc_varid, rainc_dimid, rainsh_varid, rainsh_dimid, &
                                            & rainnc_varid, rainnc_dimid, snownc_varid, snownc_dimid

  CHARACTER*10, PARAMETER                :: TKE_PBL_NAME = "TKE_PBL"
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: TKE_PBL
  REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: TKE
  INTEGER                                :: tke_pbl_varid, tke_pbl_dimid

  CHARACTER*2, PARAMETER                 :: P_NAME = "P", PB_NAME = "PB"
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: P,PB
  INTEGER                                :: p_varid, p_dimid, pb_varid, pb_dimid

  CHARACTER*1, PARAMETER                 :: T_NAME = "T" ! Perturbation Potential temperature
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: T
  INTEGER                                :: t_varid, t_dimid

  CHARACTER*10, PARAMETER                :: QVAPOR_NAME = "QVAPOR"
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: QVAPOR
  INTEGER                                :: qvapor_varid, qvapor_dimid

  CHARACTER*10, PARAMETER                :: Q2M_NAME = "Q2"
  REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: Q2M
  INTEGER                                :: q2m_varid, q2m_dimid

  CHARACTER*10, PARAMETER                :: PSFC_NAME = "PSFC"
  REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: PSFC
  INTEGER                                :: psfc_varid, psfc_dimid

  CHARACTER*5, PARAMETER                 :: PH_NAME = "PH", PHB_NAME = "PHB"
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: PH, PHB
  INTEGER                                :: ph_varid, ph_dimid, phb_varid, phb_dimid

  CHARACTER*1, PARAMETER                 :: U_NAME = "U", V_NAME = "V"
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: U,V
  INTEGER                                :: u_varid, u_dimid, v_varid, v_dimid

  CHARACTER*4, PARAMETER                 :: ZNT_NAME = "ZNT", UST_NAME = "UST", RMOL_NAME = "RMOL"
  REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: ZNT, UST, RMOL
  INTEGER                                :: znt_varid, znt_dimid, ust_varid, ust_dimid, rmol_varid, rmol_dimid

  CHARACTER*5, PARAMETER                 :: ZNW_NAME = "ZNW"
  REAL, ALLOCATABLE, DIMENSION(:,:)      :: ZNW
  INTEGER                                :: znw_varid, znw_dimid

  CHARACTER*4, PARAMETER                 :: SOILT_NAME = "TSLB"
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: SOILT
  INTEGER                                :: soilt_varid, soilt_dimid

  CHARACTER*10, PARAMETER                :: LANDMASK_NAME = "LANDMASK"
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: LANDMASK
  INTEGER                                :: landmask_varid, landmask_dimid

  CHARACTER*3, PARAMETER                 :: SURFT_NAME = "TSK"
  REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: SURFT
  INTEGER                                :: surft_varid, surft_dimid

  REAL, ALLOCATABLE, DIMENSION(:,:,:)    :: td2, es, QS, H

  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)  :: P_IN, T_IN, TD_IN

  REAL                                   :: T2m_celcius(NX,NY,NT)
  REAL                                   :: TD2_celcius(NX,NY,NT)

  CHARACTER*2, PARAMETER                 :: TD2_NAME = "TD2"
  INTEGER                                :: TD2_varid, TD2_dimid

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !DECLARATIONS TO OUTPUT FILE!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  CHARACTER (len = *), PARAMETER         :: FILE_NAME_OUT = "wrfout_out.nc"
  INTEGER                                :: ncid_out, varid_out
  INTEGER                                :: lat_varid_out, lon_varid_out

  CHARACTER (len = *), PARAMETER         :: STARTDATE = "start_date"

  CHARACTER (len = *), PARAMETER         :: UNITS = "units"
  CHARACTER (len = *), PARAMETER         :: DESCRIB = "description"
  CHARACTER (len = *), PARAMETER         :: TEMP_UNITS = "Celcius"
  CHARACTER (len = *), PARAMETER         :: LAT_UNITS = "degrees_north"
  CHARACTER (len = *), PARAMETER         :: LAT_DESCRIB = "Latitude - south is negative"
  CHARACTER (len = *), PARAMETER         :: LON_UNITS = "degrees_east"
  CHARACTER (len = *), PARAMETER         :: LON_DESCRIB = "Longitude - west is negative"
  CHARACTER (len = *), PARAMETER         :: LAT_NAME_OUT = "south_north"
  CHARACTER (len = *), PARAMETER         :: LON_NAME_OUT = "west_east"
  CHARACTER (len = *), PARAMETER         :: TIME_NAME_OUT = "Time"
  CHARACTER (len = *), PARAMETER         :: LEVEL_NAME_OUT = "Levels"
  CHARACTER (len = *), PARAMETER         :: T2m_NAME_OUT = "T2"
  CHARACTER (len = *), PARAMETER         :: T2m_DESCRIB = "Temperature at 2 meters"

  CHARACTER (len = *), PARAMETER         :: TD2_NAME_OUT = "TD2"
  CHARACTER (len = *), PARAMETER         :: TD2_DESCRIB = "Dewpoint at 2 meters"

  REAL                                   :: WSPD(NX,NY,NT)
  INTEGER                                :: wspd_varid
  CHARACTER (len = *), PARAMETER         :: WSPD_NAME_OUT = "WSPD"
  CHARACTER (len = *), PARAMETER         :: WSPD_UNITS = "m/s"
  CHARACTER (len = *), PARAMETER         :: WSPD_DESCRIB = "Wind speed at 10 meters"

  REAL                                   :: WDIR(NX,NY,NT)
  INTEGER                                :: wdir_varid
  CHARACTER (len = *), PARAMETER         :: WDIR_NAME_OUT = "WDIR"
  CHARACTER (len = *), PARAMETER         :: WDIR_UNITS = "deg"
  CHARACTER (len = *), PARAMETER         :: WDIR_DESCRIB = "Wind direction in cardinal coordinates"

  REAL                                   :: GUST(NX,NY,NT)
  INTEGER                                :: gust_varid
  CHARACTER (len = *), PARAMETER         :: GUST_NAME_OUT = "GUST"
  CHARACTER (len = *), PARAMETER         :: GUST_UNITS = "m/s"
  CHARACTER (len = *), PARAMETER         :: GUST_DESCRIB = "Wind gust at 10m"

  REAL                                   :: FOG(NX,NY,NT)
  INTEGER                                :: fog_varid
  CHARACTER (len = *), PARAMETER         :: FOG_NAME_OUT = "FOG"
  CHARACTER (len = *), PARAMETER         :: FOG_UNITS = "C"
  CHARACTER (len = *), PARAMETER         :: FOG_DESCRIB = "2m Temperature Depression"

  REAL                                   :: RH2(NX,NY,NT)
  INTEGER                                :: rh2_varid
  CHARACTER (len = *), PARAMETER         :: RH2_NAME_OUT = "RH2"
  CHARACTER (len = *), PARAMETER         :: RH2_UNITS = "percent"
  CHARACTER (len = *), PARAMETER         :: RH2_DESCRIB = "2m Relative Humidity"

  REAL                                   :: SLP(NX,NY,NT)
  INTEGER                                :: slp_varid
  CHARACTER (len = *), PARAMETER         :: SLP_NAME_OUT = "SLP"
  CHARACTER (len = *), PARAMETER         :: SLP_UNITS = "hPa"
  CHARACTER (len = *), PARAMETER         :: SLP_DESCRIB = "Sea Level Pressure"

  REAL                                   :: PREC(NX,NY,NT)
  INTEGER                                :: prec_varid
  CHARACTER (len = *), PARAMETER         :: PREC_NAME_OUT = "PREC"
  CHARACTER (len = *), PARAMETER         :: PREC_UNITS = "kg/m**2"
  CHARACTER (len = *), PARAMETER         :: PREC_DESCRIB = "Total Precipitation"

  REAL                                   :: PRECPROB(NX,NY,NT)
  INTEGER                                :: precprob_varid
  CHARACTER (len = *), PARAMETER         :: PRECPROB_NAME_OUT = "PRECIP_PROB"
  CHARACTER (len = *), PARAMETER         :: PRECPROB_UNITS = "%"
  CHARACTER (len = *), PARAMETER         :: PRECPROB_DESCRIB = "Precipitation Probability"

  REAL                                   :: SNOW(NX,NY,NT)
  INTEGER                                :: snow_varid
  CHARACTER (len = *), PARAMETER         :: SNOW_NAME_OUT = "SNOW"
  CHARACTER (len = *), PARAMETER         :: SNOW_UNITS = "kg/m**2"
  CHARACTER (len = *), PARAMETER         :: SNOW_DESCRIB = "Grid scale snow precipitation"

  REAL                                   :: CLOUD(NX,NY,NT)
  INTEGER                                :: cloud_varid
  CHARACTER (len = *), PARAMETER         :: CLOUD_NAME_OUT = "CLOUD"
  CHARACTER (len = *), PARAMETER         :: CLOUD_UNITS = "Percentage (0-1)"
  CHARACTER (len = *), PARAMETER         :: CLOUD_DESCRIB = "Fractional Cloud Cover"

  REAL                                   :: IMONIN(NX,NY,NT)
  INTEGER                                :: imonin_varid
  CHARACTER (len = *), PARAMETER         :: IMONIN_NAME_OUT = "IMONIN"
  CHARACTER (len = *), PARAMETER         :: IMONIN_UNITS = "1/m"
  CHARACTER (len = *), PARAMETER         :: IMONIN_DESCRIB = "Inverse Monin-Obukhov Length"

  REAL                                   :: Z0(NX,NY,NT)
  INTEGER                                :: z0_varid
  CHARACTER (len = *), PARAMETER         :: Z0_NAME_OUT = "Z0"
  CHARACTER (len = *), PARAMETER         :: Z0_UNITS = "m"
  CHARACTER (len = *), PARAMETER         :: Z0_DESCRIB = "Roughness Length"

  REAL                                   :: USTAR(NX,NY,NT)
  INTEGER                                :: ustar_varid
  CHARACTER (len = *), PARAMETER         :: USTAR_NAME_OUT = "UST"
  CHARACTER (len = *), PARAMETER         :: USTAR_UNITS = "m/s"
  CHARACTER (len = *), PARAMETER         :: USTAR_DESCRIB = "U* in Similarity Theory"

  REAL                                   :: SOILTC(NX,NY,NT)
  INTEGER                                :: soiltc_varid
  CHARACTER (len = *), PARAMETER         :: SOILTC_NAME_OUT = "TSOIL"
  CHARACTER (len = *), PARAMETER         :: SOILTC_UNITS = "C"
  CHARACTER (len = *), PARAMETER         :: SOILTC_DESCRIB = "Soil Temperature"

  REAL                                   :: SURFTC(NX,NY,NT)
  INTEGER                                :: surftc_varid
  CHARACTER (len = *), PARAMETER         :: SURFTC_NAME_OUT = "TSURF"
  CHARACTER (len = *), PARAMETER         :: SURFTC_UNITS = "C"
  CHARACTER (len = *), PARAMETER         :: SURFTC_DESCRIB = "Surface Temperature"

  REAL                                   :: AT(NX,NY,NT)
  INTEGER                                :: at_varid
  CHARACTER (len = *), PARAMETER         :: AT_NAME_OUT = "AT"
  CHARACTER (len = *), PARAMETER         :: AT_UNITS = "C"
  CHARACTER (len = *), PARAMETER         :: AT_DESCRIB = "Apparent Temperature"

  REAL                                   :: PRECPCTH(NX,NY,NT)
  INTEGER                                :: precpcth_varid
  CHARACTER (len = *), PARAMETER         :: PRECPCTH_NAME_OUT = "PRECHR"
  CHARACTER (len = *), PARAMETER         :: PRECPCTH_UNITS = "mm"
  CHARACTER (len = *), PARAMETER         :: PRECPCTH_DESCRIB = "Precipitation Probability Lowest Range"

  REAL                                   :: PRECPCTL(NX,NY,NT)
  INTEGER                                :: precpctl_varid
  CHARACTER (len = *), PARAMETER         :: PRECPCTL_NAME_OUT = "PRECLR"
  CHARACTER (len = *), PARAMETER         :: PRECPCTL_UNITS = "mm"
  CHARACTER (len = *), PARAMETER         :: PRECPCTL_DESCRIB = "Precipitation Probability Highest Range"

  REAL                                   :: CAPE(NX,NY,NT)
  INTEGER                                :: cape_varid
  CHARACTER (len = *), PARAMETER         :: CAPE_NAME_OUT = "CAPE"
  CHARACTER (len = *), PARAMETER         :: CAPE_UNITS = "J/kg"
  CHARACTER (len = *), PARAMETER         :: CAPE_DESCRIB = "Convective Available Potential Energy"

  narg=command_argument_count()
  if(narg>0)then
  !loop across options
     do cptArg=1,narg
        CALL get_command_argument(cptArg,name)
        select case(adjustl(name))
          case("--help","-h")
          write(*,*)"Vaavud Processing. Usage ./process.x NetCDFFILE for input"
          case default
          write(*,*)"File given is: ",adjustl(name)
        end select
     end do
  end if


  ! READ DATA IN FROM INPUT FILE
  FILE_NAME = name

  CALL check( nf90_open(FILE_NAME, NF90_NOWRITE, ncid) ) ! Opens NetCDF file and gives an id

  CALL check( nf90_inq_varid(ncid, LAT_NAME, varid) )    ! Gets the id of "LAT_NAME"
  CALL check( nf90_get_var(ncid, varid, lats) )          ! Reads the data of "varid"

  CALL check( nf90_inq_varid(ncid, LON_NAME, varid) )
  CALL check( nf90_get_var(ncid, varid, lons) )

  !INQUIRE AND GET ATTRIBUTES
  CALL check( nf90_inquire_attribute(ncid, nf90_global, "START_DATE", &
                          len = titleLength) )

  CALL check( nf90_get_att(ncid, nf90_global, "START_DATE", starttime) )

  print*,'Start date is: ', starttime
  print*,''

  ! - - PROCESS DATA - - !

  !TEMPERATURE AND DEWPOINT
  ALLOCATE(Q2M(NX,NY,NT), PSFC(NX,NY,NT))
  ALLOCATE(td2(NX,NY,NT), T2m(NX,NY,NT))
  ALLOCATE(QS(NX,NY,NT), es(NX,NY,NT))
  ALLOCATE(PHB(NX,NY,BOTTOM_TOP_STAG,NT), PH(NX,NY,BOTTOM_TOP_STAG,NT), H(NX,NY,NT))

  CALL check( nf90_inq_varid(ncid, Q2M_NAME, q2m_varid) )
  CALL check( nf90_get_var(ncid, q2m_varid, Q2M) )
  CALL check( nf90_inq_varid(ncid, PSFC_NAME, psfc_varid) )
  CALL check( nf90_get_var(ncid, psfc_varid, PSFC) )

  CALL check( nf90_inq_varid(ncid, PH_NAME, ph_varid) )
  CALL check( nf90_get_var(ncid, ph_varid, PH) )
  CALL check( nf90_inq_varid(ncid, PHB_NAME, phb_varid) )
  CALL check( nf90_get_var(ncid, phb_varid, PHB) )

  CALL check( nf90_inq_varid(ncid, T2m_NAME, t2m_varid) )
  CALL check( nf90_get_var(ncid, t2m_varid, T2m) )



    CALL T2K_TO_T2C(T2m,T2m_celcius)
    print*,'Highest temperature: ',MAXVAL(T2m_celcius)

    CALL DEWPOINT(Q2M,PSFC,TD2_celcius)
    print*,'Highest dewpoints: ',MAXVAL(TD2_celcius)

    CALL DEPRESSION(T2m_celcius,TD2_celcius,FOG)
    print*,'Highest depression: ',MAXVAL(FOG)

    CALL RELATIVE_HUM(T2m_celcius,PSFC,Q2M,RH2)
    print*,'Highest rel hum: ',MAXVAL(RH2)
    print*,'Lowest rel hum: ',MINVAL(RH2)

    CALL SEA_LVL_PRESSURE(PH,PHB,T2m_celcius,PSFC,SLP)
    print*,'Highest SLP: ',MAXVAL(SLP)


  DEALLOCATE(Q2M,PSFC,td2,T2m,H,PHB,PH,es)


  !10M WIND AND WIND GUST
  ALLOCATE(U10(NX,NY,NT), V10(NX,NY,NT))
  ALLOCATE(TKE_PBL(NX,NY,BOTTOM_TOP_STAG,NT), TKE(NX,NY,NT))
  CALL check( nf90_inq_varid(ncid, U10_NAME, u10_varid) )
  CALL check( nf90_get_var(ncid, u10_varid, U10) )
  CALL check( nf90_inq_varid(ncid, V10_NAME, v10_varid) )
  CALL check( nf90_get_var(ncid, v10_varid, V10) )
  CALL check( nf90_inq_varid(ncid, TKE_PBL_NAME, tke_pbl_varid) )
  CALL check( nf90_get_var(ncid, tke_pbl_varid, TKE_PBL) )

  CALL WINDSPEED(U10,V10,WSPD,WDIR)
  print*,'Highest windspeed: ',MAXVAL(WSPD)

  CALL WINDGUST(WSPD,TKE_PBL,GUST)
  print*,'Highest windgust: ',MAXVAL(GUST)

  DEALLOCATE(U10,V10,TKE_PBL,TKE)

  CALL APPARENT_TEMPERATURE(T2m_celcius,WSPD,AT)
  print*,'Highest Apparent Temperature: ',MAXVAL(AT)

  ALLOCATE(RAINC(NX,NY,NT),RAINSH(NX,NY,NT),RAINNC(NX,NY,NT),SNOWNC(NX,NY,NT))

  CALL check( nf90_inq_varid(ncid, RAINC_NAME, rainc_varid) )
  CALL check( nf90_get_var(ncid, rainc_varid, RAINC) )
  CALL check( nf90_inq_varid(ncid, RAINSH_NAME, rainsh_varid) )
  CALL check( nf90_get_var(ncid, rainsh_varid, RAINSH) )
  CALL check( nf90_inq_varid(ncid, RAINNC_NAME, rainnc_varid) )
  CALL check( nf90_get_var(ncid, rainnc_varid, RAINNC) )
  CALL check( nf90_inq_varid(ncid, SNOWNC_NAME, snownc_varid) )
  CALL check( nf90_get_var(ncid, snownc_varid, SNOWNC) )

  CALL PRECIPITATION(RAINC,RAINSH,RAINNC,SNOWNC,PREC,SNOW)
  print*,'Highest precipitation: ',MAXVAL(PREC)
  print*,'Highest snow: ',MAXVAL(SNOW)

  DEALLOCATE(RAINC,RAINSH,RAINNC,SNOWNC)
  CALL PRECIPITATION_PROBABILITY(PREC,PRECPROB)
  print*,'Highest precipitation probability: ',MAXVAL(PRECPROB)
  print*,'Lowest precipitation probability: ',MINVAL(PRECPROB)

  CALL PRECIPITATION_PERCENTILE(PREC,PRECPCTH,PRECPCTL)
  print*,'Highest precipitation percentile: ',MAXVAL(PRECPCTH)
  print*,'Lowest precipitation percentile: ',MINVAL(PRECPCTL)

  ! - - CLOUD COVER - - !
  ALLOCATE(P(NX,NY,BOTTOM_TOP,NT), PB(NX,NY,BOTTOM_TOP,NT))
  ALLOCATE(T(NX,NY,BOTTOM_TOP,NT), QVAPOR(NX,NY,BOTTOM_TOP,NT))

  CALL check( nf90_inq_varid(ncid, P_NAME, p_varid) )
  CALL check( nf90_get_var(ncid, p_varid, P) )
  CALL check( nf90_inq_varid(ncid, PB_NAME, pb_varid) )
  CALL check( nf90_get_var(ncid, pb_varid, PB) )
  CALL check( nf90_inq_varid(ncid, T_NAME, t_varid) )
  CALL check( nf90_get_var(ncid, t_varid, T) )
  CALL check( nf90_inq_varid(ncid, QVAPOR_NAME, qvapor_varid) )
  CALL check( nf90_get_var(ncid, qvapor_varid, QVAPOR) )

  CALL CLOUDCOVER(P,PB,T,QVAPOR,CLOUD)
  print*,'Highest Cloudcover: ',MAXVAL(CLOUD)
  print*,'Lowest Cloudcover: ',MINVAL(CLOUD)

  DEALLOCATE(P,PB,T,QVAPOR)

  ALLOCATE(SOILT(NX,NY,SOIL_LAYERS_STAG,NT))
  ALLOCATE(LANDMASK(NX,NY,NT))

  CALL check( nf90_inq_varid(ncid, SOILT_NAME, soilt_varid) )
  CALL check( nf90_get_var(ncid, soilt_varid, SOILT) )

  CALL check( nf90_inq_varid(ncid, LANDMASK_NAME, landmask_varid) )
  CALL check( nf90_get_var(ncid, landmask_varid, LANDMASK) )

  CALL SOIL_TEMPERATURE(SOILT,LANDMASK,SOILTC)
  print*,'Highest Soil Temperature: ',MAXVAL(SOILTC)

  DEALLOCATE(SOILT)

  ALLOCATE(SURFT(NX,NY,NT))
  CALL check( nf90_inq_varid(ncid, SURFT_NAME, surft_varid) )
  CALL check( nf90_get_var(ncid, surft_varid, SURFT) )

  CALL SURFACE_TEMPERATURE(SURFT,LANDMASK,SURFTC)
  print*,'Highest Surface Temperature: ',MAXVAL(SURFTC)

  DEALLOCATE(LANDMASK,SURFT)

  ! - - THESE VALUES GOES DIRECTLY WITHOUT PROCESSING - - !
  ALLOCATE(ZNT(NX,NY,NT), UST(NX,NY,NT), RMOL(NX,NY,NT))

  CALL check( nf90_inq_varid(ncid, ZNT_NAME, znt_varid) )
  CALL check( nf90_get_var(ncid, znt_varid, ZNT) )

  CALL check( nf90_inq_varid(ncid, UST_NAME, ust_varid) )
  CALL check( nf90_get_var(ncid, ust_varid, UST) )

  CALL check( nf90_inq_varid(ncid, RMOL_NAME, rmol_varid) )
  CALL check( nf90_get_var(ncid, rmol_varid, RMOL) )

  IMONIN = RMOL
  Z0 = ZNT
  USTAR = UST

  DEALLOCATE(RMOL,ZNT,UST)


  ! - - - - - CAPE - - - - - - - !

  ALLOCATE(P_IN(NX,NY,BOTTOM_TOP,NT))

  ALLOCATE(TD_IN(NX,NY,BOTTOM_TOP,NT))

  ALLOCATE(P(NX,NY,BOTTOM_TOP,NT), PB(NX,NY,BOTTOM_TOP,NT))
  ALLOCATE(T(NX,NY,BOTTOM_TOP,NT), QVAPOR(NX,NY,BOTTOM_TOP,NT))

  CALL check( nf90_inq_varid(ncid, P_NAME, p_varid) )
  CALL check( nf90_get_var(ncid, p_varid, P) )
  CALL check( nf90_inq_varid(ncid, PB_NAME, pb_varid) )
  CALL check( nf90_get_var(ncid, pb_varid, PB) )
  CALL check( nf90_inq_varid(ncid, T_NAME, t_varid) )
  CALL check( nf90_get_var(ncid, t_varid, T) )
  CALL check( nf90_inq_varid(ncid, QVAPOR_NAME, qvapor_varid) )
  CALL check( nf90_get_var(ncid, qvapor_varid, QVAPOR) )

  P_IN = P+PB


  print*,MAXVAL(P_IN)
  T = (T+300.)*(P_IN/100000.0)**0.2854
  CALL DEWPOINT_PROFILE(QVAPOR,P_IN,TD_IN)

  print*,MAXVAL(T)

  DO k=1,NT
    print*,k
    DO i=1,NX
     DO j=1,NY
        CALL getcape(BOTTOM_TOP_STAG,P_IN(i,j,:,k),T(i,j,:,k),TD_IN(i,j,:,k),CAPE(i,j,k))
     END DO
    END DO
  END DO

  print*,'Highest CAPE: ',MAXVAL(CAPE)
  print*,'Lowest CAPE: ',MINVAL(CAPE)

  DEALLOCATE(P,PB,T)
  DEALLOCATE(P_IN,TD_IN)

  ! - - SAVE PROCESSED DATA - - !
  CALL check( nf90_create(FILE_NAME_OUT, nf90_64bit_offset, ncid_out) )

  CALL check( nf90_def_dim(ncid_out, LAT_NAME_OUT, NY, lat_dimid) )
  CALL check( nf90_def_dim(ncid_out, LON_NAME_OUT, NX, lon_dimid) )
  CALL check( nf90_def_dim(ncid_out, LEVEL_NAME_OUT, z_levels, lvl_dimid) )
  CALL check( nf90_def_dim(ncid_out, TIME_NAME_OUT, NT, time_dimid) )

  !LATITUDE
  CALL check( nf90_def_var(ncid_out, LAT_NAME, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), lat_varid_out) )
  CALL check( nf90_put_att(ncid_out, lat_varid_out, UNITS, LAT_UNITS) )
  CALL check( nf90_put_att(ncid_out, lat_varid_out, DESCRIB, LAT_DESCRIB) )
  !LONGITUDE
  CALL check( nf90_def_var(ncid_out, LON_NAME, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), lon_varid_out) )
  CALL check( nf90_put_att(ncid_out, lon_varid_out, UNITS, LON_UNITS) )
  CALL check( nf90_put_att(ncid_out, lon_varid_out, DESCRIB, LON_DESCRIB) )
  !T2m
  CALL check( nf90_def_var(ncid_out, T2m_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), t2m_varid) )
  CALL check( nf90_put_att(ncid_out, t2m_varid, UNITS, TEMP_UNITS) )
  CALL check( nf90_put_att(ncid_out, t2m_varid, DESCRIB, T2m_DESCRIB) )
  !DEWPOINT
  CALL check( nf90_def_var(ncid_out, TD2_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), TD2_varid) )
  CALL check( nf90_put_att(ncid_out, TD2_varid, UNITS, TEMP_UNITS) )
  CALL check( nf90_put_att(ncid_out, TD2_varid, DESCRIB, TD2_DESCRIB) )
  !FOG
  CALL check( nf90_def_var(ncid_out, FOG_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), fog_varid) )
  CALL check( nf90_put_att(ncid_out, fog_varid, UNITS, TEMP_UNITS) )
  CALL check( nf90_put_att(ncid_out, fog_varid, DESCRIB, fog_DESCRIB) )
  !RH2
  CALL check( nf90_def_var(ncid_out, RH2_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), rh2_varid) )
  CALL check( nf90_put_att(ncid_out, rh2_varid, UNITS, RH2_UNITS) )
  CALL check( nf90_put_att(ncid_out, rh2_varid, DESCRIB, RH2_DESCRIB) )
  !SLP
  CALL check( nf90_def_var(ncid_out, SLP_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), slp_varid) )
  CALL check( nf90_put_att(ncid_out, slp_varid, UNITS, SLP_UNITS) )
  CALL check( nf90_put_att(ncid_out, slp_varid, DESCRIB, SLP_DESCRIB) )
  !INVERSE MONIN OBUKHOV LENGTH
  CALL check( nf90_def_var(ncid_out, IMONIN_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), imonin_varid) )
  CALL check( nf90_put_att(ncid_out, imonin_varid, UNITS, IMONIN_UNITS) )
  CALL check( nf90_put_att(ncid_out, imonin_varid, DESCRIB, IMONIN_DESCRIB) )
  !ROUGHNESS LENGTH
  CALL check( nf90_def_var(ncid_out, Z0_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), z0_varid) )
  CALL check( nf90_put_att(ncid_out, z0_varid, UNITS, Z0_UNITS) )
  CALL check( nf90_put_att(ncid_out, z0_varid, DESCRIB, Z0_DESCRIB) )
  !U*
  CALL check( nf90_def_var(ncid_out, USTAR_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), ustar_varid) )
  CALL check( nf90_put_att(ncid_out, ustar_varid, UNITS, USTAR_UNITS) )
  CALL check( nf90_put_att(ncid_out, ustar_varid, DESCRIB, USTAR_DESCRIB) )
  !WSPD
  CALL check( nf90_def_var(ncid_out, WSPD_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), wspd_varid) )
  CALL check( nf90_put_att(ncid_out, wspd_varid, UNITS, WSPD_UNITS) )
  CALL check( nf90_put_att(ncid_out, wspd_varid, DESCRIB, WSPD_DESCRIB) )
  !WSPD
  CALL check( nf90_def_var(ncid_out, WDIR_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), wdir_varid) )
  CALL check( nf90_put_att(ncid_out, wdir_varid, UNITS, WDIR_UNITS) )
  CALL check( nf90_put_att(ncid_out, wdir_varid, DESCRIB, WDIR_DESCRIB) )
  !GUST
  CALL check( nf90_def_var(ncid_out, GUST_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), gust_varid) )
  CALL check( nf90_put_att(ncid_out, gust_varid, UNITS, GUST_UNITS) )
  CALL check( nf90_put_att(ncid_out, gust_varid, DESCRIB, GUST_DESCRIB) )
  !PREC
  CALL check( nf90_def_var(ncid_out, PREC_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), prec_varid) )
  CALL check( nf90_put_att(ncid_out, prec_varid, UNITS, PREC_UNITS) )
  CALL check( nf90_put_att(ncid_out, prec_varid, DESCRIB, PREC_DESCRIB) )
  !SNOW
  CALL check( nf90_def_var(ncid_out, SNOW_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), snow_varid) )
  CALL check( nf90_put_att(ncid_out, snow_varid, UNITS, SNOW_UNITS) )
  CALL check( nf90_put_att(ncid_out, snow_varid, DESCRIB, SNOW_DESCRIB) )
  !CLOUD COVER
  CALL check( nf90_def_var(ncid_out, CLOUD_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), cloud_varid) )
  CALL check( nf90_put_att(ncid_out, cloud_varid, UNITS, CLOUD_UNITS) )
  CALL check( nf90_put_att(ncid_out, cloud_varid, DESCRIB, CLOUD_DESCRIB) )
  !SOIL TEMPERATURE
  CALL check( nf90_def_var(ncid_out, SOILTC_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), soiltc_varid) )
  CALL check( nf90_put_att(ncid_out, soiltc_varid, UNITS, soiltc_UNITS) )
  CALL check( nf90_put_att(ncid_out, soiltc_varid, DESCRIB, soiltc_DESCRIB) )
  !SURFACE TEMPERATURE
  CALL check( nf90_def_var(ncid_out, SURFTC_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), surftc_varid) )
  CALL check( nf90_put_att(ncid_out, surftc_varid, UNITS, surftc_UNITS) )
  CALL check( nf90_put_att(ncid_out, surftc_varid, DESCRIB, surftc_DESCRIB) )
  !APPARENT TEMPERATURE
  CALL check( nf90_def_var(ncid_out, AT_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), at_varid) )
  CALL check( nf90_put_att(ncid_out, at_varid, UNITS, at_UNITS) )
  CALL check( nf90_put_att(ncid_out, at_varid, DESCRIB, at_DESCRIB) )
  !PRECIPITATION PROBABILITY
  CALL check( nf90_def_var(ncid_out, PRECPROB_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), precprob_varid) )
  CALL check( nf90_put_att(ncid_out, precprob_varid, UNITS, precprob_UNITS) )
  CALL check( nf90_put_att(ncid_out, precprob_varid, DESCRIB, precprob_DESCRIB) )
  !PRECIPITATION HIGH RANGE
  CALL check( nf90_def_var(ncid_out, PRECPCTH_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), precpcth_varid) )
  CALL check( nf90_put_att(ncid_out, precpcth_varid, UNITS, PRECPCTH_UNITS) )
  CALL check( nf90_put_att(ncid_out, precpcth_varid, DESCRIB, PRECPCTH_DESCRIB) )
  !PRECIPITATION LOW RANGE
  CALL check( nf90_def_var(ncid_out, PRECPCTL_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), precpctl_varid) )
  CALL check( nf90_put_att(ncid_out, precpctl_varid, UNITS, PRECPCTL_UNITS) )
  CALL check( nf90_put_att(ncid_out, precpctl_varid, DESCRIB, PRECPCTL_DESCRIB) )
  !CAPE
  CALL check( nf90_def_var(ncid_out, CAPE_NAME_OUT, NF90_REAL, (/lon_dimid,lat_dimid,time_dimid/), cape_varid) )
  CALL check( nf90_put_att(ncid_out, cape_varid, UNITS, CAPE_UNITS) )
  CALL check( nf90_put_att(ncid_out, cape_varid, DESCRIB, CAPE_DESCRIB) )


  CALL check( nf90_put_att(ncid_out, NF90_GLOBAL, STARTDATE,  starttime) )

  !DONE DEFINE!
  CALL check( nf90_enddef(ncid_out) )
  !WRITE VARIABLES TO FILE!
  CALL check( nf90_put_var(ncid_out, lat_varid_out, lats) )
  CALL check( nf90_put_var(ncid_out, lon_varid_out, lons) )
  CALL check( nf90_put_var(ncid_out, t2m_varid, T2m_celcius) )
  CALL check( nf90_put_var(ncid_out, TD2_varid, TD2_celcius) )
  CALL check( nf90_put_var(ncid_out, fog_varid, FOG) )
  CALL check( nf90_put_var(ncid_out, rh2_varid, RH2))
  CALL check( nf90_put_var(ncid_out, slp_varid, SLP))
  CALL check( nf90_put_var(ncid_out, imonin_varid, IMONIN))
  CALL check( nf90_put_var(ncid_out, z0_varid, Z0))
  CALL check( nf90_put_var(ncid_out, ustar_varid, USTAR))
  CALL check( nf90_put_var(ncid_out, wspd_varid, WSPD) )
  CALL check( nf90_put_var(ncid_out, wdir_varid, WDIR) )
  CALL check( nf90_put_var(ncid_out, gust_varid, GUST) )
  CALL check( nf90_put_var(ncid_out, prec_varid, PREC) )
  CALL check( nf90_put_var(ncid_out, snow_varid, SNOW) )
  CALL check( nf90_put_var(ncid_out, cloud_varid, CLOUD))
  CALL check( nf90_put_var(ncid_out, soiltc_varid, SOILTC))
  CALL check( nf90_put_var(ncid_out, surftc_varid, SURFTC))
  CALL check( nf90_put_var(ncid_out, at_varid, AT))
  CALL check( nf90_put_var(ncid_out, precprob_varid, PRECPROB))
  CALL check( nf90_put_var(ncid_out, precpctl_varid, PRECPCTL))
  CALL check( nf90_put_var(ncid_out, precpcth_varid, PRECPCTH))
  CALL check( nf90_put_var(ncid_out, cape_varid, CAPE))

  CALL check( nf90_close(ncid_out) )

CONTAINS
SUBROUTINE check(status)
  INTEGER, INTENT ( IN) :: status
  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop "Stopped"
  end if
END SUBROUTINE check

END PROGRAM WRF_POST_PROCESSING
