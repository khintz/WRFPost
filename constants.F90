MODULE CONSTANTS
  IMPLICIT NONE

  INTEGER, PARAMETER         :: z_levels = 30
  REAL, PARAMETER            :: pi = 3.14159265359
  REAL, PARAMETER            :: gravit = 9.82
  REAL, PARAMETER            :: karman = 0.4
  REAL, PARAMETER            :: za=2.53*10**8*1000, zb=5.42*10**3, e=0.622
  REAL, PARAMETER            :: KELVIN = 273.15
  REAL, PARAMETER            :: R=287.05, a=0.0065, c=0.12
  INTEGER, PARAMETER         :: Neighbours_points_to_consider = 32

END MODULE CONSTANTS
