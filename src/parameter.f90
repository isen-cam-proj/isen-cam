!====================================================================
!
!   Parameter
!
!====================================================================
module parameter
  implicit none
  
  real(4),parameter :: rd      = 287.0          ! gas constant [J/K/kg]
  real(4),parameter :: cp      = 1004.5         ! specific heat [J/K/kg]
  real(4),parameter :: rdvcp   = rd/cp          ! Rd/Cp
  real(4),parameter :: p0      = 1000.0         ! standard pressure [hPa]
  real(4),parameter :: eradius = 6.371e+6       ! radius of earth [m]
  real(4),parameter :: pi      = 3.14159265359  ! 
  real(4),parameter :: rad     = pi / 180.0     ! radian
  real(4),parameter :: grav    = 9.81           ! gravitational const.[m/s^2]
  real(4),parameter :: gamma   = 6.5e-3         ! lapse late
  real(4),parameter :: undef   = 9.999E+20      ! undefine value
  real(4),parameter :: eps     = 1.e-2          !  [hPa]
 
  ! possible range of each variable ( used in check_range() ) 
  real(4),parameter :: t_min    = 100.0       ! Temperature [K]
  real(4),parameter :: t_max    = 10000.0     ! Temperature [K]
  real(4),parameter :: pt_min   = 100.0       ! Potential temperature [K]
  real(4),parameter :: pt_max   = 10000.0     ! Potential temperature [K]
  real(4),parameter :: wind_min = -500.0      ! Horizontal wind [m/s]
  real(4),parameter :: wind_max = 500         ! Horizontal wind [m/s]
  real(4),parameter :: p_min    = 1.0e-10     ! Pressure [hPa]
  real(4),parameter :: p_max    = 1200.0      ! Pressure [hPa]
  real(4),parameter :: g_min    = -3000.0     ! Height [m]
  real(4),parameter :: g_max    = 1.0e+6      ! Height [m]
  real(4),parameter :: topo_min = -1000.0     ! Altitude [m]
  real(4),parameter :: topo_max = 10000.0     ! Altitude [m]
  !
  real(4),parameter :: dp_min   = -1.0        ! Cold air mass amount [hPa]
  real(4),parameter :: dp_max   = 1200.0      ! Cold air mass amount [hPa]
  real(4),parameter :: flux_min = -1.0e+6     ! Cold air mass flux [hPa*m/s]
  real(4),parameter :: flux_max =  1.0e+6     ! Cold air mass flux [hPa*m/s]
  real(4),parameter :: nhc_min  = -1.0        ! Negative heat content [K*hPa]
  real(4),parameter :: nhc_max  =  1.0e+7     ! Negative heat content [K*hPa]
  real(4),parameter :: nhf_min  = -1.0e+10    ! Negative heat flux [K*hPa*m/s]
  real(4),parameter :: nhf_max  =  1.0e+10    ! Negative heat flux [K*hPa*m/s]

end module parameter
