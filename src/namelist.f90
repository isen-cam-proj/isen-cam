!===========================================================
!
!    Namelist
!
!==========================================================
module namelist
  implicit none

  integer,parameter :: ny_max = 512
  integer,parameter :: nz_max = 2048

  character(1024) :: INPUT_U_FILENAME
  character(1024) :: INPUT_V_FILENAME
  character(1024) :: INPUT_T_FILENAME
  character(1024) :: INPUT_Z_FILENAME
  character(1024) :: INPUT_US_FILENAME
  character(1024) :: INPUT_VS_FILENAME
  character(1024) :: INPUT_TS_FILENAME
  character(1024) :: INPUT_PS_FILENAME
  character(1024) :: INPUT_MSL_FILENAME
  character(1024) :: INPUT_TOPO_FILENAME

  character(1024) :: INPUT_UNIT_Z
  character(1024) :: INPUT_UNIT_PS
  character(1024) :: INPUT_UNIT_MSL
  character(1024) :: INPUT_UNIT_TOPO

  character(1024) :: INPUT_UNDEF_DEFAULT
  character(1024) :: INPUT_UNDEF_U
  character(1024) :: INPUT_UNDEF_V
  character(1024) :: INPUT_UNDEF_T
  character(1024) :: INPUT_UNDEF_Z
  character(1024) :: INPUT_UNDEF_US
  character(1024) :: INPUT_UNDEF_VS
  character(1024) :: INPUT_UNDEF_TS
  character(1024) :: INPUT_UNDEF_PS
  character(1024) :: INPUT_UNDEF_MSL
  real(4)         :: UNDEF_DEFAULT
  real(4)         :: UNDEF_U
  real(4)         :: UNDEF_V
  real(4)         :: UNDEF_T
  real(4)         :: UNDEF_Z
  real(4)         :: UNDEF_US
  real(4)         :: UNDEF_VS
  real(4)         :: UNDEF_TS
  real(4)         :: UNDEF_PS
  real(4)         :: UNDEF_MSL

  character(1024) :: INPUT_ENDIAN_DEFAULT
  character(1024) :: INPUT_ENDIAN_U
  character(1024) :: INPUT_ENDIAN_V
  character(1024) :: INPUT_ENDIAN_T
  character(1024) :: INPUT_ENDIAN_Z
  character(1024) :: INPUT_ENDIAN_US
  character(1024) :: INPUT_ENDIAN_VS
  character(1024) :: INPUT_ENDIAN_TS
  character(1024) :: INPUT_ENDIAN_PS
  character(1024) :: INPUT_ENDIAN_MSL
  character(1024) :: INPUT_ENDIAN_TOPO
  integer         :: ENDIAN_DEFAULT
  integer         :: ENDIAN_U
  integer         :: ENDIAN_V
  integer         :: ENDIAN_T
  integer         :: ENDIAN_Z
  integer         :: ENDIAN_US
  integer         :: ENDIAN_VS
  integer         :: ENDIAN_TS
  integer         :: ENDIAN_PS
  integer         :: ENDIAN_MSL
  integer         :: ENDIAN_TOPO

  integer         :: INPUT_XDEF_NUM

  character(1024) :: INPUT_YDEF_TYPE
  integer         :: INPUT_YDEF_NUM
  real(4)         :: INPUT_YDEF_LEVEL(ny_max)
  real(4)         :: INPUT_YDEF_SOUTH
  real(4)         :: INPUT_YDEF_NORTH
  integer         :: INPUT_YDEF_YREV_DEFAULT
  integer         :: INPUT_YDEF_YREV_TOPO

  integer         :: INPUT_ZDEF_NUM
  real(4)         :: INPUT_ZDEF_LEVEL(nz_max)
  integer         :: INPUT_ZDEF_ZREV

  character(1024) :: INPUT_TDEF_TYPE
  integer         :: INPUT_TDEF_DAYNUM
  integer         :: INPUT_TDEF_LEAP_YEAR
  integer         :: INPUT_TDEF_TSTEP
  real(4)         :: INPUT_TDEF_DT
  integer         :: INITIAL_TIME(5)
  integer         :: END_TIME(5)

  character(1024) :: OUTPUT_FILENAME
  character(1024) :: OUTPUT_LOG_FILENAME
  
  integer         :: OUTPUT_P
  integer         :: OUTPUT_G
  integer         :: OUTPUT_NHC
  integer         :: OUTPUT_UNHF
  integer         :: OUTPUT_VNHF
  integer         :: OUTPUT_NHCGEN
  
  integer         :: OUTPUT_ISEN_LEV_NUM
  real(4)         :: OUTPUT_ISEN_LEVEL(nz_max)


contains
  !
  !  read Namelist and read default value
  !
  subroutine namelist_init()


    !**** declare *****
    namelist/INPUT/INPUT_U_FILENAME, &
         &         INPUT_V_FILENAME, &
         &         INPUT_T_FILENAME, &
         &         INPUT_Z_FILENAME, &
         &         INPUT_US_FILENAME, &
         &         INPUT_VS_FILENAME, &
         &         INPUT_TS_FILENAME, &
         &         INPUT_PS_FILENAME, &
         &         INPUT_MSL_FILENAME, &
         &         INPUT_TOPO_FILENAME

    namelist/INPUT_UNIT/INPUT_UNIT_Z, &
         &              INPUT_UNIT_PS, &
         &              INPUT_UNIT_MSL, &
         &              INPUT_UNIT_TOPO

    namelist/INPUT_UNDEF/INPUT_UNDEF_DEFAULT, &
         &               INPUT_UNDEF_U, &
         &               INPUT_UNDEF_V, &
         &               INPUT_UNDEF_T, &
         &               INPUT_UNDEF_Z, &
         &               INPUT_UNDEF_US, &
         &               INPUT_UNDEF_VS, &
         &               INPUT_UNDEF_TS, &
         &               INPUT_UNDEF_PS, &
         &               INPUT_UNDEF_MSL

    namelist/INPUT_ENDIAN/INPUT_ENDIAN_DEFAULT,&
         &               INPUT_ENDIAN_U, &
         &               INPUT_ENDIAN_V, &
         &               INPUT_ENDIAN_T, &
         &               INPUT_ENDIAN_Z, &
         &               INPUT_ENDIAN_US, &
         &               INPUT_ENDIAN_VS, &
         &               INPUT_ENDIAN_TS, &
         &               INPUT_ENDIAN_PS, &
         &               INPUT_ENDIAN_MSL, &
         &               INPUT_ENDIAN_TOPO

    namelist/INPUT_XDEF/INPUT_XDEF_NUM

    namelist/INPUT_YDEF/INPUT_YDEF_TYPE, &
         &              INPUT_YDEF_NUM, &
         &              INPUT_YDEF_LEVEL, &
         &              INPUT_YDEF_SOUTH, &
         &              INPUT_YDEF_NORTH, &
         &              INPUT_YDEF_YREV_DEFAULT, &
         &              INPUT_YDEF_YREV_TOPO

    namelist/INPUT_ZDEF/INPUT_ZDEF_NUM, &
         &              INPUT_ZDEF_LEVEL, &
         &              INPUT_ZDEF_ZREV

    namelist/INPUT_TDEF/INPUT_TDEF_TYPE, &
         &              INPUT_TDEF_DAYNUM, &
         &              INPUT_TDEF_LEAP_YEAR, &
         &              INPUT_TDEF_TSTEP, &
         &              INITIAL_TIME, &
         &              END_TIME

    namelist/OUTPUT/OUTPUT_FILENAME, &
         &          OUTPUT_LOG_FILENAME
    
    namelist/OUTPUT_VAR/OUTPUT_P, &
                        OUTPUT_G, &
                        OUTPUT_NHC, &
                        OUTPUT_UNHF, &
                        OUTPUT_VNHF, &
                        OUTPUT_NHCGEN

    namelist/OUTPUT_PTDEF/OUTPUT_ISEN_LEV_NUM, &
         &                OUTPUT_ISEN_LEVEL


    !---- default values ----
    INPUT_PS_FILENAME   = ''
    INPUT_MSL_FILENAME  = ''
    INPUT_TOPO_FILENAME = ''

    INPUT_UNIT_Z    = 'm'
    INPUT_UNIT_PS   = 'hPa'
    INPUT_UNIT_MSL  = 'hPa'
    INPUT_UNIT_TOPO = 'm'

    INPUT_UNDEF_DEFAULT = '9.999E+20'
    INPUT_UNDEF_U      = ''
    INPUT_UNDEF_V      = ''
    INPUT_UNDEF_T      = ''
    INPUT_UNDEF_Z      = ''
    INPUT_UNDEF_US     = ''
    INPUT_UNDEF_VS     = ''
    INPUT_UNDEF_TS     = ''
    INPUT_UNDEF_PS     = ''
    INPUT_UNDEF_MSL    = ''

    INPUT_ENDIAN_DEFAULT = 'little'
    INPUT_ENDIAN_U       = ''
    INPUT_ENDIAN_V       = ''
    INPUT_ENDIAN_T       = ''
    INPUT_ENDIAN_Z       = ''
    INPUT_ENDIAN_US      = ''
    INPUT_ENDIAN_VS      = ''
    INPUT_ENDIAN_TS      = ''
    INPUT_ENDIAN_PS      = ''
    INPUT_ENDIAN_MSL     = ''

    INPUT_YDEF_SOUTH        = -90.0
    INPUT_YDEF_NORTH        =  90.0
    INPUT_YDEF_YREV_DEFAULT = 0
    INPUT_YDEF_YREV_TOPO    = -1

    INPUT_ZDEF_ZREV    = 0

    INPUT_TDEF_LEAP_YEAR = 0

    OUTPUT_P = 0
    OUTPUT_G = 0
    OUTPUT_NHC = 0
    OUTPUT_UNHF = 0
    OUTPUT_VNHF = 0
    OUTPUT_NHCGEN = 0

    !====== read ======
    read(5, nml=INPUT)
    read(5, nml=INPUT_UNIT)
    read(5, nml=INPUT_ENDIAN)
    read(5, nml=INPUT_XDEF)
    read(5, nml=INPUT_YDEF)
    read(5, nml=INPUT_ZDEF)
    read(5, nml=INPUT_TDEF)
    read(5, nml=OUTPUT)
    read(5, nml=OUTPUT_VAR)
    read(5, nml=OUTPUT_PTDEF)


    !====== undef ======
    read(INPUT_UNDEF_DEFAULT,*) UNDEF_DEFAULT

    call undef( INPUT_UNDEF_U,   UNDEF_DEFAULT, UNDEF_U )
    call undef( INPUT_UNDEF_V,   UNDEF_DEFAULT, UNDEF_V )
    call undef( INPUT_UNDEF_T,   UNDEF_DEFAULT, UNDEF_T )
    call undef( INPUT_UNDEF_Z,   UNDEF_DEFAULT, UNDEF_Z )
    call undef( INPUT_UNDEF_US,  UNDEF_DEFAULT, UNDEF_US )
    call undef( INPUT_UNDEF_VS,  UNDEF_DEFAULT, UNDEF_VS )
    call undef( INPUT_UNDEF_TS,  UNDEF_DEFAULT, UNDEF_TS )
    call undef( INPUT_UNDEF_PS,  UNDEF_DEFAULT, UNDEF_PS )
    call undef( INPUT_UNDEF_MSL, UNDEF_DEFAULT, UNDEF_MSL )


    if( INPUT_ENDIAN_DEFAULT == 'little' ) then
       ENDIAN_DEFAULT = 1
    else if( INPUT_ENDIAN_DEFAULT == 'big' ) then
       ENDIAN_DEFAULT = -1
    else
       write(0,*) ' error in namelist_init() : INPUT_ENDIAN_DEFAULT = '&
            &     // INPUT_ENDIAN_DEFAULT
    endif

    call endian( INPUT_ENDIAN_U,    ENDIAN_DEFAULT, ENDIAN_U )
    call endian( INPUT_ENDIAN_V,    ENDIAN_DEFAULT, ENDIAN_V )
    call endian( INPUT_ENDIAN_T,    ENDIAN_DEFAULT, ENDIAN_T )
    call endian( INPUT_ENDIAN_Z,    ENDIAN_DEFAULT, ENDIAN_Z )
    call endian( INPUT_ENDIAN_US,   ENDIAN_DEFAULT, ENDIAN_US )
    call endian( INPUT_ENDIAN_VS,   ENDIAN_DEFAULT, ENDIAN_VS )
    call endian( INPUT_ENDIAN_TS,   ENDIAN_DEFAULT, ENDIAN_TS )
    call endian( INPUT_ENDIAN_PS,   ENDIAN_DEFAULT, ENDIAN_PS )
    call endian( INPUT_ENDIAN_MSL,  ENDIAN_DEFAULT, ENDIAN_MSL )
    call endian( INPUT_ENDIAN_TOPO, ENDIAN_DEFAULT, ENDIAN_TOPO )


    if( INPUT_YDEF_YREV_TOPO == -1 ) &
         &  INPUT_YDEF_YREV_TOPO = INPUT_YDEF_YREV_DEFAULT

    INPUT_TDEF_DT = 24.0 * 60.0 * 60.0 / INPUT_TDEF_DAYNUM 

    !===== check =====
    call namelist_check()

  end subroutine namelist_init


  subroutine undef( undef_char, undef_default, undef_out )
    character(*),intent(in) :: undef_char
    real(4),intent(in)      :: undef_default
    real(4),intent(out)     :: undef_out

    if( undef_char == '' ) then
       undef_out = undef_default
    else
       read(undef_char,*) undef_out
    endif

  end subroutine undef


  subroutine endian( endian_char, endian_default, endian_out )
    character(*),intent(in)  :: endian_char
    integer,intent(in)       :: endian_default
    integer,intent(out)      :: endian_out

    if( endian_char == 'little' ) then
       endian_out = 1
    else if( endian_char == 'big' ) then
       endian_out = -1
    else 
       endian_out = endian_default
    endif

  end subroutine endian


  !
  !  check namelist
  !
  subroutine namelist_check()

    if( INPUT_UNIT_Z /= 'm' .and. INPUT_UNIT_Z /= 'm^2/s^2' ) then
       write(*,*) 'error: INPUT_UNIT_Z=' // trim(INPUT_UNIT_Z)
       stop
    endif

    if( INPUT_UNIT_PS /= 'Pa' .and. INPUT_UNIT_PS /= 'hPa' ) then
       write(*,*) 'error: INPUT_UNIT_PS=' // trim(INPUT_UNIT_PS)
       stop
    endif

    if( INPUT_UNIT_MSL /= 'Pa' .and. INPUT_UNIT_MSL /= 'hPa' ) then
       write(*,*) 'error: INPUT_UNIT_MSL=' // trim(INPUT_UNIT_MSL)
       stop
    endif

    if( INPUT_UNIT_TOPO /= 'm' .and. INPUT_UNIT_TOPO /= 'm^2/s^2' ) then
       write(*,*) 'error: INPUT_UNIT_TOPO=' // trim(INPUT_UNIT_TOPO)
       stop
    endif

    if( INPUT_XDEF_NUM < 1 ) then
       write(*,*) 'error: INPUT_XDEF_NUM=',INPUT_XDEF_NUM
       stop
    endif

    if(    INPUT_YDEF_TYPE /= 'lat_degree' .and. &
         & INPUT_YDEF_TYPE /= 'lat_radian' .and. &
         & INPUT_YDEF_TYPE /= 'linear' ) then
       write(*,*) 'error: INPUT_YDEF_TYPE=', INPUT_YDEF_TYPE
       stop
    endif

    if( INPUT_YDEF_NUM < 1 .or. INPUT_YDEF_NUM > ny_max ) then
       write(*,*) 'error: INPUT_YDEF_NUM=', INPUT_YDEF_NUM
       stop
    endif

    if( INPUT_ZDEF_NUM < 1 .or. INPUT_ZDEF_NUM > nz_max ) then
       write(*,*) 'error: INPUT_ZDEF_NUM=', INPUT_ZDEF_NUM
       stop
    endif


    if(    INPUT_TDEF_TYPE /= 'tstep'   .and. &
         & INPUT_TDEF_TYPE /= 'monthly' .and. &
         & INPUT_TDEF_TYPE /= 'annual' ) then
       write(*,*) 'error: INPUT_TDEF_TYPE=' // trim(INPUT_TDEF_TYPE)
       stop
    endif

    if( INPUT_TDEF_DAYNUM < 0 ) then
       write(*,*) ' error: INPUT_TDEF_DAYNUM=',INPUT_TDEF_DAYNUM
       stop
    endif

    if( INPUT_TDEF_TYPE == 'monthly' .or. INPUT_TDEF_TYPE == 'annual' ) then
       if( INITIAL_TIME(2) < 1 .or. INITIAL_TIME(2) > 12 ) then
          write(*,*) 'error: INITIAL_TIME(month)=', INITIAL_TIME(2)
          stop
       endif
       
       if( END_TIME(2) < 1 .or. END_TIME(2) > 12 ) then
          write(*,*) 'error: END_TIME(month)=', END_TIME(2)
          stop
       endif
       
       if( INITIAL_TIME(4) < 0 .or. INITIAL_TIME(4) > 24 ) then
          write(*,*) 'error: INITIAL_TIME(hour)=', INITIAL_TIME(4)
          stop
       endif
       
       if( END_TIME(4) < 0 .or. END_TIME(4) > 24 ) then
          write(*,*) 'error: END_TIME(hour)=', END_TIME(4)
          stop
       endif

       if( INITIAL_TIME(5) < 0 .or. INITIAL_TIME(5) > 60 ) then
          write(*,*) 'error: INITIAL_TIME(minute)=', INITIAL_TIME(5)
          stop
       endif
       
       if( END_TIME(5) < 0 .or. END_TIME(5) > 60 ) then
          write(*,*) 'error: END_TIME(minute)=', END_TIME(5)
          stop
       endif
    endif

    if( INPUT_TDEF_TYPE == 'tstep' .and. INPUT_TDEF_TSTEP < 1 ) then
       write(*,*) 'error: INPUT_TDEF_TSTEP=',INPUT_TDEF_TSTEP
       stop
    endif

    if( OUTPUT_P /= 0 .and. OUTPUT_P /= 1 ) then
       write(*,*) 'error: OUTPUT_P=',OUTPUT_P
       stop
    endif

    if( OUTPUT_G /= 0 .and. OUTPUT_G /= 1 ) then
       write(*,*) 'error: OUTPUT_G=',OUTPUT_G
       stop
    endif

    if( OUTPUT_NHC /= 0 .and. OUTPUT_NHC /= 1 ) then
       write(*,*) 'error: OUTPUT_NHC=',OUTPUT_NHC
       stop
    endif

    if( OUTPUT_UNHF /= 0 .and. OUTPUT_UNHF /= 1 ) then
       write(*,*) 'error: OUTPUT_UNHF=',OUTPUT_UNHF
       stop
    endif

    if( OUTPUT_NHCGEN /= 0 .and. OUTPUT_NHCGEN /= 1 ) then
       write(*,*) 'error: OUTPUT_NHCGEN=',OUTPUT_NHCGEN
       stop
    endif

    if( OUTPUT_ISEN_LEV_NUM < 1 .or. OUTPUT_ISEN_LEV_NUM > nz_max ) then
       write(*,*) 'error: OUTPUT_ISEN_LEV_NUM=',OUTPUT_ISEN_LEV_NUM
       stop
    endif

  end subroutine namelist_check
end module namelist
