!===========================================================================
!
!  Commom variable thoughout the program
!    Do not change variable out of this module
!
!===========================================================================
module com_var
  implicit none
  
  integer :: nx         ! x (East -> West) direction grid number
  integer :: ny         ! y (South -> North) direction gird number
  integer :: nz         ! input data z direction levels 
  integer :: nz_pt      ! output data isentrope levels
  !
  real(4),allocatable :: p_ref(:)   ! standard pressure levels [hPa]
  real(4),allocatable :: pt_ref(:)  ! output isentrope levels [K]
  !
  real(4),allocatable :: alat(:)     ! latitude [rad]
  real(4),allocatable :: stg_sintbl(:)   ! staggered sin(latitude)  
  real(4),allocatable :: stg_costbl(:)   ! staggered cos(latitude)


contains
  subroutine com_var_ini( nx_in, ny_in, nz_in, nz_pt_in )
    integer,intent(in) :: nx_in
    integer,intent(in) :: ny_in
    integer,intent(in) :: nz_in
    integer,intent(in) :: nz_pt_in
    
    nx    = nx_in
    ny    = ny_in
    nz    = nz_in
    nz_pt = nz_pt_in

    allocate( p_ref(nz) )
    allocate( pt_ref(nz_pt) )
    allocate( alat(ny) )
    allocate( stg_sintbl(ny-1) )
    allocate( stg_costbl(ny-1) )
    
  end subroutine com_var_ini


  subroutine com_var_end()
    deallocate( p_ref )
    deallocate( pt_ref )
    deallocate( alat )
    deallocate( stg_sintbl )
    deallocate( stg_costbl )
  end subroutine com_var_end


  !==================================
  !
  !          Set variables  
  !
  !==================================

  ! p_ref [hPa]
  subroutine com_var_pref()
    use namelist, only : INPUT_ZDEF_LEVEL
    if( INPUT_ZDEF_LEVEL(1) > INPUT_ZDEF_LEVEL(2) ) then  ! Lower -> Upper
       p_ref(1:nz) = INPUT_ZDEF_LEVEL(1:nz)
    else
       p_ref(1:nz) = INPUT_ZDEF_LEVEL(nz:1:-1)
    endif
  end subroutine com_var_pref

  ! pt_ref [K]
  subroutine com_var_ptref()
    use namelist, only : OUTPUT_ISEN_LEVEL
    if( OUTPUT_ISEN_LEVEL(1) < OUTPUT_ISEN_LEVEL(2) ) then ! Lower -> Upper
       pt_ref(1:nz_pt) = OUTPUT_ISEN_LEVEL(1:nz_pt)
    else 
       pt_ref(1:nz_pt) = OUTPUT_ISEN_LEVEL(nz_pt:1:-1)
    endif
  end subroutine com_var_ptref

  ! alat [rad]
  subroutine com_var_alat()
    use namelist, only : INPUT_YDEF_TYPE, INPUT_YDEF_LEVEL, &
         &               INPUT_YDEF_SOUTH, INPUT_YDEF_NORTH
    use parameter, only : rad
    !
    real(4) :: rjm
    integer :: j
    
    ! - all variables should be NOT YREV (South -> North)
    if( INPUT_YDEF_TYPE == 'lat_radian' ) then
       
       if( INPUT_YDEF_LEVEL(1) < INPUT_YDEF_LEVEL(2) ) then
          alat(1:ny) = INPUT_YDEF_LEVEL(1:ny)
       else
          alat(1:ny) = INPUT_YDEF_LEVEL(ny:1:-1)
       endif
       
    else if( INPUT_YDEF_TYPE == 'lat_degree' ) then
       
       if( INPUT_YDEF_LEVEL(1) < INPUT_YDEF_LEVEL(2) ) then
          alat(1:ny) = INPUT_YDEF_LEVEL(1:ny) * rad
       else 
          alat(1:ny) = INPUT_YDEF_LEVEL(ny:1:-1) * rad
       endif

    else if( INPUT_YDEF_TYPE == 'linear' ) then
       
       rjm = ( INPUT_YDEF_NORTH - INPUT_YDEF_SOUTH ) / ( ny - 1 )
       do j = 1, ny
          alat(j) = ( INPUT_YDEF_SOUTH + (j-1) * rjm ) * rad
       enddo
    
    end if
    
  end subroutine com_var_alat


  ! staggered sine/cosine table
  subroutine com_var_stg_tbl()
    real(4) :: phi(ny-1)
    integer :: j
    do j = 1,ny-1
       phi(j) = 0.5 * ( alat(j) + alat(j+1) )
    enddo
    stg_sintbl(:) = sin( phi(:) )
    stg_costbl(:) = cos( phi(:) )

  end subroutine com_var_stg_tbl

end module com_var
