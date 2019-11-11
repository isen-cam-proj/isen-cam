!======================================================================
!
! Variable list 
!     
!=====================================================================
module var
  implicit none
  
  !==== input ====
  
  !==== pressure sureface ====
  real(4),allocatable :: u(:,:,:)    ! zonal wind [m/s] 
  real(4),allocatable :: v(:,:,:)    ! meridional wind [m/s]
  real(4),allocatable :: t(:,:,:)    ! temperature [K]
  real(4),allocatable :: g(:,:,:)    ! geopotential height [m]

  !==== surface ====
  real(4),allocatable :: us(:,:)     ! surface zonal wind [m/s]
  real(4),allocatable :: vs(:,:)     ! surface meridional wind [m/s]
  real(4),allocatable :: ts(:,:)     ! surface temperature [m/s]
  real(4),allocatable :: ps(:,:)     ! surrace pressure [hPa]
  real(4),allocatable :: msl(:,:)    ! mean sea level pressure [hPa]
  real(4),allocatable :: topo(:,:)   ! surface height [m]
  
  !==== derived ====
  
  !==== pressure surface ====
  real(4),allocatable :: pt(:,:,:)   ! potential temperature [K]

  !==== surface ====
  real(4),allocatable :: pts(:,:)    ! surface potential temepratur [K]

  !==== isentropic surface ====
  real(4),allocatable :: u_pt(:,:,:) ! zonal wind [m/s]
  real(4),allocatable :: v_pt(:,:,:) ! meridional wind  [m/s]
  real(4),allocatable :: p_pt(:,:,:) ! pressure [hPa]
  real(4),allocatable :: g_pt(:,:,:) ! geopotential height [m]
  !
  real(4),allocatable :: dp_pt(:,:,:) ! cold air mass amount [hPa]
  real(4),allocatable :: uf_pt(:,:,:) ! zonal cold air mass flux [hPa*m/s]
  real(4),allocatable :: vf_pt(:,:,:) ! meridional cold air mass flux [hPa*m/s]
  real(4),allocatable :: cgen_pt(:,:,:) ! cold air mass generation rate [hPa/s]
  !
  real(4),allocatable :: nhc_pt(:,:,:)  ! negarive heat content [K*hPa]
  real(4),allocatable :: unhf_pt(:,:,:) ! NHC flux zonal comp.  [K*hPa*m/s]
  real(4),allocatable :: vnhf_pt(:,:,:) ! NHC flux meridional comp. [K*hPa*m/s]
  real(4),allocatable :: nhcgen_pt(:,:,:) ! NHC generation rate [K*hPa/s]
  !
  real(4),allocatable :: dp_past(:,:,:)  ! DP(t=t-1)  [hPa]
  real(4),allocatable :: dp_dot(:,:,:)   ! time derivative of DP [hPa/s]
  real(4),allocatable :: nhc_past(:,:,:) ! NHC(t=t-1) [K*hPa]
  real(4),allocatable :: nhc_dot(:,:,:)  ! time derivative of NHC [K*hPa/s]
  
  !==== others ====
  integer,allocatable :: ks(:,:)

contains
  subroutine var_ini(nx, ny, nz, nz_pt)
    integer,intent(in) :: nx      ! x(west->east) direction grid number
    integer,intent(in) :: ny      ! y(south->north) direction grid number
    integer,intent(in) :: nz      ! input data z direction levels
    integer,intent(in) :: nz_pt   ! output data z direction levels
    
    allocate( u(nx,ny,nz) )
    allocate( v(nx,ny,nz) )
    allocate( t(nx,ny,nz) )
    allocate( g(nx,ny,nz) )
    !
    allocate( us(nx,ny) )
    allocate( vs(nx,ny) )
    allocate( ts(nx,ny) )
    allocate( ps(nx,ny) )
    allocate( msl(nx,ny) )
    allocate( topo(nx,ny) )
    !
    allocate( pt(nx,ny,nz) )
    !
    allocate( pts(nx,ny) )
    !
    allocate( u_pt(nx,ny,nz_pt) )
    allocate( v_pt(nx,ny,nz_pt) )
    allocate( p_pt(nx,ny,nz_pt) )
    allocate( g_pt(nx,ny,nz_pt) )
    !
    allocate( dp_pt(nx,ny,nz_pt) )
    allocate( uf_pt(nx,ny,nz_pt) )
    allocate( vf_pt(nx,ny,nz_pt) )
    allocate( cgen_pt(nx,ny,nz_pt) )
    !
    allocate( nhc_pt(nx,ny,nz_pt) )
    allocate( unhf_pt(nx,ny,nz_pt) )
    allocate( vnhf_pt(nx,ny,nz_pt) )
    allocate( nhcgen_pt(nx,ny,nz_pt) )
    !
    allocate( dp_past(nx,ny,nz_pt) )
    allocate( dp_dot(nx,ny,nz_pt) )
    allocate( nhc_past(nx,ny,nz_pt) )
    allocate( nhc_dot(nx,ny,nz_pt) )
    !
    allocate( ks(nx,ny) )

  end subroutine var_ini



  subroutine var_end()
    deallocate( u )
    deallocate( v )
    deallocate( t )
    deallocate( g )
    !
    deallocate( us )
    deallocate( vs )
    deallocate( ts )
    deallocate( ps )
    deallocate( msl )
    deallocate( topo )
    !
    deallocate( pt )
    !
    deallocate( pts )
    !
    deallocate( u_pt )
    deallocate( v_pt )
    deallocate( p_pt )
    deallocate( g_pt )
    !
    deallocate( dp_pt )
    deallocate( uf_pt )
    deallocate( vf_pt )
    deallocate( cgen_pt )
    !
    deallocate( nhc_pt )
    deallocate( unhf_pt )
    deallocate( vnhf_pt )
    deallocate( nhcgen_pt )
    !
    deallocate( dp_past )
    deallocate( dp_dot )
    deallocate( nhc_past )
    deallocate( nhc_dot )
    !
    deallocate( ks )
  end subroutine var_end

end module var
  
