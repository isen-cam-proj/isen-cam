!
! Function
!   calculate horizontal divergence
!
!   int{divF}ds = eradius*d(phi)*(Fe-Fw)   
!               + eradius*d(lambda)*cos(j+1/2j)*fn
!               - eradius*d(lambda)*cos(j-1/2j)*fs
!  
!   int{ds} = eradius^2 * d(lambda) * (sin(j+1/2)-sin(j-1/2))
!
!   int{divF}ds / int{ds} = divF
! 
!  
! Arguments(in)
!   uf_pt   : cold air mass flux zonal component
!   vf_pt   : cold air mass flux meridional component
!
! Arguments(out)
!   cgen_pt : horizontal divergence [/s]
!
! Note
!  -sin/cos table are staggered grid   j -> (1+1/2)j
!
subroutine calc_div(uf_pt,vf_pt, cgen_pt)
  use parameter, only : undef, eps, eradius, pi, rad
  use com_var, only : nx, ny, nz_pt, alat, stg_sintbl, stg_costbl
  implicit none
  real(4),intent(in) :: uf_pt(nx,ny,nz_pt), vf_pt(nx,ny,nz_pt)
  !
  real(4),intent(out) :: cgen_pt(nx,ny,nz_pt)
  !
  !---- work array ----
  real(4),allocatable :: uft(:,:,:), vft(:,:,:)
  !
  real(4) :: fn, fs, fe, fw  !flux of north/south/west/east boundary
  real(4) :: int, ds
  real(4) :: dsn, dss
  real(4) :: dlon, dlat
  !
  integer :: i, j, kk
  !
  allocate( uft(0:nx+1,ny,nz_pt), vft(nx,ny,nz_pt) )
  !
  uft(1:nx,1:ny,1:nz_pt) = uf_pt(1:nx,1:ny,1:nz_pt)
  uft(   0,1:ny,1:nz_pt) = uf_pt(  nx,1:ny,1:nz_pt)
  uft(nx+1,1:ny,1:nz_pt) = uf_pt(   1,1:ny,1:nz_pt)
  !
  vft(1:nx,1:ny,1:nz_pt) = vf_pt(1:nx,1:ny,1:nz_pt)
  !
  dlon = 360 / real(nx)     ! [degree]                    
  
  !-------------- calc divergence ------------------
  cgen_pt = 0.0
  !
  do kk = 1,nz_pt
     do j = 2,ny-1
        do i = 1, nx
           !---- calculate flux at boundary
           ! north
           fn = 0.5 * ( vft(i,j,kk) + vft(i,j+1,kk) )
           ! south
           fs = 0.5 * ( vft(i,j-1,kk) + vft(i,j,kk) )
           ! east
           fe = 0.5 * ( uft(i,j,kk) + uft(i+1,j,kk) ) 
           ! west
           fw = 0.5 * ( uft(i-1,j,kk) + uft(i,j,kk) )
           !
           dlat = 0.5 * (alat(j+1) - alat(j-1))  ! [rad]

           !---- calc integration
           int =   eradius * dlat * ( fe - fw )  &
                & -eradius * stg_costbl(j-1) * dlon *rad* fs  &
                & +eradius * stg_costbl(j) * dlon *rad* fn 
           !---- ds :area element
           ds = eradius**2 * (stg_sintbl(j) - stg_sintbl(j-1) )*dlon*rad
           !---- flux
           cgen_pt(i,j,kk) = int/ds
        enddo
     enddo
     !---- pole
     fn = 0.0
     fs = 0.0
     do i = 1,nx
        fn = -0.5 * ( vft(i,ny,kk) + vft(i,ny-1,kk) ) * eradius &
             &   * stg_costbl(ny-1) * dlon * rad + fn
        
        fs = 0.5 * ( vft(i,1,kk) + vft(i,2,kk) ) * eradius &
             &   * stg_costbl(1) * dlon * rad + fs
     enddo
     dsn = 2 * pi * eradius**2 * ( 1-stg_sintbl(ny-1) )
     dss = 2 * pi * eradius**2 * ( -1-stg_sintbl(1) )
     cgen_pt(1,ny,kk) = fn / dsn
     cgen_pt(1, 1,kk) = fs / dss
     !
     do i = 1,nx
        cgen_pt(i, 1,kk) = cgen_pt(1, 1,kk)
        cgen_pt(i,ny,kk) = cgen_pt(1,ny,kk)
     enddo
  enddo



  return
end subroutine calc_div
