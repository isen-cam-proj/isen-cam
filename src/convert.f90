!
! Function 
!    convert pressure coordinate to isentropic coordinate
!   
! Arguments(in)
!   p_ref  : reference pressure level [hPa]
!   pt_ref : reference isentrope level [K]
!   ks     : number of the lowest layer 
!   pts    : surface potential temperature [K]
!   ps     : surface pressure [hPa]
!   us     : surface zonal wind [m/s]
!   vs     : surface meridional wind [m/s]
!   topo   : altitude [m]
!   pt     : potential temperature [K]
!   g      : Height [m]
!   u      : zonal wind [m/s]
!   v      : meridional wind [m/s]
!
! Arguments(out)
!   p_pt   : pressure of isentropic coordinate [hPa]
!   g_pt   : Height of isentropic coordinate [m]
!   u_pt   : zonal wind of isentropic coordiante [m/s]
!   v_pt   : meridional wind of isentropic coordinate [m/s]
!  
! Note
!  -Potential temperature under the ground is set to its surface value
subroutine convert_vertical_coordinate(p_ref, pt_ref, ks, &
     &                                 pts,ps,us,vs,topo, pt,g,u,v, & ! input 
     &                                 p_pt,g_pt,u_pt,v_pt)          ! output
  use parameter, only : undef, eps, p_min, p_max, g_min, g_max, &
       &                wind_min, wind_max
  use com_var, only : nx, ny, nz, nz_pt
  implicit none
  real(4),intent(in) :: p_ref(nz), pt_ref(nz_pt)
  integer,intent(in) :: ks(nx,ny)
  real(4),intent(in) :: pts(nx,ny), ps(nx,ny), us(nx,ny), vs(nx,ny)
  real(4),intent(in) :: topo(nx,ny)
  real(4),intent(in) :: pt(nx,ny,nz), g(nx,ny,nz), &
       & u(nx,ny,nz), v(nx,ny,nz)
  !
  real(4),intent(out) :: p_pt(nx,ny,nz_pt), g_pt(nx,ny,nz_pt), &
       & u_pt(nx,ny,nz_pt), v_pt(nx,ny,nz_pt)

  ! work array
  real(4),allocatable :: ptwk(:,:,:), gwk(:,:,:), uwk(:,:,:), vwk(:,:,:)
  !
  real(4) :: ptmin, gmin, umin, vmin
  real(4) :: p1, p2
  !
  real(4) :: a
  real(4) :: w1, w2
  integer :: kst, ken, kmin
  !
  integer :: i, j, k, kk
  !
  allocate( ptwk(nx,ny,0:nz),gwk(nx,ny,0:nz),uwk(nx,ny,0:nz),vwk(nx,ny,0:nz) )
  !
  ptwk(1:nx, 1:ny, 1:nz) = pt(1:nx, 1:ny, 1:nz)
  gwk(1:nx, 1:ny, 1:nz) = g(1:nx, 1:ny, 1:nz)
  uwk(1:nx, 1:ny, 1:nz) = u(1:nx, 1:ny, 1:nz)
  vwk(1:nx, 1:ny, 1:nz) = v(1:nx, 1:ny, 1:nz)
  
  !---- set lower boundary ---!
  do j = 1,ny
    do i = 1,nx
      kmin = ks(i,j)
      ptmin = pts(i,j)
      gmin = max( topo(i,j), 0.0 )
      umin = us(i,j)
      vmin = vs(i,j)
      do k = 0,kmin
         ptwk(i,j,k) = ptmin
         gwk(i,j,k) = gmin
         uwk(i,j,k) = umin
         vwk(i,j,k) = vmin
      enddo
    enddo
  enddo
  !
  p_pt = undef
  g_pt = undef
  u_pt = undef
  v_pt = undef
  !
  do k = 1,nz_pt
    do j = 1,ny
      do i = 1,nx
        ! --- lower boundary check
        kmin = ks(i,j)
        if( ptwk(i,j,kmin) <= pt_ref(k) )then
          ! --- search 
          kkloop: do kk = kmin,nz-1
            a = (ptwk(i,j,kk)-pt_ref(k))*(ptwk(i,j,kk+1)-pt_ref(k))
            if( a <= 0.0 )then
              kst = kk
              ken = kk + 1
           !--- check inversion layer
           !if( ptwk(i,j,ken)-ptwk(i,j,kst) <= 0.0 )then
           !print'(A,3I4,2F9.2)','IL: ',i,j,kk,ptwk(i,j,kst),ptwk(i,j,kst)
           !endif
              ! --- calculate weight
              w1 = (ptwk(i,j,ken) - pt_ref(k))/(ptwk(i,j,ken)-ptwk(i,j,kst))
              w2 = (pt_ref(k) - ptwk(i,j,kst))/(ptwk(i,j,ken)-ptwk(i,j,kst))
              ! --- linear interpolation
              if( kk == 0 )then
                p1 = ps(i,j)
              else
                if( ps(i,j) <= p_ref(kst) )then
                  p1 = ps(i,j)
                else
                  p1 = p_ref(kst)
                endif
              endif
              p2 = p_ref(ken)
              p_pt(i,j,k) = w1 * p1 + w2 * p2
              g_pt(i,j,k) = w1 * gwk(i,j,kst) + w2 * gwk(i,j,ken)
              u_pt(i,j,k) = w1 * uwk(i,j,kst) + w2 * uwk(i,j,ken)
              v_pt(i,j,k) = w1 * vwk(i,j,kst) + w2 * vwk(i,j,ken)
              ! --- check pressure inversion
              !if( p1-p2 <= 0.0 )then
              !  print'(A,3I4,2F9.2)','IL: ',i,j,kk,p1,p2
              !endif
              exit kkloop
            endif
          enddo kkloop
          ! ---
        else
          p_pt(i,j,k) = ps(i,j) + eps
          !p_pt(i,j,k) = ps(i,j)
          g_pt(i,j,k) = 0.0
          u_pt(i,j,k) = us(i,j)
          v_pt(i,j,k) = vs(i,j)
        endif
      enddo
    enddo
  enddo

  ! --- check min/max values
  call check_range( nx, ny, nz_pt, p_pt, p_min, p_max, 'convert()','p_pt')
  call check_range( nx, ny, nz_pt, g_pt, g_min, g_max, 'convert()','g_pt')
  call check_range( nx, ny, nz_pt, u_pt, wind_min,wind_max, 'convert()','u_pt')
  call check_range( nx, ny, nz_pt, v_pt, wind_min,wind_max, 'convert()','v_pt')

  write(45,'(A7,2E12.4)') ' p_pt: ',minval(p_pt),maxval(p_pt)
  write(45,'(A7,2E12.4)') ' g_pt: ',minval(g_pt),maxval(g_pt)
  write(45,'(A7,2E12.4)') ' u_pt: ',minval(u_pt),maxval(u_pt)
  write(45,'(A7,2E12.4)') ' v_pt: ',minval(v_pt),maxval(v_pt)

  return
end subroutine convert_vertical_coordinate
