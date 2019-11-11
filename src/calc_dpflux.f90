!
! Function
!   calculate cold air mass flux
! 
!    int^{ps}_{p_pt} u dp
!
! Arguments(in)
!   ks    : number of the lowest layer
!   p_ref : reference pressure [hPa]
!   ps    : surface pressure [hPa]
!   us    : surface zonal wind [m/s]
!   vs    : sruface meridional wind [m/s]
!   u     : zonal wind of pressure coordinate [m/s]
!   v     : meridional wind of pressure coordinate [m/s]
!   p_pt  : pressure of isentropic coordinate [hPa]
!   u_pt  : zonal wind of isentropic coordinate [m/s]
!   v_pt  : meridional wind of isentropic coordinate [m/s]
!
! Arguments(out)
!   uf_pt : cold air mass flux zonal component [hPa*m/s]
!   vf_pt : cold air mass flux meridional component [hPa*m/s]
!
subroutine calc_cold_air_flux(ks, p_ref, ps,us,vs, u,v, p_pt,u_pt,v_pt, &
     &                        uf_pt,vf_pt)
  use parameter, only : undef, flux_min, flux_max
  use com_var, only : nx, ny, nz, nz_pt
  implicit none
  integer,intent(in) :: ks(nx,ny)
  real(4),intent(in) :: p_ref(nz)
  real(4),intent(in) :: ps(nx,ny), us(nx,ny), vs(nx,ny)
  real(4),intent(in) :: u(nx,ny,nz), v(nx,ny,nz)               
  real(4),intent(in) :: p_pt(nx,ny,nz_pt), u_pt(nx,ny,nz_pt), v_pt(nx,ny,nz_pt)
  !
  real(4),intent(out) :: uf_pt(nx,ny,nz_pt), vf_pt(nx,ny,nz_pt)  
  !
  integer :: i, j, k, kk,kst, iflag, kkcount(nx,ny)
  real(4) :: dp, p1, u1, v1, p2, u2, v2

  uf_pt = 0.0
  vf_pt = 0.0
  iflag = 0
  kkcount = 1


  do k = 1, nz
     do j = 1, ny
        do i = 1, nx
           kk = kkcount(i,j)
           if( kk <= nz_pt )then
              kst = ks(i,j)
11               continue
                 !---- lower ----
                 if( iflag /= 0 ) then
                    iflag = 0
                    p1 = p_pt(i,j,kk-1)  
                    u1 = u_pt(i,j,kk-1)
                    v1 = v_pt(i,j,kk-1)
                 else
                    if( kst >= k-1) then
                       p1 = ps(i,j)        
                       u1 = us(i,j)        
                       v1 = vs(i,j)        
                    else 
                       p1 = p_ref(k-1)
                       u1 = u(i,j,k-1)
                       v1 = v(i,j,k-1)
                    endif
                 endif
                    
                 !---- upper ----
                 if( p_pt(i,j,kk) > p_ref(k) ) then
                    p2 = p_pt(i,j,kk)
                    u2 = u_pt(i,j,kk)
                    v2 = v_pt(i,j,kk)
                    iflag = 1
                 else
                    p2 = p_ref(k)
                    u2 = u(i,j,k)
                    v2 = v(i,j,k)
                 endif
                 
                 ! trapeziodal integration
                 dp = max( p1-p2, 0.0)

                 uf_pt(i,j,kk) = 0.5 * ( u1 + u2 ) * dp &
                      &        + uf_pt(i,j,kk)
                 
                 vf_pt(i,j,kk) = 0.5 * ( v1 + v2 ) * dp &
                      &        + vf_pt(i,j,kk)
                 
              !---- upper check ----
              if( iflag /= 0 ) then
                 kkcount(i,j) = kkcount(i,j) + 1
                 kk = kkcount(i,j)
                 if( kk <= nz_pt ) then 
                    uf_pt(i,j,kk) = uf_pt(i,j,kk-1)
                    vf_pt(i,j,kk) = vf_pt(i,j,kk-1)
                    goto 11
                 else
                    iflag = 0
                 endif
              endif
           endif
        enddo
     enddo
  enddo


  !--- check min/max values ---
  call check_range( nx, ny, nz_pt, uf_pt, flux_min, flux_max, &
       &            'calc_dpflux()', 'uf_pt')
  call check_range( nx, ny, nz_pt, vf_pt, flux_min, flux_max, &
       &            'calc_dpflux()', 'vf_pt')

  write(45,'(A7,2E12.4)') 'uf_pt: ',minval(uf_pt),maxval(uf_pt)
  write(45,'(A7,2E12.4)') 'vf_pt: ',minval(vf_pt),maxval(vf_pt)


  return
endsubroutine calc_cold_air_flux
