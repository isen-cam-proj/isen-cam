!
! Function 
!    calculate negative heat content and its flux
!
! Arguments(in)
!   ks      : number of lowest layer
!   p_ref   : reference pressure [hPa]
!   pt_ref  : reference potential temperature [K]
!   ps      : surface pressure [hPa]
!   pts     : surface potential temperature [K]
!   us      : surface zonal wind [m/s]
!   vs      : surface meridional wind [m/s]
!   pt      : potential temperature [K]
!   u       : zonal wind [m/s] 
!   v       : meridional wind [m/s]
!   p_pt    : pressure of isentropic coordinate [hPa]
!   u_pt    : zonal wind of isentropic coordinate [m/s]
!   v_pt    : meridional wind of isentropic coordinate [m/s]
!   dp_pt   : cold air mass amount [hPa]
!   uf_pt   : zonal cold air mass flux [hPa*m/s]
!   vf_pt   : meridional cold air mass flux [hPa*m/s]
!   
! Arguments(out)
!   nhc_pt  : negative heat content [K*hPa]
!   unhf_pt : zonal component of negative heat flux [K*hPa*m/s]
!   vnhf_pt : meridional component of negarive heat flux [K*hPa*m/s]
!
subroutine calc_nhc(ks, p_ref,pt_ref, ps,pts,us,vs, &
           &        pt,u,v, p_pt,u_pt,v_pt, dp_pt,uf_pt,vf_pt, &
           &        nhc_pt,unhf_pt,vnhf_pt)
  use parameter, only : undef, nhc_min,nhc_max, nhf_min, nhf_max
  use com_var, only : nx, ny, nz, nz_pt
  implicit none
  integer,intent(in) :: ks(nx,ny)
  real(4),intent(in) :: p_ref(nz), pt_ref(nz_pt)
  real(4),intent(in) :: ps(nx,ny), pts(nx,ny)
  real(4),intent(in) :: us(nx,ny), vs(nx,ny)
  real(4),intent(in) :: pt(nx,ny,nz), u(nx,ny,nz), v(nx,ny,nz)
  real(4),intent(in) :: p_pt(nx,ny,nz_pt) 
  real(4),intent(in) :: u_pt(nx,ny,nz_pt), v_pt(nx,ny,nz_pt)  
  real(4),intent(in) :: dp_pt(nx,ny,nz_pt)
  real(4),intent(in) :: uf_pt(nx,ny,nz_pt), vf_pt(nx,ny,nz_pt)
  !
  real(4),intent(out) :: nhc_pt(nx,ny,nz_pt)
  real(4),intent(out) :: unhf_pt(nx,ny,nz_pt), vnhf_pt(nx,ny,nz_pt)  
  !
  integer :: i, j, k, kk,kst, iflag, kkcount(nx,ny)
  real(4) :: dp, p1, u1, v1, p2, u2, v2, pt1,pt2, pth, dpt

  nhc_pt = 0.0
  unhf_pt = 0.0
  vnhf_pt = 0.0
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
                 p1  = p_pt(i,j,kk-1)
                 pt1 = pt_ref(kk-1)
                 u1  = u_pt(i,j,kk-1)
                 v1  = v_pt(i,j,kk-1)
              else
                 if( kst >= k-1) then
                    p1  = ps(i,j)
                    pt1 = pts(i,j)
                    u1  = us(i,j)        
                    v1  = vs(i,j)        
                 else 
                    p1  = p_ref(k-1)
                    pt1 = pt(i,j,k-1)
                    u1  = u(i,j,k-1)
                    v1  = v(i,j,k-1)
                 endif
              endif
                    
              !---- upper ----
              if( p_pt(i,j,kk) > p_ref(k) ) then
                 p2  = p_pt(i,j,kk)
                 pt2 = pt_ref(kk)
                 u2  = u_pt(i,j,kk)
                 v2  = v_pt(i,j,kk)
                 iflag = 1
              else
                 p2  = p_ref(k)
                 pt2 = pt(i,j,k)
                 u2  = u(i,j,k)
                 v2  = v(i,j,k)
              endif
                 
              ! trapeziodal integration
              dp  = max( p1-p2, 0.0)
              pth = 0.5 * ( pt1 + pt2 )
              dpt = pt_ref(kk) - pth
              !
              nhc_pt(i,j,kk) =  dpt *dp &
                   &         + nhc_pt(i,j,kk) 

              unhf_pt(i,j,kk) = 0.5 * ( u1 + u2 ) * dpt * dp &
                   &          + unhf_pt(i,j,kk)
              
              vnhf_pt(i,j,kk) = 0.5 * ( v1 + v2 ) * dpt * dp &
                   &         + vnhf_pt(i,j,kk)
              
              !---- upper check ----
              if( iflag /= 0 ) then
                 kkcount(i,j) = kkcount(i,j) + 1
                 kk = kkcount(i,j)
                 !
                 if( kk <= nz_pt ) then 
                    nhc_pt(i,j,kk) = nhc_pt(i,j,kk-1) + &
                         &       ( pt_ref(kk) - pt_ref(kk-1) )*dp_pt(i,j,kk-1)
                 
                    unhf_pt(i,j,kk) = unhf_pt(i,j,kk-1) + &
                         &       ( pt_ref(kk) - pt_ref(kk-1) )*uf_pt(i,j,kk-1)

                    vnhf_pt(i,j,kk) = vnhf_pt(i,j,kk-1) + &
                         &       ( pt_ref(kk) - pt_ref(kk-1) )*vf_pt(i,j,kk-1)
                    
                    goto 11
                 else
                    iflag = 0
                 endif
              endif
           endif
        enddo
     enddo
  enddo


  ! --- check min/max values
  call check_range( nx, ny, nz_pt, nhc_pt, nhc_min, nhc_max, &
       &            'calc_nhc()', 'nhc_pt')
  call check_range( nx, ny, nz_pt, unhf_pt, nhf_min, nhf_max, &
       &            'calc_nhc()', 'unhf_pt')
  call check_range( nx, ny, nz_pt, vnhf_pt, nhf_min, nhf_max, &
       &            'calc_nhc()', 'vnhf_pt')

  write(45,'(A7,2E12.4)') 'nhcpt: ', minval(nhc_pt), maxval(nhc_pt)
  write(45,'(A7,2E12.4)') ' unhf: ',minval(unhf_pt),maxval(unhf_pt)
  write(45,'(A7,2E12.4)') ' vnhf: ',minval(vnhf_pt),maxval(vnhf_pt)
  
  



  return
endsubroutine calc_nhc

