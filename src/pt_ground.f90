!
! Function 
!    check lower boundary
!                 1. Level of surface pressure (ks)
!                 2. modify reverse layer 
!
! Arguments(in)
!   p_ref  : reference pressure level [hPa]
!   ps     : surface pressure [hPa]
!  
! Arguments(inout)
!   pts    : surface potential temperature [K]
!   pt     : potential temperature [K]
!  
! Arguments(out)
!   ks     : number of surface layer
!
! Note
!  - If the surface pressure is higher any values of p_ref, ks becomes 0 
!  - If atmosphere is unstable, potential temperature profile is adjusted
!   
subroutine calc_lv_ptmin(p_ref,ps, pts,pt, ks)
  use com_var, only : nx, ny, nz
  implicit none
  real(4),intent(in) :: p_ref(nz)
  real(4),intent(in) :: ps(nx,ny)
  real(4),intent(inout) :: pts(nx,ny), pt(nx,ny,nz)
  !
  integer,intent(out) :: ks(nx,ny)
  !
  real(4) :: ptmin
  integer :: i, j, k, kk
  integer :: rev(nx,ny)
  !
  do j = 1,ny
    do i = 1,nx
      kloop: do k = 1,nz
        !--- check lower boundary ---!
        if( p_ref(k) < ps(i,j) )then
          ptmin = pts(i,j)
          ks(i,j) = k - 1
          rev(i,j) = ks(i,j)
          !--- find reverse layer ---!
          do kk = k,nz
            if( pt(i,j,kk) <= ptmin )then
              ptmin = pt(i,j,kk)
              rev(i,j) = kk
            endif
          enddo
          exit kloop
        endif
      enddo kloop
    enddo
  enddo

  !---- modify potential temperature ---!
  do j = 1,ny
     do i = 1,nx
        if(rev(i,j) /= ks(i,j))then
           pts(i,j) = pt(i,j,rev(i,j))-0.01
           if(rev(i,j) >= 2 )then
              do k = rev(i,j)-1, 1, -1
                 pt(i,j,k) = pt(i,j,k+1)-0.01
              enddo
              pts(i,j) = pt(i,j,1) - 0.01
           endif
        endif
     enddo
  enddo

  
  write(45,'(A7,2I3)') '   ks: ',minval(ks),maxval(ks)

  return
end subroutine calc_lv_ptmin
