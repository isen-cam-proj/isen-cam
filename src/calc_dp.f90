!
! Function
!   caclulate cold air mass amount ( ps - p_pt )
! 
! Arguments(in)
!   ps    : surface pressure [hPa]
!   p_pt  : pressure of isentropic coordinate [hPa]
! 
! Arguments(out)
!   dp_pt : cold air mass amount [hPa]
! 
! ====================================================================
subroutine calc_dp(ps,p_pt, dp_pt)
  use parameter, only : undef, eps, dp_min, dp_max
  use com_var, only : nx, ny, nz_pt
  implicit none
  real(4),intent(in) :: ps(nx,ny), p_pt(nx,ny,nz_pt)
  !
  real(4),intent(out) :: dp_pt(nx,ny,nz_pt)
  !
  integer :: i, j, kk
  real(4) :: dp

  do kk = 1, nz_pt
     do j = 1, ny
        do i = 1, nx
           dp = ps(i,j) - p_pt(i,j,kk)
           if( dp >= 0.0 ) then
              dp_pt(i,j,kk) = dp
           else
              dp_pt(i,j,kk) = -eps
           endif
        enddo
     enddo
  enddo

  !---- check value ----!
  call check_range( nx, ny, nz_pt, dp_pt, dp_min, dp_max, 'calc_dp()','dp')

  write(45,'(A7,2E12.4)') 'dp_pt: ',minval(dp_pt),maxval(dp_pt)

  return
end subroutine calc_dp
 
