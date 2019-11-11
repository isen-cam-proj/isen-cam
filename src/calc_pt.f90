!
! Function
!     calculate surface potential temperature  
!
! Arguments (in) 
!   ps   : surface pressure [hPa]
!   ts   : surface temperature [K]
!
! Arguments (out)
!   pts  : surface potential temperature  [K]
!  
! 
subroutine calc_pts(ps,ts, pts)
  use parameter, only : rdvcp, p0, pt_min, pt_max
  use com_var, only : nx, ny 
  implicit none
  real(4),intent(in) :: ps(nx,ny)
  real(4),intent(in) :: ts(nx,ny)
  !
  real(4),intent(out) :: pts(nx,ny)
  !
  real(4) :: fac
  integer :: i, j
  !
  do j = 1,ny
    do i = 1,nx
      fac = (p0/ps(i,j))**rdvcp
      pts(i,j) = ts(i,j) * fac
    enddo
  enddo

  !---- check value ----!
  call check_range( nx, ny, 1, pts, pt_min, pt_max, 'calc_pts()','pts')

  write(45,'(A7,2F8.2)') '  pts: ',minval(pts),maxval(pts)

  return
end subroutine calc_pts




!
! Function
!   calculate potential temperature on each grid
!
! Arguments(in)
!   p_ref : reference pressure level [hPa]
!   t     : temperature [K]
!
! Arguments(out)
!   pt    : potential temperature [K]
!
subroutine calc_pt(p_ref, t,  pt)
  use parameter, only : rdvcp, p0, pt_min, pt_max
  use com_var, only : nx, ny, nz
  implicit none
  real(4),intent(in) :: p_ref(nz)
  real(4),intent(in) :: t(nx,ny,nz)
  !
  real(4),intent(out) :: pt(nx,ny,nz)
  !
  real(4) :: fac
  integer :: i, j, k
  !
  do k = 1,nz
    fac = (p0/p_ref(k))**rdvcp
    do j = 1,ny
      do i = 1,nx
        pt(i,j,k) = t(i,j,k) * fac
      enddo
    enddo
  enddo

  !---- check value ----!
  call check_range( nx, ny, nz, pt, pt_min, pt_max, 'calc_pt()','pt')

  write(45,'(A7,2F8.2)') '   pt: ',minval(pt),maxval(pt)

  return
end subroutine calc_pt

