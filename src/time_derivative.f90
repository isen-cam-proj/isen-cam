!
! Function 
!    calculate time derivative 
!
! Arguments(in)
!   INPUT_TDEF_DT : time difference [s]
!   var           : variable 
!   var_past      : past variable
!
! Arguments(out)
!   var_dot  : time derivative of variable
!
!
! Note
!  - ( var - var_past ) / INPUT_TDEF_DT
!
subroutine d_dt(var,var_past, var_dot)
  use namelist, only : INPUT_TDEF_DT
  use com_var, only : nx, ny, nz_pt
  real(4),intent(in) :: var(nx,ny,nz_pt)
  real(4),intent(in) :: var_past(nx,ny,nz_pt)
  !
  real(4),intent(out) :: var_dot(nx,ny,nz_pt)
  !
  integer :: i,j,kk
  !
  do kk = 1, nz_pt
     do j = 1, ny
        do i = 1, nx
           var_dot(i,j,kk) = ( var(i,j,kk) - var_past(i,j,kk) ) / INPUT_TDEF_DT
        enddo
     enddo
  enddo
  
  return
end subroutine d_dt
