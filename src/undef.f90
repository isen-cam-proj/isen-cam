!
! Function
!   interpolate/extrapolate undef data (pressure level only)
!
! Arguements (in)
!   nx     : number of x-direction grid
!   ny     : number of y-direction grid
!   nz     : number of z-direction grid
!   undef  : undef value
!   p_ref  : pressure levels
!
! Arguements (inout)
!   var    : variable
!
subroutine undef_fill( nx, ny, nz, undef, p_ref, &
     &                 var )
  implicit none
  integer,intent(in)    :: nx, ny, nz
  real(4),intent(in)    :: undef
  real(4),intent(in)    :: p_ref(nz)
  real(4),intent(inout) :: var(nx, ny, nz)

  integer :: i, j, k
  real(4) :: temp

!  do k=3, nz
  do k=nz-2, 1, -1
     do j=1, ny
        do i=1, nx

           if( var(i,j,k) == undef ) then
              call undef_hokan( p_ref(k), &
                   &            p_ref(k+1), var(i,j,k+1), &
                   &            p_ref(k+2), var(i,j,k+2), &
                   &            temp )
!              call undef_hokan( p_ref(k), &
!                   &            p_ref(k-1), var(i,j,k-1), &
!                   &            p_ref(k-2), var(i,j,k-2), &
!                   &            temp )
              var(i,j,k) = temp
           end if

        end do
     end do
  end do

end subroutine undef_fill



!
! Function
!   interpolate/extrapolate undef data
!
! Arguements (in)
!   p    : interpolated/extrapolated level
!   p1   : level (1) to be used in interpolation/extrapolation
!   A1   : value at p=p1
!   p2   : level (2) to be used in interpolation/extrapolation
!   A2   : value at p=p2
!
! Arguements (inout)
!   ret  : value at p
!
! Note
!   -log(p) linear interpolation/extrapolation is used.
!
subroutine undef_hokan( p, p1, A1, p2, A2, ret )
  implicit none
  real(4),intent(in)  :: p, p1, A1, p2, A2
  real(4),intent(out) :: ret

  ret = ( (A2-A1)*log(p) + A1*log(p2) - A2*log(p1) ) / (log(p2)-log(p1))
!  ret = ( (A2-A1)*p + A1*p2 - A2*p1 ) / (p2-p1)
end subroutine undef_hokan
