subroutine addtime(idate,ktdel, INPUT_TDEF_LEAP_YEAR)
  integer,intent(inout) :: idate(5)
  integer,intent(in) :: ktdel
  integer,intent(in) :: INPUT_TDEF_LEAP_YEAR
  ! idate(1-5) ** 1:year, 2:month, 3:day, 4:hour, 5:min
  integer :: eday
  !
  idate(4)=idate(4)+ktdel
  if(idate(4) >= 24) then
     idate(3) = idate(3)+1
     idate(4) = 0
  endif

  if( idate(2) ==  1 .or. idate(2) == 3 .or. idate(2) ==  5 .or. &
    & idate(2) ==  7 .or. idate(2) == 8 .or. idate(2) == 10 .or. &
    & idate(2) == 12 ) then
    eday = 31
  elseif( idate(2) == 4 .or. idate(2) ==  6 .or. &
       &  idate(2) == 9 .or. idate(2) == 11 ) then
     eday = 30
  elseif( idate(2) == 2 ) then
     if( (( mod(idate(1),4) == 0 .and. mod(idate(1),100) /= 0 ) &
    & .or. mod(idate(1),400) == 0 ).and. &
    & INPUT_TDEF_LEAP_YEAR == 0 )then
        eday = 29
     else
        eday = 28
     endif
  endif

  if(idate(3) > eday) then
     idate(2)=idate(2)+1
     idate(3)=1
  endif
  ! ---- check year ----
  if( idate(2) == 13 )then
     idate(2) = 1
     idate(1) = idate(1) + 1
  endif
  !
  return
end subroutine addtime

!=========================================================================
subroutine get_record(INPUT_TDEF_TYPE,INPUT_TDEF_LEAP_YEAR, &
     &                idate,ktdel,  nrec)
  implicit none
  character(*),intent(in) :: INPUT_TDEF_TYPE
  integer,intent(in) :: INPUT_TDEF_LEAP_YEAR
  integer,intent(in) :: idate(5)
  integer,intent(in) :: ktdel
  integer,intent(out) :: nrec
  !
  integer :: year, tdate(5)
  !

     tdate(1:5) = idate(1:5)
     year = idate(1)
     nrec = tdate(2)-1
  if( INPUT_TDEF_TYPE == 'annual' ) then
9002 continue
     
     if( nrec == 0 ) go to 9003
     if( nrec >= 1 ) then
        if(  (tdate(2) == 1).or.(tdate(2) == 2).or.(tdate(2) == 4).or. &
             &  (tdate(2) == 6).or.(tdate(2) == 8).or.(tdate(2) == 9).or. &
             &  (tdate(2) == 11)) then
           tdate(3) = tdate(3)+31
        endif
        if(  (tdate(2) ==  5).or.(tdate(2) ==  7).or. &
             &  (tdate(2) == 10).or.(tdate(2) == 12) ) then
           tdate(3) = tdate(3)+30
        endif
        if(tdate(2) == 3) then
           if( ((mod(year,4) == 0 .and. mod(year,100) /= 0) &
                & .or. mod(year,400) == 0) .and. &
                & INPUT_TDEF_LEAP_YEAR == 0 )then
              tdate(3) = tdate(3)+29
           else
              tdate(3) = tdate(3)+28
           endif
        endif
        
        nrec = nrec-1
        
        if( nrec /= 0 )then
           tdate(2) = tdate(2)-1
           if(tdate(2) == 0 )then
              tdate(2) = 12
           endif
           go to 9002
        endif
     endif
  endif

9003 continue  
  if( INPUT_TDEF_TYPE /= 'tstep' )then 
     nrec = (tdate(4) + (tdate(3)-1)*24  )/real(ktdel) + 1
  else
     nrec = 1
  endif
  
  return
end subroutine get_record

!
! Function
!    diagnosis date check (exsist or not, idate<edate, year/month check)
!
! Arguments(in)
!   idate : diagnosis initial date
!   edate : diagnosis end date
!
! Note
!  date(5) -> 1.year 2.month 3.date 4.hour 5.minute
!
subroutine check_time(idate, edate)
  use namelist, only : INPUT_TDEF_TYPE, INPUT_TDEF_LEAP_YEAR
  implicit none
  integer,intent(in) :: idate(5), edate(5)
  !
  integer :: t
  !
  !                          1   2   3   4   5   6   7   8   9  10  11  12
  integer :: edays(12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  !
  if( idate(1) /= edate(1) ) then
     print*,'INPUT_TDEF_TYPE is ',trim(INPUT_TDEF_TYPE)
     print*,', but diagnosis initial and end year is different'
     stop
  endif

  if( INPUT_TDEF_TYPE == 'monthly' .and. idate(2) /= edate(2) ) then
     print*,'INPUT_TDEF_TYPE is ',trim(INPUT_TDEF_TYPE)
     print*,'but diagnosis initial and end month is different'
     stop
  endif

  !---- set edays ----
  if( (( mod(idate(1),4) == 0 .and. mod(idate(1),100) /= 0 ) &
       & .or. mod(idate(1),400) == 0 ).and. INPUT_TDEF_LEAP_YEAR == 0 ) then
     edays(2) = 29
  endif
  !---- check end days ----!
  do t = 1, 12
     if( idate(2) == t .and. idate(3) > edays(t) ) then
        print*,'INITIAL_TIME does not exsist ', idate(1:5)
        stop
     endif
     !
     if( edate(2) == t .and. edate(3) > edays(t) ) then
        print*,'END_TIME does not exsist ', edate(1:5)
        stop
     endif
  enddo
  
  return
end subroutine check_time
