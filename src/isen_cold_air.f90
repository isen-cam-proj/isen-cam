program isen_cold_air
!========================================================================
!  Isentropic polar cold air mass analysis 
!                                     2014.05.01 Y.Kanno
!========================================================================
  use namelist
  use parameter
  use com_var, only : nx, ny, nz, nz_pt, p_ref, pt_ref, &
       &              alat, stg_sintbl, stg_costbl, &
       &              com_var_ini, com_var_end, &
       &              com_var_pref, com_var_ptref, &
       &              com_var_alat, com_var_stg_tbl
  use var
  use grads, only : grads_info, &
       &            grads_open, grads_close, &
       &            grads_read, grads_write
  implicit none

  ! temporal variables
  integer :: idate(5)  ! 1:year 2:month 3:day 4:hour 5:min
  integer :: wdate(5)
  integer :: edate(5)
  integer :: ktdel
  integer :: tstep
  integer :: tcount=1
!  integer :: iflag = 0
  integer :: nrec_ini, nrec_end
  
  ! input information
  type(grads_info) :: ginfo_topo
  type(grads_info) :: ginfo_u
  type(grads_info) :: ginfo_v
  type(grads_info) :: ginfo_t
  type(grads_info) :: ginfo_z
  type(grads_info) :: ginfo_us
  type(grads_info) :: ginfo_vs
  type(grads_info) :: ginfo_ts
  type(grads_info) :: ginfo_ps
  type(grads_info) :: ginfo_msl
  
  ! output information
  type(grads_info) :: ginfo_out

  !================ initiate & read namelist ======================
  
  !---- initialization namelist ----
  call namelist_init()

  !---- set commom variables ----
  call com_var_ini( INPUT_XDEF_NUM, INPUT_YDEF_NUM, INPUT_ZDEF_NUM, &
       &            OUTPUT_ISEN_LEV_NUM )
  
  !---- allocate arrays ----
  call var_ini( INPUT_XDEF_NUM, INPUT_YDEF_NUM, INPUT_ZDEF_NUM, &
       &            OUTPUT_ISEN_LEV_NUM )

  !---- standard pressure & potential tempearure ----
  call com_var_pref()
  call com_var_ptref()
  
  !---- latitude & cosine table ----
  call com_var_alat()
  call com_var_stg_tbl()



  !================ time loop setting =============================
  if( INPUT_TDEF_TYPE == 'monthly' .or. INPUT_TDEF_TYPE == 'annual' ) then
     idate(1:5) =  INITIAL_TIME(1:5)
     edate(1:5) =  END_TIME(1:5)
     ! check
     call check_time(idate,edate)

     wdate(1:5) = idate(1:5)
     
     ktdel = INPUT_TDEF_DT/60/60
     

     !---- get record ----
     call get_record(INPUT_TDEF_TYPE, INPUT_TDEF_LEAP_YEAR, &
          &          idate,ktdel, nrec_ini)

     call get_record(INPUT_TDEF_TYPE, INPUT_TDEF_LEAP_YEAR, &
          &          edate,ktdel, nrec_end)

     tstep = nrec_end - nrec_ini + 1
     if( tstep < 0 ) then
        write(*,*) 'error: END_TIME is before INITIAL_TIME'
        write(*,*) 'INITIAL_TIME ',idate(1:5)
        write(*,*) '    END_TIME ',edate(1:5)
        stop
     endif
  else
     tstep = INPUT_TDEF_TSTEP
     nrec_ini = 1
  endif

  !================ file open =======================================
  !
  !  all variable should be not YREV (south->north)
  !
  !==================================================================
  

  !---- U ----
  call grads_open(11, INPUT_U_FILENAME, nx, ny, nz, &
       &          0, INPUT_YDEF_YREV_DEFAULT, INPUT_ZDEF_ZREV, &
       &          ENDIAN_U, nrec_ini, &
       &          ginfo_u )
  
  !---- V ----
  call grads_open(12, INPUT_V_FILENAME, nx, ny, nz, &
       &          0, INPUT_YDEF_YREV_DEFAULT, INPUT_ZDEF_ZREV, &
       &          ENDIAN_V, nrec_ini, &
       &          ginfo_v )

  !---- T ----
  call grads_open(13, INPUT_T_FILENAME, nx, ny, nz, &
       &          0, INPUT_YDEF_YREV_DEFAULT, INPUT_ZDEF_ZREV, &
       &          ENDIAN_T, nrec_ini, &
       &          ginfo_t )


  !---- Z ----
  call grads_open(20, INPUT_Z_FILENAME, nx, ny, nz, &
       &          0, INPUT_YDEF_YREV_DEFAULT, INPUT_ZDEF_ZREV, &
       &          ENDIAN_Z, nrec_ini, &
       &          ginfo_z)

  !---- surface ----
  call grads_open(30, INPUT_US_FILENAME, nx, ny, 1, &
       &          0, INPUT_YDEF_YREV_DEFAULT, 0, &
       &          ENDIAN_US, nrec_ini, &
       &          ginfo_us)  ! us
  call grads_open(31, INPUT_VS_FILENAME, nx, ny, 1, &
       &          0, INPUT_YDEF_YREV_DEFAULT, 0, &
       &          ENDIAN_VS, nrec_ini, &
       &          ginfo_vs)  ! vs
  call grads_open(32, INPUT_TS_FILENAME, nx, ny, 1, &
       &          0, INPUT_YDEF_YREV_DEFAULT, 0, &
       &          ENDIAN_TS, nrec_ini, &
       &          ginfo_ts)  ! ts

  
  !---- surface pressure ----
  if( INPUT_PS_FILENAME /= '' ) then
  call grads_open(40, INPUT_PS_FILENAME, nx, ny, 1, &
       &          0, INPUT_YDEF_YREV_DEFAULT, 0, &
       &          ENDIAN_PS, nrec_ini, &
       &          ginfo_ps)  ! ps
  else
  call grads_open(41, INPUT_MSL_FILENAME, nx, ny, 1, &
       &          0, INPUT_YDEF_YREV_DEFAULT, 0, &
       &          ENDIAN_MSL, nrec_ini, &
       &          ginfo_msl)  ! msl
  endif

  !---- topo ----
  call grads_open(50, INPUT_TOPO_FILENAME, nx, ny, 1, &
       &          0, INPUT_YDEF_YREV_TOPO, 0, &
       &          ENDIAN_TOPO, 1, &
       &          ginfo_topo)  ! topo


  !**** open output file ****
  call grads_open(100, OUTPUT_FILENAME, nx, ny, nz_pt, &
       &          0, 0, 0, 1, 1,&
       &          ginfo_out)  ! output

  !**** open log file ****
  open(45, file=OUTPUT_LOG_FILENAME)
  write(45,*) 'OUTPUT FILE:', trim(OUTPUT_FILENAME)


  !***** load topo *****
  call grads_read(ginfo_topo, topo)
  if( INPUT_UNIT_TOPO == 'm^2/s^2' ) then
     topo(:,:) = topo(:,:) / grav   ! [m^2/s^2] -> [m]
     
  else if( INPUT_UNIT_TOPO == 'm' ) then
     topo(:,:) = topo(:,:)          ! [m]

  else
     write(0,*) 'error in read topo : TOPO_UNIT = ' &
          &     // INPUT_UNIT_TOPO // 'is invalid'
     stop
  endif


  if( sum( topo(:,ny/4) ) > sum( topo(:,ny-ny/4) ) ) then
     write(0,*) 'error in load topography'
     write(0,*) ' Probably, INPUT_YDEF_YREV_TOPO in namelist should be changed'
     stop
  endif
  
  !================================================================
  !
  !   time loop start
  !
  !================================================================
  date_loop: do
     if( INPUT_TDEF_TYPE == 'monthly' .or. &
          & INPUT_TDEF_TYPE == 'annual' ) then
        ! ---- date ----
        print'(A,I5,3I3,A)','<<<<<< DATE: ',wdate(1:4),' >>>>>>'
        write(45,'(A,I5,3I3,A)') '<<<<<< DATE: ',wdate(1:4),' >>>>>>'
     endif

        write(*,*)'tcount=',tcount,'/',tstep
        write(45,*)'tcount=',tcount,'/',tstep

     ! ---- read input data ----
     
     !===== U =====
     call grads_read(ginfo_u, u)
     
     !===== V =====
     call grads_read(ginfo_v, v)

     !===== T =====
     call grads_read(ginfo_t, t)


     !===== geopotential height [m] =====
     call grads_read(ginfo_z, g)

     if( INPUT_UNIT_Z == 'm^2/s^2' ) then
        g(:,:,:) = g(:,:,:) / grav     ! [m^2/s^2] -> [m]
     
     else if( INPUT_UNIT_Z == 'm' ) then
        g(:,:,:) = g(:,:,:)            ! [m]
     
     else 
        write(0,*) 'error in read geopotential height : INPUT_UNIT_Z = ' &
             &     // INPUT_UNIT_Z // 'is invalid'
        stop
     endif

     !===== surface wind [m/s] and temperature=====
     call grads_read(ginfo_us, us)
     call grads_read(ginfo_vs, vs)
     call grads_read(ginfo_ts, ts)
     
     !===== surface pressure [hPa] =====
     if( INPUT_PS_FILENAME/= '' ) then
        call grads_read(ginfo_ps, ps)
        
        if( INPUT_UNIT_PS == 'Pa' ) then
           ps(:,:) = ps(:,:) * 0.01  ! [Pa] -> [hPa]
        endif
     else
        call grads_read(ginfo_msl, msl)
        if( INPUT_UNIT_MSL == 'Pa' ) then
           msl(:,:) = msl(:,:) * 0.01 ! [Pa] -> [hPa]
        endif
        ! mean sea level -> surface pressure
        ps(:,:) = msl(:,:) * ( 1 + gamma * topo(:,:) / ts(:,:) ) &
             &                  ** ( -grav / ( rd * gamma ) )
     endif

     !======== interpolate/extrapolate to undef data ============== 
     call undef_fill( nx, ny, nz, UNDEF_U, p_ref, u )
     call undef_fill( nx, ny, nz, UNDEF_V, p_ref, v )
     call undef_fill( nx, ny, nz, UNDEF_T, p_ref, t )
     call undef_fill( nx, ny, nz, UNDEF_Z, p_ref, g )


     !================== check value =====================
     call check_range( nx, ny, nz, u, wind_min, wind_max, 'isen()','u')
     call check_range( nx, ny, nz, v, wind_min, wind_max, 'isen()','v')
     call check_range( nx, ny, nz, t, t_min, t_max, 'isen()','t')
     call check_range( nx, ny, nz, g, g_min, g_max, 'isen()','g')
     call check_range( nx, ny,  1,ps, p_min, p_max, 'isen()','ps')
     call check_range( nx, ny,  1,us, wind_min, wind_max, 'isen()','us')
     call check_range( nx, ny,  1,vs, wind_min, wind_max, 'isen()','vs')
     call check_range( nx, ny,  1,ts, t_min, t_max, 'isen()','ts')


     !================= start diagnosis =======================
      ! ---- calculate potential temperature  ----
      call calc_pts(ps,ts, pts)
      call calc_pt(p_ref,t, pt)
    
      ! ---- define level of minimum potential temperature ----
      call calc_lv_ptmin(p_ref,ps, pts,pt, ks)


      ! ---- convert p-level to isentropic ----
      call convert_vertical_coordinate(p_ref, pt_ref, ks, &
           &                           pts,ps,us,vs,topo, pt,g,u,v, &
           &                           p_pt,g_pt,u_pt,v_pt)

      ! ---- calculate cold air mass amount ----
      call calc_dp(ps,p_pt, dp_pt)
     
      ! ---- calculate cold air mass flux ----
      call calc_cold_air_flux(ks, p_ref, ps,us,vs, u,v, p_pt,u_pt,v_pt, &
           &                  uf_pt,vf_pt)
      
      ! ---- calculate cold air mass generation rate ----
      call calc_div(uf_pt,vf_pt, cgen_pt)
     
      ! ---- calculate negative heat content ----
      call calc_nhc(ks, p_ref,pt_ref, ps,pts,us,vs, &
           &        pt,u,v, p_pt,u_pt,v_pt, dp_pt,uf_pt,vf_pt, &
           &        nhc_pt,unhf_pt,vnhf_pt)
     
      ! ---- calculate negative heat content generation rate ----
      call calc_div(unhf_pt,vnhf_pt, nhcgen_pt)

      ! ---- dp & nhc past @ t=1 ----
      if( tcount == 1 ) then
         dp_past(:,:,:) = dp_pt(:,:,:)
         nhc_past(:,:,:) = nhc_pt(:,:,:)
      endif

      ! ---- calculate time derivative ----
      call d_dt( dp_pt,dp_past, dp_dot)
      call d_dt( nhc_pt,nhc_past, nhc_dot)
      !
      cgen_pt(:,:,:) = cgen_pt(:,:,:) + dp_dot(:,:,:)
      nhcgen_pt(:,:,:) = nhcgen_pt(:,:,:) + nhc_dot(:,:,:)

     
     if( OUTPUT_FILENAME /= '' ) then
        call grads_write( ginfo_out,  dp_pt)
        call grads_write( ginfo_out,  uf_pt)
        call grads_write( ginfo_out,  vf_pt)
        call grads_write( ginfo_out,  cgen_pt)
        
        if( OUTPUT_P == 1 ) then
           call grads_write( ginfo_out,  p_pt)
        endif
        
        if( OUTPUT_G == 1 ) then
           call grads_write( ginfo_out,  g_pt)
        endif

        if( OUTPUT_NHC == 1 ) then
           call grads_write( ginfo_out,  nhc_pt)
        endif

        if( OUTPUT_UNHF == 1 ) then
           call grads_write( ginfo_out,  unhf_pt)
        endif
        
        if( OUTPUT_VNHF == 1 ) then
           call grads_write( ginfo_out,  vnhf_pt)
        endif

        if( OUTPUT_NHCGEN == 1 ) then
           call grads_write( ginfo_out,  nhcgen_pt)
        endif
     endif

     ! ---- check loop limit  ----
     if( tcount == tstep ) then
        print'(A)',' ==== TIME loop finished ==== '
        call var_end()
        exit
     else
        tcount = tcount + 1

        !---- save dp & nhc ----
        dp_past(:,:,:) = dp_pt(:,:,:)
        nhc_past(:,:,:) = nhc_pt(:,:,:)

        if( INPUT_TDEF_TYPE == 'annual' .or. &
             & INPUT_TDEF_TYPE == 'monthly') then
           ! ---- shift date
           call addtime(wdate,ktdel,INPUT_TDEF_LEAP_YEAR)
        endif

     endif
  
 end do date_loop
  ! ===== main loop : end ================================================

end program isen_cold_air
