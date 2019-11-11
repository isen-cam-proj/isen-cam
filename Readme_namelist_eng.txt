isentropic polar cold air mass analysis (namelist by Y. Kanno)

-4 bytes unformatted binary data (GrADS format) is acceptable.

-These explanation are based on GrADS control file style

-More detail information (e.g. variable type) are in src/namelist.f90


###### Input file setting #####
&INPUT : input filename setting
  INPUT_U_FILENAME    : U filename
  INPUT_V_FILENAME    : V filename
  INPUT_T_FILENAME    : T filename
  INPUT_Z_FILENAME    : Z filename
  INPUT_US_FILENAME   : surface zonal wind filename
  INPUT_VS_FILENAME   : surface meridional wind filename
  INPUT_TS_FILENAME   : surface temperature filename
  INPUT_PS_FILENAME   : surface pressure filename (default='')
  INPUT_MSL_FILENAME  : MSL pressure filename (default='')
			(valid if INPUT_PS_FILENAME is not specified)
  INPUT_TOPO_FILENAME : topography filename
/


&INPUT_UNIT : unit setting
  INPUT_UNIT_Z    : = "m"  (height, default)
		    = "m^2/s^2"  (geopotential)
  INPUT_UNIT_PS   : = "hPa"  (default)
		  : = "Pa"
  INPUT_UNIT_MSL  : = "hPa"  (default)
		      "Pa" 
  INPUT_UNIT_TOPO : = "m"  (height, default)
		  : = "m^2/s^2"  (geopotential)
/


&INPUT_UNDEF : undef setting
  INPUT_UNDEF_DEFAULT : common UNDEF value (default=9.999e+20) 
  INPUT_UNDEF_U       : U file UNDEF value (default=INPUT_UNDEF_DEFAULT)
  INPUT_UNDEF_V       : V file UNDEF value (default=INPUT_UNDEF_DEFAULT)
  INPUT_UNDEF_T       : T file UNDEF value (default=INPUT_UNDEF_DEFAULT)
  INPUT_UNDEF_Z       : Z file UNDEF value (default=INPUT_UNDEF_DEFAULT)
  INPUT_UNDEF_US      : US file UNDEF valule (default=INPUT_UNDEF_DEFAULT)
  INPUT_UNDEF_VS      : VS file UNDEF value (default=INPUT_UNDEF_DEFAULT)
  INPUT_UNDEF_TS      : TS file UNDEF value (default=INPUT_UNDEF_DEFAULT)
  INPUT_UNDEF_PS      : PS file UNDEF value (default=INPUT_UNDEF_DEFAULT)
  INPUT_UNDEF_MSL     : MSL file UNDEF value (default=INPUT_UNDEF_DEFAULT)
/

&INPUT_ENDIAN : endian setting
  INPUT_ENDIAN_DEFAULT : common endian setting
		         = 'little' (little endian, default)
			 = 'big'    (big endian)
  INPUT_ENDIAN_U       : U file endian (default=INPUT_ENDIAN_DEFAULT)
  INPUT_ENDIAN_V       : V file endian (default=INPUT_ENDIAN_DEFAULT)
  INPUT_ENDIAN_T       : T file endian (default=INPUT_ENDIAN_DEFAULT)
  INPUT_ENDIAN_Z       : Z file endian (default=INPUT_ENDIAN_DEFAULT)
  INPUT_ENDIAN_US      : US file endian (default=INPUT_ENDIAN_DEFAULT)
  INPUT_ENDIAN_VS      : VS file endian (default=INPUT_ENDIAN_DEFAULT)
  INPUT_ENDIAN_TS      : TS file endian (default=INPUT_ENDIAN_DEFAULT)
  INPUT_ENDIAN_PS      : PS file endian (default=INPUT_ENDIAN_DEFAULT)
  INPUT_ENDIAN_MSL     : MSL file endian (default=INPUT_ENDIAN_DEFAULT)
  INPUT_ENDIAN_TOPO    : TOPO file endian (default=INPUT_ENDIAN_DEFAULT)
/

&INPUT_XDEF : x direction setting
	      only for uniform grid
	      output setting is same as input setting
  INPUT_XDEF_NUM : number of grid point
/

&INPUT_YDEF : y direction setting
	      output setting is same as input setting
  INPUT_YDEF_TYPE : level type
		    = "lat_degree" (levels [degree], default)
		    = "lat_radian" (levels [radian])
		    = "linear"     (uniform interval [degree])
  INPUT_YDEF_NUM  : number of grid point
  INPUT_YDEF_LEVEL : latitude levels ( South -> North or North -> South)
		     valid if INPUT_YDEF_TYPE = "lat_degree" or "lat_radian"
  INPUT_YDEF_SOUTH : Southern edge latitude [degree] (default=-90)
  INPUT_YDEF_NORTH : Northern edge latitude [degree] (default=90)
  INPUT_YDEF_YREV_DEFAULT : common YREV setting
			    = 0 (NOT YREV i.e. south -> north, default)
			    = 1 (YREV i.e. north -> south)
  INPUT_YDEF_YREV_TOPO : YREV setting for TOPO file
			    = 0 (NOT YREV i.e. south -> north)
			    = 1 (YREV i.e. north -> south)
			  without specified, INPUTYDEF_YDEF_YREV_TOPO equals to
			      INPUT_YDEF_YREV_DEFAULT
/

&INPUT_ZDEF : input Z direction setting
  INPUT_ZDEF_NUM  : number of grid point
  INPUT_ZDEF_LEVEL : pressure levels (Upper -> Lower or Lower -> Upper)	
  INPUT_ZDEF_ZREV : common ZREV setting
		    = 0 (NOT ZREV i.e. Lower -> Upper, default)
		    = 1 (ZREV i.e. Upper -> Lower)
/

&INPUT_TDEF : time setting
  INPUT_TDEF_TYPE : time step type
		    = "tstep"   (directly specify time step)
		    = "monthly" (input file has 1 month time step)
		    = "annual"  (input file has 1 year time step)
  INPUT_TDEF_DAYNUM : number of timestep per day
		      (if this is 0, then cgen and nhcgen do not include 
		      time derivatives of DP, NHC)
  INPUT_TDEF_LEAP_YEAR : 365 day/year switch
			 = 0 (default, leap year considered)
			 = 1 (fixed 365 day/year i.e. no leap year)
			 valid if INPUT_TDEF_TYPE = "annual" or "monthly"
  INPUT_TDEF_TSTEP : number of time step 
		     valid if INPUT_TDEF_TYPE = "tstep"
  INITIAL_TIME : diagnosis start time 
	       = year,month(1-12),day,hour(0-24),minute
		 valid if INPUT_TDEF_TYPE = "annual" or "monthly"
  END_TIME     : diagnosis end time
	       = year,month(1-12),day,hour(0-24),minute
	         valid if INPUT_TDEF_TYPE = "annual" or "monthly"
/

###### Output file setting #####
&OUTPUT : output file setting
  OUTPUT_FILENAME : output filename
  OUTPUT_LOG_FILENAME = calculation log filename(time & minimum/maximum value) 
/

&OUTPUT_VAR : optional output variable 
  OUTPUT_P      : pressure of isentropic coordinate
		= 0 (default, not output)
		= 1 (output)
  OUTPUT_G      : geopotential height of isentropic coordiante
		= 0 (default, not output)
		= 1 (output)
  OUTPUT_NHC    : negative heat content
		= 0 (default, not output)
		= 1 (output)
  OUTPUT_UNHF   : zonal negative heat flux
		= 0 (default, not output)
		= 1 (output)
  OUTPUT_VNHF   : meridional negative heat flux
		= 0 (default, not output)
		= 1 (output)
  OUTPUT_NHCGEN : negative heat content generation rate
		= 0 (default, not output)
		= 1 (output)
/ 

&OUTPUT_PTDEF : output Z direction setting 
  OUTPUT_ISEN_LEV_NUM : number of isentrope
  OUTPUT_ISEN_LEVEL : isentrope levles
/

