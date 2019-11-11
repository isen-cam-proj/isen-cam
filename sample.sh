#!/bin/sh

##########################################################################
#  This is a sample shell script for 
#                        NCEP-NCAR reanalysis data (Kalnay et al.,1996)
##########################################################################

echo "

&INPUT
  INPUT_U_FILENAME    = 'input/u201201.dr',
  INPUT_V_FILENAME    = 'input/v201201.dr',
  INPUT_T_FILENAME    = 'input/t201201.dr',
  INPUT_Z_FILENAME    = 'input/z201201.dr',
  INPUT_US_FILENAME   = 'input/us201201.dr',
  INPUT_VS_FILENAME   = 'input/vs201201.dr',
  INPUT_TS_FILENAME   = 'input/ts201201.dr',
  INPUT_PS_FILENAME   = 'input/ps201201.dr',
  INPUT_TOPO_FILENAME = 'input/hgs.dr',
/

&INPUT_UNIT
  INPUT_UNIT_Z    = 'm',
  INPUT_UNIT_PS   = 'Pa',
  INPUT_UNIT_MSL  = 'Pa',
  INPUT_UNIT_TOPO = 'm',
/

&INPUT_UNDEF
  INPUT_UNDEF_DEFAULT = -9.99e+33,
  INPUT_UNDEF_U   = '',
  INPUT_UNDEF_V   = '',
  INPUT_UNDEF_T   = '',
  INPUT_UNDEF_Z   = '',
  INPUT_UNDEF_US  = '',
  INPUT_UNDEF_VS  = '',
  INPUT_UNDEF_TS  = '',
  INPUT_UNDEF_PS  = '',
  INPUT_UNDEF_MSL = '',
/

&INPUT_ENDIAN
  INPUT_ENDIAN_DEFAULT = 'little',
  INPUT_ENDIAN_U   = '',
  INPUT_ENDIAN_V   = '',
  INPUT_ENDIAN_T   = '',
  INPUT_ENDIAN_Z   = '',
  INPUT_ENDIAN_US  = '',
  INPUT_ENDIAN_VS  = '',
  INPUT_ENDIAN_TS  = '',
  INPUT_ENDIAN_PS  = '',
  INPUT_ENDIAN_MSL = '',
  INPUT_ENDIAN_TOPO = '',
/

&INPUT_XDEF
  INPUT_XDEF_NUM = 144,
/

&INPUT_YDEF
  INPUT_YDEF_TYPE = 'linear',
  INPUT_YDEF_NUM  = 73,
  INPUT_YDEF_SOUTH = -90,
  INPUT_YDEF_NORTH = 90,
  INPUT_YDEF_YREV_DEFAULT = 1,
  INPUT_YDEF_YREV_TOPO = 1,
/

&INPUT_ZDEF
  INPUT_ZDEF_NUM  = 17,
  INPUT_ZDEF_LEVEL =
       10.00,   20.00,  30.00,  50.00, 70.00,
      100.00,  150.00, 200.00, 250.00, 300.00,
      400.00,  500.00, 600.00, 700.00, 850.00,
      925.00, 1000.00,
  INPUT_ZDEF_ZREV = 0,
/

&INPUT_TDEF
  INPUT_TDEF_TYPE = 'monthly',
  INPUT_TDEF_DAYNUM = 4,
  INPUT_TDEF_LEAP_YEAR = 0,
  INITIAL_TIME = 2012, 1,  1, 0, 0,
  END_TIME     = 2012, 1,  3,18, 0,
/

###### Output file setting #####
&OUTPUT
  OUTPUT_FILENAME = 'work/isen201201.dr',
  OUTPUT_LOG_FILENAME = 'work/log.txt', 
/

&OUTPUT_VAR
  OUTPUT_P      = 1
  OUTPUT_G      = 1
  OUTPUT_NHC    = 1
  OUTPUT_UNHF   = 1
  OUTPUT_VNHF   = 1
  OUTPUT_NHCGEN = 1
/

&OUTPUT_PTDEF
  OUTPUT_ISEN_LEV_NUM = 3
  OUTPUT_ISEN_LEVEL =
         270.0, 280.0,290.0,
/
" > namelist_sample

./isen_cold_air < namelist_sample

