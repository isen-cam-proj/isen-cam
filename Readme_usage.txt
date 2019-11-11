%==========================================================
%  Readme of isentropic cold air mass analysis tool
%==========================================================
1.Operating environment
  Linux + inter fortran (ifort10.1) is checked.
  LInux + Fujitu f90 is also checked.

2.Directories
  ${isen_cam}/
              src/  source code
              Doc/  Document(Japanese)
              Work/ grads ctl file is here
              Makefile
              Readme_namelist_eng.txt   explanation of namelist
              sample.sh    shell script for sample data

3.Input data
  Input data should be grads format.
  ・u-wind
  ・v-wind
  ・geopotential height
  ・air temperature
  ・surface pressure
  ・surface u-wind 
  ・surface v-wind
  ・surface temperature
  ・surface altitude
Sea level pressure can be sabstitutable for surface pressure

4.usage
  4.1 compile
    Check a compiler in ${isen_cam}/Makefile
    $cd ${isen_cam}/
    $make
    if compile is successfully finished, isen_cold_air is made

  4.2 making namelist
    making namelist with reference to sample.sh
    Detail information of namelist is in Readme_namelist_eng.txt

  4.3 analysis
    run program with namelist
    $./isen_cold_air < namelist

    grads ctl file is work/isen.ctl
    

  5 caution
 
  -Inversion layer near the ground surface is removed using the lowest 
   potential temperature level.
  -This program is for middle and lower troposphere.
  -The inversion layer near the tropopause is not considered.

  -If you want to calculate specific date in an imput file
   (For example, input file has 1 year data but calculate only January),
   you can set the calculation period using INITIAL_TIME and END_TIME in the namelist.

  -Genesis/loss rate is calculated only from divergence of 
   the cold air mass flux when there is no information about a previons time step.

  -INPUT_TDEF_DAYNUM is used to calculate time derivatives of cold air mass.
