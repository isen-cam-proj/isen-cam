DSET    ^isen%y4%m2.dr
UNDEF   -9.99E+33
TITLE   NCEP/NCAR
OPTIONS little_endian template
XDEF    144  linear   0.0 2.5
YDEF     73  linear -90.0 2.5
ZDEF      3  linear  270.0 10.0
TDEF     12  linear 00z01jan2012 6hr
*TDEF  120000 linear 00Z01jan1979 6hr
VARS      10
dp 3 99 ** polar cold air mass amount [hPa]
uf 3 99 ** polar cold air mass flux in zonal direction [hPa*m/sec]
vf 3 99 ** polar cold air mass flux in meridional direction [hPa*m/sec]
cgen 3 99 ** polar cold air mass generation rate [hPa/sec]
*
p 3 99 ** pressure [hPa]
g 3 99 ** geopotential height [m]
nhc 3 99 ** negative heat content [K*hPa]
unhf 3 99 ** negative heat flux in zontal direction [K*hPa*m/sec]
vnhf 3 99 ** negative heat flux in meridonal direction [K*hPa*m/sec]
nhcgen 3 99 ** negative heat content generation rate [K*hPa/sec]
ENDVARS

