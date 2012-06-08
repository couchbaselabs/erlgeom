setlocal
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
set temp=c:\tmp
set ERL_PATH=c:\erlang
path=%path%;c:\mozilla-build\7zip;c:\opt\cmake\bin;%ERL_PATH%\bin;c:\openssl\bin;
set GEOAPP=erlgeom-0.1.0

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: clean up existing erlgeom release bundle
if exist %GEOAPP% rd /s/q %GEOAPP%
del /f/s/q priv\* ebin\*.beam c_src\*.o* rel\*
mkdir %GEOAPP%\priv %GEOAPP%\ebin %GEOAPP%\src

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
call rebar.cmd -vv compile

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: build erlgeom release bundle
xcopy priv\*.dll rel\%GEOAPP%\priv\ /y /f
xcopy deps\geos\bin\geos_c.dll  rel\%GEOAPP%\priv\ /y /f
xcopy ebin\*     rel\%GEOAPP%\ebin\ /y /f
xcopy src\*      rel\%GEOAPP%\src\ /y /f

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: update erlgeom in current erlang release
robocopy rel\%GEOAPP% %ERL_PATH%\lib\%GEOAPP% -mir
path=%path%;%ERL_PATH%\lib\%GEOAPP%\priv;
start /max werl
:::::::::::::::::::::::::::
endlocal
:: l(erlgeom). Geom1 = erlgeom:to_geom({'Point',[5,5]}), Geom2 = erlgeom:to_geom({'LineString', [[1,1],[14,14]]}), erlgeom:disjoint(Geom1, Geom2), Geom3 = erlgeom:to_geom({'Point', [2.5,65.7]}), erlgeom:disjoint(Geom1, Geom3).
echo l(erlgeom).
echo Geom1 = erlgeom:to_geom({'Point',[5,5]}), Geom2 = erlgeom:to_geom({'LineString', [[1,1],[14,14]]}),
echo erlgeom:disjoint(Geom1, Geom2), Geom3 = erlgeom:to_geom({'Point', [2.5,65.7]}),
echo erlgeom:disjoint(Geom1, Geom3).
