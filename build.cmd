setlocal
set temp=c:\tmp
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
path=%path%;c:\mozilla-build\7zip;c:\opt\cmake\bin;c:\erlang\bin;c:\openssl\bin;z:\source\rebar;
::set include=%include%;c:\erlang\usr\include

set GEOAPP=erlgeom-0.1.0
if exist %GEOAPP% rd /s/q %GEOAPP%
del /f/q priv\* ebin\*.beam c_src\*.o
mkdir %GEOAPP%\priv %GEOAPP%\ebin %GEOAPP%\src

::set geos=z:\source\erlgeom\deps\geos
::set include=%include%;%geos%\include;
::set libpath=%libpath%;%geos%\lib;
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rebar.cmd -vv compile
xcopy priv\*.dll %GEOAPP%\priv\ /y
xcopy ebin\*     %GEOAPP%\ebin\ /y
xcopy src\*      %GEOAPP%\src\ /y
start /max werl -pa ebin -s erlgeom
echo l(erlgeom).

:::::::::::::::::::::::::::
endlocal
