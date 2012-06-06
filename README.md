# Building on Unix/Linux:

    make

Run tests with:

    make check

Here's an example session in the erlang shell. See the src/erlgeom.erl file for
more examples.

    $ erl -pa ebin
    Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [kernel-poll:false]

    Eshell V5.8.4  (abort with ^G)
    1> Geom1 = erlgeom:to_geom({'Point',[5,5]}).
    geom: POINT (5.0000000000000000 5.0000000000000000)
    <<>>
    2> Geom2 = erlgeom:to_geom({'LineString', [[1,1],[14,14]]}).
    geom: LINESTRING (1.0000000000000000 1.0000000000000000, 14.0000000000000000 14.0000000000000000)
    <<>>
    3> erlgeom:disjoint(Geom1, Geom2).
    false
    4> Geom3 = erlgeom:to_geom({'Point', [2.5,65.7]}).
    geom: POINT (2.5000000000000000 65.7000000000000028)
    <<>>
    5> erlgeom:disjoint(Geom1, Geom3).
    true
    6>

# Building on Windows

First you will need to build Geos-3.3.4 from source, including the missing
integer type headers.

## Set up your environment as follows;

- install MSVC10 SDK 7.1
- install Erlang/OTP R15B01
- install 7zip
- install cmake
- run `setenv.cmd /x86 /release`

    wget http://download.osgeo.org/geos/geos-3.3.4.tar.bz2
    7z x -y geos-*.bz2
    7z x -y -ogeos geos-*.tar

- finally, build geos:

    cd geos
    setenv.cmd /x86 /release
    path=%path%;c:\mozilla-build\7zip;c:\opt\cmake\bin;c:\erlang\bin;c:\openssl\bin;
    mkdir dist build & cd build
    cmake -G "NMake Makefiles" ..
    cmake -DCMAKE_INSTALL_PREFIX=..\dist -DBUILD_SHARED_LIBS=OFF ..
    cmake --build ..\build --target install --config Release

- copy this dist folder as erlgeom/deps/geos
- alter build.cmd as required to point to your erlang installation
- run `build.cmd` and complete tests
