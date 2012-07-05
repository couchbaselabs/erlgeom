Build it with:

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


On Windows
----------

You need to have GEOS installed, let's say it was installed to `C:\cygwin\opt\couchbase`.

Open a shell which has all compilers and the MSVC environment set up (e.g. the Windows SDK 7.1 Command Prompt).

Now set it up so that GEOS and Erlang can be found:

    SET INCLUDE=%INCLUDE%;C:\cygwin\opt\couchbase\include
    SET LIB=%LIB%;C:\cygwin\opt\couchbase\lib
    SET PATH=%PATH%;C:\cygwin\opt\couchbase\bin;C:\erl5.9.1\bin

And finally compile the whole thing:

    rebar compile
