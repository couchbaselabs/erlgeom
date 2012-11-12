Install
-------

Build it with:

    make

Run tests with:

    make check

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


Examples
--------

Here's an example session in the erlang shell. See the src/erlgeom.erl file for
more examples.

    $ erl -pa ebin
    Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [kernel-poll:false]

    Eshell V5.8.4  (abort with ^G)

    1> Geom1 = erlgeom:to_geom({'LineString', [[3,3],[10,10]]}),
    1> Geom2 = erlgeom:to_geom({'LineString', [[1,1],[7,7]]}),
    1> erlgeom:intersects(Geom1, Geom2).
    true

    2> Geom1 = erlgeom:to_geom({'LineString', [[3,3],[10,10]]}),
    2> Geom2 = erlgeom:to_geom({'LineString', [[1,1],[7,7]]}),
    2> Geom3 = erlgeom:intersection(Geom1, Geom2),
    2> erlgeom:from_geom(Geom3).
    {'LineString', [[3,3],[7,7]]}

    3> WktReader = erlgeom:wktreader_create(),
    3> Geom2 = erlgeom:wktreader_read(WktReader, 
    3>    "POLYGON((0 0, 1 1, 1 2, 1 1, 0 0))"),
    3> erlgeom:is_valid(Geom2).
    false

    4> Geom1 = erlgeom:to_geom({'LineString', [[4,4],[10,10]]}),
    4> Geom2 = erlgeom:get_centroid(Geom1),
    4> erlgeom:from_geom(Geom2).
    {'Point',[7.0,7.0]}

    5> Geom1 = erlgeom:to_geom({'LineString', [[4,4], [4.5, 4.5], [10,10]]}),
    5> erlgeom:topology_preserve_simplify(Geom1, 1).
    {'LineString',[[4.0,4.0],[10.0,10.0]]}

    6> Geom1 = erlgeom:to_geom({'LineString', [[4,4], [4.5, 4.5], [10,10]]}),
    6> erlgeom:is_valid(Geom1).
    true

    7> WktReader = erlgeom:wktreader_create(),
    7> Geom = erlgeom:wktreader_read(WktReader, "POINT(10 10)"),
    7> erlgeom:from_geom(Geom).
    {'Point',[10.0,10.0]}

    8> WktReader = erlgeom:wktreader_create(),
    8> Geom = erlgeom:wktreader_read(WktReader, "POINT(10.0 10.0)"),
    8> WkbWriter = erlgeom:wkbwriter_create(),
    8> Bin = erlgeom:wkbwriter_write(WkbWriter, Geom),
    8> WkbReader = erlgeom:wkbreader_create(),
    8> Geom2 = erlgeom:wkbreader_read(WkbReader, Bin),
    8> erlgeom:from_geom(Geom2).
    {'Point',[10.0,10.0]}

    9> WkbReader = erlgeom:wkbreader_create(),
    9> Geom = erlgeom:wkbreader_readhex(WkbReader,
    9>     "010100000000000000000024400000000000002440"),
    9> erlgeom:from_geom(Geom).
    {'Point',[10.0,10.0]}

    10> WktReader = erlgeom:wktreader_create(),
    10> Geom = erlgeom:wktreader_read(WktReader, "POINT(10 10)"),
    10> WktWriter = erlgeom:wktwriter_create(),
    10> erlgeom:wktwriter_write(WktWriter, Geom).
    "Point(10.0000000000000000 10.0000000000000000)"

    11> WktReader = erlgeom:wktreader_create(),
    11> Geom = erlgeom:wktreader_read(WktReader, "POINT(10.0 10.0)"),
    11> WkbWriter = erlgeom:wkbwriter_create(),
    11> erlgeom:wkbwriter_write(WkbWriter, Geom).
    <<1,1,0,0,0,0,0,0,0,0,0,36,64,0,0,0,0,0,0,36,64>>

    12> WktReader = erlgeom:wktreader_create(),
    12> Geom = erlgeom:wktreader_read(WktReader, "POINT(10.0 10.0)"),
    12> WkbWriter = erlgeom:wkbwriter_create(),
    12> erlgeom:wkbwriter_writehex(WkbWriter, Geom).
    "010100000000000000000024400000000000002440"




