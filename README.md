Change the path in erlgeos.erl to your erlgeos.so file. The path is directly
to the file without the .so extension (it must be an absolute path).

Then compile the C and Erlang file:

gcc -fPIC -g -Wall -shared -o erlgeos.so erlgeos.c -lgeos_c
erlc erlgeos.erl

Here's an example session in the erlang shell. See the erlgeos.erl file for
more examples.

$ erl
Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [kernel-poll:false]

Eshell V5.8.4  (abort with ^G)
1> Geom1 = erlgeos:to_geom({'Point',[5,5]}).
geom: POINT (5.0000000000000000 5.0000000000000000)
<<>>
2> Geom2 = erlgeos:to_geom({'LineString', [[1,1],[14,14]]}).
geom: LINESTRING (1.0000000000000000 1.0000000000000000, 14.0000000000000000 14.0000000000000000)
<<>>
3> erlgeos:disjoint(Geom1, Geom2).
false
4> Geom3 = erlgeos:to_geom({'Point', [2.5,65.7]}).
geom: POINT (2.5000000000000000 65.7000000000000028)
<<>>
5> erlgeos:disjoint(Geom1, Geom3).
true
6>
