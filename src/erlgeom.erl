% Copyright 2011 Couchbase, Inc.
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(erlgeom).

-export([init/0, hello/0, disjoint/2, from_geom/1, to_geom/1,
    topology_preserve_simplify/2]).

-export([test/0, disjoint_test/0, simplify_test/0]).

-on_load(init/0).


init() ->
    SoName = case code:priv_dir(?MODULE) of
    {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
            filename:join(["..", "priv", "erlgeom"]);
        false ->
            filename:join(["priv", "erlgeom"])
        end;
    Dir ->
        filename:join(Dir, "erlgeom")
    end,
    (catch erlang:load_nif(SoName, 0)).


hello() ->
    "NIF library not loaded".

disjoint(_Geom1, _Geom2) ->
    "NIF library not loaded".

topology_preserve_simplify(_Geom1, _Tolerance) ->
    "NIF library not loaded".

% @doc Convert a GeoCouch geometry to a GEOS geometry
to_geom(_Geom) ->
    "NIF library not loaded".

% @doc Convert a GEOS geometry to a GeoCouch geometry
from_geom(_Geom) ->
    "NIF library not loaded".



%% tests

test() ->
    point(),
    multipoint(),
    linestring(),
    multilinestring(),
    polygon(),
    multipolygon(),
    geometrycollection().

point() ->
    Pt = {'Point',[0.0, 1.1]},
    Pt1 = erlgeom:to_geom(Pt),
    Pt = erlgeom:from_geom(Pt1).

multipoint() ->
    Mp = {'MultiPoint', [[1.0,1.0],[5.0,5.0]]},
    Mp1 = erlgeom:to_geom(Mp),
    Mp = erlgeom:from_geom(Mp1),

    Mp2 = {'MultiPoint', [[1.0,1.0],[5.0,5.0],[23.42,592.13],[98.2,40.2]]},
    Mp3 = erlgeom:to_geom(Mp2),
    Mp2 = erlgeom:from_geom(Mp3).

linestring() ->
    Ls = {'LineString', [[1.0,1.0],[5.0,5.0],[23.42,592.13],[98.2,40.2]]},
    Ls1 = erlgeom:to_geom(Ls),
    Ls = erlgeom:from_geom(Ls1).

multilinestring() ->
    Ml = {'MultiLineString', [[[5.2,6.3],[70.5,58.7],[0.1,20.55],[5.2,6.3]], [[10.0,20.1],[10.1,20.4],[9.8,20.2],[10.0,20.1]]]},
    Ml1 = erlgeom:to_geom(Ml),
    Ml = erlgeom:from_geom(Ml1).

polygon() ->
    Py = {'Polygon', [[[5.2,6.3],[70.5,58.7],[0.1,20.55],[5.2,6.3]], [[10.0,20.1],[10.1,20.4],[9.8,20.2],[10.0,20.1]]]},
    Py1 = erlgeom:to_geom(Py),
    Py = erlgeom:from_geom(Py1).

multipolygon() ->
    % From GeoJSON spec
    My = {'MultiPolygon',[[[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]],[[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]],[[100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2]]]]},
    My1 = erlgeom:to_geom(My),
    My = erlgeom:from_geom(My1).

geometrycollection() ->
    % From GeoJSON spec
    Gc = {'GeometryCollection',[{'Point',[100.0, 0.0]},{'LineString',[[101.0, 0.0],[102.0, 1.0]]}]},
    Gc1 = erlgeom:to_geom(Gc),
    Gc = erlgeom:from_geom(Gc1).

disjoint_test() ->
    Pt = {'Point',[3.0, 3.0]},
    Ls = {'LineString', [[1.0,1.0],[5.0,5.0]]},
    Pt1 = erlgeom:to_geom(Pt),
    Ls1 = erlgeom:to_geom(Ls),
    erlgeom:disjoint(Pt1, Ls1).

simplify_test() ->
    Polygon = {'Polygon', [[[-43.59375, -0.3515625], [-31.640625, 15.8203125], [-33.046875, 25.6640625], [-37.265625, 39.7265625], [-34.453125, 67.8515625], [6.328125, 58.7109375], [21.09375, 65.0390625], [35.15625, 63.6328125], [78.046875, 63.6328125], [75.234375, 48.1640625], [65.390625, 33.3984375], [43.59375, 36.2109375], [-6.328125, 36.2109375], [-0.703125, 31.9921875], [2.109375, 5.9765625], [3.515625, -16.5234375], [-17.578125, -19.3359375], [-24.609375, -5.9765625], [-40.078125, -11.6015625], [-40.078125, -11.6015625], [-43.59375, -0.3515625]]]},
    erlgeom:topology_preserve_simplify(erlgeom:to_geom(Polygon), 30.0).
