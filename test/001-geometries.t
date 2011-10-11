#!/usr/bin/env escript
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

main(_) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(9),
    test_point(),
    test_linestring(),
    test_polygon(),
    test_multipoint(),
    test_multilinestring(),
    test_multipolygon(),
    test_geometrycollection(),
    test_invalid_geoms(),

    etap:end_tests().

test_point() ->
    Pt = {'Point',[0.0, 1.1]},
    Pt1 = erlgeom:to_geom(Pt),
    etap:is(erlgeom:from_geom(Pt1), Pt, "Point conversion works").

test_linestring() ->
    Ls = {'LineString', [[1.0,1.0],[5.0,5.0],[23.42,592.13],[98.2,40.2]]},
    Ls1 = erlgeom:to_geom(Ls),
    etap:is(erlgeom:from_geom(Ls1), Ls, "LineString conversion works").

test_polygon() ->
    Py = {'Polygon', [[[5.2,6.3],[70.5,58.7],[0.1,20.55],[5.2,6.3]], [[10.0,20.1],[10.1,20.4],[9.8,20.2],[10.0,20.1]]]},
    Py1 = erlgeom:to_geom(Py),
    etap:is(erlgeom:from_geom(Py1), Py, "Polygon conversion works").

test_multipoint() ->
    Mp = {'MultiPoint', [[1.0,1.0],[5.0,5.0]]},
    Mp1 = erlgeom:to_geom(Mp),
    etap:is(erlgeom:from_geom(Mp1), Mp, "MultiPoint conversion works (a)"),

    Mp2 = {'MultiPoint', [[1.0,1.0],[5.0,5.0],[23.42,592.13],[98.2,40.2]]},
    Mp3 = erlgeom:to_geom(Mp2),
    etap:is(erlgeom:from_geom(Mp3), Mp2, "MultiPoint conversion works (b)").

test_multilinestring() ->
    Ml = {'MultiLineString', [[[5.2,6.3],[70.5,58.7],[0.1,20.55],[5.2,6.3]], [[10.0,20.1],[10.1,20.4],[9.8,20.2],[10.0,20.1]]]},
    Ml1 = erlgeom:to_geom(Ml),
    etap:is(erlgeom:from_geom(Ml1), Ml, "MultiLineString conversion works").

test_multipolygon() ->
    % From GeoJSON spec
    My = {'MultiPolygon',[[[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]],[[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0], [100.0, 0.0]],[[100.2, 0.2], [100.8, 0.2], [100.8, 0.8], [100.2, 0.8], [100.2, 0.2]]]]},
    My1 = erlgeom:to_geom(My),
    etap:is(erlgeom:from_geom(My1), My, "MultiPolygon conversion works").

test_geometrycollection() ->
    % From GeoJSON spec
    Gc = {'GeometryCollection',[{'Point',[100.0, 0.0]},{'LineString',[[101.0, 0.0],[102.0, 1.0]]}]},
    Gc1 = erlgeom:to_geom(Gc),
    etap:is(erlgeom:from_geom(Gc1), Gc, "GeometryCollection conversion works").

test_invalid_geoms() ->
    Pt = {'MultiPoint',[0.0, 10]},
    etap:throws_ok(
        fun() -> erlgeom:to_geom_validate(Pt) end,
        "Not every position of the MultiPoint is a valid Point",
        "Invalid geometry").
