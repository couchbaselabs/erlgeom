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

    etap:plan(8),
    test_get_centroid(),
    test_get_centroid_geom(),
    test_disjoint(),
    test_intersection(),
    test_intersects(),
    test_is_valid(),
    test_simplify(),

    etap:end_tests().

test_get_centroid() ->
    Pt = {'Point',[3,3]},
    Pt1 = erlgeom:to_geom(Pt),
    Centroid = erlgeom:get_centroid(Pt1),
    etap:is(Centroid, Pt,
        "Point get_centroid works").

test_get_centroid_geom() ->
    Pt = {'Point',[3,3]},
    Pt1 = erlgeom:to_geom(Pt),
    CentroidGeom = erlgeom:get_centroid_geom(Pt1),
    etap:is(erlgeom:from_geom(CentroidGeom), Pt,
        "Point get_centroid_geom works").

test_disjoint() ->
    Pt = {'Point',[3.0, 3.0]},
    Ls = {'LineString', [[1.0,1.0],[5.0,5.0]]},
    Pt1 = erlgeom:to_geom(Pt),
    Ls1 = erlgeom:to_geom(Ls),
    Disjoint = erlgeom:disjoint(Pt1, Ls1),
    etap:is(Disjoint, false,
        "Geometries are not disjoint"),

    % Some geometries are based on the GeoJSON specification
    % http://geojson.org/geojson-spec.html (2010-08-17)
    Geoms = [
        {'Point', [100.0, 0.0]},
        {'LineString', [
            [100.0, 0.0],
            [101.0, 1.0]
        ]},
        {'Polygon', [
            [[100.0, 0.0], 
             [101.0, 0.0],
             [100.0, 1.0],
             [100.0, 0.0]]
        ]},
        {'Polygon', [
            [[100.0, 0.0],
             [101.0, 0.0],
             [100.0, 1.0],
             [100.0, 0.0]],
            [[100.2, 0.2],
             [100.6, 0.2],
             [100.2, 0.6],
             [100.2, 0.2]]
        ]},
        {'MultiPoint', [
            [100.0, 0.0],
            [101.0, 1.0]
        ]},
        {'MultiLineString', [
            [[100.0, 0.0],
             [101.0, 1.0]],
            [[102.0, 2.0],
             [103.0, 3.0]]
        ]},
        {'MultiPolygon', [
            [
                [[102.0, 2.0],
                 [103.0, 2.0],
                 [103.0, 3.0],
                 [102.0, 3.0],
                 [102.0, 2.0]]
            ],[
                [[100.0, 0.0],
                 [101.0, 0.0],
                 [101.0, 1.0],
                 [100.0, 1.0],
                 [100.0, 0.0]],
                [[100.2, 0.2],
                 [100.8, 0.2],
                 [100.8, 0.8],
                 [100.2, 0.8],
                 [100.2, 0.2]]
            ]
        ]},
        {'GeometryCollection', [
            {'Point', [100.0, 0.0]},
            {'LineString', [
                [101.0, 0.0],
                [102.0, 1.0]
            ]}
        ]}
    ],

    QueryGeom = erlgeom:to_geom(
        {'MultiPolygon',[[[[102.21960449216,1.66524628779],
                   [101.10998535158,2.0385856805057],
                   [100.30798339848,3.0483190208145],
                   [101.29675292969,3.3225525920246],
                   [102.83483886713,3.5418849006547],
                   [104.1641845702,2.5764772510785],
                   [103.60388183585,2.4337915164603],
                   [102.88977050775,3.2128679544585],
                   [101.58239746093,3.1251117377839],
                   [101.27478027344,2.3898851337089],
                   [101.42858886719,2.0605442798878],
                   [101.42858886719,2.0715234662377],
                   [102.21960449216,1.66524628779]]],
                 [[[100.13220214849,2.3679314141203],
                   [100.20910644536,1.3797028906988],
                   [100.58264160159,0.68768106684542],
                   [101.04406738283,1.5334617387448],
                   [101.51647949219,1.1490463293633],
                   [102.52722167964,-0.015427421675798],
                   [102.68103027339,1.2588853394239],
                   [100.13220214849,2.3679314141203]]]]}),
    Results = [erlgeom:disjoint(QueryGeom, erlgeom:to_geom(Geom))
        || Geom <- Geoms],
    etap:is(Results, [true,true,true,true,true,true, false, false],
        "Two geometries are not disjoint").

test_intersection() ->
    Geom1 = erlgeom:to_geom({'LineString', [[1,1],[10,10]]}),
    Geom2 = erlgeom:to_geom({'LineString', [[2,2],[9,9]]}),
    Intersection = {'LineString', [[2,2],[9,9]]},
    Intersection1 = erlgeom:intersection(Geom1, Geom2),
    etap:is(erlgeom:from_geom(Intersection1), Intersection,
        "Linestrings intersection works").

test_intersects() ->
    Geom1 = erlgeom:to_geom({'LineString', [[1,1],[10,10]]}),
    Geom2 = erlgeom:to_geom({'LineString', [[2,2],[9,9]]}),
    etap:is(erlgeom:intersects(Geom1, Geom2), true,
        "Linestrings intersects works").

test_is_valid() ->
    Geom1 = erlgeom:to_geom({'LineString', [[1,1],[10,10]]}),
    etap:is(erlgeom:is_valid(Geom1), true,
        "Linestrings is_valid works").

test_simplify() ->
    Polygon = {'Polygon', [[
        [-43.59375, -0.3515625],
        [-31.640625, 15.8203125],
        [-33.046875, 25.6640625],
        [-37.265625, 39.7265625],
        [-34.453125, 67.8515625],
        [6.328125, 58.7109375],
        [21.09375, 65.0390625],
        [35.15625, 63.6328125],
        [78.046875, 63.6328125],
        [75.234375, 48.1640625],
        [65.390625, 33.3984375],
        [43.59375, 36.2109375],
        [-6.328125, 36.2109375],
        [-0.703125, 31.9921875],
        [2.109375, 5.9765625],
        [3.515625, -16.5234375],
        [-17.578125, -19.3359375],
        [-24.609375, -5.9765625],
        [-40.078125, -11.6015625],
        [-40.078125, -11.6015625],
        [-43.59375, -0.3515625]]]},
    {'Polygon', 
        [NewCoords]} = erlgeom:topology_preserve_simplify(
            erlgeom:to_geom(Polygon),
            30.0),
    etap:is(length(NewCoords), 6, "Geometry was simplified").


