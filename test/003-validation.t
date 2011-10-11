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

    etap:plan(40),
    test_is_point(),
    test_is_valid_geometry(),
    test_is_valid_geometry_multi(),

    etap:end_tests().

test_is_point() ->
    etap:is(erlgeom:is_point([20, 30]), true, "Valid point with integers"),
    etap:is(erlgeom:is_point([5.4, 10.92]), true, "Valid point with floats"),
    etap:is(erlgeom:is_point([9, 39.2]), true,
        "Valid point with integer and float"),
    etap:is(erlgeom:is_point([]), true, "Valid point with no coords"),

    etap:is(erlgeom:is_point([20, 30, 40]), false, "Too many coords"),
    etap:is(erlgeom:is_point([38.32]), false, "Not enough coords").

test_is_valid_geometry() ->
    % Tests for Point geometries
    Point1 = {'Point', [20, 30]},
    etap:is(erlgeom:is_valid_geometry(Point1), true,
        "Valid Point with integers"),
    Point2 = {'Point', [5.4, 10.92]},
    etap:is(erlgeom:is_valid_geometry(Point2), true,
        "Valid Point with floats"),
    Point3 = {'Point', [9, 39.2]},
    etap:is(erlgeom:is_valid_geometry(Point3), true,
        "Valid Point with integer and float"),
    Point4 = {'Point', []},
    etap:is(erlgeom:is_valid_geometry(Point4),true,
        "Valid Point with no coords"),

    Point5 = {'Point', [20, 30, 40]},
    {PFalse5, _} = erlgeom:is_valid_geometry(Point5),
    etap:is(PFalse5, false, "Invalid Point: too many coords"),
    Point6 = {'Point', [38.32]},
    {PFalse6, _} = erlgeom:is_valid_geometry(Point6),
    etap:is(PFalse6, false, "Invalid Point: not enough coords"),

    % Tests for LineString geometries
    LineString1 = {'LineString', [[20, 30], [50, 60]]},
    etap:is(erlgeom:is_valid_geometry(LineString1), true,
        "Valid LineString (a)"),
    LineString2 = {'LineString', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    etap:is(erlgeom:is_valid_geometry(LineString2), true,
        "Valid LineString (b)"),
    LineString3 = {'LineString', [[20, 30]]},
    {LSFalse3, _} = erlgeom:is_valid_geometry(LineString3),
    etap:is(LSFalse3, false, "Invalid LineString: not enough positions"),
    LineString4 = {'LineString', [[20, 40, 10], [30, 20, 5]]},
    {LSFalse4, _} = erlgeom:is_valid_geometry(LineString4),
    etap:is(LSFalse4, false, "Invalid LineString: too many coords"),
    LineString5 = {'LineString', [[20], [30]]},
    {LSFalse5, _} = erlgeom:is_valid_geometry(LineString5),
    etap:is(LSFalse5, false, "Invalid LineString: not enough coords"),

    % Tests for Polygon geometries
    Polygon1 = {'Polygon', [[[20, 30], [50, 60], [30, 10], [20, 30]]]},
    etap:is(erlgeom:is_valid_geometry(Polygon1), true,
        "Valid Polygon"),
    %POLYGON((20 30, 50 60, 10 70, 20 30))
    %POLYGON((20 55, 28 60, 33 53, 25 48, 20 55))
    Polygon2 = {'Polygon', [[[20, 30], [50, 60], [10, 70], [20, 30]],
        [[20, 55], [28, 60], [33, 53], [25, 48], [20, 55]]]},
    etap:is(erlgeom:is_valid_geometry(Polygon2), true,
        "Valid Polygon with hole"),
    Polygon3 = {'Polygon', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    {PYFalse3, _} = erlgeom:is_valid_geometry(Polygon3),
    etap:is(PYFalse3, false,
        "Invalid Polygon: not nested enough"),
    Polygon4 = {'Polygon', [[[20, 30], [50, 60], [30], [20, 30]]]},
    {PYFalse4, _} = erlgeom:is_valid_geometry(Polygon4),
    etap:is(PYFalse4, false,
        "Invalid Polygon: invalid point somewhere"),

    % Invalid GeometryType
    Something = {'Something', [[[20, 30], [50, 60], [30], [20, 30]]]},
    {SomethingFalse, _} = erlgeom:is_valid_geometry(Something),
    etap:is(SomethingFalse, false,
        "Invalid geometry type").

% Tests for multi-geometries
test_is_valid_geometry_multi() ->
    % Tests for MultiPoint geometries
    MultiPoint1 = {'MultiPoint', [[20, 30], [50, 60]]},
    etap:is(erlgeom:is_valid_geometry(MultiPoint1), true,
        "Valid MultiPoint (a)"),
    MultiPoint2 = {'MultiPoint', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    etap:is(erlgeom:is_valid_geometry(MultiPoint2), true,
        "Valid MultiPoint (b)"),
    MultiPoint3 = {'MultiPoint', [[20, 30]]},
    etap:is(erlgeom:is_valid_geometry(MultiPoint3), true,
        "Valid MultiPoint (c)"),
    MultiPoint4 = {'MultiPoint', [[20, 40, 10], [30, 20, 5]]},
    {MPFalse4, _} = erlgeom:is_valid_geometry(MultiPoint4),
    etap:is(MPFalse4, false, "Invalid MultiPoint: too many coords"),
    MultiPoint5 = {'MultiPoint', [[20], [30]]},
    {MPFalse5, _} = erlgeom:is_valid_geometry(MultiPoint5),
    etap:is(MPFalse5, false, "Invalid MultiPoint: not enough coords"),

    % Tests for MultiLineString geometries (basically the same as for polygons
    MLS1 = {'MultiLineString', [[[20, 30], [50, 60], [30, 10], [20, 30]]]},
    etap:is(erlgeom:is_valid_geometry(MLS1), true,
        "Valid MultiLineString (a)"),
    MLS2 = {'MultiLineString', [[[20, 30], [50, 60], [10, 70], [20, 30]],
        [[20, 55], [28, 60], [33, 53], [25, 48], [20, 55]]]},
    etap:is(erlgeom:is_valid_geometry(MLS2), true,
        "Valid MultiLineString (b)"),
    MLS3 = {'MultiLineString', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    {MLSFalse3, _} = erlgeom:is_valid_geometry(MLS3),
    etap:is(MLSFalse3, false,
        "Invalid MultiLineString: not nested enough"),
    MLS4 = {'MultiLineString', [[[20, 30], [50, 60], [30], [20, 30]]]},
    {MLSFalse4, _} = erlgeom:is_valid_geometry(MLS4),
    etap:is(MLSFalse4, false,
        "Invalid MultiLineString: invalid point somewhere"),

    % Tests for MultiPolygons
    MPY1 = {'MultiPolygon', [[[[20, 30], [50, 60], [30, 10], [20, 30]]],
        [[[120, 130], [150, 160], [110, 170], [120, 130]]]]},
    etap:is(erlgeom:is_valid_geometry(MPY1), true,
        "Valid MultiPolygon"),
    MPY2 = {'MultiPolygon', [[[[20, 30], [50, 60], [10, 70], [20, 30]],
        [[20, 55], [28, 60], [33, 53], [25, 48], [20, 55]]],
        [[[120, 130], [150, 160], [130, 110], [120, 130]]]]},
    etap:is(erlgeom:is_valid_geometry(MPY2), true,
        "Valid MultiPolygon with hole"),
    MPY3 = {'MultiPolygon', [[20, 30], [50, 60], [30, 10], [20, 30]]},
    {MPYFalse3, _} = erlgeom:is_valid_geometry(MPY3),
    etap:is(MPYFalse3, false,
        "Invalid MultiPolygon: not nested enough (a)"),
    MPY4 = {'MultiPolygon', [[[20, 30], [50, 60], [30, 10], [20, 30]]]},
    {MPYFalse4, _} = erlgeom:is_valid_geometry(MPY4),
    etap:is(MPYFalse4, false,
        "Invalid MultiPolygon: not nested enough (b)"),
    MPY5 = {'MultiPolygon', [[[[20, 30], [50, 60], [30, 10], [20, 30]]],
        [[[120, 130], [150, 160], [110], [120, 130]]]]},
    {MPYFalse5, _} = erlgeom:is_valid_geometry(MPY5),
    etap:is(MPYFalse5, false,
        "Invalid MultiLineString: invalid point somewhere (a)"),
    MPY6 = {'MultiPolygon', [[[[20, 30], [50, 60], [30, 10, 93], [20, 30]]],
        [[[120, 130], [150, 160], [110,170], [120, 130]]]]},
    {MPYFalse6, _} = erlgeom:is_valid_geometry(MPY6),
    etap:is(MPYFalse6, false,
        "Invalid MultiLineString: invalid point somewhere (b)"),

    % Tests for GeometryCollection
    GC1 = {'GeometryCollection', [
        {'Point', [20, 30]},
        {'LineString', [[20, 30], [50, 60]]},
        {'Polygon', [[[20, 30], [50, 60], [10, 70], [20, 30]],
                [[20, 55], [28, 60], [33, 53], [25, 48], [20, 55]]]},
        {'MultiPoint', [[20, 30], [50, 60]]}
    ]},
    etap:is(erlgeom:is_valid_geometry(GC1), true,
        "Valid GeometryCollection"),
    GC2 = {'GeometryCollection', [
        {'Point', [20, 30]},
        {'LineString', [[20, 30], [50, 60]]},
        {'Polygon', [[[20, 30], [50, 60], [10, 70], [20, 30]],
                [[20, 55], [28, 60], [33], [25, 48], [20, 55]]]},
        {'MultiPoint', [[20, 30], [50, 60]]}
    ]},
    {MPYFalse2, _} = erlgeom:is_valid_geometry(GC2),
    etap:is(MPYFalse2, false,
        "Invalid GeometryCollection: invalid point somewhere"),
    GC3 = {'GeometryCollection', [
        {'Something', [[20, 30], [50, 60]]}
    ]},
    {GCFalse3, _} = erlgeom:is_valid_geometry(GC3),
    etap:is(GCFalse3, false,
        "Invalid GeometryCollection: invalid geometry type").
