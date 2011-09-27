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

    etap:plan(2),
    test_disjoint(),
    test_simplify(),

    etap:end_tests().


test_disjoint() ->
    Pt = {'Point',[3.0, 3.0]},
    Ls = {'LineString', [[1.0,1.0],[5.0,5.0]]},
    Pt1 = erlgeom:to_geom(Pt),
    Ls1 = erlgeom:to_geom(Ls),
    Disjoint = erlgeom:disjoint(Pt1, Ls1),
    etap:is(Disjoint, false, "Geometries are not disjoint").

test_simplify() ->
    Polygon = {'Polygon', [[[-43.59375, -0.3515625], [-31.640625, 15.8203125], [-33.046875, 25.6640625], [-37.265625, 39.7265625], [-34.453125, 67.8515625], [6.328125, 58.7109375], [21.09375, 65.0390625], [35.15625, 63.6328125], [78.046875, 63.6328125], [75.234375, 48.1640625], [65.390625, 33.3984375], [43.59375, 36.2109375], [-6.328125, 36.2109375], [-0.703125, 31.9921875], [2.109375, 5.9765625], [3.515625, -16.5234375], [-17.578125, -19.3359375], [-24.609375, -5.9765625], [-40.078125, -11.6015625], [-40.078125, -11.6015625], [-43.59375, -0.3515625]]]},
    Simplified = erlgeom:topology_preserve_simplify(erlgeom:to_geom(Polygon), 30.0),
    etap:is(Simplified, {'Polygon',[[[-43.59375,-0.3515625],[-34.453125,67.8515625],[78.046875,63.6328125],[-6.328125,36.2109375],[3.515625,-16.5234375],[-43.59375,-0.3515625]]]}, "Geometry was simplified").
