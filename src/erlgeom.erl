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

-export([hello/0, disjoint/2, from_geom/1, to_geom/1,
    topology_preserve_simplify/2]).

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
