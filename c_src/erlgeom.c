/*
 *     Copyright 2011 Couchbase, Inc.
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include <geos_c.h>

#include "erl_nif.h"

/* From comp.lang.c FAQ Question 17.3 */
#define Streq(s1, s2) (strcmp((s1), (s2)) == 0)

#define DIMENSION 2

static ErlNifResourceType* GEOSGEOM_RESOURCE;

/* Currently support for 2 dimensions only */
int
set_GEOSCoordSeq_from_eterm_list(GEOSCoordSequence *seq, int pos,
        ErlNifEnv *env, const ERL_NIF_TERM *coords) {
    double dbl_coord;
    int int_coord;
    ERL_NIF_TERM head, tail;

    if (enif_get_list_cell(env, *coords, &head, &tail)) {
        if (enif_get_int(env, head, &int_coord)) {
            dbl_coord = int_coord;
        }
        else if (!enif_get_double(env, head, &dbl_coord)) {
            return 0;
        }
        GEOSCoordSeq_setX(seq, pos, dbl_coord);

        enif_get_list_cell(env, tail, &head, &tail);
        if (enif_get_int(env, head, &int_coord)) {
            dbl_coord = int_coord;
        }
        else if (!enif_get_double(env, head, &dbl_coord)) {
            return 0;
        }
        GEOSCoordSeq_setY(seq, pos, dbl_coord);
        return 1;
    }
    return 0;
}

GEOSGeometry*
eterm_to_geom_point(ErlNifEnv *env, const ERL_NIF_TERM *coords_list)
{
    GEOSCoordSequence *coords_seq;

    coords_seq = GEOSCoordSeq_create(1, DIMENSION);
    set_GEOSCoordSeq_from_eterm_list(coords_seq, 0, env, coords_list);
    return GEOSGeom_createPoint(coords_seq);
}

GEOSCoordSeq
eterm_to_geom_linestring_coords(ErlNifEnv *env,
        const ERL_NIF_TERM *coords_list)
{
    unsigned int i=0, len;
    GEOSCoordSequence *coords_seq;
    ERL_NIF_TERM head, tail;

    enif_get_list_length(env, *coords_list, &len);
    coords_seq = GEOSCoordSeq_create(len, DIMENSION);
    while (enif_get_list_cell(env, *coords_list, &head, &tail)) {
        if (!set_GEOSCoordSeq_from_eterm_list(coords_seq, i, env, &head)) {
            return NULL;
        }
        i++;
        coords_list = &tail;
    }
    return coords_seq;
}

GEOSGeometry*
eterm_to_geom_linestring(ErlNifEnv *env, const ERL_NIF_TERM *eterm)
{
    GEOSCoordSequence *coords_seq = eterm_to_geom_linestring_coords(env,
        eterm);
    return GEOSGeom_createLineString(coords_seq);
}

GEOSGeometry*
eterm_to_geom_polygon(ErlNifEnv *env, const ERL_NIF_TERM *eterm)
{
    ERL_NIF_TERM outer_eterm, inner_eterm, tail;
    unsigned int rings_num, i;
    GEOSCoordSequence *outer_seq, *inner_seq;
    GEOSGeometry *outer_geom, *geom;
    GEOSGeometry **geoms;

    enif_get_list_length(env, *eterm, &rings_num);
    enif_get_list_cell(env, *eterm, &outer_eterm, &inner_eterm);
    outer_seq = eterm_to_geom_linestring_coords(env, &outer_eterm);
    outer_geom = GEOSGeom_createLinearRing(outer_seq);

    // if there are holes
    geoms = malloc(sizeof(GEOSGeometry*)*rings_num-1);
    for (i=0; enif_get_list_cell(env, inner_eterm, &inner_eterm, &tail); i++) {
        inner_seq = eterm_to_geom_linestring_coords(env, &inner_eterm);
        geoms[i] = GEOSGeom_createLinearRing(inner_seq);
        inner_eterm = tail;
    }
    geom = GEOSGeom_createPolygon(outer_geom, geoms, rings_num-1);
    free(geoms);
    return geom;
}

GEOSGeometry*
eterm_to_geom_multi(ErlNifEnv *env, ERL_NIF_TERM eterm, int type,
        GEOSGeometry*(*eterm_to_geom)(ErlNifEnv *env, const ERL_NIF_TERM *eterm))
{
    unsigned int i, geoms_num;
    GEOSGeometry *geom;
    GEOSGeometry **geoms;
    ERL_NIF_TERM tail;

    enif_get_list_length(env, eterm, &geoms_num);
    geoms = malloc(sizeof(GEOSGeometry*)*geoms_num);
    for (i=0; enif_get_list_cell(env, eterm , &eterm, &tail); i++) {
        geoms[i] = (*eterm_to_geom)(env, &eterm);
        eterm = tail;
    }
    geom = GEOSGeom_createCollection(type, geoms, geoms_num);
    free(geoms);
    return geom;
}

GEOSGeometry*
eterm_to_geom(ErlNifEnv *env, const ERL_NIF_TERM *eterm)
{
    // coords_num is the number coordinates for Points and LineStrings,
    // in a case of Polygons it's the number of rings (inner and outer),
    // in case of all Multi* types it's the number of geometries
    unsigned int coords_num;
    int tmp_size;
    GEOSGeometry *geom;
    // longest geometry type is "GeometryCollection"
    char type[19];
    const ERL_NIF_TERM *geom_tuple;

    // Split into geometry type and coordinates
    if (!enif_get_tuple(env, *eterm, &tmp_size, &geom_tuple)) {
        return NULL;
    }
    // Get geometry
    if (!enif_get_atom(env, geom_tuple[0], type, 19, ERL_NIF_LATIN1)) {
        return NULL;
    }

    if (!enif_get_list_length(env, geom_tuple[1], &coords_num)) {
        return NULL;
    }

    if(Streq(type, "Point")) {
        // Needs GEOS 3.3.0
        //if (coords_num==0) {
        //    geom = GEOSGeom_createEmptyPoint();
        //}
        //else {
            geom = eterm_to_geom_point(env, &geom_tuple[1]);
        //}
    }
    else if(Streq(type, "LineString")) {
        // Needs GEOS 3.3.0
        //if (coords_num==0) {
        //    geom = GEOSGeom_createEmptyLineString();
        //}
        //else {
            geom = eterm_to_geom_linestring(env, &geom_tuple[1]);
       //}
    }
    // The polygon follows the GeoJSON specification. First element
    // in the list is the polygon, subsequent ones are holes.
    else if(Streq(type, "Polygon")) {
        // Needs GEOS 3.3.0
        //if (coords_num==0) {
        //    geom = GEOSGeom_createEmptyPolygon();
        //}
        //else {
            geom = eterm_to_geom_polygon(env, &geom_tuple[1]);
        //}
    }
    else if(Streq(type, "MultiPoint")) {
        // Needs GEOS 3.3.0
        //if (coords_num==0) {
        //    geom = GEOSGeom_createEmptyCollection(GEOS_MULTIPOINT);
        //}
        //else {
            geom = eterm_to_geom_multi(env, geom_tuple[1], GEOS_MULTIPOINT,
                eterm_to_geom_point);
        //}
    }
    else if(Streq(type, "MultiLineString")) {
        // Needs GEOS 3.3.0
        //if (coords_num==0) {
        //    geom = GEOSGeom_createEmptyCollection(GEOS_MULTILINESTRING);
        //}
        //else {
            geom = eterm_to_geom_multi(env, geom_tuple[1],
                GEOS_MULTILINESTRING, eterm_to_geom_linestring);
        //}
    }
    else if(Streq(type, "MultiPolygon")) {
        // Needs GEOS 3.3.0
        //if (coords_num==0) {
        //    geom = GEOSGeom_createEmptyCollection(GEOS_MULTIPOLYGON);
        //}
        //else {
            geom = eterm_to_geom_multi(env, geom_tuple[1], GEOS_MULTIPOLYGON,
                eterm_to_geom_polygon);
        //}
    }
    else if(Streq(type, "GeometryCollection")) {
        // Needs GEOS 3.3.0
        //if (coords_num==0) {
        //    geom = GEOSGeom_createEmptyCollection(GEOS_GEOMETRYCOLLECTION);
        //}
        //else {
            geom = eterm_to_geom_multi(env, geom_tuple[1],
                GEOS_GEOMETRYCOLLECTION, eterm_to_geom);
        //}
    }
    else {
        return NULL;
    }
    //printf("geom: %s\r\n", GEOSGeomToWKT(geom));
    return geom;
}



/* Currently support for 2 dimensions only */
ERL_NIF_TERM
GEOSCoordSequence_to_eterm_list(ErlNifEnv *env,
        const GEOSCoordSequence *coords_seq, unsigned int len) {
    int i = 0;
    double coordx, coordy;
    ERL_NIF_TERM *coords_list;
    ERL_NIF_TERM coords;

    coords_list = malloc(sizeof(ERL_NIF_TERM)*len);
    for(i=0; i<len; i++) {
        GEOSCoordSeq_getX(coords_seq, i, &coordx);
        GEOSCoordSeq_getY(coords_seq, i, &coordy);
        coords = enif_make_list2(env, enif_make_double(env, coordx),
            enif_make_double(env, coordy));
        coords_list[i] = coords;
    }
    return enif_make_list_from_array(env, coords_list, len);
}

ERL_NIF_TERM
geom_to_eterm_point_coords(ErlNifEnv *env, const GEOSGeometry *geom)
{
    const GEOSCoordSequence *coords_seq;
    double coordx, coordy;

    coords_seq = GEOSGeom_getCoordSeq(geom);
    GEOSCoordSeq_getX(coords_seq, 0, &coordx);
    GEOSCoordSeq_getY(coords_seq, 0, &coordy);
    return enif_make_list2(env, enif_make_double(env, coordx),
        enif_make_double(env, coordy));
}

ERL_NIF_TERM
geom_to_eterm_linestring_coords(ErlNifEnv *env, const GEOSGeometry *geom)
{
    const GEOSCoordSequence *coords_seq;

    coords_seq = GEOSGeom_getCoordSeq(geom);
    return GEOSCoordSequence_to_eterm_list(env, coords_seq,
        GEOSGetNumCoordinates(geom));
}

ERL_NIF_TERM
geom_to_eterm_polygon_coords(ErlNifEnv *env, const GEOSGeometry *geom)
{
    unsigned int inner_num, i;
    const GEOSGeometry *outer, *inner;
    const GEOSCoordSequence *coords_seq;
    ERL_NIF_TERM coords;
    ERL_NIF_TERM *rings;

    inner_num = GEOSGetNumInteriorRings(geom);
    // all rings, outer + inner
    rings = malloc(sizeof(ERL_NIF_TERM)*inner_num+1);

    outer = GEOSGetExteriorRing(geom);
    coords_seq = GEOSGeom_getCoordSeq(outer);
    rings[0] = GEOSCoordSequence_to_eterm_list(env, coords_seq,
        GEOSGetNumCoordinates(outer));

    for (i=0; i<inner_num; i++) {
        inner = GEOSGetInteriorRingN(geom, i);
        coords_seq = GEOSGeom_getCoordSeq(inner);
        rings[i+1] = GEOSCoordSequence_to_eterm_list(env,
            coords_seq, GEOSGetNumCoordinates(inner));
    }
    coords = enif_make_list_from_array(env, rings, inner_num+1);
    free(rings);
    return coords;
}

// Creates the coordinates for a multi-geometry.
static ERL_NIF_TERM
geom_to_eterm_multi_coords(ErlNifEnv *env, const GEOSGeometry *multi_geom,
        ERL_NIF_TERM(*geom_to_eterm_coords)(ErlNifEnv *env, const GEOSGeometry *geom))
{
    int geom_num, i;
    const GEOSGeometry *geom;
    ERL_NIF_TERM coords;
    ERL_NIF_TERM *coords_multi;

    geom_num = GEOSGetNumGeometries(multi_geom);
    coords_multi = malloc(sizeof(ERL_NIF_TERM)*geom_num);
    for (i=0; i<geom_num; i++) {
        geom = GEOSGetGeometryN(multi_geom, i);
        coords_multi[i] = (*geom_to_eterm_coords)(env, geom);
    }
    coords = enif_make_list_from_array(env, coords_multi, geom_num);
    free(coords_multi);
    return coords;
}

static ERL_NIF_TERM
geom_to_eterm(ErlNifEnv *env, const GEOSGeometry *geom)
{
    ERL_NIF_TERM coords;
    int type = GEOSGeomTypeId(geom);

    switch(type) {
    case GEOS_POINT:
        if (GEOSisEmpty(geom)) {
            coords = enif_make_list(env, 0);
        }
        else {
            coords = geom_to_eterm_point_coords(env, geom);
        }
        return enif_make_tuple2(env, enif_make_atom(env, "Point"), coords);
    case GEOS_LINESTRING:
        if (GEOSisEmpty(geom)) {
            coords = enif_make_list(env, 0);
        }
        else {
            coords = geom_to_eterm_linestring_coords(env, geom);
        }
        return enif_make_tuple2(env, enif_make_atom(env, "LineString"),
            coords);
    case GEOS_POLYGON:
        if (GEOSisEmpty(geom)) {
            coords = enif_make_list(env, 0);
        }
        else {
            coords = geom_to_eterm_polygon_coords(env, geom);
        }
        return enif_make_tuple2(env, enif_make_atom(env, "Polygon"), coords);
    case GEOS_MULTIPOINT:
        if (GEOSisEmpty(geom)) {
            coords = enif_make_list(env, 0);
        }
        else {
            coords = geom_to_eterm_multi_coords(env, geom,
                geom_to_eterm_point_coords);
        }
        return enif_make_tuple2(env, enif_make_atom(env, "MultiPoint"),
            coords);
    case GEOS_MULTILINESTRING:
        if (GEOSisEmpty(geom)) {
            coords = enif_make_list(env, 0);
        }
        else {
            coords = geom_to_eterm_multi_coords(env, geom,
                geom_to_eterm_linestring_coords);
        }
        return enif_make_tuple2(env, enif_make_atom(env, "MultiLineString"),
            coords);
    case GEOS_MULTIPOLYGON:
        if (GEOSisEmpty(geom)) {
            coords = enif_make_list(env, 0);
        }
        else {
            coords = geom_to_eterm_multi_coords(env, geom,
                geom_to_eterm_polygon_coords);
        }
        return enif_make_tuple2(env, enif_make_atom(env, "MultiPolygon"),
            coords);
    case GEOS_GEOMETRYCOLLECTION:
        if (GEOSisEmpty(geom)) {
            coords = enif_make_list(env, 0);
        }
        else {
            coords = geom_to_eterm_multi_coords(env, geom, geom_to_eterm);
        }
        return enif_make_tuple2(env, enif_make_atom(env, "GeometryCollection"),
            coords);
    }
    return -1;
}


/* From http://trac.gispython.org/lab/browser/PCL/trunk/PCL-Core/cartography/geometry/_geommodule.c */
static void
notice_handler(const char *fmt, ...) {
    va_list ap;
    fprintf(stderr, "NOTICE: ");
    va_start (ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n" );
}

/* From http://trac.gispython.org/lab/browser/PCL/trunk/PCL-Core/cartography/geometry/_geommodule.c */
static void
error_handler(const char *fmt, ...) {
    va_list ap;
    va_start (ap, fmt);
    va_end(ap);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n" );
}

static void
geom_destroy(ErlNifEnv *env, void *obj)
{
    GEOSGeometry **geom = (GEOSGeometry**)obj;
    GEOSGeom_destroy(*geom);
}

/* From https://github.com/iamaleksey/iconverl/blob/master/c_src/iconverl.c */
static int
load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    initGEOS(notice_handler, error_handler);

    GEOSGEOM_RESOURCE = enif_open_resource_type(
        env, NULL, "geosgeom_resource", &geom_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return 0;
}
void
unload(ErlNifEnv* env, void* priv_data)
{
    finishGEOS();
}

static ERL_NIF_TERM
disjoint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSGeometry **geom1;
    GEOSGeometry **geom2;

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom1)) {
        return 0;
    }
    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom2)) {
        return 0;
    }

    if (GEOSDisjoint(*geom1, *geom2)) {
        return enif_make_atom(env, "true");
    }
    else {
        return enif_make_atom(env, "false");
    }
}

static ERL_NIF_TERM
topology_preserve_simplify(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSGeometry **geom;
    GEOSGeometry *simpler_geom;
    double dbl_tol;
    int int_tol;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return 0;
    }
    if (enif_get_int(env, argv[1], &int_tol)) {
        dbl_tol = int_tol;
    }
    else if (!enif_get_double(env, argv[1], &dbl_tol)) {
        return 0;
    }

    simpler_geom = GEOSTopologyPreserveSimplify(*geom, dbl_tol);
    eterm = geom_to_eterm(env, simpler_geom);
    GEOSGeom_destroy(simpler_geom);
    return eterm;
}

static
ERL_NIF_TERM to_geom(ErlNifEnv* env, int argc,
        const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM eterm;
    GEOSGeometry **geom = enif_alloc_resource(GEOSGEOM_RESOURCE, sizeof(GEOSGeometry*));

    *geom = eterm_to_geom(env, argv);
    eterm = enif_make_resource(env, geom);
    enif_release_resource(geom);
    return eterm;
}

static
ERL_NIF_TERM from_geom(ErlNifEnv* env, int argc,
        const ERL_NIF_TERM argv[]) {
    GEOSGeometry **geom;
    ERL_NIF_TERM eterm;

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return 0;
    }

    eterm = geom_to_eterm(env, *geom);
    return eterm;
}


static ErlNifFunc nif_funcs[] =
{
    {"disjoint", 2, disjoint},
    {"topology_preserve_simplify", 2, topology_preserve_simplify},
    {"to_geom", 1, to_geom},
    {"from_geom", 1, from_geom}
};

ERL_NIF_INIT(erlgeom, nif_funcs, &load, NULL, NULL, unload);
