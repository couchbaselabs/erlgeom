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
static ErlNifResourceType* GEOSWKTREADER_RESOURCE;
static ErlNifResourceType* GEOSWKTWRITER_RESOURCE;
static ErlNifResourceType* GEOSWKBREADER_RESOURCE;
static ErlNifResourceType* GEOSWKBWRITER_RESOURCE;
static ErlNifResourceType* GEOSSTRTREE_RESOURCE;


/* Currently support for 2 dimensions only */
int
set_GEOSCoordSeq_from_eterm_list(GEOSCoordSequence *seq, int pos,
        ErlNifEnv *env, const ERL_NIF_TERM *coords) 
{
    double dbl_coord;
    int int_coord;
    ERL_NIF_TERM head, tail;

    if (enif_get_list_cell(env, *coords, &head, &tail)) {
        if (enif_get_int(env, head, &int_coord)) {
            dbl_coord = int_coord;
        }
        else if (!enif_get_double(env, head, &dbl_coord)) {
            return enif_make_badarg(env);
        }
        GEOSCoordSeq_setX(seq, pos, dbl_coord);

        enif_get_list_cell(env, tail, &head, &tail);
        if (enif_get_int(env, head, &int_coord)) {
            dbl_coord = int_coord;
        }
        else if (!enif_get_double(env, head, &dbl_coord)) {
            return enif_make_badarg(env);
        }
        GEOSCoordSeq_setY(seq, pos, dbl_coord);
        return 1;
    }
    return enif_make_badarg(env);
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
    geoms = enif_alloc(sizeof(GEOSGeometry*)*rings_num-1);
    for (i=0; enif_get_list_cell(env, inner_eterm, &inner_eterm, &tail); i++) {
        inner_seq = eterm_to_geom_linestring_coords(env, &inner_eterm);
        geoms[i] = GEOSGeom_createLinearRing(inner_seq);
        inner_eterm = tail;
    }
    geom = GEOSGeom_createPolygon(outer_geom, geoms, rings_num-1);
    enif_free(geoms);
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
    geoms = enif_alloc(sizeof(GEOSGeometry*)*geoms_num);
    for (i=0; enif_get_list_cell(env, eterm , &eterm, &tail); i++) {
        geoms[i] = (*eterm_to_geom)(env, &eterm);
        eterm = tail;
    }
    geom = GEOSGeom_createCollection(type, geoms, geoms_num);
    enif_free(geoms);
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
    const GEOSCoordSequence *coords_seq, unsigned int len)
{
    int i = 0;
    double coordx, coordy;
    ERL_NIF_TERM *coords_list;
    ERL_NIF_TERM coords, coords_list_eterm;

    coords_list = enif_alloc(sizeof(ERL_NIF_TERM)*len);
    for(i=0; i<len; i++) {
        GEOSCoordSeq_getX(coords_seq, i, &coordx);
        GEOSCoordSeq_getY(coords_seq, i, &coordy);
        coords = enif_make_list2(env, enif_make_double(env, coordx),
            enif_make_double(env, coordy));
        coords_list[i] = coords;
    }
    coords_list_eterm = enif_make_list_from_array(env, coords_list, len);
    enif_free(coords_list);
    return coords_list_eterm;
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
    rings = enif_alloc(sizeof(ERL_NIF_TERM)*inner_num+1);

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
    enif_free(rings);
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
    coords_multi = enif_alloc(sizeof(ERL_NIF_TERM)*geom_num);
    for (i=0; i<geom_num; i++) {
        geom = GEOSGetGeometryN(multi_geom, i);
        coords_multi[i] = (*geom_to_eterm_coords)(env, geom);
    }
    coords = enif_make_list_from_array(env, coords_multi, geom_num);
    enif_free(coords_multi);
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

typedef struct {
    int count;
    int size;
    GEOSGeometry** *geoms;
} GeosSTRtree_acc_t;

void
geosstrtree_cb(void *item, void *acc) {
    GEOSGeometry **geom = (GEOSGeometry **) item;
    GeosSTRtree_acc_t *acc_ptr  = (GeosSTRtree_acc_t *) acc;
    //fprintf(stderr, "Count:%d Size:%d\n", acc_ptr->count, acc_ptr->size);
    ++(acc_ptr->count);
    if (acc_ptr->count == acc_ptr->size) {
        acc_ptr->size *=2;
        acc_ptr->geoms = enif_realloc(acc_ptr->geoms, acc_ptr->size);
    }
    //fprintf(stderr, "Points: %d\n", GEOSGeomGetNumPoints(*geom));
    acc_ptr->geoms[acc_ptr->count-1] = geom;
}


/* From http://trac.gispython.org/lab/browser/PCL/trunk/PCL-Core/cartography/
    geometry/_geommodule.c */
static void
notice_handler(const char *fmt, ...) {
    va_list ap;
    fprintf(stderr, "NOTICE: ");
    va_start (ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n" );
}

/* From http://trac.gispython.org/lab/browser/PCL/trunk/PCL-Core/cartography/
    geometry/_geommodule.c */
static void
error_handler(const char *fmt, ...)
{
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

static void
wktreader_destroy(ErlNifEnv *env, void *obj)
{
    GEOSWKTReader **wkt_reader = (GEOSWKTReader**)obj;
    GEOSWKTReader_destroy(*wkt_reader);
}

static void
wktwriter_destroy(ErlNifEnv *env, void *obj)
{
    GEOSWKTWriter **wkt_writer = (GEOSWKTWriter**)obj;
    GEOSWKTWriter_destroy(*wkt_writer);
}

static void
wkbreader_destroy(ErlNifEnv *env, void *obj)
{
    GEOSWKBReader **wkb_reader = (GEOSWKBReader**)obj;
    GEOSWKBReader_destroy(*wkb_reader);
}

static void
wkbwriter_destroy(ErlNifEnv *env, void *obj)
{
    GEOSWKBWriter **wkb_writer = (GEOSWKBWriter**)obj;
    GEOSWKBWriter_destroy(*wkb_writer);
}

static void
geosstrtree_destroy(ErlNifEnv *env, void *obj)
{
    GEOSSTRtree **tree = (GEOSSTRtree**)obj;
    GEOSSTRtree_destroy(*tree);
}


/* From https://github.com/iamaleksey/iconverl/blob/master/c_src/iconverl.c */
static int
load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    initGEOS(notice_handler, error_handler);

    GEOSGEOM_RESOURCE = enif_open_resource_type(
        env, NULL, "geosgeom_resource", &geom_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    GEOSWKTREADER_RESOURCE = enif_open_resource_type(
        env, NULL, "geoswktreader_resource", &wktreader_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    GEOSWKTWRITER_RESOURCE = enif_open_resource_type(
        env, NULL, "geoswktwriter_resource", &wktwriter_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    GEOSWKBREADER_RESOURCE = enif_open_resource_type(
        env, NULL, "geoswkbreader_resource", &wkbreader_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    GEOSWKBWRITER_RESOURCE = enif_open_resource_type(
        env, NULL, "geoswkbwriter_resource", &wkbwriter_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    GEOSSTRTREE_RESOURCE = enif_open_resource_type(
        env, NULL, "geosstrtree_resource", &geosstrtree_destroy,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    return 0;
}

void
unload(ErlNifEnv* env, void* priv_data)
{
    finishGEOS();
}


/************************************************************************
 *
 *  Binary predicates - return 2 on exception, 1 on true, 0 on false
 *
 ***********************************************************************/

/*
Geom1 = erlgeom:to_geom({'Point',[5,5]}),
Geom2 = erlgeom:to_geom({'LineString', [[1,1],[14,14]]}),
erlgeom:disjoint(Geom1, Geom2).
false
*/
static ERL_NIF_TERM
disjoint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSGeometry **geom1;
    GEOSGeometry **geom2;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom1)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom2)) {
        return enif_make_badarg(env);
    }
    
    int result;
    if ((result = GEOSDisjoint(*geom1, *geom2)) == 1) {
        return enif_make_atom(env, "true");
    } else if (result == 0) {
        return enif_make_atom(env, "false");
    } else {
        return enif_make_atom(env, "error");
    }
}

/*
Geom1 = erlgeom:to_geom({'LineString', [[3,3],[10,10]]}),
Geom2 = erlgeom:to_geom({'LineString', [[1,1],[7,7]]}),
erlgeom:intersects(Geom1, Geom2).
true
*/
static ERL_NIF_TERM
intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSGeometry **geom1;
    GEOSGeometry **geom2;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom1)) {
        return enif_make_badarg(env);
    }
    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom2)) {
        return enif_make_badarg(env);
    }

    int result;
    if ((result = GEOSIntersects(*geom1, *geom2)) == 1 ) {
        return enif_make_atom(env, "true");
    } else if (result == 0) {
        return enif_make_atom(env, "false");
    } else {
        return enif_make_atom(env, "error");
    }
}


/************************************************************************
 *
 * Topology operations - return NULL on exception.
 *
 ***********************************************************************/

/*
Geom1 = erlgeom:to_geom({'LineString', [[3,3],[10,10]]}),
Geom2 = erlgeom:to_geom({'LineString', [[1,1],[7,7]]}),
Geom3 = erlgeom:intersection(Geom1, Geom2),
erlgeom:from_geom(Geom3).
{'LineString', [[3,3],[7,7]]}
*/
static ERL_NIF_TERM
intersection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSGeometry **geom1;
    GEOSGeometry **geom2;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom1)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom2)) {
        return enif_make_badarg(env);
    }

    GEOSGeometry **result_geom = \
        enif_alloc_resource(GEOSGEOM_RESOURCE, sizeof(GEOSGeometry*));
    *result_geom = GEOSIntersection(*geom1, *geom2);

    if (*result_geom == NULL) {
        eterm = enif_make_atom(env, "undefined");
    } else {
        eterm = enif_make_tuple2(env,
            enif_make_atom(env, "ok"),
            enif_make_resource(env, result_geom));
        enif_release_resource(result_geom);
    }
    return eterm;
}

/*
Geom1 = erlgeom:to_geom({'LineString', [[4,4],[10,10]]}),
Geom2 = erlgeom:get_centroid_geom(Geom1),
erlgeom:from_geom(Geom2).
{'Point',[7.0,7.0]}
*/
static ERL_NIF_TERM
get_centroid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSGeometry **geom;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env);
    }

    GEOSGeometry **result_geom = \
        enif_alloc_resource(GEOSGEOM_RESOURCE, sizeof(GEOSGeometry*));
    *result_geom = GEOSGetCentroid(*geom);

    if (*result_geom == NULL) {
        eterm = enif_make_atom(env, "undefined");
    } else {
        eterm = enif_make_tuple2(env,
            enif_make_atom(env, "ok"),
            enif_make_resource(env, result_geom));
        enif_release_resource(result_geom);
    }
    return eterm;
}

/*
Geom1 = erlgeom:to_geom({'LineString', [[4,4], [4.5, 4.5], [10,10]]}),
erlgeom:topology_preserve_simplify(Geom1, 1).
{'LineString',[[4.0,4.0],[10.0,10.0]]}
*/
static ERL_NIF_TERM
topology_preserve_simplify(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSGeometry **geom;
    GEOSGeometry *simpler_geom;
    double dbl_tol;
    int int_tol;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env);
    }
    if (enif_get_int(env, argv[1], &int_tol)) {
        dbl_tol = int_tol;
    }
    else if (!enif_get_double(env, argv[1], &dbl_tol)) {
        return enif_make_badarg(env);
    }

    simpler_geom = GEOSTopologyPreserveSimplify(*geom, dbl_tol);
    eterm = geom_to_eterm(env, simpler_geom);
    GEOSGeom_destroy(simpler_geom);
    return eterm;
}


/************************************************************************
 *
 *  Validity checking
 *
 ***********************************************************************/
/*
Geom1 = erlgeom:to_geom({'LineString', [[4,4], [4.5, 4.5], [10,10]]}),
erlgeom:is_valid(Geom1).
true
Geom2 = erlgeom:wktreader_read(WktReader,"POLYGON((0 0, 1 1, 1 2, 1 1, 0 0))"),
erlgeom:is_valid(Geom2).
false
*/
static ERL_NIF_TERM
is_valid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSGeometry **geom1;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom1)) {
        return enif_make_badarg(env);
    }

    int isvalid;
    if ((isvalid = GEOSisValid(*geom1)) == 1 ) {
        return enif_make_atom(env, "true");
    }
    else if (isvalid == 0) {
        return enif_make_atom(env, "false");
    }
    else {
        return enif_make_atom(env, "error");
    }
}

/************************************************************************
 *
 * Reader and Writer APIs
 *
 ***********************************************************************/

/*
WktReader = erlgeom:wktreader_create().
<<>>
*/
static ERL_NIF_TERM
wktreader_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM eterm;
    GEOSWKTReader **wkt_reader = \
        enif_alloc_resource(GEOSWKTREADER_RESOURCE, sizeof(GEOSWKTReader*));

    *wkt_reader = GEOSWKTReader_create();
    eterm = enif_make_resource(env, wkt_reader);
    enif_release_resource(wkt_reader);
    return eterm;
}

/*
WktReader = erlgeom:wktreader_create(),
Geom = erlgeom:wktreader_read(WktReader, "POINT(10 10)"),
erlgeom:from_geom(Geom).
{'Point',[10.0,10.0]}
*/
static ERL_NIF_TERM
wktreader_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSWKTReader **wkt_reader;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSWKTREADER_RESOURCE,
        (void**)&wkt_reader)) {
        return enif_make_badarg(env);
    }

    unsigned len;
    if (!enif_get_list_length(env, argv[1], &len)){
        return enif_make_badarg(env);
    }
    char *wkt = enif_alloc(sizeof(char)*(len+1));

    if(!enif_get_string(env, argv[1], wkt, len+1, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    GEOSGeometry **geom = \
        enif_alloc_resource(GEOSGEOM_RESOURCE, sizeof(GEOSGeometry*));

    *geom = GEOSWKTReader_read(*wkt_reader, wkt);
    eterm = enif_make_resource(env, geom);
    enif_release_resource(geom);
    enif_free(wkt);
    return eterm;
}

/*
WkbReader = erlgeom:wkbreader_create().
<<>>
*/
static ERL_NIF_TERM
wkbreader_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM eterm;

    GEOSWKBReader **wkb_reader = \
        enif_alloc_resource(GEOSWKBREADER_RESOURCE, sizeof(GEOSWKBReader*));
    *wkb_reader = GEOSWKBReader_create();

    
    eterm = enif_make_resource(env, wkb_reader);
    enif_release_resource(wkb_reader);
    return eterm;
}

/*
WktReader = erlgeom:wktreader_create(),
Geom = erlgeom:wktreader_read(WktReader, "POINT(10.0 10.0)"),
WkbWriter = erlgeom:wkbwriter_create(),
Bin = erlgeom:wkbwriter_write(WkbWriter, Geom),
WkbReader = erlgeom:wkbreader_create(),
Geom2 = erlgeom:wkbreader_read(WkbReader, Bin),
erlgeom:from_geom(Geom2).
{'Point',[10.0,10.0]}
*/
static ERL_NIF_TERM
wkbreader_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSWKBReader **wkb_reader;
    ErlNifBinary bin;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSWKBREADER_RESOURCE,
        (void**)&wkb_reader)) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &bin)){
	    return enif_make_badarg(env);
    }

    GEOSGeometry **geom = \
        enif_alloc_resource(GEOSGEOM_RESOURCE, sizeof(GEOSGeometry*));

    *geom = GEOSWKBReader_read(*wkb_reader, bin.data, bin.size);
    eterm = enif_make_resource(env, geom);
    enif_release_resource(geom);
    return eterm;
}


/*
WkbReader = erlgeom:wkbreader_create(),
Geom = erlgeom:wkbreader_readhex(WkbReader,
    "010100000000000000000024400000000000002440"),
erlgeom:from_geom(Geom).
{'Point',[10.0,10.0]}
*/
static ERL_NIF_TERM
wkbreader_readhex(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSWKBReader **wkb_reader;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSWKBREADER_RESOURCE,
        (void**)&wkb_reader)) {
        return enif_make_badarg(env);
    }

    unsigned len;
    if (!enif_get_list_length(env, argv[1], &len)){
        return enif_make_badarg(env);
    }
    char *wkb_hex = enif_alloc(sizeof(char)*(len+1));

    // TODO: Specific message in cases < 0, == 0 
    if(enif_get_string(env, argv[1], wkb_hex, len+1, ERL_NIF_LATIN1) <= 0) {
        return enif_make_badarg(env);
    }

    GEOSGeometry **geom = \
        enif_alloc_resource(GEOSGEOM_RESOURCE, sizeof(GEOSGeometry*));

    size_t size = strlen(wkb_hex);
    *geom = GEOSWKBReader_readHEX(*wkb_reader, (unsigned char *)wkb_hex, size);
    eterm = enif_make_resource(env, geom);
    enif_release_resource(geom);
    enif_free(wkb_hex);
    return eterm;
}

/*
WktWriter = erlgeom:wktwriter_create().
<<>>
*/
static ERL_NIF_TERM
wktwriter_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM eterm;
    GEOSWKTWriter **wkt_writer = \
        enif_alloc_resource(GEOSWKTWRITER_RESOURCE, sizeof(GEOSWKTWriter*));

    *wkt_writer = GEOSWKTWriter_create();
    eterm = enif_make_resource(env, wkt_writer);
    enif_release_resource(wkt_writer);
    return eterm;
}

/*
WktReader = erlgeom:wktreader_create(),
Geom = erlgeom:wktreader_read(WktReader, "POINT(10 10)"),
WktWriter = erlgeom:wktwriter_create(),
erlgeom:wktwriter_write(WktWriter, Geom).
"Point(10.0000000000000000 10.0000000000000000)"
*/
static ERL_NIF_TERM
wktwriter_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSWKTWriter **wkt_writer;
    GEOSGeometry **geom;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSWKTWRITER_RESOURCE,
        (void**)&wkt_writer)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env);
    }

    char *wkt = GEOSWKTWriter_write(*wkt_writer, *geom);
    eterm = enif_make_string(env, wkt, ERL_NIF_LATIN1);
    GEOSFree(wkt);
    return eterm;
}


/*
WkbWriter = erlgeom:wkbwriter_create().
<<>>
*/
static ERL_NIF_TERM
wkbwriter_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM eterm;
    GEOSWKBWriter **wkb_writer = \
        enif_alloc_resource(GEOSWKBWRITER_RESOURCE, sizeof(GEOSWKBWriter*));

    *wkb_writer = GEOSWKBWriter_create();
    eterm = enif_make_resource(env, wkb_writer);
    enif_release_resource(wkb_writer);
    return eterm;
}

/*
WktReader = erlgeom:wktreader_create(),
Geom = erlgeom:wktreader_read(WktReader, "POINT(10.0 10.0)"),
WkbWriter = erlgeom:wkbwriter_create(),
erlgeom:wkbwriter_write(WkbWriter, Geom).
<<1,1,0,0,0,0,0,0,0,0,0,36,64,0,0,0,0,0,0,36,64>>
*/
static ERL_NIF_TERM
wkbwriter_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSWKBWriter **wkb_writer;
    GEOSGeometry **geom;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSWKBWRITER_RESOURCE,
        (void**)&wkb_writer)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env);
    }

    size_t size;
    unsigned char *wkb = GEOSWKBWriter_write(*wkb_writer, *geom, &size);
    wkb[size] = '\0'; /* ensure it is null terminated */
 
    ErlNifBinary bin = {.size = size, .data = wkb};
    eterm = enif_make_binary(env, &bin);
    enif_release_binary(&bin);
    GEOSFree(wkb);
    return eterm;
}


/*
WktReader = erlgeom:wktreader_create(),
Geom = erlgeom:wktreader_read(WktReader, "POINT(10.0 10.0)"),
WkbWriter = erlgeom:wkbwriter_create(),
erlgeom:wkbwriter_writehex(WkbWriter, Geom).
"010100000000000000000024400000000000002440"
*/
static ERL_NIF_TERM
wkbwriter_writehex(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSWKBWriter **wkb_writer;
    GEOSGeometry **geom;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSWKBWRITER_RESOURCE,
        (void**)&wkb_writer)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env);
    }

    size_t size;
    unsigned char *wkb_hex = GEOSWKBWriter_writeHEX(*wkb_writer, *geom, &size);
    wkb_hex[size] = '\0'; /* ensure it is null terminated */
    eterm = enif_make_string(env, (char *)wkb_hex, ERL_NIF_LATIN1);
    GEOSFree(wkb_hex);
    return eterm;
}

/************************************************************************
 *
 *  STRtree functions
 *
 ***********************************************************************/

/*
extern GEOSSTRtree GEOS_DLL *GEOSSTRtree_create(size_t nodeCapacity);

GeosSTRtree = erlgeom:geosstrtree_create().
<<>>
*/
static ERL_NIF_TERM
geosstrtree_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM eterm;
    GEOSSTRtree **tree = \
        enif_alloc_resource(GEOSSTRTREE_RESOURCE, sizeof(GEOSSTRtree*));
    size_t nodeCapacity = 10;
    *tree = GEOSSTRtree_create(nodeCapacity);
    eterm = enif_make_resource(env, tree);
    enif_release_resource(tree);
    return eterm;
}

/*
extern void GEOS_DLL GEOSSTRtree_insert(GEOSSTRtree *tree,
                                        const GEOSGeometry *g,
                                        void *item);

GeosSTRtree = erlgeom:geosstrtree_create(),
Geom = erlgeom:geosstrtree_insert(GeosSTRtree, Geom),
*/
static ERL_NIF_TERM
geosstrtree_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSSTRtree **tree;
    GEOSGeometry **geom;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSSTRTREE_RESOURCE, (void**)&tree)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env);
    }

    //GEOSSTRtree_insert(*tree, GEOSEnvelope(*geom), *geom);
    GEOSSTRtree_insert(*tree, GEOSEnvelope(*geom), geom);

    return enif_make_atom(env, "ok");
}

/*
extern void GEOS_DLL GEOSSTRtree_query(GEOSSTRtree *tree,
                                       const GEOSGeometry *g,
                                       GEOSQueryCallback callback,
                                       void *userdata);

GeosSTRtree = erlgeom:geosstrtree_create(),
Geom = erlgeom:geosstrtree_insert(GeosSTRtree, Geom),
erlgeom:geosstrtree_query(GeosSTRtree, Geom).
{Geom1, Geom2, Geom3, ..., GeomN}
*/
static ERL_NIF_TERM
geosstrtree_query(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSSTRtree **tree;
    GEOSGeometry **geom;
    ERL_NIF_TERM eterm;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSSTRTREE_RESOURCE, (void**)&tree)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env);
    }
    GEOSGeometry** *geoms = \
        (GEOSGeometry***) enif_alloc(sizeof(GEOSGeometry**)*100);
    GeosSTRtree_acc_t acc = {.count=0, .size=100, .geoms=geoms};
    GEOSSTRtree_query(*tree, *geom, geosstrtree_cb, &acc);

    ERL_NIF_TERM *arr = \
        (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM)*acc.count);

    int index = 0;
    for (; index<acc.count; index++) {
        //fprintf(stderr, "Points: %d\n",
        //    GEOSGeomGetNumPoints(*(acc.geoms[index])));
        arr[index] = enif_make_resource(env, acc.geoms[index]);
    }

    eterm = enif_make_tuple_from_array(env, arr, index);

    enif_free(arr);
    enif_free(geoms);
    return eterm;
}



/* extern void GEOS_DLL GEOSSTRtree_iterate(GEOSSTRtree *tree,
                                       GEOSQueryCallback callback,
                                       void *userdata);

*/
static ERL_NIF_TERM
geosstrtree_iterate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSSTRtree **tree;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSSTRTREE_RESOURCE, (void**)&tree)) {
        return enif_make_badarg(env);
    }

    GEOSGeometry** *geoms = \
        (GEOSGeometry***) enif_alloc(sizeof(GEOSGeometry**)*100);
    GeosSTRtree_acc_t acc = {.count=0, .size=100, .geoms=geoms};
    GEOSSTRtree_iterate(*tree, geosstrtree_cb, &acc);

    ERL_NIF_TERM *arr = \
        (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM)*acc.count);
    int index = 0;
    for (; index<acc.count; index++) {
        //fprintf(stderr, "Points: %d\n", GEOSGeomGetNumPoints(*(acc.geoms[index])));
        arr[index] = enif_make_resource(env, acc.geoms[index]);
    }

    eterm = enif_make_tuple_from_array(env, arr, index);

    enif_free(arr);
    enif_free(geoms);
    return eterm;
}

/*
extern char GEOS_DLL GEOSSTRtree_remove(GEOSSTRtree *tree,
                                        const GEOSGeometry *g,
                                        void *item);

GeosSTRtree = erlgeom:geosstrtree_create(),
Geom = erlgeom:geosstrtree_insert(GeosSTRtree, Geom),
erlgeom:geosstrtree_remove(GeosSTRtree, Geom).
{ok, 1}.
*/
static ERL_NIF_TERM
geosstrtree_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    GEOSSTRtree **tree;
    GEOSGeometry **geom;

    if (argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSSTRTREE_RESOURCE, (void**)&tree)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[1], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env);
    }

    //char remove = GEOSSTRtree_remove(*tree, GEOSEnvelope(*geom), *geom);
    char remove = GEOSSTRtree_remove(*tree, GEOSEnvelope(*geom), geom);
	//printf("Rtree remove: %d.\n", remove); 

    return enif_make_tuple2(env,
        enif_make_atom(env, "ok"),
        enif_make_int(env, (int)remove));
}


/************************************************************************
 *
 *  Erlang-GEOS Translation
 *
 ***********************************************************************/

static
ERL_NIF_TERM to_geom(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM eterm;
    GEOSGeometry **geom = \
        enif_alloc_resource(GEOSGEOM_RESOURCE, sizeof(GEOSGeometry*));

    *geom = eterm_to_geom(env, argv);
    eterm = enif_make_resource(env, geom);
    enif_release_resource(geom);
    return eterm;
}

static
ERL_NIF_TERM from_geom(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GEOSGeometry **geom;
    ERL_NIF_TERM eterm;

    if (argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], GEOSGEOM_RESOURCE, (void**)&geom)) {
        return enif_make_badarg(env);
    }

    eterm = geom_to_eterm(env, *geom);
    return eterm;
}


static ErlNifFunc nif_funcs[] =
{
    {"disjoint", 2, disjoint},
    {"from_geom", 1, from_geom},
    {"geosstrtree_create", 0, geosstrtree_create},
    {"geosstrtree_insert", 2, geosstrtree_insert},
    {"geosstrtree_iterate", 1, geosstrtree_iterate},
    {"geosstrtree_query", 2, geosstrtree_query},
    {"geosstrtree_remove", 2, geosstrtree_remove},
    {"get_centroid", 1, get_centroid},
    {"intersection", 2, intersection},
    {"intersects", 2, intersects},
    {"is_valid", 1, is_valid},
    {"to_geom", 1, to_geom},
    {"topology_preserve_simplify", 2, topology_preserve_simplify},
    {"wkbreader_create", 0, wkbreader_create},
    {"wkbreader_read", 2, wkbreader_read},
    {"wkbreader_readhex", 2, wkbreader_readhex},
    {"wkbwriter_create", 0, wkbwriter_create},
    {"wkbwriter_write", 2, wkbwriter_write},
    {"wkbwriter_writehex", 2, wkbwriter_writehex},
    {"wktreader_create", 0, wktreader_create},
    {"wktreader_read", 2, wktreader_read},
    {"wktwriter_create", 0, wktwriter_create},
    {"wktwriter_write", 2, wktwriter_write}
};

ERL_NIF_INIT(erlgeom, nif_funcs, &load, NULL, NULL, unload);
