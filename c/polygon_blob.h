#pragma once

#include "swi.h"
#include "gpc.h"

extern PL_blob_t polygon_blob;

int get_polygon(term_t term, gpc_polygon **blob_out);
int unify_polygon(term_t term);
int release_polygon(atom_t atom);
