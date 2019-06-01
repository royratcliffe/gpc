#pragma once

#include "swi.h"
#include "gpc.h"

extern PL_blob_t tristrip_blob;

int get_tristrip(term_t Term, gpc_tristrip **blob_out);
int unify_tristrip(term_t Term);
