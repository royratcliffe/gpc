#pragma once

#include "swi.h"
#include "gpc.h"

int vertex_term(gpc_vertex *vertex, term_t Term);
int term_vertex(term_t Term, gpc_vertex *vertex);
