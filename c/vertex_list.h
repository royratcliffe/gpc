#pragma once

#include "swi.h"
#include "gpc.h"

int vertex_list_term(gpc_vertex_list *list, term_t term);
int term_vertex_list(term_t term, gpc_vertex_list *list);
void cleanup_vertex_list(gpc_vertex_list *list);
