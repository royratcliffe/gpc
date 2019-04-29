#include "vertex_list.h"
#include "vertex.h"

int vertex_list_term(gpc_vertex_list *list, term_t term)
{ term_t head = PL_new_term_ref();
  term_t tail = PL_new_term_ref();
  PL_put_nil(tail);
  for (int index = list->num_vertices; --index >= 0;)
  { if (!vertex_term(list->vertex + index, head)) PL_fail;
    if (!PL_cons_list(tail, head, tail)) PL_fail;
  }
  return PL_put_term(term, tail);
}

/**
 * List of vertices to a GPC vertex list. Adds X and Y ordinates from
 * vertex(X, Y) terms within the list to the pre-existing GPX vertex
 * list.
 */
int term_vertex_list(term_t term, gpc_vertex_list *list)
{ term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(term);
  for (; PL_get_list(tail, head, tail); ++list->num_vertices)
  { list->vertex = PL_realloc(list->vertex, sizeof(list->vertex[0]) * (list->num_vertices + 1));
    if (!term_vertex(head, list->vertex + list->num_vertices)) PL_fail;
  }
  PL_succeed;
}
