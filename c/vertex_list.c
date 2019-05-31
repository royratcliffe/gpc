#include "vertex_list.h"
#include "vertex.h"

int vertex_list_term(gpc_vertex_list *list, term_t Term)
{ term_t tail = PL_new_term_ref();
  PL_put_nil(tail);
  for (int index = list->num_vertices; --index >= 0;)
  { term_t head = PL_new_term_ref();
    if (!vertex_term(list->vertex + index, head)) PL_fail;
    if (!PL_cons_list(tail, head, tail)) PL_fail;
  }
  return PL_unify(Term, tail);
}

/**
 * List of vertices to a GPC vertex list. Adds X and Y ordinates from
 * vertex(X, Y) terms within the list to the pre-existing GPC vertex
 * list.
 */
int term_vertex_list(term_t Term, gpc_vertex_list *list)
{ term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(Term);
  for (; PL_get_list(tail, head, tail); ++list->num_vertices)
  { list->vertex = PL_realloc(list->vertex, sizeof(list->vertex[0]) * (list->num_vertices + 1));
    if (!term_vertex(head, list->vertex + list->num_vertices)) PL_fail;
  }
  PL_succeed;
}

/**
 * Cleans up the contents of a GPC vertex list. The given list returns
 * to its original empty condition. Does not de-allocate the list.
 * Leaves that to the caller, since the list itself could exist in the
 * heap or on the stack.
 */
void cleanup_vertex_list(gpc_vertex_list *list)
{ PL_free(list->vertex);
  list->vertex = NULL;
  list->num_vertices = 0;
}
