#include "vertex.h"
#include "pl.h"

int vertex_term(gpc_vertex *vertex, term_t term)
{ return PL_unify_term(term, PL_FUNCTOR, vertex_2_functor, PL_DOUBLE, vertex->x, PL_DOUBLE, vertex->y);
}

int term_vertex(term_t term, gpc_vertex *vertex)
{ if (!PL_is_functor(term, vertex_2_functor))
  { return PL_type_error("vertex", term);
  }
  term_t arg1 = PL_new_term_ref();
  if (!PL_get_arg(1, term, arg1) || !PL_get_float(arg1, &vertex->x))
  { return PL_type_error("float", term);
  }
  term_t arg2 = PL_new_term_ref();
  if (!PL_get_arg(2, term, arg2) || !PL_get_float(arg2, &vertex->y))
  { return PL_type_error("float", term);
  }
  PL_succeed;
}
