#include "vertex.h"
#include "pl.h"

int vertex_term(gpc_vertex *vertex, term_t Term)
{ return PL_unify_term(Term, PL_FUNCTOR, vertex_2_functor, PL_DOUBLE, vertex->x, PL_DOUBLE, vertex->y);
}

int term_vertex(term_t Term, gpc_vertex *vertex)
{ if (!PL_is_functor(Term, vertex_2_functor))
  { return PL_type_error("vertex", Term);
  }
  term_t arg1 = PL_new_term_ref();
  if (!PL_get_arg(1, Term, arg1) || !PL_get_float(arg1, &vertex->x))
  { return PL_type_error("float", Term);
  }
  term_t arg2 = PL_new_term_ref();
  if (!PL_get_arg(2, Term, arg2) || !PL_get_float(arg2, &vertex->y))
  { return PL_type_error("float", Term);
  }
  PL_succeed;
}
