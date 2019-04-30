#include "pl.h"

#include "polygon_blob.h"
#include "vertex_list.h"
#include "vertex.h"
#include "gpc.h"

atom_t diff_atom;
atom_t int_atom;
atom_t xor_atom;
atom_t union_atom;

functor_t external_1_functor;
functor_t hole_1_functor;

functor_t vertex_2_functor;

int term_op(term_t term, gpc_op *op)
{ if (term == diff_atom) *op = GPC_DIFF;
  else if (term == int_atom) *op = GPC_INT;
  else if (term == xor_atom) *op = GPC_XOR;
  else if (term == union_atom) *op = GPC_UNION;
  else PL_fail;
  PL_succeed;
}

/*
 *    __                _
 *   / _| ___  _ __ ___(_) __ _ _ __
 *  | |_ / _ \| '__/ _ \ |/ _` | '_ \
 *  |  _| (_) | | |  __/ | (_| | | | |
 *  |_|  \___/|_|  \___|_|\__, |_| |_|
 *                        |___/
 */

foreign_t version(term_t version)
{ return PL_unify_term(version, PL_FUNCTOR_CHARS, ":", 2, PL_INT, 2, PL_INT, 32);
}

foreign_t empty_polygon(term_t polygon)
{ return unify_polygon(polygon);
}

foreign_t polygon_num_contours(term_t polygon, term_t num_contours)
{ gpc_polygon *blob;
  if (!get_polygon(polygon, &blob)) PL_fail;
  return PL_unify_integer(num_contours, blob->num_contours);
}

foreign_t polygon_add_contour(term_t polygon, term_t contour)
{ gpc_polygon *blob;
  functor_t functor;
  term_t vertices;
  int hole;
  if (!get_polygon(polygon, &blob)) PL_fail;
  if (!PL_get_functor(contour, &functor)) PL_fail;
  if (functor == external_1_functor) hole = FALSE;
  else if (functor == hole_1_functor) hole = TRUE;
  else return PL_type_error("contour", contour);
  vertices = PL_new_term_ref();
  if (!PL_get_arg(1, contour, vertices)) PL_fail;
  gpc_vertex_list list;
  list.num_vertices = 0;
  list.vertex = PL_malloc(sizeof(list.vertex[0]) << 4);
  if (!term_vertex_list(vertices, &list))
  { PL_free(list.vertex);
    PL_fail;
  }
  gpc_add_contour(blob, &list, hole);
  PL_free(list.vertex);
  PL_succeed;
}

foreign_t polygon_contour(term_t polygon, term_t contour, control_t control)
{ int context = (int)PL_foreign_context(control);
  gpc_polygon *blob;
  if (!get_polygon(polygon, &blob)) PL_fail;
  switch (PL_foreign_control(control))
  {   term_t list;
    case PL_FIRST_CALL:
      if (blob->num_contours == 0) PL_fail;
    case PL_REDO:
      list = PL_new_term_ref();
      if (!vertex_list_term(&blob->contour[context], list)) PL_fail;
      functor_t functor = blob->hole[context] ? hole_1_functor : external_1_functor;
      if (!PL_unify_term(contour, PL_FUNCTOR, functor, PL_TERM, list)) PL_fail;
      if (++context < blob->num_contours) PL_retry(context);
    case PL_PRUNED:
      PL_succeed;
  }
  PL_fail;
}

/*
 *     _           _        _ _
 *    (_)_ __  ___| |_ __ _| | |
 *    | | '_ \/ __| __/ _` | | |
 *    | | | | \__ \ || (_| | | |
 *    |_|_| |_|___/\__\__,_|_|_|
 */

install_t install_gpc()
{ diff_atom = PL_new_atom("diff");
  int_atom = PL_new_atom("int");
  xor_atom = PL_new_atom("xor");
  union_atom = PL_new_atom("union");
  external_1_functor = PL_new_functor_sz(PL_new_atom("external"), 1);
  hole_1_functor = PL_new_functor_sz(PL_new_atom("hole"), 1);
  vertex_2_functor = PL_new_functor_sz(PL_new_atom("vertex"), 2);

  PL_register_foreign("gpc_version", 1, version, 0);
  PL_register_foreign("gpc_empty_polygon", 1, empty_polygon, 0);
  PL_register_foreign("gpc_polygon_num_contours", 2, polygon_num_contours, 0);
  PL_register_foreign("gpc_polygon_add_contour", 2, polygon_add_contour, 0);
  PL_register_foreign("gpc_polygon_contour", 2, polygon_contour, PL_FA_NONDETERMINISTIC);
}

install_t uninstall_gpc()
{ ;
}
