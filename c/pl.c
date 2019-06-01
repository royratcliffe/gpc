#include "pl.h"

#include "polygon_blob.h"
#include "tristrip_blob.h"
#include "vertex_list.h"
#include "vertex.h"
#include "gpc.h"

#include <assert.h>
#include <memory.h>

atom_t diff_atom;
atom_t int_atom;
atom_t xor_atom;
atom_t union_atom;

functor_t external_1_functor;
functor_t hole_1_functor;
functor_t vertex_2_functor;

int term_op(term_t Term, gpc_op *Op)
{ atom_t atom;
  if (!PL_get_atom(Term, &atom)) return PL_type_error("atom", Term);
  if (atom == diff_atom) *Op = GPC_DIFF;
  else if (atom == int_atom) *Op = GPC_INT;
  else if (atom == xor_atom) *Op = GPC_XOR;
  else if (atom == union_atom) *Op = GPC_UNION;
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

foreign_t version(term_t Version)
{ return PL_unify_term(Version, PL_FUNCTOR_CHARS, ":", 2, PL_INT, 2, PL_INT, 32);
}

foreign_t empty_polygon(term_t Polygon)
{ return unify_polygon(Polygon);
}

foreign_t polygon_num_contours(term_t Polygon, term_t NumContours)
{ gpc_polygon *blob;
  if (!get_polygon(Polygon, &blob)) PL_fail;
  return PL_unify_integer(NumContours, blob->num_contours);
}

foreign_t polygon_add_contour(term_t Polygon, term_t Contour)
{ gpc_polygon *blob;
  functor_t functor;
  term_t vertices;
  int hole;
  if (!get_polygon(Polygon, &blob)) PL_fail;
  if (!PL_get_functor(Contour, &functor)) PL_fail;
  if (functor == external_1_functor) hole = FALSE;
  else if (functor == hole_1_functor) hole = TRUE;
  else return PL_type_error("external or hole functor", Contour);
  vertices = PL_new_term_ref();
  if (!PL_get_arg(1, Contour, vertices)) PL_fail;
  gpc_vertex_list list;
  list.num_vertices = 0;
  list.vertex = PL_malloc(sizeof(list.vertex[0]) << 4);
  if (!term_vertex_list(vertices, &list))
  { cleanup_vertex_list(&list);
    PL_fail;
  }
  gpc_add_contour(blob, &list, hole);
  cleanup_vertex_list(&list);
  PL_succeed;
}

foreign_t polygon_contour(term_t Polygon, term_t Contour, control_t Control)
{ int context = (int)PL_foreign_context(Control);
  gpc_polygon *blob;
  if (!get_polygon(Polygon, &blob)) PL_fail;
  switch (PL_foreign_control(Control))
  {   term_t list;
    case PL_FIRST_CALL:
      if (blob->num_contours == 0) PL_fail;
    case PL_REDO:
      list = PL_new_term_ref();
      if (!vertex_list_term(&blob->contour[context], list)) PL_fail;
      functor_t functor = blob->hole[context] ? hole_1_functor : external_1_functor;
      if (!PL_unify_term(Contour, PL_FUNCTOR, functor, PL_TERM, list)) PL_fail;
      if (++context < blob->num_contours) PL_retry(context);
    case PL_PRUNED:
      PL_succeed;
  }
  PL_fail;
}

foreign_t polygon_clip(atom_t Op, term_t Subject, term_t Clip, term_t Result)
{ gpc_op op;
  gpc_polygon *subject, *clip, *result;
  if (!term_op(Op, &op)) PL_fail;
  if (!get_polygon(Subject, &subject)) PL_fail;
  if (!get_polygon(Clip, &clip)) PL_fail;
  unify_polygon(Result);
  assert(get_polygon(Result, &result));
  gpc_polygon_clip(op, subject, clip, result);
  PL_succeed;
}

foreign_t polygon_to_tristrip(term_t Polygon, term_t Tristrip)
{ gpc_polygon *polygon;
  gpc_tristrip *tristrip;
  if (!get_polygon(Polygon, &polygon)) PL_fail;
  unify_tristrip(Tristrip);
  assert(get_tristrip(Tristrip, &tristrip));
  gpc_polygon_to_tristrip(polygon, tristrip);
  PL_succeed;
}

/**
 * Same as polygon_clip(Op, Subject, Clip, Result) except that Result is
 * a tristrip rather than a polygon.
 */
foreign_t tristrip_clip(atom_t Op, term_t Subject, term_t Clip, term_t Result)
{ gpc_op op;
  gpc_polygon *subject, *clip;
  gpc_tristrip *result;
  if (!term_op(Op, &op)) PL_fail;
  if (!get_polygon(Subject, &subject)) PL_fail;
  if (!get_polygon(Clip, &clip)) PL_fail;
  unify_tristrip(Result);
  assert(get_tristrip(Result, &result));
  gpc_tristrip_clip(op, subject, clip, result);
  PL_succeed;
}

foreign_t tristrip_num_strips(term_t Tristrip, term_t NumStrips)
{ gpc_tristrip *tristrip;
  if (!get_tristrip(Tristrip, &tristrip)) PL_fail;
  return PL_unify_integer(NumStrips, tristrip->num_strips);
}

foreign_t tristrip_vertices(term_t Tristrip, term_t Vertices, control_t Control)
{ int context = (int)PL_foreign_context(Control);
  gpc_tristrip *tristrip;
  if (!get_tristrip(Tristrip, &tristrip)) PL_fail;
  switch (PL_foreign_control(Control))
  {   term_t list;
    case PL_FIRST_CALL:
      if (tristrip->num_strips == 0) PL_fail;
    case PL_REDO:
      if (!vertex_list_term(&tristrip->strip[context], Vertices)) PL_fail;
      if (++context < tristrip->num_strips) PL_retry(context);
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
  PL_register_foreign("gpc_polygon_clip", 4, polygon_clip, 0);
  PL_register_foreign("gpc_polygon_to_tristrip", 2, polygon_to_tristrip, 0);
  PL_register_foreign("gpc_tristrip_clip", 4, tristrip_clip, 0);
  PL_register_foreign("gpc_tristrip_num_strips", 2, tristrip_num_strips, 0);
  PL_register_foreign("gpc_tristrip_vertices", 2, tristrip_vertices, PL_FA_NONDETERMINISTIC);
}

install_t uninstall_gpc()
{ ;
}
