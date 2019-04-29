#include "polygon_blob.h"

/**
 * Gets the polygon blob from the given term. Succeeds if the given term
 * associates with a GPC polygon blob.
 */
int get_polygon(term_t term, gpc_polygon **blob_out)
{ void *blob;
  PL_blob_t *type;
  if (!PL_get_blob(term, &blob, NULL, &type) && type == &polygon_blob)
  { return PL_type_error("gpc_polygon", term);
  }
  *blob_out = blob;
  PL_succeed;
}

/**
 * Unifies term with a new empty polygon blob.
 *
 * Uses PL_malloc(), rather than standard malloc(). On memory-allocation
 * failure, PL_malloc() never returns. Does not pre-allocate the contour
 * array. It remains NULL, and therefore assumes that PL_free(NULL)
 * succeeds quietly.
 */
int unify_polygon(term_t term)
{ gpc_polygon *blob = PL_malloc(sizeof(*blob));
  memset(blob, 0, sizeof(*blob));
  return PL_unify_blob(term, blob, sizeof(*blob), &polygon_blob);
}

int release_polygon(atom_t atom)
{ gpc_polygon *blob = PL_blob_data(atom, NULL, NULL);
  gpc_free_polygon(blob);
  PL_free(blob);
  PL_succeed;
}

int write_polygon(IOSTREAM *stream, atom_t atom, int flags)
{
  gpc_polygon *blob = PL_blob_data(atom, NULL, NULL);
  Sfprintf(stream, "<gpc_polygon>(%p)", blob);
  PL_succeed;
}

PL_blob_t polygon_blob =
{ .magic = PL_BLOB_MAGIC,
  .flags = PL_BLOB_NOCOPY,
  .name = "gpc_polygon",
  .release = release_polygon,
  .write = write_polygon
};
