#include "tristrip_blob.h"

#include <memory.h>

int get_tristrip(term_t Term, gpc_tristrip **blob_out)
{ void *blob;
  PL_blob_t *type;
  if (!PL_get_blob(Term, &blob, NULL, &type) && type == &tristrip_blob)
  { return PL_type_error("gpc_tristrip", Term);
  }
  *blob_out = blob;
  PL_succeed;
}

int unify_tristrip(term_t Term)
{ gpc_tristrip *blob = PL_malloc(sizeof(*blob));
  memset(blob, 0, sizeof(*blob));
  return PL_unify_blob(Term, blob, sizeof(*blob), &tristrip_blob);
}

int release_tristrip(atom_t Atom)
{ gpc_tristrip *blob = PL_blob_data(Atom, NULL, NULL);
  gpc_free_tristrip(blob);
  PL_free(blob);
  PL_succeed;
}

int write_tristrip(IOSTREAM *stream, atom_t Atom, int flags)
{ gpc_tristrip *blob = PL_blob_data(Atom, NULL, NULL);
  Sfprintf(stream, "<gpc_tristrip>(%p)", blob);
  PL_succeed;
}

PL_blob_t tristrip_blob =
{ .magic = PL_BLOB_MAGIC,
  .flags = PL_BLOB_NOCOPY,
  .name = "gpc_tristrip",
  .release = release_tristrip,
  .write = write_tristrip
};
