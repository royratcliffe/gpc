:- module(gpc,
          [   gpc_version/1,
              gpc_empty_polygon/1,
              gpc_polygon_num_contours/2,
              gpc_polygon_add_contour/2,
              gpc_polygon/2,
              gpc_polygon_contour/2,
              gpc_polygon_clip/4,
              gpc_read_polygon/3,
              gpc_polygon_codes/2
          ]).

:- predicate_options(gpc_read_polygon/3, 3,
                     [   pass_to(readutil:read_file_to_codes/3, 3)
                     ]).

:- use_foreign_library(foreign(gpc)).

:- use_module(library(dcg/basics)).

/** <module> Generic Polygon Clipper

## What is a polygon?

It is an aggregate of zero or more contours, each
comprising zero or more vertices. Each vertex has two double-precision
ordinates: x and y. Contours can be external, or hole.

## To long, didn't read

Start with an empty polygon. Add contours. Call this the subject polygon. Do the
same again with different contours. Call this the clipping polygon. Clip
the subject polygon against the other. The result can be a difference,
intersection, exclusive-or or union.

In Prolog terms (pardon the pun) it works like this. Use clause

    gpc_empty_polygon(Polygon)

to unify Polygon with a new empty GPC polygon. Add an external contour using

    gpc_polygon_add_contour(Polygon, external([vertex(0, 0), vertex(1, 1)]))

Ignore the exact vertices; it's just an example. Then add a hole using

    gpc_polygon_add_contour(Polygon, hole([vertex(2, 2), vertex(3, 3)]))

Unify the polygon's contours non-deterministically using

    gpc_polygon_contour(Polygon, Contour)

Intersect two polygons using

    gpc_polygon_clip(int, Subject, Clip, Result)

Where the operation is one of: =diff=, =int=, =xor=, =union=.

@author Roy Ratcliffe <royratcliffe@me.com>

*/

%!  gpc_version(-Version) is det.
%
%   Version is the GPC version number, a colon-compound of major and
%   minor version integers.

%!  gpc_empty_polygon(-Polygon) is det.
%
%   Unifies Polygon with an empty polygon having no contours, no holes,
%   and consequently no vertices.

%!  gpc_polygon_num_contours(+Polygon, -NumContours:integer) is det.
%
%   NumContours unifies with the number of polygon contours, including
%   holes.

%!  gpc_polygon_add_contour(+Polygon, +Contour) is det.
%
%   Adds a new Contour to Polygon. Each contour is a list of vertex(X,
%   Y) compounds describing either an external contour or a hole.
%
%   External contours *must* wind clockwise.

%!  gpc_polygon(+Contours:list(compound), +Polygon) is det.
%
%   Builds a Polygon from a given a list of Contours.

gpc_polygon(Contours, Polygon) :-
    gpc_empty_polygon(Polygon),
    forall(member(Contour, Contours), gpc_polygon_add_contour(Polygon, Contour)).

%!  gpc_polygon_contour(+Polygon, -Contour) is nondet.
%
%   Unifies one-by-one with contours in the polygon. Each contour is a
%   compound whose functor indicates =external= or =hole=.
%
%   Fails if the polygon has no contours.

%!  gpc_polygon_clip(+Op:atom, +Subject, +Clip, -Result) is det.
%
%   Clips the Subject contours against the Clip contours, unifying the
%   resulting contours at Result polygon.

%!  gpc_read_polygon(Spec, Polygon, Options) is semidet.
%
%   Reads Polygon from a file Spec. Replaces the foreign implementation.

gpc_read_polygon(Spec, Polygon, Options) :-
  read_file_to_codes(Spec, Codes, Options),
  gpc_polygon_codes(Polygon, Codes).

%!  gpc_polygon_codes(+Polygon, -Codes) is det.
%!  gpc_polygon_codes(-Polygon, +Codes) is semidet.
%
%   The clipper conventionally serialises polygons as a series of
%   whitespace-delimited integer and floating-point numbers. The first
%   number is the number of contours, an integer. This encoding appears
%   in GPF (generic polygon) files.
%
%   There is one slight complication: hole serialisation is optional.
%   Defaults to external contour. Applies a definite-clause grammar to
%   the Polygon or the Codes, generating or parsing appropriately. The
%   grammar is flexible enough to transform contours either with or
%   without a hole flag, but always generates a serialisation with the
%   hole flag indicating external contour or hole.

gpc_polygon_codes(Polygon, Codes) :-
    var(Polygon),
    !,
    phrase(gpf(Contours), Codes),
    gpc_polygon(Contours, Polygon).
gpc_polygon_codes(Polygon, Codes) :-
    findall(Contour, gpc_polygon_contour(Polygon, Contour), Contours),
    phrase(gpf(Contours), Codes).

gpf(Contours) -->
    {   var(Contours)
    },
    !,
    blanks,
    integer(NumContours),
    contours(Contours),
    blanks,
    {   length(Contours, NumContours)
    }.
gpf(Contours) -->
    {   length(Contours, NumContours)
    },
    integer(NumContours),
    nl,
    contours(Contours).

contours([Contour|Contours]) -->
    contour(Contour),
    !,
    contours(Contours).
contours([]) -->
    [].

contour(Contour) -->
    {   var(Contour)
    },
    !,
    blanks,
    integer(NumVertices),
    external_or_hole(NumVertices, Contour).
contour(external(Vertices)) -->
    !,
    {   length(Vertices, NumVertices)
    },
    integer(NumVertices),
    nl,
    integer(0),
    nl,
    vertices(NumVertices, Vertices).
contour(hole(Vertices)) -->
    {   length(Vertices, NumVertices)
    },
    integer(NumVertices),
    nl,
    integer(1),
    nl,
    vertices(NumVertices, Vertices).

external_or_hole(NumVertices, external(Vertices)) -->
    blanks,
    integer(0),
    blank,
    !,
    vertices(NumVertices, Vertices).
external_or_hole(NumVertices, hole(Vertices)) -->
    blanks,
    integer(1),
    blank,
    !,
    vertices(NumVertices, Vertices),
    {   length(Vertices, NumVertices)
    }.
external_or_hole(NumVertices, external(Vertices)) -->
    vertices(NumVertices, Vertices),
    {   length(Vertices, NumVertices)
    }.

vertices(0, []) -->
    !,
    [].
vertices(NumVertices0, [Vertex|Vertices]) -->
    vertex(Vertex),
    {   NumVertices is NumVertices0 - 1
    },
    vertices(NumVertices, Vertices).

vertex(Vertex) -->
    {   var(Vertex)
    },
    !,
    blanks,
    number(X),
    blanks,
    number(Y),
    {   Vertex = vertex(X, Y)
    }.
vertex(vertex(X, Y)) -->
    number(X),
    " ",
    number(Y),
    nl.

nl -->
    "\r\n",
    !.
nl -->
    "\n".
