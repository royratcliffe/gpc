:- module(gpc,
          [   gpc_version/1,
              gpc_empty_polygon/1,
              gpc_polygon_num_contours/2,
              gpc_polygon_add_contour/2,
              gpc_polygon/2,
              gpc_polygon_contour/2,
              gpc_polygon_clip/4
          ]).

:- use_foreign_library(foreign(gpc)).

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
