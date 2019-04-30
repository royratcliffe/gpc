:- module(gpc,
          [   gpc_version/1,
              gpc_empty_polygon/1,
              gpc_polygon_num_contours/2,
              gpc_polygon_add_contour/2,
              gpc_polygon_contour/2
          ]).

:- load_foreign_library(foreign(gpc)).

/** <module> gpc

## What is a polygon?

It is an aggregate of zero or more contours, each
comprising zero or more vertices. Each vertex has two double-precision
ordinates: x and y. Contours can be external, or holes.

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

%!  gpc_polygon_contour(+Polygon, -Contour) is nondet.
%
%   Unifies one-by-one with contours in the polygon. Each contour is a
%   compound whose functor indicates external or hole.
