:- begin_tests(gpc).

:- use_module(gpc).

test(version) :-
    gpc_version(2:32).

%   Note that two new empty polygons fail to unify. They are not the
%   same mutable entities.

test(empty_polygon, fail) :-
    gpc_empty_polygon(Polygon),
    gpc_empty_polygon(Polygon).

test(num_contours) :-
    gpc_empty_polygon(Polygon),
    gpc_polygon_num_contours(Polygon, 0).

%   Adds an empty contour. The number of contours increases from zero to
%   one.

test(add_contour) :-
    gpc_empty_polygon(Polygon),
    gpc_polygon_add_contour(Polygon, []),
    gpc_polygon_num_contours(Polygon, 1).

:- end_tests(gpc).
