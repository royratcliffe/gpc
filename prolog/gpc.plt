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
    gpc_polygon_add_contour(Polygon, external([])),
    gpc_polygon_num_contours(Polygon, 1).

test(add_contour,
     all(Contours == [   external([vertex(0.0,0.0),vertex(1.0,1.0)]),
                         hole([vertex(2.0,2.0),vertex(3.0,3.0)])
                     ])) :-
    gpc_empty_polygon(Polygon),
    gpc_polygon_add_contour(Polygon, external([vertex(0, 0), vertex(1, 1)])),
    gpc_polygon_add_contour(Polygon, hole([vertex(2, 2), vertex(3, 3)])),
    gpc_polygon_contour(Polygon, Contours).

:- end_tests(gpc).
