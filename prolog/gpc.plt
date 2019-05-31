:- begin_tests(gpc).

:- use_module(library(plunit)).

:- use_module(gpc).

:- public
    test/1,
    test/2.

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

%   Add an empty contour. The number of contours increases from zero to
%   one.

test(add_contour) :-
    gpc_empty_polygon(Polygon),
    gpc_polygon_num_contours(Polygon, 0),
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

test(contour, fail) :-
    gpc_empty_polygon(Polygon),
    gpc_polygon_contour(Polygon, _).

%   The union of two empty polygons clips to another empty polygon.

test(clip) :-
    gpc_empty_polygon(Subject),
    gpc_empty_polygon(Clip),
    gpc_polygon_clip(union, Subject, Clip, Result),
    gpc_polygon_num_contours(Result, 0).

test(clip_int,
     Contour == external([   vertex(1.0, 0.5),
                             vertex(0.5, 0.5),
                             vertex(0.5, 1.0),
                             vertex(1.0, 1.0)
                         ])) :-
    gpc_empty_polygon(Subject),
    gpc_empty_polygon(Clip),
    gpc_polygon_add_contour(Subject,
                            external([   vertex(0.5, 0.5),
                                         vertex(0.5, 1.5),
                                         vertex(1.5, 1.5),
                                         vertex(1.5, 0.5)
                                     ])),
    gpc_polygon_add_contour(Clip,
                            external([   vertex(0, 0),
                                         vertex(0, 1),
                                         vertex(1, 1),
                                         vertex(1, 0)
                                     ])),
    gpc_polygon_clip(int, Subject, Clip, Result),
    gpc_polygon_contour(Result, Contour).

%   Union with empty just re-orders the vertices. The contour remains
%   unchanged.

test(clip_union,
     Contour == external([   vertex(1.0, 0.0),
                             vertex(0.0, 0.0),
                             vertex(0.0, 1.0),
                             vertex(1.0, 1.0)
                         ])) :-
    gpc_empty_polygon(Subject),
    gpc_polygon_add_contour(Subject,
                            external([   vertex(0, 0),
                                         vertex(0, 1),
                                         vertex(1, 1),
                                         vertex(1, 0)
                                     ])),
    gpc_empty_polygon(Clip),
    gpc_polygon_clip(union, Subject, Clip, Result),
    gpc_polygon_num_contours(Result, 1),
    gpc_polygon_contour(Result, Contour).

test(tristrip, NumStrips == 0) :-
    gpc_empty_polygon(Polygon),
    gpc_polygon_to_tristrip(Polygon, Tristrip),
    gpc_tristrip_num_strips(Tristrip, NumStrips).

polygon(Polygon) :-
    maplist([X-Y, vertex(X, Y)]>>true, [0-0, 0-1, 1-1, 1-0], Vertices0),
    gpc_polygon([external(Vertices0)], Polygon).

test(polygon_to_tristrip,
     Vertices == [   vertex(0.0, 0.0),
                     vertex(1.0, 0.0),
                     vertex(0.0, 1.0),
                     vertex(1.0, 1.0)
                 ]) :-
    polygon(Polygon),
    gpc_polygon_num_contours(Polygon, 1),
    gpc_polygon_to_tristrip(Polygon, Tristrip),
    gpc_tristrip_vertices(Tristrip, Vertices).

test(tristrip_area, Area == 1.0) :-
    polygon(Polygon),
    gpc_polygon_to_tristrip(Polygon, Tristrip),
    gpc_tristrip_area(Tristrip, Area).

:- end_tests(gpc).
