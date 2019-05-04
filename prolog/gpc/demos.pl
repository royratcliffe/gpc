:- module(gpc_demos, [britain_arrows/1]).

:- use_module(library(gpc)).
:- use_module(library(autowin)).

:- public
    britain_arrows/1.

%!  britain_arrows(+Op) is det.
%
%   Britain has two external contours: one for the mainland, England
%   and Wales, the other for Ireland.
%
%   The default coordinate transformation needs reversing. X and Y
%   increase downwards and to the right.

britain_arrows(Op) :-
    read_polygon(britain, Subject),
    read_polygon(arrows, Clip),
    gpc_polygon_clip(Op, Subject, Clip, Result),
    new(Picture, auto_sized_picture(Op)),
    display_polygon(Picture, Result),
    send(Picture, open).

display_polygon(Picture, Polygon) :-
    forall(gpc_polygon_contour(Polygon, Contour),
           display_contour(Picture, Contour)).

display_contour(Picture, Contour) :-
    Contour =.. [Hole, Vertices],
    hole_colour(Hole, Colour),
    new(Path, path),
    send(Path, fill_pattern, colour(Colour)),
    forall(member(Vertex, Vertices), display_vertex(Path, Vertex)),
    send(Picture, display, Path).

hole_colour(external, orange).
hole_colour(hole, pink).

display_vertex(Path, vertex(X0, Y0)) :-
    X is X0 - 0,
    Y is 0 - Y0,
    send(Path, append, point(X, Y)).

read_polygon(Spec, Polygon) :-
    module_property(gpc_demos, file(File)),
    gpc_read_polygon(Spec, Polygon, [relative_to(File), extensions([gpf])]).
