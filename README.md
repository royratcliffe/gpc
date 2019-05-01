# Generic Polygon Clipper

Alan Murta from Manchester wrote a very useful [generic two-dimensional polygon clipper][gpc]. Cheers Alan. Nice work. This is the Prolog wrapper for his GPC.

[gpc]:http://www.cs.man.ac.uk/~toby/alan/software/gpc.html

## To long, didn't read

Start with an empty polygon. Add contours. Call this the subject. Do the same again with different contours. Call this the clipping polygon. Clip the subject polygon against the other. The result can be a difference, intersection, exclusive-or or union.

In Prolog terms (pardon the pun) it works like this.

## Polygon types

Implements polygons as mutable BLOBs.

It was tempting to make a purely functional interface with no side effects. However, the underlying implementation maintains mutable polygon entities. Decision is to include pure functional interfaces using arity.

This makes some assumptions about threading.

## External and hole contours

The underlying interface uses a flag to indicate the difference in-between external or internal contours.

The implementation uses external and hole functors.

## Modifications to the GPC sources

There are no modifications, except to substitute standard memory allocation requests for Prolog allocations. The sources does not permit a redefinition. Consequently, it also includes the SWI-Prolog header.

## Work-in-progress

* Currently, at version 0.1.0, does not support triangle strips.
