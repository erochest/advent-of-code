! Copyright (C) 2022 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays assocs combinators hashtables
io.encodings.utf8 io.files kernel math math.parser math.ranges
namespaces sequences splitting
io prettyprint ;
IN: advent.year2021.day05

SYMBOL: diagonals
f diagonals set

TUPLE: point
    { x integer read-only }
    { y integer read-only } ;
C: <point> point

TUPLE: segment
    { from point read-only }
    { to point read-only }
    cache ;
: <segment> ( from to -- segment ) f segment boa ;

: debug-diagonal ( tag-str -- ) print " diagonals" print diagonals get . nl ;

: pick-ends ( seq -- first last ) [ first ] [ last ] bi ;
: 2array>point ( 2array -- point ) pick-ends <point> ;
: split-segment ( segment -- from to ) [ from>> ] [ to>> ] bi ;
: xs ( from to -- x1 x2 ) [ x>> ] [ x>> ] bi* ;
: ys ( from to -- y1 y2 ) [ y>> ] [ y>> ] bi* ;
: xs-same? ( from to -- ? ) xs = ;
: ys-same? ( from to -- ? ) ys = ;
: expand-ys ( from to -- y-range x ) 2dup ys [a,b] -rot xs drop ;
: expand-xs ( from to -- x-range y ) 2dup xs [a,b] -rot ys drop ;
: pair-x ( y-range x -- seq ) [ swap <point> ] curry map ;
: pair-y ( x-range y -- seq ) [ <point> ] curry map ;
: on-diagonal? ( from to -- ? )
    [ x>> swap x>> - abs ] [ y>> swap y>> - abs ] 2bi = ;
: expand-straight-lines ( segment -- seq )
    split-segment {
        { [ 2dup xs-same? ] [ expand-ys pair-x ] }
        { [ 2dup ys-same? ] [ expand-xs pair-y ] }
        { [ 2dup on-diagonal? diagonals get and ]
          [
            [ expand-xs drop ] [ expand-ys drop ] 2bi
            zip
            [ 2array>point ] map
        ] }
        [ 2drop { } clone ]
    } cond
    ;
: expanded>> ( segment -- array )
    dup cache>> [
        dup expand-straight-lines
        [ >>cache ] keep
    ] unless*
    nip ;

INSTANCE: segment sequence
M: segment length ( segment -- n )
    split-segment {
        { [ 2dup xs-same? ] [ ys - abs 1 + ] }
        { [ 2dup ys-same? ] [ xs - abs 1 + ] }
        {
            [ 2dup on-diagonal? diagonals get and ]
            [ x>> swap x>> - abs 1 + ]
        }

        [ 2drop 0 ]
    } cond ;
M: segment nth ( n segment -- elt ) expanded>> nth ;
M: segment set-nth ( elt n segment -- ) immutable ;

: strip-nl ( line -- line ) 10 swap remove ;
: parse-split-point ( pair -- point )
    "," split [ string>number ] map 2array>point ;
: parse-points ( x y -- x' y' ) [ parse-split-point ] bi@ ;
: parse-line-segment ( line -- segment )
    strip-nl " " split pick-ends parse-points <segment> ;
: read05 ( path -- lines ) utf8 file-lines [
        parse-line-segment
    ] map ;

: .point ( point -- )
    [ x>> pprint ] keep
    "," write
    y>> pprint ;
: .segment ( segment -- )
    [ from>> .point ] keep
    " -> " write
    [ to>> .point ] keep
    " " write
    expanded>> . ;

: frequencies ( seq hash-table -- )
    swap [ over inc-at ] each drop ;
: segment-frequencies ( segment-seq -- hash-table )
    H{ } clone swap [ over frequencies ] each ;
: parta ( path -- checksum )
    read05
    segment-frequencies
    >alist [ second 2 >= ] filter length ;

: with-diagonals ( quot -- )
    diagonals swap with-variable-on ; inline

: partb ( path -- checksum ) [ parta ] with-diagonals ;
