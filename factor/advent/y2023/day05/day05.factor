! Copyright (C) 2023 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: accessors advent.io arrays assocs formatting grouping
       io kernel math math.order math.parser namespaces
       prettyprint ranges sequences sets splitting ;
FROM: advent.io => split-words ;
IN: advent.y2023.day05

SYMBOL: path
{
    "seed"
    "soil"
    "fertilizer"
    "water"
    "light"
    "temperature"
    "humidity"
    "location"
} path set-global

TUPLE: mapping-range
    { dest integer }
    { src integer }
    { extent integer }
    { range range } ;
: <mapping-range> ( dest src extent -- mapping-range )
    2dup dupd + [a..b)
    mapping-range boa ;
M: mapping-range in? ( elt mapping -- ? ) range>> in? ;
M: mapping-range at* ( key assoc -- value/f ? )
    2dup in? [
        ! x - s + d
        [ dest>> ] [ src>> ] bi - +
        t
    ] [
        2drop f f
    ] if ;

TUPLE: mapping { mapping-ranges array } ;
: <mapping> ( ranges -- mapping ) mapping boa ;
M: mapping at* ( key assoc -- value/f ? )
    dupd mapping-ranges>> assoc-stack [
        nip
    ] when*
    t ;

! parses "seeds: 1 2 3 4"
: parse-seeds ( line -- seeds )
    ":" split1
    [ "seeds" assert= ] dip
    split-numbers ;

! adds a parsed "dest src span" line to a hash table
! it adds all the mappings from src to src+span, mapped
! to dest to dest+span to the table
: add-range-mapping ( mappings dest-src-span-array -- mappings )
    [ [ first ] [ third ] bi dupd + [a..b) ] keep
    [ second ] [ third ] bi dupd + [a..b)
    zip
    swap [ first2 pick set-at ] reduce ;

! parses "FROM-to-TO:" into { FROM TO }
: parse-mapping-label ( string -- pair )
    split-words first2
    "map:" assert=
    "-" split 
    1 swap remove-nth ;

: parse-mapping ( lines -- label-pair mapping-array )
    [ first parse-mapping-label "processing" print dup . flush ]
    [
        rest-slice
        [ split-numbers first3 <mapping-range> ] map
        <mapping>
    ] bi
    ;

: mapping-at-pair ( mappings from-to from-value -- to-value )
    [
        ?of [ "no mapping for %s" sprintf throw ] unless
    ] dip
    ?of drop ;

! taking a mapping from (parse-input), this retrieves the value
! from the from-value. If there are no value mappings for
! from/source to to/destination, it throws an error.
: mapping-at ( mappings from to from-value -- to-value )
    [ 2array ] dip mapping-at-pair ;

: (get-deep-value) ( map-value key -- map-value )
    [ first2 ] dip swap
    [ dup ] 2dip
    mapping-at-pair
    2array
    ;

! this walks a chain of mappings to get the final value from
! the input array.
: get-deep-value
    ( mappings path-array start-value -- destination-value )
    [ "getting location for seed %d" printf nl ] keep
    [ 2 clump ] dip
    swapd 2array
    [ (get-deep-value) ] reduce
    second ;

! this walks over all of the seeds and gets the lowest deep
! value for the given path
: (get-min-deep-value) ( seeds mappings path -- n )
    rot 
    [
        [ get-deep-value ] 3keep drop rot
    ] map
    1000000 [ min ] reduce
    [ 2drop ] dip
    ;

: get-next-values ( seeds paragraph -- values )
    parse-mapping nip swap
    [ dupd ?of drop ] map
    nip ;

! entry-point for first part
: get-min-location ( data-path -- n )
    (file-lines)
    [ empty? ] split-when
    [ rest-slice ] [ first first parse-seeds ] bi
    [ get-next-values ] reduce
    1000000 [ min ] reduce ;

