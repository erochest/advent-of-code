! Copyright (C) 2022 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors advent2021.io arrays assocs io.encodings.utf8
io.files kernel math math.parser namespaces sequences 
splitting ;
IN: advent2021.day02

TUPLE: position distance depth ;

: <position> ( -- position ) 0 0 position boa ;

: forward ( position step -- position ) over distance>> + >>distance ;
: down ( position step -- position ) over depth>> + >>depth ;
: up ( position step -- position ) over depth>> swap - >>depth ;

SYMBOL: +direction-codes+
H{ { "forward" [ forward ] } { "down" [ down ] } { "up" [ up ] } } +direction-codes+ set-global
: parse-direction ( string -- callable ) +direction-codes+ get-global at ;
: parse-sub-command ( string -- closure )
    " " split [ second string>number ] [ first parse-direction ] bi curry ;
: exec-commands ( position command-seq -- position ) [ call( pos -- pos ) ] each ;
: dump-position ( position -- dist depth ) [ distance>> ] [ depth>> ] bi ;
: read02a ( path -- seq ) (file-lines) [ parse-sub-command ] map ;

: day02a ( path -- n ) read02a <position> swap exec-commands dump-position * ;

TUPLE: submarine distance depth aim ;

: <submarine> ( -- sub ) 0 0 0 submarine boa ;
: submarine->array ( sub -- array ) [ distance>> ] [ depth>> ] [ aim>> ] tri 3array ;

: down-sub ( sub amount -- sub ) over aim>> + >>aim ;
: up-sub ( sub amount -- sub ) over aim>> swap - >>aim ;
: forward-distance ( sub amount -- sub ) over distance>> + >>distance ;
: forward-delta ( sub amount -- sub ) over aim>> * over depth>> + >>depth ;
: forward-sub ( sub amount -- sub ) [ forward-distance ] keep forward-delta ;

SYMBOL: +sub-commands+
H{ { "forward" [ forward-sub ] } { "down" [ down-sub ] } { "up" [ up-sub ] } } +sub-commands+ set-global
: parse-sub ( str -- callable ) +sub-commands+ get-global at ;
: parse-line ( str -- callable ) " " split [ second string>number ] [ first parse-sub ] bi curry ;
: read02b ( path -- n ) (file-lines) [ parse-line ] map ;
: exec-sub ( sub seq -- sub ) [ call( sub -- sub ) ] each ;

: day02b ( path -- n ) read02b <submarine> swap exec-sub [ distance>> ] [ depth>> ] bi * ;
