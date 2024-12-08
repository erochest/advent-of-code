! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: kernel math namespaces sequences ;
IN: advent.map

: mod-y ( y-x quot: ( ... y -- ... y' ) -- y-x' )
    [ dup first ] dip call( y -- y ) over set-first ;
: mod-x ( y-x quot: ( ... x -- ... x' ) -- y-x' )
    [ dup second ] dip call( x -- x ) over set-second ;

: up ( y-x -- y-x' ) [ 1 - ] mod-y ;
: down ( y-x -- y-x' ) [ 1 + ] mod-y ;
: left ( y-x -- y-x' ) [ 1 - ] mod-x ;
: right ( y-x -- y-x' ) [ 1 + ] mod-x ;
: up-left ( y-x -- y-x' ) up left ;
: up-right ( y-x -- y-x' ) up right ;
: down-left ( y-x -- y-x' ) down left ;
: down-right ( y-x -- y-x' ) down right ;

: map-move ( pos dir -- a )
    call( x -- x ) ;

: char-at ( y-x grid -- char )
    [ first2 swap ] dip nth nth ;
: char-at? ( y-x grid -- char/f )
    [ first2 swap ] dip ?nth ?nth ;
