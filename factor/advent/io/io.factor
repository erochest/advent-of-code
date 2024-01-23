! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: ascii formatting io.encodings.utf8 io io.files kernel
       math.parser prettyprint namespaces sequences splitting
       vocabs.loader io.pathnames ui.clipboards ;
IN: advent.io

! LOGGING

SYMBOLS: debug-logging ;
f debug-logging set

: with-logging ( path quot -- )
    utf8 swap
    t debug-logging rot
    [ with-variable ] 3curry
    with-file-writer ; inline

: with-output-logging ( quot -- )
    t debug-logging rot with-variable ; inline

: when-logging ( quot -- )
    debug-logging get swap when ; inline

: trace ( name -- )
    [ write nl .s nl ] curry when-logging ;

! FILE READING AND PARSING

: (file-lines) ( path -- seq ) utf8 file-lines ;
: seq>numbers ( seq -- seq ) [ string>number ] map ;
: read-lines>numbers ( path -- seq ) (file-lines) seq>numbers ;
: (read-file-contents) ( path -- seq ) utf8 file-contents ;
: split-fields ( seq -- seq )
    ",\n\r" split [ empty? not ] filter ;
: read>numbers ( path -- seq )
    (read-file-contents) split-fields seq>numbers ;

! FILE LOCATIONS

: advent-subdirectory ( subdir-name -- path )
    "advent" find-vocab-root parent-directory 
    prepend-path ;

: in-fixture-dir ( basename year -- path )
    "sample" advent-subdirectory
    prepend-path prepend-path ;
: fixture-basename ( day -- basename )
    "day%02d.fixture" sprintf ;
: fixture ( year day -- path )
    [ number>string ] [ fixture-basename ] bi*
    swap in-fixture-dir ;
: fixture-a-basename ( day -- basename )
    "day%02da.fixture" sprintf ;
: fixture-a ( year day -- path )
    [ number>string ] [ fixture-a-basename ] bi*
    swap in-fixture-dir ;
: fixture+-basename ( day n -- basename )
    "day%02d-%d.fixture" sprintf ;
: fixture+ ( year day n -- path )
    [ number>string ] 2dip fixture+-basename
    swap in-fixture-dir ;

: data-basename ( day -- basename )
    "day%02d.data" sprintf ;
: data-file ( year day -- path )
    [ number>string ] [ data-basename ] bi*
    swap
    "data" advent-subdirectory
    prepend-path prepend-path ;

! PARSING HELPERS

! TODO: move this into a package with a better name.
: trim-ws ( string -- string ) [ blank? ] trim ;

: split-words ( string -- array ) 
    " " split [ empty? not ] filter ;

: split-numbers ( string -- array )
    split-words [ string>number ] map ;

! CLIPBOARD
: fixture-from-clipboard ( year day n -- path )
    fixture+ dup
    dup .
    utf8 [
        clipboard get clipboard-contents print
    ] with-file-writer ;
