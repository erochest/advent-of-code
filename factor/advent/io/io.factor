! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: ascii formatting io.encodings.utf8 io.files kernel math.parser 
       sequences splitting vocabs.loader io.pathnames ;
IN: advent.io

: (file-lines) ( path -- seq ) utf8 file-lines ;
: seq>numbers ( seq -- seq ) [ string>number ] map ;
: read-lines>numbers ( path -- seq ) (file-lines) seq>numbers ;
: (read-file-contents) ( path -- seq ) utf8 file-contents ;
: split-fields ( seq -- seq )
    ",\n\r" split [ empty? not ] filter ;
: read>numbers ( path -- seq )
    (read-file-contents) split-fields seq>numbers ;

: advent-subdirectory ( subdir-name -- path )
    "advent" find-vocab-root parent-directory 
    prepend-path ;

: fixture-basename ( day -- basename )
    "day%02d.fixture" sprintf ;
: fixture ( year day -- path )
    [ number>string ] [ fixture-basename ] bi*
    swap
    "sample" advent-subdirectory
    prepend-path prepend-path ;
: fixture-a-basename ( day -- basename )
    "day%02da.fixture" sprintf ;
: fixture-a ( year day -- path )
    [ number>string ] [ fixture-a-basename ] bi*
    swap
    "sample" advent-subdirectory
    prepend-path prepend-path ;
: data-basename ( day -- basename )
    "day%02d.data" sprintf ;
: data-file ( year day -- path )
    [ number>string ] [ data-basename ] bi*
    swap
    "data" advent-subdirectory
    prepend-path prepend-path ;

! TODO: move this into a package with a better name.
: trim-ws ( string -- string ) [ blank? ] trim ;
