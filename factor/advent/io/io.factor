! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: io.encodings.utf8 io.files kernel math.parser sequences
splitting vocabs.loader io.pathnames ;
IN: advent.io

: (file-lines) ( path -- seq ) utf8 file-lines ;
: seq>numbers ( seq -- seq ) [ string>number ] map ;
: read-lines>numbers ( path -- seq ) (file-lines) seq>numbers ;
: (read-file-contents) ( path -- seq ) utf8 file-contents ;
: split-fields ( seq -- seq )
    ",\n\r" split [ empty? not ] filter ;
: read>numbers ( path -- seq )
    (read-file-contents) split-fields seq>numbers ;

: fixture-basename ( day -- basename )
    number>string "day" swap append ".fixture" append ;
: fixture ( year day -- path )
    "advent" find-vocab-root parent-directory
    "sample" append-path
    rot number>string append-path
    swap fixture-basename append-path ;

: data-basename ( day -- basename )
    number>string "day" swap append ".data" append ;
: data-file ( year day -- path )
    "advent" find-vocab-root parent-directory
    "data" append-path
    rot number>string append-path
    swap data-basename append-path ;