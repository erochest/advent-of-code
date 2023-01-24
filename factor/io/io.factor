! Copyright (C) 2022 Eric Rochester.
! See http://factorcode.org/license.txt for BSD license.
USING: io.encodings.utf8 io.files kernel math.parser sequences
splitting ;
IN: advent2021.io

: (file-lines) ( path -- seq ) utf8 file-lines ;
: seq>numbers ( seq -- seq ) [ string>number ] map ;
: read-lines>numbers ( path -- seq ) (file-lines) seq>numbers ;
: (read-file-contents) ( path -- seq ) utf8 file-contents ;
: split-fields ( seq -- seq )
    ",\n\r" split [ empty? not ] filter ;
: read>numbers ( path -- seq )
    (read-file-contents) split-fields seq>numbers ;
