! Copyright (C) 2022 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: advent2021.io grouping io.encodings.utf8 io.files kernel
math math.parser sequences ;
IN: advent2021.day01

: >pairs ( seq -- seq' ) 2 clump ;
: ascending? ( pair -- ? ) [ first ] [ second ] bi < ;
: count-t ( seq -- n ) sift length ;
: day01a ( path -- increasing-count )
    read-lines>numbers >pairs [ ascending? ] map count-t ;

: >windows ( seq -- seq ) 3 clump ;
: read>windows ( path -- seq ) read-lines>numbers >windows ;
: sum ( seq -- n ) [ 0 [ + ] reduce ] map ;
: day01b ( path -- increasing-window-count )
    read>windows sum >pairs [ ascending? ] map count-t ;
