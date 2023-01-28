! Copyright (C) 2022 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: advent.io grouping io.encodings.utf8 io.files kernel
math math.parser sequences ;
IN: advent.year2021.day01

: >pairs ( seq -- seq' ) 2 clump ;
: ascending? ( pair -- ? ) [ first ] [ second ] bi < ;
: count-t ( seq -- n ) sift length ;
: parta ( path -- increasing-count )
    read-lines>numbers >pairs [ ascending? ] map count-t ;

: >windows ( seq -- seq ) 3 clump ;
: read>windows ( path -- seq ) read-lines>numbers >windows ;
: sum ( seq -- n ) [ 0 [ + ] reduce ] map ;
: partb ( path -- increasing-window-count )
    read>windows sum >pairs [ ascending? ] map count-t ;
