! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays assocs kernel math sequences sorting
       splitting ;
IN: advent.y2024.day01

: read-data ( path -- seqs ) (file-lines) [ split-numbers ] map ;

: sort-data ( seqs -- sorted-seqs )
    [ [ first ] map sort ] [ [ second ] map sort ] bi 2array ;

: compute-distances ( seq -- seq )
    [ first ] [ second ] bi
    [ - abs ] 2map ;

: find-site-distances ( seqs -- checksum )
    sort-data compute-distances sum ;
