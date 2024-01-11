! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io grouping kernel math math.parser sequences
       vectors ;
IN: advent.y2023.day09

: parse-readings ( str-array -- array )
    [
        split-words [ string>number ] map
    ] map ;

: read-readings ( path -- array ) (file-lines) parse-readings ;

! This returns the difference between each pair of numbers in
! an increasing sequence.
: deltas ( ary -- ary ) 2 <clumps> [ first2 swap - ] map ;

: all-zeros? ( seq -- ? ) [ zero? ] all? ;

! This extrapolates the next value of a sequencen by adding the
! derivative to the last item, unless the sequences is all
! zeros, in which case the next value will also be 0.
: (extrapolate) ( derivative seq -- next-value )
    dup all-zeros? [ 2drop 0 ] [ last + ] if ;

: (prepolate) ( derivative seq -- prev-value )
    dup all-zeros? [ 2drop 0 ] [ first swap - ] if ;

! This takes a sequence and derives its deltas until they're
! all zeros, and then it returns a vector containing all of
! derivatives.
: (derive-to-zero) ( seq -- vec )
    1vector
    [ dup last all-zeros? ] [
        dup last
        deltas
        suffix
    ] until ;

! This extrapolates the next value of a sequence by getting the 
! derivatives/deltas until they are all zero, and then building
! backward to figure the next value using (extrapolate).
: extrapolate ( seq -- next-value )
    (derive-to-zero) <reversed>
    f [ (extrapolate) ] reduce ;

: prepolate ( seq -- next-value )
    (derive-to-zero) <reversed>
    f [ (prepolate) ] reduce ;

: sum-extrapolations ( seq -- sum ) [ extrapolate ] map-sum ;
: sum-prepolations ( seq -- sum ) [ prepolate ] map-sum ;
