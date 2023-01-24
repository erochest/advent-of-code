
s" files.fth" included

: over-addr ( offset addr c -- offset addr c offset addr )
  \ duplicate an address and offset or count over another number on top of the stack.
  { a b c }
  a b c a b ;

: offset! ( n offset addr -- )
  \ Computer the memory location from the offset and store the number.
  swap cells + ! ;

: read-counter+ ( n x -- n x )
  \ read-input keeps a count of lines/items buried under the address of the array. This
  \ increments that.
  1 under+ ;

: line-buffer>n ( line-buffer-size -- n )
  \ Parses the contents of line-buffer for a single.
  line-buffer swap assert( s>number? ) d>s ;

: read-input ( addr n -- addr n )
  \ Takes the file name of the input file and returns the address and length of an array of the
  \ numbers in the input.
  ( right now, the address of the file name is returned as the address of the array. )
  open-input
  0              ( counter )
  here 1 allot   ( memory array )
  begin
    read-input-line
  end-of-file? invert while
    line-buffer>n
    over-addr offset!
    read-counter+
  repeat
  drop swap
  ;

: loop-range ( addr n -- addr n to from )
  \ this takes an address and count and adds the range for a do-loop to iterate over the addresses
  \ of the array.
  2dup cells over + swap ;

: find-pair-totalling ( addr n n-target -- n1 n2 )
  \ Takes the address and length of the array of input numbers and a target sum, and it returns
  \ the first two items in the array that total n-target.
  { target }
  loop-range u+do
    loop-range u+do
      i j <> if
        i @ j @ + target = if
          2drop
          i @ j @ unloop unloop exit
        then
      then
    1 cells +loop
  1 cells +loop
  ;

: find-triplet-totalling ( addr n n-target -- n1 n2 n3 )
  \ takes the address and length of the array of input numbers and a target number, and it returns
  \ the first three items in the array that total n-target.
  { target }
  loop-range u+do
    loop-range u+do
      i j <> if
        loop-range u+do
          i j <> i k <> and if
            i @ j @ k @ + + target = if
              2drop
              i @ j @ k @ unloop unloop unloop exit
            then
          then
        1 cells +loop
      then
    1 cells +loop
  1 cells +loop
  ;

: day01a ( addr n -- n )
  read-input
  2020 find-pair-totalling
  *
  ;
  
: day01b ( addr n --- n )
  read-input
  2020 find-triplet-totalling
  * *
  ;
