
: third ( n1 n2 n3 -- n1 n2 n3 n1 )
  \ Duplicates the third item on the stack onte the top.
  \ rot dup 2swap rot ;
  2 pick ;

: 3dup ( n1 n2 n3 -- n1 n2 n3 n1 n2 n3 )
  \ Duplicates the top three items on the stack.
  2 pick 2 pick 2 pick ;
