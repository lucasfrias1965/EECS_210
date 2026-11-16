>> source format is free
*>    <@
*>    (ku//
*>      "
*> rock chalk
*> eecs 210 lab 10
*> by lucas frias
*> created nov 16th
*> dijkstra's algorithm
*> desc: given a closed matrix with non-negative edges,
*> calculates the shortest path from one vertx to all others


identification division.

program-id. hello.

data division.

working-storage section.
    01 first-line pic x(25).
    01 temp_string pic x(25).
    01 string_i 9(05) value 1.
    01 matrix-index 9(04) value zero.
  
    01 n-count pic 9(02) value zero.
    01 n-squared pic 9(04) value zero.     


    01 user-matrix.
        05 cell pic 9(04)
        occurs 1 to 100 times
        depending on n-count.
    
procedure division.
    100-main. 
        display "input your matrix".
        perform 200-get-first-line.
        
        stop run.

    200-get-first-line.
        *> get user input and feed into buffer
        accept first-line.
        move 1 to string_i.
        preform until string_i > 25
            move spaces to temp_string
            *> here, we split all values, like .split in python
            unstring first-line delimited by all spaces
                into temp-string
                with pointer string_i
            end-unstring

            if temp-string not = spaces
                add 1 to n-count
                compute cell(matrix-index) = function numval(temp-string)
                add 1 to matrix-index
            end-if
            
            end-perform.
    
