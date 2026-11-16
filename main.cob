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

    01 input-line pic x(25).
    01 temp-string      pic x(25).
    01 string-i         pic 9(05) value 1.
    01 matrix-index     pic 9(04) value 1.
    01 lines-left       pic 9(03) value zero.
  
    01 n-count          pic 9(02) value zero.
    01 n-squared        pic 9(04) value zero.     


    01 row-idx          pic 9(02) value 1.
    01 col-idx          pic 9(02) value 1.
    01 disp-cell        pic z(03)9.
    01 idx              pic 9(03) value 0.

    *> make it n^2 for the whole length of our nxn (n^2) matrix
    01 user-matrix.
        05 cell         pic 9(04)
                        occurs 1 to 100 times
                        depending on n-squared.

    
procedure division.
    100-main. 
        display "input your matrix".
        perform 200-get-first-line.
        perform 400-get-rest-of-lines.
        
        stop run.

    200-get-first-line.
        *> get user input and feed into buffer
        accept input-line.
        move 1 to string-i.
        move 0 to n-count.
        perform until string-i > 25
            move spaces to temp-string 
            *> here, we split all values, like .split in python
            *> cobol implicitly increments string_i the required amount

            unstring input-line delimited by all spaces
                into temp-string
                with pointer string-i 
            end-unstring
        
            *> if we don't have a string that's all spaces
            if temp-string not = spaces
                *> increment our n, compute what value the matrix_index should be
                add 1 to n-count
                compute n-squared = n-count * n-count
                if temp-string not = "*"
                    compute cell(matrix-index) = function numval(temp-string)
                else
                    compute cell(matrix-index) = 99
                end-if
                add 1 to matrix-index
            end-if
            
            end-perform.

        *> n-count is lines-left minus 1
        compute lines-left = n-count.
        subtract 1 from lines-left.

        perform 300-print-matrix.

    300-print-matrix.
        perform varying row-idx from 1 by 1 until row-idx > n-count
            perform varying col-idx from 1 by 1 until col-idx > n-count
                compute idx = ((row-idx - 1) * n-count) + col-idx
                move cell(idx) to disp-cell
                display disp-cell " " with no advancing
            end-perform
            display " "
        end-perform.

    400-get-rest-of-lines.
        *> now we calculate rest, based on the n-1 size left
        perform until lines-left = 0

            accept input-line
            move 1 to string-i
            perform until string-i > 25
                move spaces to temp-string 
                *> here, we split all values, like .split in python
                *> cobol implicitly increments string_i the required amount

                unstring input-line delimited by all spaces
                    into temp-string
                    with pointer string-i 
                end-unstring
            
                *> if we don't have a string that's all spaces
                if temp-string not = spaces
                    *> increment our n, compute what value the matrix_index should be
                    if temp-string not = "*"
                        compute cell(matrix-index) = function numval(temp-string)
                    else
                        compute cell(matrix-index) = 99
                    end-if
                    add 1 to matrix-index
                end-if

            end-perform
            *> this is our main perform loop 
            subtract 1 from lines-left    

            end-perform

        perform 300-print-matrix.
