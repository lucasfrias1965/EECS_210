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
*> calculates the shortest path from one vertex to another
*> vertex, using that algorithm on 10.6 on page 54 on the pdf
*> this lab made me go coocoo bananas


identification division.

program-id. hello.

data division.

working-storage section.

    *> these are to help get input
    01 input-line pic x(25).
    01 temp-string      pic x(25).
    01 string-i         pic 9(05) value 1.
    01 matrix-index     pic 9(04) value 1.
    01 lines-left       pic 9(03) value zero.
  
    *> this is just n of our nxn and then the n^2 value
    01 n-count          pic 9(02) value zero.
    01 n-squared        pic 9(04) value zero.     

    *> these vars are just for printing
    01 row-idx          pic 9(02) value 1.
    01 col-idx          pic 9(02) value 1.
    01 disp-cell        pic z(03)9.
    01 idx              pic 9(03) value 0.
    
    *> make it n^2 for the whole length of our nxn (n^2) matrix
    01 user-matrix.
        05 cell         pic 9(04)
                        occurs 1 to 100 times
                        depending on n-squared.

   *> this is the seen list of our algorithm, which is N long
    01 seen-i           pic 9(02) value 1.
    01 seen-list.
        05 element-seen pic 1(01)
                        occurs 1 to 10 times
                        depending on n-count.


    01 weights-i        pic 9(02).
    01 weights-list.
        05 weight       pic 9(04)
                        occurs 1 to 100 times
                        depending on n-squared.

    01 min-dist          pic 9(04).
    01 min-index         pic 9(02).
    01 relax-i           pic 9(02).
    01 edge-weight       pic 9(04).

    01 start-letter     pic x(05).
    01 end-letter       pic x(05).
    01 start-index      pic 9(02).
    01 end-index        pic 9(02).

    *> these track the path taken through the algorithm
    01 predecessor-list.
        05 predecessor  pic 9(02)
                        occurs 1 to 10 times
                        depending on n-count.

    01 path-i            pic 9(02) value 0.
    01 path-string       pic x(60) value spaces.
    01 path-ptr          pic 9(03) value 1.
    01 disp-letter-code  pic 9(03).
    01 disp-letter       pic x(01).


procedure division.
    100-main. 
        display "input your matrix".
        perform 200-get-first-line.
        perform 400-get-rest-of-lines.
        perform 500-dih-algorithm. 

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
                    compute cell(matrix-index) = 9999
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
                        compute cell(matrix-index) = 9999
                    end-if
                    add 1 to matrix-index
                end-if

            end-perform
            *> this is our main perform loop 
            subtract 1 from lines-left    

            end-perform

            perform 490-get-start-n-end.

    490-get-start-n-end.
        display "Enter two distinct letters: "

        accept input-line.

        *> same old, split but without a loop

        unstring input-line delimited by all spaces into start-letter end-letter.

        *> here, we conver it to uppercase, then get the ordinality of the char and move it to the start index
        move function ord(function upper-case (start-letter)) to start-index.
        subtract 65 from start-index.
        display start-index.
        
        unstring input-line delimited by all spaces into temp-string.

        move function ord(function upper-case (end-letter)) to end-index.
        subtract 65 from end-index.

        display end-index.
        


    500-dih-algorithm.

        perform varying weights-i from 1 by 1 until weights-i > n-count
           compute weight(weights-i) = 9999
        end-perform.

        compute weight(start-index) = 0.

        perform varying seen-i from 1 by 1 until seen-i > n-count
            compute element-seen(seen-i) = 0
            compute predecessor(seen-i) = 0
        end-perform.
    
        *> keep going till we see this element

        perform until element-seen(end-index) = 1
            *> the min distance is inf, starting min-index is 0

            compute min-dist = 9999
            compute min-index = 0

            *> keep going through all weights, weight-i++ for all the stuff

            perform varying weights-i from 1 by 1 until weights-i > n-count

                *> okay so here if the element-seen at the current index is nothing
                *> which means we haven't seen this element, and also
                *> the weights is less then our min, chang it
                if element-seen(weights-i) = 0 and weight(weights-i) < min-dist
                    *> here we set the minimum distance to the weights
                    *> we're just trying to find the smallest canidate this iteration
                    compute min-dist = weight(weights-i)
                    compute min-index = weights-i
                end-if

            end-perform
            
            *> we have seen the next smallest node
            compute element-seen(min-index) = 1
            
            *> keep going through every element

            perform varying relax-i from 1 by 1 until relax-i > n-count
                *> if we haven't seen the element before
                if element-seen(relax-i) = 0

                    *> make our new index, which is the 
                    compute idx = ((min-index - 1) * n-count) + relax-i
                    
                    *> put the edge-weight to be the position of our cell
                    move cell(idx) to edge-weight

                    if edge-weight < 9999 and min-dist + edge-weight < weight(relax-i)
                        compute weight(relax-i) = min-dist + edge-weight
                        *> min-index got us here with a shorter path, so it's our predecessor
                        move min-index to predecessor(relax-i)
                    end-if

                end-if
            end-perform
        end-perform.

        display weight(end-index).

        perform 600-print-path.

    600-print-path.
        *> walk backwards from end to start, appending each letter
        *> to a string, then reverse the whole string at the end
        *> to get start-to-end order
        move spaces to path-string
        move 1 to path-ptr
        move end-index to path-i

        compute disp-letter-code = path-i + 65
        move function char(disp-letter-code) to disp-letter
        string disp-letter delimited by size
            into path-string
            with pointer path-ptr
        end-string
        move predecessor(path-i) to path-i

        perform until path-i = 0
            compute disp-letter-code = path-i + 65
            move function char(disp-letter-code) to disp-letter

            *> comma goes first here since the whole string gets
            *> reversed at the end, a single-char separator reverses
            *> cleanly, so it ends up between the right letters
            string "," disp-letter delimited by size
                into path-string
                with pointer path-ptr
            end-string

            move predecessor(path-i) to path-i
        end-perform.

        display function trim(function reverse(path-string)).

