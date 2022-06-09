val maxMemSize = 10000;
val maxint = valOf(Int.maxInt);

local
        exception divByZero;
        exception indexOutOfBound;
        exception intergerOverflow;

        val arr = Array.array(maxMemSize, 0);

        fun prin w = print("input: \n");
        fun prinval w = print(Int.toString(w)^"\n");

        fun operation0 (n2, n3, n4, i) = 0

        fun operation1 (n2, n3, n4, i) =
            let 
                val wef = prin(3)
                val t = valOf(TextIO.inputLine TextIO.stdIn)
                val t1 = Int.fromString t
                val x1 = valOf(t1)
                val temp1 = Array.update(arr, n4, x1)
            in 
                if n4 < 0 orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation2 (n2, n3, n4, i) =
            let 
                val t = Array.sub(arr, n2)
                val temp1 = Array.update(arr, n4, t)
            in
                if n4 < 0 orelse n2 < 0 orelse n4 > maxMemSize orelse n2 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation3 (n2, n3, n4, i) =      (*considering 1 as true and 0 as false*)
            let 
                val t = Array.sub(arr, n2)
                val x = if t = 1 then 0
                        else 1
                val temp1 = Array.update(arr, n4, x)
            in
                if n4 < 0 orelse n2 < 0 orelse n4 > maxMemSize orelse n2 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation4 (n2, n3, n4, i) =
            let 
                val t1 = Array.sub(arr, n2)
                val t2 = Array.sub(arr, n3)
                val x = if t1 = 1 orelse t2 = 1 then 1
                        else 0
                val temp1 = Array.update(arr, n4, x)
            in
                if n4 < 0 orelse n2 < 0 orelse n3 < 0 orelse n2 > maxMemSize orelse n3 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation5 (n2, n3, n4, i) =
            let 
                val t1 = Array.sub(arr, n2)
                val t2 = Array.sub(arr, n3)
                val x = if t1 = 1 andalso t2 = 1 then 1
                        else 0
                val temp1 = Array.update(arr, n4, x)
            in
                if n4 < 0 orelse n2 < 0 orelse n3 < 0 orelse n2 > maxMemSize orelse n3 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end;

        fun operation6 (n2, n3, n4, i) =
            let 
                val t1 = Array.sub(arr, n2)
                val t2 = Array.sub(arr, n3)
                val x = t1+t2
                val temp1 = Array.update(arr, n4, x)
            in
                if x > maxint then raise intergerOverflow
                else if n4 < 0 orelse n2 < 0 orelse n3 < 0 orelse n2 > maxMemSize orelse n3 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation7 (n2, n3, n4, i) =
            let 
                val t1 = Array.sub(arr, n2)
                val t2 = Array.sub(arr, n3)
                val x = t1-t2
                val temp1 = Array.update(arr, n4, x)
            in
                if x < (~1*maxint) then raise intergerOverflow
                else if n4 < 0 orelse n2 < 0 orelse n3 < 0 orelse n2 > maxMemSize orelse n3 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation8 (n2, n3, n4, i) =
            let 
                val t1 = Array.sub(arr, n2)
                val t2 = Array.sub(arr, n3)
                val x = t1*t2
                val temp1 = Array.update(arr, n4, x)
            in
                if x > maxint orelse x < (~1*maxint) then raise intergerOverflow
                else if n4 < 0 orelse n2 < 0 orelse n3 < 0 orelse n2 > maxMemSize orelse n3 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation9 (n2, n3, n4, i) =
            let 
                val t1 = Array.sub(arr, n2)
                val t2 = Array.sub(arr, n3)
                val x = t1 div t2
                val temp1 = Array.update(arr, n4, x)
            in 
                if t2 = 0 then raise divByZero
                else if n4 < 0 orelse n2 < 0 orelse n3 < 0 orelse n2 > maxMemSize orelse n3 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation10 (n2, n3, n4, i) =
            let 
                val t1 = Array.sub(arr, n2)
                val t2 = Array.sub(arr, n3)
                val x = t1 mod t2
                val temp1 = Array.update(arr, n4, x)
            in 
                if t2 = 0 then raise divByZero
                else if n4 < 0 orelse n2 < 0 orelse n3 < 0 orelse n2 > maxMemSize orelse n3 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation11 (n2, n3, n4, i) =
            let 
                val t1 = Array.sub(arr, n2)
                val t2 = Array.sub(arr, n3)
                val x = if t1 = t2 then 1 else 0
                val temp1 = Array.update(arr, n4, x)
            in
                if n4 < 0 orelse n2 < 0 orelse n3 < 0 orelse n2 > maxMemSize orelse n3 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end
        
        fun operation12 (n2, n3, n4, i) =
            let 
                val t1 = Array.sub(arr, n2)
                val t2 = Array.sub(arr, n3)
                val x = if t1 > t2 then 1 else 0
                val temp1 = Array.update(arr, n4, x)
            in
                if n4 < 0 orelse n2 < 0 orelse n3 < 0 orelse n2 > maxMemSize orelse n3 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun operation13 (n2, n3, n4, i) =
            let 
                val x = Array.sub(arr, n2)
            in  
                if n4 < 0 orelse n2 < 0 orelse n2 > maxMemSize orelse n4 > maxMemSize then raise indexOutOfBound
                else if x=1 then n4
                else i+1
            end

        fun operation14 (n2, n3, n4, i) = 
            if n4 < 0 orelse n4 > maxMemSize then raise indexOutOfBound
            else n4

        fun operation15 (n2, n3, n4, i) =
            let 
                val x = if n2 < 0  orelse n2 > maxMemSize then raise indexOutOfBound
                        else Array.sub(arr, n2)
                val x1 = prinval(x)
            in i+1
            end
        
        fun operation16 (n2, n3, n4, i) =
            let 
                val x = n2
                val temp1 = Array.update(arr, n4, x)
            in
                if x > maxint then raise intergerOverflow
                else if n4 < 0 orelse n4 > maxMemSize then raise indexOutOfBound
                else i+1
            end

        fun nextcomma (a, i) =       
            if String.compare(substring(a, i, 1), ",") = EQUAL
            then i
            else nextcomma (a, i+1)
        
        fun abcd (ithcommand, y) = 
            let 
                val len = List.length(y)
                val h = List.nth(y, ithcommand)                              (*h will store ith  quadruple*)
                val c1 = nextcomma(h, 1)
                val c2 = nextcomma(h, c1+1)
                val c3 = nextcomma(h, c2+1)
                val l = String.size (h) 
                val n1 = valOf(Int.fromString(substring(h, 1, (c1-1))))      (*n1 will store 1st integer of  quadruple similarly for n2, n3, n4*)
                val n2 = valOf(Int.fromString(substring(h, c1+1, (c2-c1-1))))
                val n3 = valOf(Int.fromString(substring(h, c2+1, (c3-c2-1))))
                val n4 = valOf(Int.fromString(substring(h, c3+1, (l-c3-2))))
                val next =  if n1 = 0 then operation0(n2, n3, n4, ithcommand)        (*next will store the next line of code to be executed*)
                            else if n1 = 1 then  operation1(n2, n3, n4, ithcommand)
                            else if n1 = 2 then  operation2(n2, n3, n4, ithcommand)
                            else if n1 = 3 then  operation3(n2, n3, n4, ithcommand)
                            else if n1 = 4 then  operation4(n2, n3, n4, ithcommand)
                            else if n1 = 5 then  operation5(n2, n3, n4, ithcommand)
                            else if n1 = 6 then  operation6(n2, n3, n4, ithcommand)
                            else if n1 = 7 then  operation7(n2, n3, n4, ithcommand)
                            else if n1 = 8 then  operation8(n2, n3, n4, ithcommand)
                            else if n1 = 9 then  operation9(n2, n3, n4, ithcommand)
                            else if n1 = 10 then  operation10(n2, n3, n4, ithcommand)
                            else if n1 = 11 then  operation11(n2, n3, n4, ithcommand)
                            else if n1 = 12 then  operation12(n2, n3, n4, ithcommand)
                            else if n1 = 13 then  operation13(n2, n3, n4, ithcommand)
                            else if n1 = 14 then  operation14(n2, n3, n4, ithcommand)
                            else if n1 = 15 then  operation15(n2, n3, n4, ithcommand)
                            else operation16(n2, n3, n4, ithcommand)
            in
                if n1 = 0 then "halt" 
                else if ithcommand = len-1 then ""
                else abcd(next, y)
            end
in
        fun interpret filename = 
            let
                val instream = TextIO.openIn filename;
                val xfn = String.tokens Char.isSpace o TextIO.inputAll;
                val y = xfn instream;
            in
                abcd(0, y)
            end
end;