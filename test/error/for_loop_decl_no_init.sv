// pattern: for loop declaration missing initialization
module top;
    initial
        for (integer x; x < 3; x = x + 1)
            ;
endmodule
