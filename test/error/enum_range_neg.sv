// pattern: expected a non-negative number, but found 1'sd1
module top;
    enum {
        A[1'sd1]
    } x;
endmodule
