`define DUMP(id, A, B) \
    $display(`"id: $bits(T) = %0d, $left(T) = %0d, $right(T) = %0d`", \
    A >= B ? A - B + 1: B - A + 1, A, B)

module top;
    parameter A = 1;
    parameter B = 2;
    parameter ONE = 1;
    initial `DUMP(X1, A, B);
    if (1) begin : blk
        localparam _A = ONE * 3;
        localparam _B = ONE * 4;
        initial `DUMP(X2, A, B);
        if (1) begin : nest
            initial `DUMP(Y0, _A, _B);
        end
    end
    initial begin : foo
        localparam _A = ONE * 5;
        localparam _B = ONE * 6;
        `DUMP(X3, A, B);
        `DUMP(Z0, _A, _B);
    end
endmodule
