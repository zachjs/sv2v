`define DUMP(id) \
    $display(`"id: $bits(T) = %0d, $left(T) = %0d, $right(T) = %0d`", \
    $bits(T), $left(T), $right(T))

module top;
    parameter A = 1;
    parameter B = 2;
    parameter ONE = 1;
    typedef logic [A:B] T;
    initial `DUMP(X1);
    if (1) begin : blk
        localparam A = ONE * 3;
        localparam B = ONE * 4;
        initial `DUMP(X2);
        if (1) begin : nest
            typedef logic [A:B] T;
            initial `DUMP(Y0);
        end
    end
    initial begin
        localparam A = ONE * 5;
        localparam B = ONE * 6;
        `DUMP(X3);
        begin
            localparam type T = logic [A:B];
            `DUMP(Z0);
        end
    end
endmodule
