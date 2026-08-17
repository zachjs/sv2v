// Enum item values scoped at the typedef, not at the call site
module top;
    localparam int BASE = 4;
    typedef enum logic [2:0] {
        P = BASE,
        Q,
        R,
        S
    } e_t;

    if (1) begin : blk
        localparam int BASE = 10;
        e_t x;
        initial begin
            x = P;
            $display("scoped_next=%0d", x.next());
        end
    end
endmodule
