typedef struct packed {
    byte x;
    shortint y;
} T;

interface intf;
    parameter T P = 0;
    wire [P.x-1:0] z = '1;
    initial begin
        $display("intf.P.x %0d", P.x);
        $display("intf.P.y %0d", P.y);
    end
    if (P.x) begin : blk
        wire [31:0] z;
    end
    wire [31:0] z = P.y;
    if (P.x) begin : blk2
        integer z;
        assign intf.blk.z = intf.z;
    end
    modport X (
        input .x(P),
        input .y(intf.blk.z)
    );
endinterface

module mod(
    intf.X f
);
    integer i;
    initial begin
        $display("mod.f.x %b", f.x);
        $display("mod.f.y %b", f.y);
    end
endmodule

module top;
    localparam T Q = '{ x: 1, y: 2 };
    localparam T R = '{ x: 4, y: 6 };
    intf #(Q) i();
    intf #(R) is [1:0] ();
    mod m(i);
    mod ms(is[0]);
endmodule
