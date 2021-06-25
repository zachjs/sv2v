module top;
    typedef struct packed { integer y, z; } T;
    // TODO iverilog doesn't allow references to variables in initializers
    // if (1) begin : blk
    // integer a = 11;
    // integer b = 12;
    const integer w = 11;
    const T x = '{ y: 11, z: 12 };
    initial $display("%b %b %b %b", w, x, x.y, x.z);
endmodule
