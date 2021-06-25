module Flip(x, y);
    input x;
    output y;
    assign y = ~x;
endmodule

module Test1(o);
    output [1:0] o;
    logic x = 0;
    for (genvar i = 0; i < 1; ++i) begin
        Flip flip(x, o[i]);
    end
    initial begin
        integer i = 0;
    end
endmodule

module Test2(o);
    output o;
    logic x = 0;
    Flip flip(x, o);
    initial begin
        integer o = 0;
        x = 0;
    end
endmodule

module Test3(o);
    output o;
    logic [1:0] x;
    Flip flip(x[0], x[1]);
    assign o = x[0];
    initial x[0] = 0;
    initial begin
        integer x = 0;
    end
endmodule
