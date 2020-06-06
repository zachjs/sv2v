module Flip(x, y);
    input x;
    output y;
    assign y = ~x;
endmodule

module Test1(o);
    output [1:0] o;
    wire x = 0;
    generate
        genvar i;
        for (i = 0; i < 1; i = i + 1) begin
            Flip flip(x, o[i]);
        end
    endgenerate
    initial begin : blah
        integer i;
        i = 0;
    end
endmodule

module Test2(o);
    output o;
    wire x = 0;
    Flip flip(x, o);
    initial begin : blah
        integer o;
        o = 0;
    end
endmodule

module Test3(o);
    output o;
    reg x_0;
    wire x_1;
    Flip flip(x_0, x_1);
    assign o = x_0;
    initial x_0 = 0;
    initial begin : blah
        integer x;
        x = 0;
    end
endmodule
