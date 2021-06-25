module top;
    wire [5:0] w;
    wire [2:0] x;
    wire y;
    wire z;

    wire [5:0] a;
    wire [1:0] b;

    initial begin
        $display("%b %b %b %b", w, x, y, z);
        $display("%b %b", a, b);
    end
endmodule
