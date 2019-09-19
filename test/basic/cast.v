module top;
    initial begin : foo_block
        reg [31:0] w;
        reg signed [31:0] x;
        reg signed [31:0] y;
        reg [4:0] z;
        w = 1234;
        x = -235;
        y = 1234;
        z = y;
        $display("%0d %0d", w, w[4:0]);
        $display("%0d %0d", x, $signed(x[4:0]));
        $display("%0d %0d", y, $signed(y[4:0]));
        $display("%0d %0d", z, z[4:0]);
        $display("%0d %0d", w+1, w[4:0]+1);
        $display("%0d %0d", x+1, $signed(x[4:0])+1);
        $display("%0d %0d", y+1, $signed(y[4:0])+1);
        $display("%0d %0d", z+1, z[4:0]+1);
        $display("%b %b", w, {8'b0, w});
        $display("%b %b", x, {8'hFF, x});
        $display("%b %b", y, {8'b0, y});
        $display("%b %b", z, {35'b0, z});
    end
    localparam foo = 0;
    localparam [31:0] bar = 32'b0;
    initial $display("%b %b", foo, bar);
endmodule
