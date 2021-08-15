module top;
    reg [63:0] a = { 8'd5, 16'd6, 32'd7, 8'd8 };
    reg [63:0] b = { 8'd13, 16'd14, 32'd15, 8'd16 };
    wire [63:0] c = { 8'd9, 16'd10, 32'd11, 8'd12 };
    initial begin
        #1;
        $display("a %b", a);
        $display("b %b", b);
        $display("c %b", c);
        $display("x %b", { 8'd1, 16'd2, 32'd3, 8'd4 } );
        $display("$bits(S) = %0d", 40);
        $display("$bits(T) = %0d", 64);
        $display("$bits(a) = %0d", 64);
        $display("$bits(a.x) = %0d", 8);
        $display("$bits(a.y) = %0d", 16);
        $display("$bits(a.z) = %0d", 40);
        $display("$bits(a.z.x) = %0d", 32);
        $display("$bits(a.z.y) = %0d", 8);
    end
endmodule
