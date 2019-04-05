module top;
    wire [31:0] a;
    wire [0:31] b;
    assign a = 'h64ded943;
    assign b = 'hb7151d17;
    initial begin
        $display(a[0+:8]);
        $display(a[15-:8]);
        $display(b[0+:8]);
        $display(b[15-:8]);
    end
endmodule
