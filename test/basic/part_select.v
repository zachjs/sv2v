module top;
    wire [31:0] a;
    wire [0:31] b;
    assign a = 'h64ded943;
    assign b = 'hb7151d17;
    initial begin
        $display(a[7:0]);
        $display(a[15:8]);
        $display(b[0:7]);
        $display(b[8:15]);
    end
endmodule
