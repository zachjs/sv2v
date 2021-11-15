module top;
    wire [3:0] a, b, w, x, y, z;
    assign a = 4'b1111;
    assign b = 4'b0001;

    // loses upper bit
    assign w = (a + b) >> 1;

    // preserves upper bit via implicit extension
    assign y = (0 + a + b) >> 1;
    assign y = (a + b + 0) >> 1;

    // preserves upper bit via "casting"
    wire [4:0] tmp;
    assign tmp = x + b;
    assign z = tmp >> 1;
endmodule
