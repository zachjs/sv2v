module top;
    wire [3:0] a, b, w, x, y, z;
    assign a = 4'b1011;
    assign b = 4'b0101;

    // loses upper bits
    assign w = (a * b) >> 4;

    // preserves upper bits via implicit extension
    assign x = (1 * a * b) >> 4;
    assign y = (a * b * 1) >> 4;

    // preserves upper bits via "casting"
    wire [7:0] tmp;
    assign tmp = a * b;
    assign z = tmp >> 4;
endmodule
