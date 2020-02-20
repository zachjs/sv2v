module Example(data, p1, p2, out_x, out_y);
    input wire [31:0] data;
    input wire p1, p2;
    output wire [3:0] out_x;
    output wire [3:0] out_y;

    assign out_x = p2 ? data[p1 * 8 + 20 +:4] : data[p1 * 8 + 16 +:4];
    assign out_y = p2 ? data[p1 * 8 +  4 +:4] : data[p1 * 8 +  0 +:4];
endmodule
