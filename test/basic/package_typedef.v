module top;
    localparam AW = 16;
    wire [AW+2:0] foo;
    assign foo = 19'b110_01001000_01110100;

    wire [7:0] bar;
    parameter FLAG = 1;
    assign bar = FLAG ? foo[2+:8] : 8'hFF;
endmodule
