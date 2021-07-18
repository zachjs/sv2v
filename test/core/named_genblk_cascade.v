module mod(
    output wire [31:0] out
);
    parameter P = 0;
    if (P == 1)
        assign out = 2;
    else if (P == 2)
        assign out = 3;
    else if (P == 3)
        assign out = 5;
    else
        assign out = 7;
endmodule
