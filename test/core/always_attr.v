module top;
    reg a, b, c;
    (* test *) always @(posedge a)
        c <= b;
    (* test *) always @*
        if (c)
            b <= 1;
    (* test *) always @*
        a = b;
endmodule
