module top;
    reg a, b, c;
    (* test *) always_ff @(posedge a)
        c <= b;
    (* test *) always_latch
        if (c)
            b <= 1;
    (* test *) always_comb
        a = b;
endmodule
