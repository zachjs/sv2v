module top;
`define LOOP(ID, START) \
    x = 0; \
    for (x = START; x < 3; x = x + 1) \
        $display(`"ID x = %0d`", x);

    initial begin : blk
        integer x;
        `LOOP(A, 1)
        `LOOP(B, 1)
        `LOOP(C, 1)
        `LOOP(D, 0)
        `LOOP(E, 0)
        `LOOP(F, 1)
        `LOOP(G, 1)
        `LOOP(H, -1)
        `LOOP(I, -1)
        `LOOP(J, -1)
    end
endmodule
