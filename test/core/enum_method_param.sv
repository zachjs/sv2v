module mod(inp);
    parameter type T;
    input T inp;
    initial
        $display("%s=%b %s=%b %s=%b %s=%b %b",
            inp.first.name, inp.first,
            inp.first.next.name, inp.first.next,
            inp.last.prev.name, inp.last.prev,
            inp.last.name, inp.last,
            inp.num);
endmodule

module top;
    parameter P = 0;
    typedef enum byte {
        A = P, B, C
    } T;
    typedef enum shortint {
        D = A, E, F
    } U;
    mod #(T) t(A);
    mod #(U) u(D);
endmodule
