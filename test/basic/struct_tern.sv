module Example(flag, out);
    typedef struct packed {
        logic a, b;
    } T;
    output T out;
    input logic flag;
    assign out =
        flag
        ? '{ a: 1'b1, b: 1'b0 }
        : '{ a: 1'b1, b: 1'b1 }
        ;
endmodule
