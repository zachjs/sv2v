module ExampleA;
    typedef enum logic {
        A = 1,
        B = 0,
        C = 2
    } Enum;
    Enum x = A;
    initial $display("ExampleA: x=%b, A=%b, B=%b", x, A, B);
endmodule

module ExampleB;
    typedef enum logic {
        A = 0,
        B = 1
    } Enum;
    Enum x = A;
    initial $display("ExampleB: x=%b, A=%b, B=%b", x, A, B);
endmodule
