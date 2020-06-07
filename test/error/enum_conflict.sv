module top;
    typedef enum {
        A = 0,
        B, // implicitly 1
        C = 1
    } Enum;
    Enum e;
endmodule
