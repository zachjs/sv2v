module top;
    typedef struct packed {
        logic [1:0][1:0][1:0] x;
    } U;
    typedef union packed {
        logic [$bits(U) - 1:0][7:0] a;
        logic [3:0][15:0] b;
        U [7:0] c;
    } T;
    typedef struct packed {
        logic [$bits(U) - 1:0][7:0] a;
        logic [3:0][15:0] b;
        U [7:0] c;
        T [2:0] d;
    } S;
    if ($bits(U) != 8)
        $error("invalid width U");
    if ($bits(T) != 64)
        $error("invalid width T");
    if ($bits(S) != 64 * 6)
        $error("invalid width S");
    logic [31:0] x;
    assign x[$bits(U) - 1:0] = '1;
endmodule
