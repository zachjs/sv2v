module ExampleA;
    localparam [0:0] A = 1;
    localparam [0:0] B = 0;
    reg x = A;
    initial $display("ExampleA: x=%b, A=%b, B=%b", x, A, B);
endmodule

module ExampleB;
    localparam [0:0] A = 0;
    localparam [0:0] B = 1;
    reg x = A;
    initial $display("ExampleB: x=%b, A=%b, B=%b", x, A, B);
endmodule

