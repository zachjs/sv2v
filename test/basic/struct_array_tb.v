module top;
    reg [56-1:0] in;
    reg [2:0] select;

    wire a;
    wire [3:0] b;
    wire [1:0] c;

    Unpacker unpacker(in, select, a, b, c);

    initial begin
        $monitor("%d: %01b %04b %02b", select, a, b, c);
        in = 'b01111011011011101111100111110111001010001011100110101000;
        select = 0; #1;
        select = 1; #1;
        select = 2; #1;
        select = 3; #1;
        select = 4; #1;
        select = 5; #1;
        select = 6; #1;
        select = 7; #1;
        $finish;
    end

    // 0 1010 00
    // 1 1100 11
    // 0 1000 10
    // 0 1110 01
    // 0 0111 11
    // 1 0111 11
    // 1 0110 11
    // 0 1111 01

endmodule
