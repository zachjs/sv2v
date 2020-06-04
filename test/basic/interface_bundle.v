module impl(b_index, b_clock, b_inp, b_out);
    output reg [1:0] b_index;
    output reg b_clock;
    output reg [3:0] b_inp;
    output wire b_out;

    initial b_index = 0;
    always @(posedge b_clock)
        b_index <= b_index + 1;

    initial b_inp = 4'b1111;
    always @(posedge b_clock)
        b_inp[b_index] <= b_out;

    assign b_out = ^b_inp;

    initial begin
        b_clock <= 0;
        forever
            #5 b_clock <= ~b_clock;
    end
endmodule

module top;
    wire [1:0] b_index;
    wire b_clock;
    wire [3:0] b_inp;
    wire b_out;

    impl impl(b_index, b_clock, b_inp, b_out);

    initial begin
        $monitor("%b %b %b %b", b_index, b_clock, b_inp, b_out);
        #100;
        $finish;
    end
endmodule

