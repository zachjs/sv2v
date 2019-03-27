`default_nettype none

module top;

    reg [7:0] doubleNibble;
    wire [3:0] sum;

    Device dut(
        .doubleNibble(doubleNibble),
        .sum(sum)
    );


    reg [8:0] i; // Note that i is 1 bit wider than doubleNibble
    initial begin
        $monitor($time, " %h + %h = %h", doubleNibble[7:4], doubleNibble[3:0], sum);
        doubleNibble = 8'h00;
        for(i = 0; i <= 8'hff; i = i + 8'h1) begin
            #10 doubleNibble = i; // This drops upper order bits
        end
    end

endmodule
