`include "always_sense.vh"
module top;
    reg `INPUTS;
    wire `OUTPUTS;
    mod m(`INPUTS, `OUTPUTS);
    initial begin
        $monitor(`INPUTS, `OUTPUTS);
        repeat (2) begin
            #1 inp1 = 0;
            #1 inp2 = 0;
            #1 inp2 = 1;

            #1 inp1 = 1;
            #1 inp2 = 0;
            #1 inp2 = 1;

            #1 inp2 = 0;
            #1 inp1 = 0;
            #1 inp1 = 1;

            #1 inp2 = 1;
            #1 inp1 = 0;
            #1 inp1 = 1;
        end
    end
endmodule
