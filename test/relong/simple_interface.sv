`default_nettype none

interface SimpleInterface(input logic clock, clear);
    logic [31:0] data;
    logic shift;

    modport Producer(
        output data,
        input shift,

        input clock, clear
    );

    modport Consumer(
        input data,
        output shift,

        input clock, clear
    );
endinterface : SimpleInterface

module Device(
    input logic [7:0] dataIn,
    output logic [31:0] dataOut,

    input logic clock, clear
);

    SimpleInterface theInterface(.clock, .clear);


    Producer producer(
        .myInterface(theInterface.Producer),
        .dataIn
    );

    Consumer consumer(
        .myInterface(theInterface.Consumer),
        .dataOut
    );

endmodule


// The producer takes the input and then transforms it for 4 cycles
module Producer(
    SimpleInterface.Producer myInterface,
    input logic [7:0] dataIn
);
    logic [31:0] inProgress;
    always_ff @(posedge myInterface.clock) begin
        if(myInterface.clear) begin
            inProgress <= 32'b0;
        end else if(myInterface.shift) begin
            inProgress <= {inProgress[23:0], dataIn};
        end
    end

    assign myInterface.data = inProgress;

endmodule

module Consumer(
    SimpleInterface.Consumer myInterface,
    output logic [31:0] dataOut
);

    // Just want this variable to make the test bench nicer
    logic local_shift;
    assign local_shift = myInterface.shift;

    always_ff @(posedge myInterface.clock)
        if(myInterface.clear) begin
            myInterface.shift <= 1'b0;
        end else begin
            myInterface.shift <= ~myInterface.shift;
        end

    assign dataOut = myInterface.data;

endmodule