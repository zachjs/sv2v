// Ignored the compiler directive

module Device(
    input wire [7:0] dataIn,
    output wire [31:0] dataOut,

    input wire clock, clear
);

    // Expanded interface declaration
    wire theInterface_clock;
    wire theInterface_clear;
    wire [31:0] theInterface_data;
    wire theInterface_shift;

    // Interface instantiation
    assign theInterface_clock = clock;
    assign theInterface_clear = clear;

    Producer producer(
        // Expanded interface
        .myInterface_clock(theInterface_clock),
        .myInterface_clear(theInterface_clear),
        .myInterface_data(theInterface_data),
        .myInterface_shift(theInterface_shift),

        .dataIn(dataIn)
    );

    Consumer consumer(
        // Expanded interface
        .myInterface_clock(theInterface_clock),
        .myInterface_clear(theInterface_clear),
        .myInterface_data(theInterface_data),
        .myInterface_shift(theInterface_shift),

        .dataOut(dataOut)
    );

endmodule

module Producer(
    // Port direction from SimpleInterface.Producer modport
    input wire myInterface_clock,
    input wire myInterface_clear,
    output wire [31:0] myInterface_data,
    input wire myInterface_shift,

    input wire [7:0] dataIn
);

    reg [31:0] inProgress;
    always @(posedge myInterface_clock) begin
        if(myInterface_clear) begin
            inProgress <= 32'b0;
        end else if(myInterface_shift) begin
            inProgress <= {inProgress[23:0], dataIn};
        end
    end

    assign myInterface_data = inProgress;

endmodule

module Consumer(
    // Port direction from SimpleInterface.Consumer modport
    input wire myInterface_clock,
    input wire myInterface_clear,
    input wire [31:0] myInterface_data,
    output reg myInterface_shift,

    output wire [31:0] dataOut
);

    // Just want this variable to make the test bench nicer
    wire local_shift;
    assign local_shift = myInterface_shift;

    always @(posedge myInterface_clock)
        if(myInterface_clear) begin
            myInterface_shift <= 1'b0;
        end else begin
            myInterface_shift <= ~myInterface_shift;
        end

    assign dataOut = myInterface_data;

endmodule