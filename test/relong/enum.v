`default_nettype none

// So Verilog is very sad when it comes to enumerations. The easiest way to emulate this behavior is probably to define a macro for the enumeration and then instantiate it everywhere the type is used.
`define MODE_T\
parameter MODE_A = 32'b0;\
parameter MODE_B = 32'b1

`define OPERATION_T\
parameter READ = 2'd1;\
parameter WRITE = 2'd2;\
parameter NONE = 2'd0

module Example(
    input wire rawMode,
    output wire [1:0] rawOperation
);
    `MODE_T;
    `OPERATION_T;

    // Enumeration variables don't have types, so they need to be default wires.

    // Technically this could be just one bit if the tool is smart enough to realize.
    wire [31:0] mode;
    reg [1:0] operation;


    // No need for a cast since everything is a wire
    assign mode = rawMode;
    assign rawOperation = operation;

    always @* begin
        case(mode)
            MODE_A: operation = READ;
            MODE_B: operation = WRITE;
            default: operation = NONE;
        endcase
    end

endmodule