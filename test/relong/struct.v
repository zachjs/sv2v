`default_nettype none

// Technically the tool could do something much smarter than having a bunch of
// macros everywhere, but this is the easiest way I know to make it clear what
// is happening.

// "packed" structs are effectively a single bit-vector that has syntactic sugar
// to access specific parts. SystemVerilog struct layout is amusingly the
// complete opposite of C struct layout

`define InnerStruct_t_data [32:1]
`define InnerStruct_t_valid [0:0]

`define MODE_T\
parameter MODE_A = 32'b0;\
parameter MODE_B = 32'b1

// Nested structs are a bit trickier to resolve
`define OuterStruct_t_inner [64:32]
`define OuterStruct_t_mode [31:0]

module Example(
    input wire clock, clear,
    output reg success
);

    `MODE_T;

    // Verilog must unfuse these declarations since *_in is a reg and *_out is a wire
    reg [31:0] data_in;
    wire [31:0] data_out;
    reg valid_in;
    wire valid_out;
    reg [31:0] mode_in;
    wire [31:0] mode_out;
    wire [64:0] object;

    // Sadly Verilog doesn't support built-in functions like $bits or $clog2
    // (these are the only functions that I've used in my core and would expect
    // to synthesize. Luckily the implementation is easy-ish)

    always @(posedge clock) begin
        if(clear) begin
            data_in <= 32'h0;
            valid_in <= 1'b0;
            mode_in <= MODE_A;
        end else if(mode_in == MODE_A) begin
            valid_in <= 1'b1;
            mode_in <= MODE_B;
            data_in <= data_in + 32'd65; // Magically computed by looking at the type object
        end else begin
            mode_in <= MODE_A;
            data_in <= data_in + 32'h1;
            valid_in <= 1'b1;
        end
    end

    StructCompose compose(
        .data(data_in),
        .valid(valid_in),
        .mode(mode_in),
        .object(object)
    );

    StructDecompose decompose(
        .data(data_out),
        .valid(valid_out),
        .mode(mode_out),
        .object(object)
    );

    always @* begin
        success = 1'b0;
        if(data_in == data_out)
            if(valid_in == valid_out)
                if(mode_in == mode_out)
                    success = 1'b1;
    end

endmodule

module StructCompose(
    input wire [31:0] data,
    input wire valid,
    input wire [31:0] mode,
    output reg [64:0] object
);

    // Technically the tool could inline all of this, but it's easier to
    // understand if the structs are built separately.
    reg [32:0] __inner;
    always @* begin
        __inner `InnerStruct_t_data = data;
        __inner `InnerStruct_t_valid = valid;
        object `OuterStruct_t_mode = mode;
        object `OuterStruct_t_inner = __inner;
    end

endmodule

module StructDecompose(
    input wire [64:0] object,
    output reg [31:0] data,
    output reg valid,
    output reg [31:0] mode
);

    // Technically the tool could inline the bit selection directly
    reg [32:0] __inner;
    always @* begin
        __inner = object `OuterStruct_t_inner;
        data = __inner `InnerStruct_t_data;
        valid = __inner `InnerStruct_t_valid;
        mode = object `OuterStruct_t_mode;
    end

endmodule