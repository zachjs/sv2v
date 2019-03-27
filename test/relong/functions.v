`default_nettype none

// Enum's default to 32-bit, but this is a tool specific implementation detail
`define STATE_T\
parameter FOO = 32'd0, BAR = 32'd1;

// As an alternative to the macro based structs, they could also simply be computed
// typedef struct packed {
//     State_t state;  // [63:32]
//     logic [31:0] data; // [31:0]
// } MyStruct_t;

module Example(
    input wire clock, clear,
    input wire [7:0] dataIn,
    output wire check1,
    output reg check2, // Split since check2 is used in an always_comb block
    output wire [63:0] checkData
);

    `STATE_T

    // The automatic keyword here is super confusing, but generally does what
    // you expect a C function to do. Sadly VTR doesn't support the automatic
    // keyword.
    function [31:0] swapState(input [31:0] state);
        case(state)
            // To return from a function assign the function name with a variable
            FOO: swapState =  BAR;
            BAR: swapState = FOO;
        endcase
    // Scope ending labels are not supported in Verilog and only provide
    // human readability benifits (plus compiler warnings if they are
    // incorrect)
    endfunction

    reg [31:0] state;

    always @(posedge clock)
        if(clear)
            state <= FOO;
        else
            state <= swapState(state);

    wire [15:0] magicToken;
    assign magicToken = 16'habcd;

    function [63:0] packStruct(input [31:0] state, input [7:0] data);
        // Something interesting about system verilog is that all local variable
        // declarations must be first in the function (at least for VCS to
        // compile it)
        reg [31:0] fullData;
        reg [31:0] nextState;
    begin
        fullData = {~data, data, magicToken};
        nextState = swapState(state);
        packStruct = {nextState, fullData};
    end
    endfunction

    wire [63:0] myStruct;
    assign myStruct = packStruct(state, dataIn);

    function [0:0] doCheck(input [63:0] inputStruct);
        doCheck = inputStruct[63:32] == FOO; // inputStruct.state
    endfunction // : doCheck

    assign check1 = doCheck(myStruct);

    reg [63:0] myStruct2;
    always @* begin
        myStruct2 = packStruct(swapState(state), ~dataIn);
        check2 = doCheck(myStruct2);
    end

    assign checkData = {myStruct[31:0], myStruct2[31:0]}; // *.data



endmodule