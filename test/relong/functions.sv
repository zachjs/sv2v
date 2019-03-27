`default_nettype none

typedef enum {FOO, BAR} State_t;

typedef struct packed {
    State_t state;
    logic [31:0] data;
} MyStruct_t;

module Example(
    input logic clock, clear,
    input logic [7:0] dataIn,
    output logic check1, check2,
    output logic [63:0] checkData
);

    // The automatic keyword here is super confusing, but generally does what
    // you expect a C function to do. Sadly VTR doesn't support the automatic
    // keyword.
    function automatic State_t swapState(State_t state);
        // The state variable is not shadowing the state variable in the global
        // function since the function is declared above the state variable
        // (Declaring functions at the start of the module before module scope
        // variables is a good practice to avoid accidentally using a module scope
        // in a function)
        unique case(state)
            FOO: return BAR;
            BAR: return FOO;
        endcase
    endfunction : swapState

    State_t state;

    always_ff @(posedge clock)
        if(clear)
            state <= FOO;
        else
            state <= swapState(state);

    logic [15:0] magicToken;
    assign magicToken = 16'habcd;

    function automatic MyStruct_t packStruct(State_t state, logic [7:0] data);
        // Something interesting about system verilog is that all local variable
        // declarations must be first in the function (at least for VCS to
        // compile it)
        logic [31:0] fullData;
        State_t nextState;
        // System Verilog functions can also access "local" variables from the
        // module scope. This means that any variable with the same name in the
        // function as something else defined in the module is shadowing the
        // module variable (e.g. state in this function)
        fullData = {~data, data, magicToken};
        nextState = swapState(state);
        return '{
            state: nextState,
            data: fullData
        };
    endfunction

    MyStruct_t myStruct;
    assign myStruct = packStruct(state, dataIn);

    function automatic logic doCheck(MyStruct_t inputStruct);
        return inputStruct.state == FOO;
    endfunction : doCheck

    assign check1 = doCheck(myStruct);

    MyStruct_t myStruct2;
    always_comb begin
        myStruct2 = packStruct(swapState(state), ~dataIn);
        check2 = doCheck(myStruct2);
    end

    assign checkData = {myStruct.data, myStruct2.data};



endmodule