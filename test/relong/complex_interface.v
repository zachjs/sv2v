// interface CacheSetInterface(
//     input logic [7:0] request,
//     output logic [7:0] response
// );

//     modport CacheSet(
//         input request,
//         output response
//     );

// endinterface

module CacheWithInterface(
    input wire [7:0] dataIn,
    output wire [7:0] dataOut,
    input wire clock, clear
);

    wire [7:0] myRequest;
    wire [7:0] myResponse;

    // CacheSetInterface dataInterface(
    //     .request(myRequest),
    //     .response(myResponse)
    // );

    wire [7:0] dataInterface_request;
    wire [7:0] dataInterface_response;
    generate
        assign dataInterface_request = myRequest;
        // dataInterface.myResponse is an output
        assign myResponse = dataInterface_response;
    endgenerate

    CacheSet set(
        .data_request(dataInterface_request),
        .data_response(dataInterface_response),
        .clock(clock),
        .clear(clear)
    );

    assign myRequest = dataIn;
    assign dataOut = myResponse;

endmodule

module CacheSet (
    input wire [7:0] data_request,
    output reg [7:0] data_response,
    input wire clock, clear
);

    always @(posedge clock)
        if(clear)
            data_response <= 8'h0;
        else
            data_response <= ~data_request;

endmodule