interface CacheSetInterface(
    input logic [7:0] request,
    output logic [7:0] response
);

    modport CacheSet(
        input request,
        output response
    );

endinterface

module CacheWithInterface(
    input logic [7:0] dataIn,
    output logic [7:0] dataOut,
    input logic clock, clear
);

    logic [7:0] myRequest;
    logic [7:0] myResponse;

    CacheSetInterface dataInterface(
        .request(myRequest),
        .response(myResponse)
    );

    CacheSetWrapper set(
        .data(dataInterface.CacheSet),
        .clock,
        .clear
    );

    assign myRequest = dataIn;
    assign dataOut = myResponse;

endmodule

// to test binding a modport to another modport
module CacheSetWrapper (
    CacheSetInterface.CacheSet data,
    input logic clock, clear
);
    CacheSet set(data, clock, clear);
endmodule

module CacheSet (
    CacheSetInterface.CacheSet data,
    input logic clock, clear
);

    always_ff @(posedge clock)
        if(clear)
            data.response <= 8'h0;
        else
            data.response <= ~data.request;

endmodule
