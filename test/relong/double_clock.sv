`default_nettype none

module Device(
    input logic clock, clear,
    output logic [3:0] data
);

    SharedMemory memory(
        .clock1(clock),
        .clock2(clock),
        .clear,
        .data1(data[1:0]),
        .data2(data[3:2])
    );

endmodule

module SharedMemory(
    input logic clock1, clock2, clear,
    output logic [1:0] data1, data2
);

    logic [3:0] memory;

    // Just a dumb example to generate interesting values
    always_ff @(posedge clock1) begin
        if(clear)
            memory <= 4'b0;
        else
            memory <= {~memory[2:0], 1'b0};
    end

    always_ff @(posedge clock1)
        data1 <= memory[1:0];

    // Technically this is pretty dangerous since it is being generated on
    // clock1 domain so we would need a bunch of other logic to make sure this
    // is safe, but for the example we'll just ignore that stuff.
    always_ff @(posedge clock2) begin
        data2 <= memory[3:2];
    end


endmodule