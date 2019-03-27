`default_nettype none

module Device(
    input logic [7:0] doubleNibble,
    output logic [3:0] sum
);

    // I would probably write the example instance2, but that is mostly me just
    // being overly cautious.
    Helper instance1(doubleNibble[7:4], doubleNibble[3:0], sum);

    logic [3:0] ignored;
    Helper instance2(
        .a(doubleNibble[7:4]),
        .b(doubleNibble[3:0]),
        .result(ignored)
    );

endmodule

module Helper(
    input logic [3:0] a, b,
    output logic [3:0] result
);

    assign result = a + b;

endmodule