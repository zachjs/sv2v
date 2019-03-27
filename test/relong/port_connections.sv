`default_nettype none

module Device(
    input logic [31:0] data,
    output logic parity
);

    logic [3:0] partParity;

    // This is passing a bit-slice as the input to the module assuming that all
    // of the ports are connected in order.
    Helper bottom(data[7:0], partParity[0]);

    // This is the most common syntax explicitly binding the names of the
    // connections to avoid errors where the order of the port definitions change.
    Helper bottomMid(.parity(partParity[1]), .data(data[15:8]));

    Wrapper1 topMid(.data(data[23:16]), .parity(partParity[2]));

    Wrapper2 top(.data(data[31:24]), .parity1(partParity[3]));


    assign parity = ^partParity;

endmodule

module Helper(
    input logic [7:0] data,
    output logic parity
);

    // This is a bit-wise reduction operator
    assign parity = ^data;

endmodule

module Wrapper1(
    input logic [7:0] data,
    output logic parity
);
    // This is SystemVerilog shorthand to make it easy to write trivial systems.
    // For the most part the wire names don't line up nicely so this doesn't
    // work. The compiler replaces ".*" with ".data(data), .parity(parity)" and
    // typically checks that the port widths are consistent.
    Helper doTheRightThingMode(.*);

endmodule

module Wrapper2(
    input logic [7:0] data,
    output logic parity1
);

    // This is a SystemVerilog shorthand similar to .* but actually usable in
    // real projects since it only applies to a specific port. The compiler
    // replaces the ".data" as ".data(data)" and typically checks that the port
    // widths are consistent.
    Helper compilerSaveMe(.data, .parity(parity1));

endmodule