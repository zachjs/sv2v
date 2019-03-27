`default_nettype none

module Device(
    input wire [31:0] data,
    output wire parity
);

    wire [3:0] partParity;

    Helper bottom(data[7:0], partParity[0]);

    Helper bottomMid(.parity(partParity[1]), .data(data[15:8]));

    Wrapper1 topMid(.data(data[23:16]), .parity(partParity[2]));

    Wrapper2 top(.data(data[31:24]), .parity1(partParity[3]));

    assign parity = ^partParity;

endmodule

module Helper(
    input wire [7:0] data,
    output wire parity
);

    // This is a bit-wise reduction operator
    assign parity = ^data;

endmodule

module Wrapper1(
    input wire [7:0] data,
    output wire parity
);

    // Expand .* from SystemVerilog
    Helper doTheRightThingMode(.data(data), .parity(parity));

endmodule

module Wrapper2(
    input wire [7:0] data,
    output wire parity1
);

    // Expand .data from SystemVerilog
    Helper compilerSaveMe(.data(data), .parity(parity1));

endmodule