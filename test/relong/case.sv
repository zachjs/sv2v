`default_nettype none

module Example(
    input logic [1:0] select,
    // This is an array of 3 (4-bit wide) elements
    output logic [2:0][3:0] data
);
    UniqueCase case0(.select, .data(data[0]));
    WildcardCase case1(.select, .data(data[1]));
    DefaultCase case2(.select, .data(data[2]));

endmodule

module UniqueCase(
    input logic [1:0] select,
    output logic [3:0] data
);

    always_comb begin
        data = 4'b0;
        unique case(select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
            /* unique means that the toolchain can assume 2'd3 will never happen */
        endcase
    end

endmodule

module WildcardCase(
    input logic [1:0] select,
    output logic [3:0] data
);

    always_comb begin
        data = 4'b0;
        unique casez(select)
            2'b00: data = 4'h3;
            2'b1?: data = 4'hd;
            // Unique means that the toolchain can assume 2'b01 will never happen
        endcase
    end

endmodule

module DefaultCase(
    input logic [1:0] select,
    output logic [3:0] data
);

    always_comb begin
        data = 4'b0;
        case (select)
            2'b00: data = 4'h7;
            2'b01: data = 4'h9;
            default: data = 4'h8;
        endcase
    end

endmodule

// There is also a casex construct, but it is very dangerous and not recommend to be used in real designs.
// https://www.verilogpro.com/verilog-case-casez-casex/