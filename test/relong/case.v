`default_nettype none

module Example(
    input wire [1:0] select,
    // This is an array of 3 (4-bit wide) elements
    output wire [11:0] data
);
    // Unflatten the array
    wire [3:0] __data[2:0];
    assign data = {__data[2], __data[1], __data[0]};

    UniqueCase case0(.select(select), .data(__data[0]));
    WildcardCase case1(.select(select), .data(__data[1]));
    DefaultCase case2(.select(select), .data(__data[2]));

endmodule

module UniqueCase(
    input wire [1:0] select,
    output reg [3:0] data
);

    always @* begin
        data = 4'b0;
        // Unique keyword doesn't exist in Verilog
        case(select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
        endcase
    end

endmodule

module WildcardCase(
    input wire [1:0] select,
    output reg [3:0] data
);

    always @* begin
        data = 4'b0;
        // Unique keyword doesn't exist in Verilog
        // casez doesn't exist in VTR, so manually elaborating it
        case(select) // casez
            2'b00: data = 4'h3;
            // 2'b1?: data = 4'hd;
            2'b10: data = 4'hd;
            2'b11: data = 4'hd;
        endcase
    end

endmodule

module DefaultCase(
    input wire [1:0] select,
    output reg [3:0] data
);

    always @* begin
        data = 4'b0;
        case (select)
            2'b00: data = 4'h7;
            2'b01: data = 4'h9;
            default: data = 4'h8;
        endcase
    end

endmodule
