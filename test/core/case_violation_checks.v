module top;
    reg [1:0] select;
    wire [3:0] data [2:0];
    UniqueCase case0(.select(select), .data(data[0]));
    Unique0Case case1(.select(select), .data(data[1]));
    PriorityCase case2(.select(select), .data(data[2]));
    initial begin end
endmodule

module UniqueCase(
    input [1:0] select,
    output reg [3:0] data
);
    always @* begin
        data = 4'b0;
        case(select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
        endcase
    end
endmodule

module Unique0Case(
    input [1:0] select,
    output reg [3:0] data
);
    always @* begin
        data = 4'b0;
        case(select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
        endcase
    end
endmodule

module PriorityCase(
    input [1:0] select,
    output reg [3:0] data
);
    always @* begin
        data = 4'b0;
        case(select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
        endcase
    end
endmodule
