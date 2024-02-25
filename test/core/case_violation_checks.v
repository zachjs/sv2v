module UniqueCase(
    input wire [1:0] select,
    output reg [3:0] data
);
    always @* begin
        data = 4'b0;
        case (select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
        endcase
    end
endmodule

module Unique0Case(
    input wire [1:0] select,
    output reg [3:0] data
);
    always @* begin
        data = 4'b0;
        case (select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
        endcase
    end
endmodule

module PriorityCase(
    input wire [1:0] select,
    output reg [3:0] data
);
    always @* begin
        data = 4'b0;
        case (select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
        endcase
    end
endmodule
