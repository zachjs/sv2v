
module top;
    logic [1:0] select;
    logic [2:0][3:0] data;

    UniqueCase case0(.select, .data(data[0]));
    Unique0Case case1(.select, .data(data[1]));
    PriorityCase case2(.select, .data(data[2]));

    initial ;
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
        endcase
    end

endmodule

module Unique0Case(
    input logic [1:0] select,
    output logic [3:0] data
);

    always_comb begin
        data = 4'b0;
        unique0 case(select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
        endcase
    end

endmodule

module PriorityCase(
    input logic [1:0] select,
    output logic [3:0] data
);

    always_comb begin
        data = 4'b0;
        priority case(select)
            2'd0: data = 4'ha;
            2'd1: data = 4'h6;
            2'd2: data = 4'h3;
        endcase
    end

endmodule
