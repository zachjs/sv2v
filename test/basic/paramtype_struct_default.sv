module example(inp, data, valid);
    parameter W = 16;
    typedef struct packed {
        logic [W-1:0] data;
        logic valid;
    } line_t;
    parameter type local_line_t = line_t;
    input local_line_t inp;
    output logic [W-1:0] data;
    assign data = inp.data;
    output logic valid;
    assign valid = inp.valid;
endmodule

module top;
    reg [16:0] inp;
    wire [15:0] data;
    wire valid;
    example e(.*);
    initial inp = 17'b1_01100011_11001111;
endmodule
