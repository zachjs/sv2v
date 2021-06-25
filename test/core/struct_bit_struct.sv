typedef struct packed {
    logic [3:0] a;
    logic [3:0] b;
} pair;

typedef struct packed {
    pair [1:0] x;
    pair [1:0] y;
} pair_list_pair;

module Example(data, p1, p2, out_x, out_y);
    input pair_list_pair data;
    input logic p1;
    input logic p2;
    output logic [3:0] out_x;
    output logic [3:0] out_y;

    assign out_x = p2 ? data.x[p1].a : data.x[p1].b;
    assign out_y = p2 ? data.y[p1].a : data.y[p1].b;
endmodule
