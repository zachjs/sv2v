`default_nettype none

typedef struct packed {
    logic [1:0] last;
    logic [3:0] middle;
    logic [1:0] first;
} MyStruct_t;


module Example(
    input logic [3:0] a,
    output logic [31:0] all
);

    generate
        genvar i;
        for(i = 0; i < 4; i = i + 1) begin
            MyStruct_t s;
            always_comb begin
                s = '{
                    first: i,
                    middle: a,
                    last: 3 - i
                };
            end
            assign all[i*$bits(s)+:$bits(s)] = s;
        end
    endgenerate

endmodule