`default_nettype none

typedef struct packed {
    logic [1:0] last;
    logic [1:0] first;
} MyStruct_t;


module Example(
    input logic [1:0] a, b,
    output logic [3:0] result
);

   MyStruct_t s;
   assign result = s;

   assign s.last = a;
   always_comb begin
        s.first = b;
   end

endmodule