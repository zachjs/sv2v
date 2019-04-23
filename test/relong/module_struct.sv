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

  SubModule sub(
    .in(a),
    .out(s.last)
  );

   always_comb begin
        s.first = b;
   end

endmodule

module SubModule(
  input logic [1:0] in,
  output logic [1:0] out
);

  assign out = in;

endmodule