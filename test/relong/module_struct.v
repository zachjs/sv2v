`default_nettype none

// typedef struct packed {
//     logic [1:0] last;
//     logic [1:0] first;
// } MyStruct_t;


module Example(
    input wire [1:0] a, b,
    output wire [3:0] result
);

   reg [3:0] s;
   assign result = s;

  wire [1:0] __s_out;
  always @* s[3:2] = __s_out;
  SubModule sub(
    .in(a),
    .out(__s_out)
  );

   always @* begin
        s[1:0] = b;
   end

endmodule

module SubModule(
  input wire [1:0] in,
  output wire [1:0] out
);

  assign out = in;

endmodule