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

   // Convert to always @* block
   // It might be easier to go directly to always @*
   wire [1:0] _s_3_2;
   assign _s_3_2[1:0] = a;
   always @* s[3:2] = _s_3_2;

   always @* begin
        s[1:0] = b;
   end

endmodule