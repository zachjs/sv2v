`default_nettype none

module ArrayOrReduction #(
    parameter SIZE = 16, WIDTH=32) (
    input logic [SIZE-1:0][WIDTH-1:0] inputs,
    output logic [WIDTH-1:0] result
    );

    // Recursively generate a pair-wise reduction
    generate
        if(SIZE <= 0) begin : error_case
            DoesNotExit foo();
        end else if (SIZE == 1) begin : base_case_1
            assign result = inputs[0];
        end else if(SIZE == 2) begin : base_case_2
            assign result = inputs[0] | inputs[1];
        end else begin : recursive_case
            logic [1:0][WIDTH-1:0] subResults;
            ArrayOrReduction #(.WIDTH(WIDTH), .SIZE(SIZE/2)) top(.inputs(inputs[SIZE-1:SIZE/2]), .result(subResults[1]));
            ArrayOrReduction #(.WIDTH(WIDTH), .SIZE(SIZE/2)) bot(.inputs(inputs[SIZE/2-1:0]), .result(subResults[0]));
            assign result = subResults[0] | subResults[1];
        end
    endgenerate

endmodule


module Array #(parameter ELEMENTS=16, WIDTH=32)(
  // $clog2 is a built in function that does the natural log of 2
  input logic [$clog2(ELEMENTS)-1:0] index,
  input logic [WIDTH-1:0] element,
  output logic [ELEMENTS-1:0][WIDTH-1:0] array,
  input logic clock, clear, enable
  );

  localparam ZERO_ELEMENT = {WIDTH{1'b0}};

  always_ff @(posedge clock)
    if(clear)
      array <= {ELEMENTS{ZERO_ELEMENT}};
    else if(enable)
      array[index] <= element;

endmodule