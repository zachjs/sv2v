`default_nettype none

module ArrayOrReduction #(
    parameter SIZE = 16, WIDTH=32) (
    // Flattened array
    input wire [(SIZE*WIDTH)-1:0] inputs,
    output wire [WIDTH-1:0] result
    );

    // One option is to modify the code to use the flattened array, but probably
    // an easier option is to just insert a generate block to unflatten
    // everything.
    wire [WIDTH-1:0] __inputs[SIZE-1:0];
    genvar index;
    generate
        for(index = 0; index < SIZE; index = index + 32'd1) begin : unflatten
            localparam START_BIT = index * WIDTH;
            assign __inputs[index] = inputs[START_BIT + WIDTH - 1:START_BIT];
        end
    endgenerate

    // Recursively generate a pair-wise reduction
    generate
        if(SIZE <= 0) begin : error_case
            DoesNotExit foo();
        end else if (SIZE == 1) begin : base_case_1
            assign result = __inputs[0];
        end else if(SIZE == 2) begin : base_case_2
            assign result = __inputs[0] | __inputs[1];
        end else begin : recursive_case
            wire [WIDTH-1:0] subResults [1:0];
            // The array needs to be re-flattened here or alternatively, just use computation on the original array
            ArrayOrReduction #(.WIDTH(WIDTH), .SIZE(SIZE/2)) top(.inputs(inputs[(SIZE*WIDTH)-1:(SIZE*WIDTH)/2]), .result(subResults[1]));
            ArrayOrReduction #(.WIDTH(WIDTH), .SIZE(SIZE/2)) bot(.inputs(inputs[(SIZE * WIDTH)/2-1:0]), .result(subResults[0]));
            assign result = subResults[0] | subResults[1];
        end
    endgenerate

endmodule


module Array #(parameter ELEMENTS=16, WIDTH=32)(

  input wire [$clog2(ELEMENTS)-1:0] index,
  input wire [WIDTH-1:0] element,
  // Flattened array
  output wire [(ELEMENTS*WIDTH)-1:0] array,
  input wire clock, clear, enable
);

    reg [WIDTH-1:0] __array[ELEMENTS-1:0];
    genvar g_index;
    generate
        for(g_index = 0; g_index < ELEMENTS; g_index = g_index + 32'd1) begin : unflatten
            localparam START_BIT = g_index * WIDTH;
            assign array[START_BIT + WIDTH - 1:START_BIT] = __array[g_index];
        end
    endgenerate

  localparam ZERO_ELEMENT = {WIDTH{1'b0}};

  // I think this might synthesize correctly, but it is pretty gross.
  integer reset_index;
  always @(posedge clock)
    if(clear) begin
      for(reset_index = 0; reset_index < ELEMENTS; reset_index = reset_index + 32'd1)
        __array[reset_index] <= {ZERO_ELEMENT};
    end else if(enable)
      __array[index] <= element;

endmodule
