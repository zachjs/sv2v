`default_nettype none

// Technically the value assignment could be anything, but most tools default to a 32-bit logic assigning MODE_A = 0 and MODE_B = 1
typedef enum {MODE_A, MODE_B} Mode_t;

typedef enum logic [1:0] {READ=2'd1, WRITE=2'd2, NONE=2'd0} Operation_t;

module Example(
    input logic rawMode,
    output logic [1:0] rawOperation
);

    Mode_t mode;
    Operation_t operation;

    // cast into a strongly typed variant
    assign mode = Mode_t'(rawMode);
    assign rawOperation = operation;

    always_comb begin
        case(mode)
            MODE_A: operation = READ;
            MODE_B: operation = WRITE;
            default: operation = NONE;
        endcase
    end

endmodule