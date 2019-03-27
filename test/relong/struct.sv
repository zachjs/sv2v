`default_nettype none

typedef logic [31:0] Word_t;

// "packed" structs are effectively a single bit-vector that has syntactic sugar
// "for access specific parts
typedef struct packed {
    Word_t data;
    logic valid;
} InnerStruct_t;

typedef enum {MODE_A, MODE_B} Mode_t;

typedef struct packed {
  InnerStruct_t inner;
  Mode_t mode;
} OuterStruct_t;

module Example(
    input logic clock, clear,
    output logic success
);

    Word_t data_in, data_out;
    logic valid_in, valid_out;
    Mode_t mode_in, mode_out;
    OuterStruct_t object;

    always_ff @(posedge clock) begin
        if(clear) begin
            data_in <= 32'h0;
            valid_in <= 1'b0;
            mode_in <= MODE_A;
        end else if(mode_in == MODE_A) begin
            valid_in <= 1'b1;
            mode_in <= MODE_B;
            // $bits is a built-in SystemVerilog function that determines how
            // wide a data-type is (like sizeof in C)
            data_in <= data_in + $bits(object);
        end else begin
            mode_in <= MODE_A;
            data_in <= data_in + 32'h1;
            valid_in <= 1'b1;
        end
    end

    StructCompose compose(
        .data(data_in),
        .valid(valid_in),
        .mode(mode_in),
        .object
    );

    StructDecompose decompose(
        .data(data_out),
        .valid(valid_out),
        .mode(mode_out),
        .object
    );

    always_comb begin
        success = 1'b0;
        if(data_in == data_out)
            if(valid_in == valid_out)
                if(mode_in == mode_out)
                    success = 1'b1;
    end

endmodule

module StructCompose(
    input Word_t data,
    input logic valid,
    input Mode_t mode,
    output OuterStruct_t object
);

    always_comb begin
        object = '{
            inner: '{
                data: data,
                valid: valid
            },
            mode: mode
        };
    end

endmodule

module StructDecompose(
    input OuterStruct_t object,
    output Word_t data,
    output logic valid,
    output Mode_t mode
);

    always_comb begin
        data = object.inner.data;
        valid = object.inner.valid;
        mode = object.mode;
    end

endmodule