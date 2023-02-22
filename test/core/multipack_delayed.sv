module mod(inp, out);
    parameter COUNT = 4;
    typedef struct packed {
        logic a, b, c;
    } struct_t;
    typedef union packed {
        logic [2:0] raw;
        struct_t fields;
    } union_t;
    input union_t [COUNT - 1:0] inp;
    output logic [COUNT - 1:0] out;
    for (genvar i = 0; i < COUNT; i += 1)
        always_comb
            if ((inp[i].raw & 3'b011) != '0)
                out[i] = inp[i].fields.a ^ inp[i].fields.b;
endmodule
