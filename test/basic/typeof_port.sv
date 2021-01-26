module Example(inp, out);
    input inp;
    output out;
    type(inp) data;
    assign data = ~inp;
    assign out = data;
endmodule
