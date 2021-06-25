module Example(inp, out);
    input inp;
    output out;
    wire data;
    assign data = ~inp;
    assign out = data;
endmodule
