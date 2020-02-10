module Module(input wire x);
    initial $display("Module %d", x);
endmodule
module top;
    wire i_x;
    localparam SOME_VAL = 3;
    initial $display("Interface %d %d", i_x, SOME_VAL);
    Module m(.x(i_x));
    generate
        genvar g;
        for (g = 0; g < 5; g = g + 1) begin
            initial $display(g);
        end
        for (g = 10; g < 15; g = g + 1) begin
            initial $display(g);
        end
    endgenerate
endmodule
