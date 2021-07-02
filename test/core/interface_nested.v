module top;
    reg x = 1;
    generate
        if (1) begin : f
            wire x;
            if (1) begin : a
                wire x;
                initial begin
                    $display("bar got %b", x);
                end
            end
            assign a.x = x;
            if (1) begin : b
                wire x;
                initial begin
                    $display("bar got %b", x);
                end
            end
            assign b.x = ~x;
        end
        assign f.x = x;
    endgenerate
endmodule
