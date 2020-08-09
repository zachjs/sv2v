module top;
    wire x = 1;
    generate
        begin : f
            wire x;
            begin : a
                wire x;
                initial begin
                    $display("bar got %b", x);
                end
            end
            assign a.x = x;
            begin : b
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
