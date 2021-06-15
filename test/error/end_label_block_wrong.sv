// pattern: element "yes" has mismatched end label "no"
module top;
    initial begin : yes
        $display("Hi!");
    end : no
endmodule
