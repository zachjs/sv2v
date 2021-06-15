// pattern: element "yes" has mismatched end label "no"
module top;
    if (1) begin : yes
        wire x;
    end : no
endmodule
