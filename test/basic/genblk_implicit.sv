module top;
    if (1) begin
        // should not be visible in a top-level VCD
        wire x;
        assign x = 1;
    end
endmodule
