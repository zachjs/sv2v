module test(a, b, en);
    output logic a;
    input logic b;
    input logic en;
    always_latch begin
        if (en) begin
            a <= b;
        end
    end
endmodule
