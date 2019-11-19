module test(a, b, en);
    output reg a;
    input wire b;
    input wire en;
    always @(*) begin
        if (en) begin
            a <= b;
        end
    end
endmodule
