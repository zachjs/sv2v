module foo(clock, data);
    input clock;
    output reg [54:0] data;
    initial data[0] = 0;
    always @(clock) begin : block_name
        integer i;
        for (i = 53; i >= 0; i = i - 1) begin
            data[i+1] = data[i];
        end
        data[0] = ~data[0];
    end
endmodule
