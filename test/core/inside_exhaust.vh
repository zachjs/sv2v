function [WIDTH-1:0] incr;
    input [WIDTH-1:0] inp;
    reg carry;
    integer idx;
    begin
        carry = 1;
        idx = 0;
        while (carry) begin
            carry = 0;
            if (inp[idx] === 1'b0)
                inp[idx] = 1'b1;
            else if (inp[idx] === 1'b1)
                inp[idx] = 1'bx;
            else if (inp[idx] === 1'bx)
                inp[idx] = 1'bz;
            else begin
                inp[idx] = 1'b0;
                idx = idx + 1;
                carry = idx < WIDTH;
            end
        end
        incr = inp;
    end
endfunction

reg [WIDTH-1:0] a, b, c;
initial begin : x
    integer i, j, k;
    a = 0; b = 0; c = 0;

    for (i = 0; i < 4**WIDTH; ++i) begin
        for (j = 0; j < 4**WIDTH; ++j) begin
            $display("%b ==? %b = 1'b%b",
                a, b, test_weq(a, b));
            $display("%b !=? %b = 1'b%b",
                a, b, test_wne(a, b));
            for (k = 0; k < 4**WIDTH; ++k) begin
                $display("%b inside {%b, %b} = 1'b%b",
                    a, b, c, test_inside(a, b, c));
                c = incr(c);
            end
            b = incr(b);
        end
        a = incr(a);
    end
end
