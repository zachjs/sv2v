module top;
    function [1:0] field_next;
        input [1:0] v;
        begin
            case (v)
                2'b00: field_next = 2'b01;
                2'b01: field_next = 2'b10;
                2'b10: field_next = 2'b11;
                default: field_next = 2'b00;
            endcase
        end
    endfunction

    function [1:0] field_prev;
        input [1:0] v;
        begin
            case (v)
                2'b00: field_prev = 2'b11;
                2'b01: field_prev = 2'b00;
                2'b10: field_prev = 2'b01;
                default: field_prev = 2'b10;
            endcase
        end
    endfunction

    function [1:0] field_prev2;
        input [1:0] v;
        begin
            field_prev2 = field_prev(field_prev(v));
        end
    endfunction

    reg [1:0] q;
    reg [1:0] r;
    reg [1:0] s;
    integer n;

    initial begin
        q = 2'b01;
        r = field_next(q);
        s = field_prev(q);
        n = 4;
        $display("next=%0d prev=%0d first=%0d last=%0d num=%0d",
            r, s, 2'b00, 2'b11, n);
        q = 2'b11;
        r = field_next(q);
        s = field_prev2(q);
        $display("wrap_next=%0d prev2=%0d", r, s);
    end
endmodule
