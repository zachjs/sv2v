module top;
    typedef logic [1:0] T;
    function automatic [1:0] flip;
        input [1:0] inp;
        flip = ~inp;
    endfunction
    initial begin
        case (flip(2'b00)) inside
            2'b11: $display("good");
            default: $display("default");
        endcase
        case (T'(flip(2'b00))) inside
            2'b11: $display("good");
            default: $display("default");
        endcase
    end
endmodule
