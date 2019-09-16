(* a=1 *) module top;
    (* foo="bar" *) logic x;
    initial begin
        x = 1;
        $display(x);
    end
endmodule
