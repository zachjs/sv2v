module top;
    task t;
        $display("t %b", 8'hFF);
    endtask
    function integer f;
        input reg signed i;
        reg [15:0] j;
        begin
            j = i;
            $display("i %b", j);
            f = 16;
        end
    endfunction
    initial begin
        t;
        $display("f %b", f(0));
        $display("f %b", f(1));
    end
endmodule
