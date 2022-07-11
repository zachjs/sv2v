module top;
    generate
        if (1) begin : i
            function automatic integer f;
                input unused;
                f = 1;
            endfunction
            if (1) begin : blk
                function automatic integer f;
                    input unused;
                    f = 2;
                endfunction
            end
        end
        function automatic integer f;
            input unused;
            f = 3;
        endfunction
        if (1) begin : blk
            function automatic integer f;
                input unused;
                f = 4;
            endfunction
        end
    endgenerate
    initial begin
        $display(f(0));
        $display(blk.f(0));
        $display(i.f(0));
        $display(i.blk.f(0));
        $display(top.f(0));
        $display(top.blk.f(0));
        $display(top.i.f(0));
        $display(top.i.blk.f(0));
    end
endmodule
