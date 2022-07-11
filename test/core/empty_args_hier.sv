interface intf;
    function automatic integer f;
        return 1;
    endfunction
    if (1) begin : blk
        function automatic integer f;
            return 2;
        endfunction
    end
endinterface

module top;
    intf i();
    function automatic integer f;
        return 3;
    endfunction
    if (1) begin : blk
        function automatic integer f;
            return 4;
        endfunction
    end
    initial begin
        $display(f());
        $display(blk.f());
        $display(i.f());
        $display(i.blk.f());
        $display(top.f());
        $display(top.blk.f());
        $display(top.i.f());
        $display(top.i.blk.f());
    end
endmodule
