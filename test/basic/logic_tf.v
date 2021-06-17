module top;
    reg x, y;
    wire z;
    task t;
        x = 1;
    endtask
    function f;
        input x;
        begin
            y = 1;
            f = 0;
        end
    endfunction
    assign z = 0;
    initial begin
        t;
        $display("%b %b %b %b", x, y, z, f(0));
        $display("%b %b %b %b", x, y, z, f(0));
    end

    generate
        if (1) begin : A
            wire x;
            if (1) begin : B
                reg x;
            end
            if (1) begin : C
                wire x;
            end
            assign x = B.x ^ C.x;
        end
    endgenerate
    initial A.B.x = 0;
    assign A.C.x = 1;
    initial $display("%b %b %b %b", x, A.x, A.B.x, A.C.x);

    reg t2l;
    task t2;
        input reg t2l;
        top.t2l = t2l;
    endtask
    initial begin
        $display("%b", t2l);
        t2(1);
        $display("%b", t2l);
    end
endmodule
