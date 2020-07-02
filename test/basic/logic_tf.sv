module top;
    logic x, y, z;
    task t;
        x = 1;
    endtask
    function f;
        y = 1;
        f = 0;
    endfunction
    assign z = 0;
    initial begin
        t;
        $display("%b %b %b %b", x, y, z, f());
        $display("%b %b %b %b", x, y, z, f());
    end

    generate
        begin : A
            logic x;
            begin : B
                logic x;
            end
            begin : C
                logic x;
            end
            assign x = B.x ^ C.x;
        end
    endgenerate
    initial A.B.x = 0;
    assign A.C.x = 1;
    initial $display("%b %b %b %b", x, A.x, A.B.x, A.C.x);

    logic t2l;
    task t2;
        input logic t2l;
        top.t2l = t2l;
    endtask
    initial begin
        $display("%b", t2l);
        t2(1);
        $display("%b", t2l);
    end
endmodule
