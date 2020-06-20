interface I;
    task x;
        input logic [31:0] i;
        $display("I x(%0d)", i);
    endtask
endinterface

module top;
    I i();
    logic [31:0] w = 31;
    logic [31:0] y = 42;
    typedef struct packed { logic [31:0] x, y; } E;
    task x;
        input E i;
        $display("x('{%0d, %0d})", i.x, i.y);
    endtask
    task automatic z;
        input E t;
        begin : foo
            E i = t;
            $display("z('{%0d, %0d})", i.x, i.y);
        end
    endtask
    initial begin
        i.x(y);
        x({w, y});
        z({w, y});
    end
endmodule
