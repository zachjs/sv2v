class C #(
    parameter X
);
    localparam Y = X * 2;
endclass : C

package P;
    localparam Z = 3;
endpackage : P

`define DUMP(expr) initial $display(`"expr = %0d`", expr);

interface intf;
    initial $display("intf");
endinterface : intf

module top;
    intf i();
    function automatic integer f;
        input integer inp;
        return inp * 8;
    endfunction : f
    task t;
        $display("t()");
    endtask : t
    localparam W = 4;
    `DUMP(W)
    `DUMP(C#(3)::X)
    `DUMP(C#(3)::Y)
    `DUMP(P::Z)
    `DUMP(f(3))
    initial t();
    initial begin : blkA
        $display("Hello!");
    end : blkA
    if (1) begin : blkB
        initial $display("Bye!");
    end : blkB
    initial blkC : begin
        $display("No!");
    end : blkC
    if (1) blkD : begin
        initial $display("Way!");
    end : blkD
endmodule : top
