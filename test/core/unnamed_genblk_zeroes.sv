module example;
    initial $display("example");
endmodule

module top;
    wire genblk1;
    wire genblk33;
    (* foo = 1 *) wire genblk01;
    genvar genblk001;
    example genblk0001();

    wire x, y;
    wire o1, o2, o3;
    and genblk00001(o1, x, y);
    not genblk000001(o2, o3, o1);

    parameter genblk0000001 = 1;
`ifndef REF
    typedef logic genblk00000001;
`endif

`define BLK genblk000000001

    if (1) begin
`ifdef REF
        : `BLK
    reg
`else
    genblk00000001
`endif
        x = genblk0000001;
    end
    initial begin
        `BLK.x = 1;
        $display("%b", `BLK.x);
    end
endmodule
