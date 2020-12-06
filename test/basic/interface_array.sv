interface Interface(i);
    input i;
    logic v;
    logic o;
    task tick;
        $display("I i = %b, v = %b, o = %b", i, v, o);
    endtask
    initial $display("Hello I'm Interface!");
    modport ModportA(
        input .i(i ^ 1'b1),
        output v
    );
    modport ModportB(
        input .i(i),
        output .v(o)
    );
endinterface

module ModuleA(i);
    parameter flip = 0;
    Interface i;
    assign i.v = i.i ^ 1'(flip);
    task tick;
        $display("A i.v = %b", i.v);
    endtask
    initial $display("Hello I'm ModuleA %0d!", flip);
endmodule

module ModuleASet(is);
    parameter flip2 = 0;
    parameter flip1 = 0;
    parameter flip0 = 0;
    Interface is [2:0];
    assign is[2].v = is[2].i ^ 1'(flip2);
    assign is[1].v = is[1].i ^ 1'(flip1);
    assign is[0].v = is[0].i ^ 1'(flip0);
    task tick;
        $display("AS i.v = %b", is[2].v);
        $display("AS i.v = %b", is[1].v);
        $display("AS i.v = %b", is[0].v);
    endtask
    initial begin
        $display("Hello I'm ModuleASet %0d %0d %0d!", flip2, flip1, flip0);
    end
endmodule

module ModuleCSet(is);
    parameter flip2 = 0;
    parameter flip1 = 0;
    parameter flip0 = 0;
    Interface.ModportB is [2:0];
    assign is[2].v = is[2].i ^ 1'(flip2);
    assign is[1].v = is[1].i ^ 1'(flip1);
    assign is[0].v = is[0].i ^ 1'(flip0);
    task tick;
        $display("CS i.v = %b", is[2].v);
        $display("CS i.v = %b", is[1].v);
        $display("CS i.v = %b", is[0].v);
    endtask
    initial begin
        $display("Hello I'm ModuleCSet %0d %0d %0d!", flip2, flip1, flip0);
    end
endmodule

module ModuleB(is);
    parameter WIDTH = 1;
    Interface is [WIDTH-1:0];
    logic [WIDTH-1:0] i_concat;
    logic [WIDTH-1:0] v_concat;
    for (genvar i = WIDTH - 1; i >= 0; i = i - 1) begin
        assign i_concat[i] = is[i].i;
        assign v_concat[i] = is[i].v;
    end
    task tick;
        $display("B i_concat = %b, v_concat = %b", i_concat, v_concat);
        bn.tick;
    endtask
    initial $display("Hello I'm ModuleB %0d!", WIDTH);
    ModuleBNested #(WIDTH) bn(is);
endmodule

module ModuleBNested(is);
    parameter WIDTH = 1;
    Interface is [WIDTH-1:0];
    logic [WIDTH-1:0] i_concat;
    logic [WIDTH-1:0] v_concat;
    for (genvar i = WIDTH - 1; i >= 0; i = i - 1) begin
        assign i_concat[i] = is[i].i;
        assign v_concat[i] = is[i].v;
    end
    task tick;
        $display("BN i_concat = %b, v_concat = %b", i_concat, v_concat);
    endtask
endmodule

module top;
    logic inp;

    Interface intfX[2:0](inp);

    ModuleA #(0) xa2(intfX[2]);
    ModuleA #(1) xa1(intfX[1]);
    ModuleA #(1) xa0(intfX[0]);

    ModuleB #(3) xb20(intfX[2:0]);
    ModuleB #(2) xb21(intfX[2:1]);
    ModuleB #(1) xb22(intfX[2:2]);
    ModuleB #(1) xb11(intfX[1:1]);
    ModuleB #(1) xb00(intfX[0:0]);
    ModuleB #(3) xbf(intfX);

    ModuleASet #(1, 1, 0) xs(intfX[2:0].ModportB);

    Interface intfY[2:0](inp);

    ModuleA #(0) ya2(intfY[2].ModportA);
    ModuleA #(1) ya1(intfY[1].ModportA);
    ModuleA #(1) ya0(intfY[0].ModportA);

    ModuleB #(3) yb20(intfY[2:0].ModportA);
    ModuleB #(2) yb21(intfY[2:1].ModportA);
    ModuleB #(1) yb22(intfY[2:2].ModportA);
    ModuleB #(1) yb11(intfY[1:1].ModportA);
    ModuleB #(1) yb00(intfY[0:0].ModportA);
    ModuleB #(3) ybf(intfY.ModportA);

    ModuleCSet #(0, 0, 1) ys(intfY[2:0]);

    initial begin
        inp = 0; tick;
        inp = 1; tick;
        inp = 0; tick;
        inp = 1; tick;
    end

    task tick;
        #1;

        intfX[2].tick;
        intfX[1].tick;
        intfX[0].tick;

        xa2.tick;
        xa1.tick;
        xa0.tick;

        xb20.tick;
        xb21.tick;
        xb22.tick;
        xb11.tick;
        xb00.tick;
        xbf.tick;

        xs.tick;

        intfY[2].tick;
        intfY[1].tick;
        intfY[0].tick;

        ya2.tick;
        ya1.tick;
        ya0.tick;

        yb20.tick;
        yb21.tick;
        yb22.tick;
        yb11.tick;
        yb00.tick;
        ybf.tick;

        ys.tick;
    endtask
endmodule
