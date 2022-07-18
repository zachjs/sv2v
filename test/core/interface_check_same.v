`define TEST(loc, mod, expr) \
    initial begin \
        $display(`"loc mod.expr %b`", i.expr); \
        repeat (2) \
            $display(`"loc loc.mod.expr %b`", top.i.expr); \
    end

`define TEST_FUNC(loc, mod, expr) \
    initial begin \
        $display(`"loc mod.expr() %b`", i.expr(0)); \
        repeat (2) \
            $display(`"loc loc.mod.expr() %b`", top.i.expr(0)); \
    end

`define TEST_TASK(loc, mod, expr) \
    initial begin \
        $display(`"loc mod.expr():`"); \
        i.expr(); \
        $display(`"loc loc.mod.expr():`"); \
        top.i.expr(); \
    end

module top;
    if (1) begin : i
        localparam [3:0] P = 1;
        localparam [3:0] L = 2;
        wire [3:0] w;
        assign w = 3;
        wire [7:0] x;
        assign x = w + 8'b1;
        function automatic [3:0] F;
            input unused;
            F = -1;
        endfunction
        task T;
            $display("T called");
        endtask

        if (1) begin : blk
            localparam [3:0] P = 4;
            localparam [3:0] L = 6;
            reg [3:0] w;
            initial w = 7;
            function automatic [3:0] F;
                input unused;
                F = 8;
            endfunction
            task T;
                $display("blk.T called");
            endtask
        end
    end

    `TEST(ModAi, i, P)
    `TEST(ModAi, i, L)
    `TEST(ModAi, i, w)
    `TEST_FUNC(ModAi, i, F)
    `TEST_TASK(ModAi, i, T)

    `TEST(ModAi, i, blk.P)
    `TEST(ModAi, i, blk.L)
    `TEST(ModAi, i, blk.w)
    `TEST_FUNC(ModAi, i, blk.F)
    `TEST_TASK(ModAi, i, blk.T)

    `TEST(ModAj, j, P)
    `TEST(ModAj, j, L)
    `TEST(ModAj, j, w)
    `TEST_FUNC(ModAj, j, F)
    `TEST_TASK(ModAj, j, T)

    `TEST(ModAj, j, blk.P)
    `TEST(ModAj, j, blk.L)
    `TEST(ModAj, j, blk.w)
    `TEST_FUNC(ModAj, j, blk.F)
    `TEST_TASK(ModAj, j, blk.T)

    `TEST(ModBi, i, P)
    `TEST(ModBi, i, L)
    `TEST(ModBi, i, x)

    `TEST(ModBj, j, P)
    `TEST(ModBj, j, L)
    `TEST(ModBj, j, x)

    `TEST(top, i, P)
    `TEST(top, i, L)
    `TEST(top, i, w)
    `TEST_FUNC(top, i, F)
    `TEST_TASK(top, i, T)

    `TEST(top, i, blk.P)
    `TEST(top, i, blk.L)
    `TEST(top, i, blk.w)
    `TEST_FUNC(top, i, blk.F)
    `TEST_TASK(top, i, blk.T)
endmodule
