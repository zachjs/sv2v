// This test was adapted from Section 27.6 of IEEE 1800-2017

module mod;
    initial $dumpvars(0, mod);
    // needed because of steveicarus/iverilog#528
    `ifdef __ICARUS__
        `define BEGIN(name) begin : name
        `define END end
    `else
        `define BEGIN(name)
        `define END
    `endif

    parameter genblk2 = 0;
    genvar i;

    // The following generate block is implicitly named genblk1

    if (genblk2) `BEGIN(genblk1) logic a; `END // mod.genblk1.a
    else `BEGIN(genblk1) logic b; `END // mod.genblk1.b

    // The following generate block is implicitly named genblk02
    // as genblk2 is already a declared identifier

    if (genblk2) `BEGIN(genblk02) logic a; `END // mod.genblk02.a
    else `BEGIN(genblk02) logic b; `END // mod.genblk02.b

    // The following generate block would have been named genblk3
    // but is explicitly named g1

    for (i = 0; i < 1; i = i + 1) begin : g1 // block name
        // The following generate block is implicitly named genblk1
        // as the first nested scope inside g1
        if (1) `BEGIN(genblk1) logic a; `END // mod.g1[0].genblk1.a
    end

    // The following generate block is implicitly named genblk4 since
    // it belongs to the fourth generate construct in scope "mod".
    // The previous generate block would have been
    // named genblk3 if it had not been explicitly named g1

    for (i = 0; i < 1; i = i + 1) `BEGIN(genblk4)
        // The following generate block is implicitly named genblk1
        // as the first nested generate block in genblk4
        if (1) `BEGIN(genblk1) logic a; `END // mod.genblk4[0].genblk1.a
    `END

    // The following generate block is implicitly named genblk5
    if (1) `BEGIN(genblk5) logic a; `END // mod.genblk5.a
endmodule

module top;
    mod #(0) m0();
    mod #(1) m1();
endmodule