module top;
    reg inp;

    initial begin
        $display("Hello I'm Interface!");
        $display("Hello I'm Interface!");
        $display("Hello I'm Interface!");

        $display("Hello I'm ModuleA 0!");
        $display("Hello I'm ModuleA 1!");
        $display("Hello I'm ModuleA 1!");

        $display("Hello I'm ModuleB 3!");
        $display("Hello I'm ModuleB 2!");
        $display("Hello I'm ModuleB 1!");
        $display("Hello I'm ModuleB 1!");
        $display("Hello I'm ModuleB 1!");
        $display("Hello I'm ModuleB 3!");

        $display("Hello I'm ModuleASet 1 1 0!");

        $display("Hello I'm Interface!");
        $display("Hello I'm Interface!");
        $display("Hello I'm Interface!");

        $display("Hello I'm ModuleA 0!");
        $display("Hello I'm ModuleA 1!");
        $display("Hello I'm ModuleA 1!");

        $display("Hello I'm ModuleB 3!");
        $display("Hello I'm ModuleB 2!");
        $display("Hello I'm ModuleB 1!");
        $display("Hello I'm ModuleB 1!");
        $display("Hello I'm ModuleB 1!");
        $display("Hello I'm ModuleB 3!");

        $display("Hello I'm ModuleCSet 0 0 1!");

        inp = 0; tick;
        inp = 1; tick;
        inp = 0; tick;
        inp = 1; tick;
    end

    task tick; begin
        #1;

        $display("I i = %b, v = %b, o = %b", inp, inp, inp ^ 1'b1);
        $display("I i = %b, v = %b, o = %b", inp, inp ^ 1'b1, inp ^ 1'b1);
        $display("I i = %b, v = %b, o = %b", inp, inp ^ 1'b1, inp);

        $display("A i.v = %b", inp);
        $display("A i.v = %b", inp ^ 1'b1);
        $display("A i.v = %b", inp ^ 1'b1);

        $display("B i_concat = %b, v_concat = %b", {3 {inp}}, {inp, inp ^ 1'b1, inp ^ 1'b1});
        $display("BN i_concat = %b, v_concat = %b", {3 {inp}}, {inp, inp ^ 1'b1, inp ^ 1'b1});
        $display("B i_concat = %b, v_concat = %b", {2 {inp}}, {inp, inp ^ 1'b1});
        $display("BN i_concat = %b, v_concat = %b", {2 {inp}}, {inp, inp ^ 1'b1});
        $display("B i_concat = %b, v_concat = %b", {1 {inp}}, inp);
        $display("BN i_concat = %b, v_concat = %b", {1 {inp}}, inp);
        $display("B i_concat = %b, v_concat = %b", {1 {inp}}, inp ^ 1'b1);
        $display("BN i_concat = %b, v_concat = %b", {1 {inp}}, inp ^ 1'b1);
        $display("B i_concat = %b, v_concat = %b", {1 {inp}}, inp ^ 1'b1);
        $display("BN i_concat = %b, v_concat = %b", {1 {inp}}, inp ^ 1'b1);
        $display("B i_concat = %b, v_concat = %b", {3 {inp}}, {inp, inp ^ 1'b1, inp ^ 1'b1});
        $display("BN i_concat = %b, v_concat = %b", {3 {inp}}, {inp, inp ^ 1'b1, inp ^ 1'b1});

        $display("AS i.v = %b", inp ^ 1'b1);
        $display("AS i.v = %b", inp ^ 1'b1);
        $display("AS i.v = %b", inp);

        $display("I i = %b, v = %b, o = %b", inp, inp ^ 1'b1, inp);
        $display("I i = %b, v = %b, o = %b", inp, inp, inp);
        $display("I i = %b, v = %b, o = %b", inp, inp, inp ^ 1'b1);

        $display("A i.v = %b", inp ^ 1'b1);
        $display("A i.v = %b", inp);
        $display("A i.v = %b", inp);

        $display("B i_concat = %b, v_concat = %b", {3 {~inp}}, {inp ^ 1'b1, inp, inp});
        $display("BN i_concat = %b, v_concat = %b", {3 {~inp}}, {inp ^ 1'b1, inp, inp});
        $display("B i_concat = %b, v_concat = %b", {2 {~inp}}, {inp ^ 1'b1, inp});
        $display("BN i_concat = %b, v_concat = %b", {2 {~inp}}, {inp ^ 1'b1, inp});
        $display("B i_concat = %b, v_concat = %b", {1 {~inp}}, inp ^ 1'b1);
        $display("BN i_concat = %b, v_concat = %b", {1 {~inp}}, inp ^ 1'b1);
        $display("B i_concat = %b, v_concat = %b", {1 {~inp}}, inp);
        $display("BN i_concat = %b, v_concat = %b", {1 {~inp}}, inp);
        $display("B i_concat = %b, v_concat = %b", {1 {~inp}}, inp);
        $display("BN i_concat = %b, v_concat = %b", {1 {~inp}}, inp);
        $display("B i_concat = %b, v_concat = %b", {3 {~inp}}, {inp ^ 1'b1, inp, inp});
        $display("BN i_concat = %b, v_concat = %b", {3 {~inp}}, {inp ^ 1'b1, inp, inp});

        $display("CS i.v = %b", inp);
        $display("CS i.v = %b", inp);
        $display("CS i.v = %b", inp ^ 1'b1);
    end
    endtask
endmodule
