module impl;
    reg [4:0] im_x = 0;
    task im_hello;
        input integer inp;
        begin
            im_x = im_x + 1;
            $display("Hello from InterfaceM %0d %b", inp, im_x);
        end
    endtask

    initial #4 im_hello(1);
    initial #5 $display("Module got %b", im_x[0]);
    initial #4 im_hello(1);
    initial #5 $display("Module got %b", im_x[2:1]);
    initial #4 im_hello(1);
    initial #5 $display("Module got %b", im_x);

    initial im_hello(-1);
    initial $display("ModuleM got %b", im_x[0]);
    initial im_hello(-1);
    initial $display("ModuleM got %b", im_x[2:1]);
    initial im_hello(-1);
    initial $display("ModuleM got %b", im_x);

    task ia_hello;
        input integer inp;
        $display("Hello from InterfaceA %0d", inp);
    endtask
    wire [20:0] ia_x = 21'b01011_00100000_01011110;

    task ib_hello;
        input integer inp;
        $display("Hello from InterfaceB %0d", inp);
    endtask
    wire [10:0] ib_x = 11'b011_11110100;

    initial #4 ia_hello(1);
    initial #5 $display("Module got %b", ia_x);
    initial #4 ib_hello(1);
    initial #5 $display("Module got %b", ib_x);

    initial #4 im_hello(1);
    initial #5 $display("Module got %b", im_x[0]);
    initial #4 im_hello(1);
    initial #5 $display("Module got %b", im_x[2:1]);
    initial #4 im_hello(1);
    initial #5 $display("Module got %b", im_x);

    initial im_hello(-1);
    initial $display("ModuleM got %b", im_x[0]);
    initial im_hello(-1);
    initial $display("ModuleM got %b", im_x[2:1]);
    initial im_hello(-1);
    initial $display("ModuleM got %b", im_x);
endmodule

module top;
    impl impl();
endmodule
