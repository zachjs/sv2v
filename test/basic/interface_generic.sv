interface InterfaceA;
    task hello;
        input integer inp;
        $display("Hello from InterfaceA %0d", inp);
    endtask
    logic [20:0] x = 21'b01011_00100000_01011110;
endinterface

interface InterfaceB;
    task hello;
        input integer inp;
        $display("Hello from InterfaceB %0d", inp);
    endtask
    logic [10:0] x = 11'b011_11110100;
endinterface

// could get bound to any of the interfaces or modports
module Module(interface i);
    initial #4 i.hello(1);
    initial #5 $display("Module got %b", i.x);
endmodule

interface InterfaceM;
    logic [4:0] x = 0;
    task hello;
        input integer inp;
        x = x + 1;
        $display("Hello from InterfaceM %0d %b", inp, x);
    endtask
    modport A(input .x(x[0]));
    modport B(input .x(x[2:1]));
endinterface

// could get bound to the entire interface bundle, or either modport
module ModuleM(InterfaceM i);
    initial i.hello(-1);
    initial $display("ModuleM got %b", i.x);
endmodule

module ModuleWrapperMA(InterfaceM.A i); Module m(i); endmodule
module ModuleWrapperMB(InterfaceM.B i); Module m(i); endmodule
module ModuleWrapperM (InterfaceM   i); Module m(i); endmodule

module ModuleMWrapperMA(InterfaceM.A i); ModuleM m(i); endmodule
module ModuleMWrapperMB(InterfaceM.B i); ModuleM m(i); endmodule
module ModuleMWrapperM (InterfaceM   i); ModuleM m(i); endmodule

module top;
    InterfaceM im();

    Module c(im.A);
    Module d(im.B);
    Module e(im);

    ModuleM x(im.A);
    ModuleM y(im.B);
    ModuleM z(im);

    InterfaceA ia();
    InterfaceB ib();

    Module a(ia);
    Module b(ib);

    ModuleWrapperMA cw(im);
    ModuleWrapperMB dw(im);
    ModuleWrapperM  ew(im);

    ModuleMWrapperMA xw(im);
    ModuleMWrapperMB yw(im);
    ModuleMWrapperM  zw(im);
endmodule
