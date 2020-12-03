interface Interface(data);
    input logic data;
    initial #1 $display("Interface %b", data);
endinterface

module Module(intf);
    Interface intf;
    initial #2 $display("Module %b", intf.data);
endmodule

module top;
    logic data;
    Interface intf(.*);
    Module m(.*);
    initial begin
        data = 0;
        #1;
        data = 1;
        #1;
    end
endmodule
