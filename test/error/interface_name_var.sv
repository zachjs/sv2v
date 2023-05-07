// pattern: declaration i uses interface name I where a type name is expected
// location: interface_name_var.sv:7:12
interface I;
    logic [3:0] x;
endinterface
module top;
    if (1) var I i;
endmodule
