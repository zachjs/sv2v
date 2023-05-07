// pattern: declaration dst uses interface name I where a type name is expected
// location: interface_name_func.sv:5:12
interface I;
    logic [3:0] x;
    task t(I dst, I src);
        dst.x <= src.x;
    endtask
endinterface
module top;
    I i();
endmodule
