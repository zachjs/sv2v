interface intf_i;
    typedef int data_t;
endinterface

module sub(intf_i p, intf_i q [2]);
    typedef p.data_t p_data_t; // interface based typedef
    typedef q[0].data_t q_data_t; // interface based typedef
    p_data_t p_data;
    q_data_t q_data;
    initial begin
        p_data = 1;
        q_data = 2;
        $display("p %0d %b", $bits(p_data), p_data);
        $display("q %0d %b", $bits(q_data), q_data);
    end
endmodule

module top;
    intf_i p();
    intf_i q[2]();
    sub s(p, q);
endmodule
