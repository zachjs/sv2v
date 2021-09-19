interface intf_i;
    typedef int data_t;
endinterface

interface intf_j;
    typedef logic [1:0] data_t;
    logic dummy;
    modport m(input dummy);
endinterface

module sub(interface p, interface q [2]);
    typedef p.data_t p_data_t; // interface based typedef
    typedef q[0].data_t q_data_t; // interface based typedef
    p_data_t p_data;
    q_data_t q_data;
    p_data_t p_data_arr [2];
    q_data_t q_data_arr [2];
    initial begin
        p_data = 1;
        q_data = 2;
        p_data_arr[0] = 2;
        q_data_arr[0] = 2;
        $display("p %0d %b", $bits(p_data), p_data);
        $display("q %0d %b", $bits(q_data), q_data);
        $display("p %0d", $bits(p_data_arr));
        $display("q %0d", $bits(q_data_arr));
    end
endmodule

module top;
    intf_i p();
    intf_i q[2]();
    sub si(p, q);
    intf_j r();
    intf_j s[2]();
    sub sj(r.m, s);
endmodule
