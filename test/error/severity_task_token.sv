// pattern: unexpected severity task token
// location: severity_task_token.sv:5:16
module top;
    task t;
        $fatal x;
    endtask
endmodule
