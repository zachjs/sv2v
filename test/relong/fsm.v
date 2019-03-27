`default_nettype none

module FSM(
    input wire a,
    output reg x,
    input wire clock, clear
);

    parameter S_A = 32'd0, S_B = 32'd1, S_C = 32'd2;
    reg [31:0] currentState, nextState;

    always @(posedge clock)
        if(clear) begin
            currentState <= S_A;
        end else begin
            currentState <= nextState;
        end

    always @* begin
        nextState = currentState;
        case(currentState)
            S_A: nextState = a ? S_B : S_C;
            S_B: nextState = a ? S_A : S_B;
            S_C: nextState = S_A;
        endcase
    end

    always @* begin
        x = 1'b0;
        case(currentState)
            S_A: x = ~a;
            S_B: x = 1'b1;
            S_C: x = 1'b0;
        endcase
    end

endmodule