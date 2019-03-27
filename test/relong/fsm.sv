`default_nettype none

module FSM(
    input logic a,
    output logic x,
    input logic clock, clear
);

    enum {S_A, S_B, S_C} currentState, nextState;

    always_ff @(posedge clock)
        if(clear) begin
            currentState <= S_A;
        end else begin
            currentState <= nextState;
        end

    always_comb begin
        nextState = currentState;
        unique case(currentState)
            S_A: nextState = a ? S_B : S_C;
            S_B: nextState = a ? S_A : S_B;
            S_C: nextState = S_A;
        endcase
    end

    always_comb begin
        x = 1'b0;
        unique case(currentState)
            S_A: x = ~a;
            S_B: x = 1'b1;
            S_C: x = 1'b0;
        endcase
    end

endmodule