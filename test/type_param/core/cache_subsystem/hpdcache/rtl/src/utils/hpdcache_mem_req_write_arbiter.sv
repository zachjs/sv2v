/*
 *  Copyright 2023 CEA*
 *  *Commissariat a l'Energie Atomique et aux Energies Alternatives (CEA)
 *
 *  SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
 *
 *  Licensed under the Solderpad Hardware License v 2.1 (the “License”); you
 *  may not use this file except in compliance with the License, or, at your
 *  option, the Apache License version 2.0. You may obtain a copy of the
 *  License at
 *
 *  https://solderpad.org/licenses/SHL-2.1/
 *
 *  Unless required by applicable law or agreed to in writing, any work
 *  distributed under the License is distributed on an “AS IS” BASIS, WITHOUT
 *  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 *  License for the specific language governing permissions and limitations
 *  under the License.
 */
/*
 *  Authors       : Cesar Fuguet
 *  Creation Date : April, 2021
 *  Description   : Dcache Memory Write Channels Arbiter
 *  History       :
 */
module hpdcache_mem_req_write_arbiter
import hpdcache_pkg::*;
//  Parameters
//  {{{
#(
    parameter hpdcache_uint N = 0,
    parameter type hpdcache_mem_req_t = logic,
    parameter type hpdcache_mem_req_w_t = logic
)
//  }}}

//  Ports
//  {{{
(
    input  logic                  clk_i,
    input  logic                  rst_ni,

    output logic                 mem_req_write_ready_o      [N-1:0],
    input  logic                 mem_req_write_valid_i      [N-1:0],
    input  hpdcache_mem_req_t    mem_req_write_i            [N-1:0],

    output logic                 mem_req_write_data_ready_o [N-1:0],
    input  logic                 mem_req_write_data_valid_i [N-1:0],
    input  hpdcache_mem_req_w_t  mem_req_write_data_i       [N-1:0],

    input  logic                 mem_req_write_ready_i,
    output logic                 mem_req_write_valid_o,
    output hpdcache_mem_req_t    mem_req_write_o,

    input  logic                 mem_req_write_data_ready_i,
    output logic                 mem_req_write_data_valid_o,
    output hpdcache_mem_req_w_t  mem_req_write_data_o
);
//  }}}

    typedef enum {
        REQ_IDLE,
        REQ_META_SENT,
        REQ_DATA_SENT
    } req_send_fsm_t;

    req_send_fsm_t               req_send_fsm_q, req_send_fsm_d;
    logic                        req_valid;
    logic                        req_data_valid;

    logic                [N-1:0] mem_write_arb_req_valid;
    hpdcache_mem_req_t   [N-1:0] mem_write_arb_req;
    logic                [N-1:0] mem_write_arb_req_data_valid;
    hpdcache_mem_req_w_t [N-1:0] mem_write_arb_req_data;
    logic                [N-1:0] mem_write_arb_req_gnt;
    logic                        mem_write_arb_req_ready;

    genvar                       gen_i;


    generate
        for (gen_i = 0; gen_i < int'(N); gen_i++) begin : pack_inputs_gen
            assign mem_write_arb_req_valid     [gen_i] = mem_req_write_valid_i[gen_i],
                   mem_write_arb_req           [gen_i] = mem_req_write_i[gen_i],
                   mem_write_arb_req_data_valid[gen_i] = mem_req_write_data_valid_i[gen_i],
                   mem_write_arb_req_data      [gen_i] = mem_req_write_data_i[gen_i];
        end
    endgenerate

    //      Fixed-priority arbiter
    hpdcache_fxarb #(
        .N                   (2)
    ) hpdcache_fxarb_mem_req_write_i (
        .clk_i,
        .rst_ni,
        .req_i               (mem_write_arb_req_valid),
        .gnt_o               (mem_write_arb_req_gnt),
        .ready_i             (mem_write_arb_req_ready)
    );

    assign req_valid      = |(mem_write_arb_req_gnt & mem_write_arb_req_valid);
    assign req_data_valid = |(mem_write_arb_req_gnt & mem_write_arb_req_data_valid);

    //  Request sent FSM
    //
    //  This FSM allows to make sure that the request and its corresponding
    //  data are sent in order. This is, when a requester sends a request, this
    //  FSM keeps the grant signal on this requester until it has sent the
    //  corresponding data.
    //
    //  {{{
    always_comb
    begin : req_send_fsm_comb
        req_send_fsm_d = req_send_fsm_q;
        mem_write_arb_req_ready = 1'b0;
        case (req_send_fsm_q)
            REQ_IDLE:
                if (req_valid && mem_req_write_ready_i) begin
                    if (req_data_valid) begin
                        if (mem_req_write_data_ready_i) begin
                            mem_write_arb_req_ready = 1'b1;
                            req_send_fsm_d = REQ_IDLE;
                        end else begin
                            req_send_fsm_d = REQ_META_SENT;
                        end
                    end
                end else if (req_data_valid && mem_req_write_data_ready_i) begin
                    req_send_fsm_d = REQ_DATA_SENT;
                end

            REQ_META_SENT:
                if (req_data_valid && mem_req_write_data_ready_i) begin
                    mem_write_arb_req_ready = 1'b1;
                    req_send_fsm_d = REQ_IDLE;
                end

            REQ_DATA_SENT:
                if (req_valid && mem_req_write_ready_i) begin
                    mem_write_arb_req_ready = 1'b1;
                    req_send_fsm_d = REQ_IDLE;
                end
        endcase
    end

    always_ff @(posedge clk_i or negedge rst_ni)
    begin : req_send_fsm_ff
        if (!rst_ni) begin
            req_send_fsm_q <= REQ_IDLE;
        end else begin
            req_send_fsm_q <= req_send_fsm_d;
        end
    end
    //  }}}

    generate
        for (gen_i = 0; gen_i < int'(N); gen_i++) begin : req_ready_gen
            assign mem_req_write_ready_o[gen_i] =
                        (mem_write_arb_req_gnt[gen_i] & mem_req_write_ready_i) &
                        (req_send_fsm_q != REQ_META_SENT);

            assign mem_req_write_data_ready_o[gen_i] =
                        (mem_write_arb_req_gnt[gen_i] & mem_req_write_data_ready_i) &
                        (req_send_fsm_q != REQ_DATA_SENT);
        end
    endgenerate

    //  Output assignments
    //  {{{
    assign mem_req_write_valid_o      = req_valid      & (req_send_fsm_q != REQ_META_SENT);
    assign mem_req_write_data_valid_o = req_data_valid & (req_send_fsm_q != REQ_DATA_SENT);

    hpdcache_mux #(
        .NINPUT              (N),
        .DATA_WIDTH          ($bits(hpdcache_mem_req_t)),
        .ONE_HOT_SEL         (1'b1)
    ) mem_write_req_mux_i (
        .data_i              (mem_write_arb_req),
        .sel_i               (mem_write_arb_req_gnt),
        .data_o              (mem_req_write_o)
    );

    hpdcache_mux #(
        .NINPUT              (N),
        .DATA_WIDTH          ($bits(hpdcache_mem_req_w_t)),
        .ONE_HOT_SEL         (1'b1)
    ) mem_write_data_req_mux_i (
        .data_i              (mem_write_arb_req_data),
        .sel_i               (mem_write_arb_req_gnt),
        .data_o              (mem_req_write_data_o)
    );
    //  }}}

endmodule
