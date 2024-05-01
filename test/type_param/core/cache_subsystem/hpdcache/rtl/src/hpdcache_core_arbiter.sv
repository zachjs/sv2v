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
 *  Creation Date : September, 2023
 *  Description   : HPDcache request arbiter
 *  History       :
 */
module hpdcache_core_arbiter
import hpdcache_pkg::*;
    //  Parameters
    //  {{{
#(
    parameter int NREQUESTERS = 1
)
    //  }}}

    //  Ports
    //  {{{
(
    //      Clock and reset signals
    input  logic                          clk_i,
    input  logic                          rst_ni,

    //      Core request interface
    //         1st cycle
    input  logic                          core_req_valid_i [NREQUESTERS-1:0],
    output logic                          core_req_ready_o [NREQUESTERS-1:0],
    input  hpdcache_req_t                 core_req_i       [NREQUESTERS-1:0],
    //         2nd cycle
    input  logic                          core_req_abort_i [NREQUESTERS-1:0],
    input  hpdcache_tag_t                 core_req_tag_i   [NREQUESTERS-1:0],
    input  hpdcache_pma_t                 core_req_pma_i   [NREQUESTERS-1:0],

    //      Core response interface
    input  logic                          core_rsp_valid_i,
    input  hpdcache_rsp_t                 core_rsp_i,
    output logic                          core_rsp_valid_o [NREQUESTERS-1:0],
    output hpdcache_rsp_t                 core_rsp_o       [NREQUESTERS-1:0],

    //      Granted request
    output logic                          arb_req_valid_o,
    input  logic                          arb_req_ready_i,
    output hpdcache_req_t                 arb_req_o,
    output logic                          arb_abort_o,
    output hpdcache_tag_t                 arb_tag_o,
    output hpdcache_pma_t                 arb_pma_o
);

    //  }}}

    //  Declaration of internal signals
    //  {{{
    logic          [NREQUESTERS-1:0] core_req_valid;
    hpdcache_req_t [NREQUESTERS-1:0] core_req;
    logic          [NREQUESTERS-1:0] core_req_abort;
    hpdcache_tag_t [NREQUESTERS-1:0] core_req_tag;
    hpdcache_pma_t [NREQUESTERS-1:0] core_req_pma;

    logic [NREQUESTERS-1:0] arb_req_gnt_q, arb_req_gnt_d;
    //  }}}

    //  Requesters arbiter
    //  {{{
    //      Pack request ports
    genvar gen_i;

    generate
        for (gen_i = 0; gen_i < int'(NREQUESTERS); gen_i++) begin : gen_core_req
            assign core_req_ready_o[gen_i] = arb_req_gnt_d[gen_i] & arb_req_ready_i,
                   core_req_valid[gen_i]   = core_req_valid_i[gen_i],
                   core_req[gen_i]         = core_req_i[gen_i];

            assign core_req_abort[gen_i]   = core_req_abort_i[gen_i],
                   core_req_tag[gen_i]     = core_req_tag_i[gen_i],
                   core_req_pma[gen_i]     = core_req_pma_i[gen_i];
        end
    endgenerate

    //      Arbiter
    hpdcache_fxarb #(.N(NREQUESTERS)) req_arbiter_i
    (
        .clk_i,
        .rst_ni,
        .req_i          (core_req_valid),
        .gnt_o          (arb_req_gnt_d),
        .ready_i        (arb_req_ready_i)
    );

    //      Request multiplexor
    hpdcache_mux #(
        .NINPUT         (NREQUESTERS),
        .DATA_WIDTH     ($bits(hpdcache_req_t)),
        .ONE_HOT_SEL    (1'b1)
    ) core_req_mux_i (
        .data_i         (core_req),
        .sel_i          (arb_req_gnt_d),
        .data_o         (arb_req_o)
    );

    //      Request abort multiplexor
    hpdcache_mux #(
        .NINPUT         (NREQUESTERS),
        .DATA_WIDTH     (1),
        .ONE_HOT_SEL    (1'b1)
    ) core_req_abort_mux_i (
        .data_i         (core_req_abort),
        .sel_i          (arb_req_gnt_q),
        .data_o         (arb_abort_o)
    );

    //      Tag Multiplexor
    hpdcache_mux #(
        .NINPUT         (NREQUESTERS),
        .DATA_WIDTH     ($bits(hpdcache_tag_t)),
        .ONE_HOT_SEL    (1'b1)
    ) core_req_tag_mux_i (
        .data_i         (core_req_tag),
        .sel_i          (arb_req_gnt_q),
        .data_o         (arb_tag_o)
    );

    //      PMA Multiplexor
    hpdcache_mux #(
        .NINPUT         (NREQUESTERS),
        .DATA_WIDTH     ($bits(hpdcache_pma_t)),
        .ONE_HOT_SEL    (1'b1)
    ) core_req_pma_mux_i (
        .data_i         (core_req_pma),
        .sel_i          (arb_req_gnt_q),
        .data_o         (arb_pma_o)
    );

    //      Save the grant signal for the tag in the next cycle
    always_ff @(posedge clk_i or negedge rst_ni)
    begin : arb_req_gnt_ff
        if (!rst_ni) arb_req_gnt_q <= '0;
        else         arb_req_gnt_q <= arb_req_gnt_d;
    end

    assign arb_req_valid_o = |arb_req_gnt_d;
    //  }}}

    //  Response demultiplexor
    //  {{{
    always_comb
    begin : resp_demux
        for (int unsigned i = 0; i < NREQUESTERS; i++) begin
            core_rsp_valid_o[i]  = core_rsp_valid_i && (i == int'(core_rsp_i.sid));
            core_rsp_o[i]        = core_rsp_i;
        end
    end
    //  }}}
endmodule
