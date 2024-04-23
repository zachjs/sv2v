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
 *  Creation Date : May, 2021
 *  Description   : HPDcache Pseudo-LRU replacement policy
 *  History       :
 */
module hpdcache_plru
    //  Parameters
    //  {{{
#(
    parameter int unsigned SETS = 0,
    parameter int unsigned WAYS = 0,

    localparam type set_t        = logic [$clog2(SETS)-1:0],
    localparam type way_vector_t = logic [WAYS-1:0]
)
    //  }}}

    //  Ports
    //  {{{
(
    input  logic                  clk_i,
    input  logic                  rst_ni,

    //      PLRU update interface
    input  logic                  updt_i,
    input  set_t                  updt_set_i,
    input  way_vector_t           updt_way_i,

    //      Victim replacement interface
    input  logic                  repl_i,
    input  set_t                  repl_set_i,
    input  way_vector_t           repl_dir_valid_i,
    input  logic                  repl_updt_plru_i,

    output way_vector_t           victim_way_o
);
    //  }}}

    //  Internal signals and registers
    //  {{{
    way_vector_t [SETS-1:0] plru_q, plru_d;
    way_vector_t            updt_plru;
    way_vector_t            repl_plru;
    way_vector_t            used_victim_way, unused_victim_way;
    //  }}}

    //  Victim way selection
    //  {{{
    hpdcache_prio_1hot_encoder #(.N(WAYS))
        used_victim_select_i (
            .val_i     (~plru_q[repl_set_i]),
            .val_o     (used_victim_way)
        );

    hpdcache_prio_1hot_encoder #(.N(WAYS))
        unused_victim_select_i (
            .val_i     (~repl_dir_valid_i),
            .val_o     (unused_victim_way)
        );

    //  If there is a free entry in the directory (valid == 0), choose it as victim
    assign victim_way_o = |unused_victim_way ? unused_victim_way : used_victim_way;
    //  }}}

    //  Pseudo-LRU update process
    //  {{{
    assign updt_plru = plru_q[updt_set_i] | updt_way_i;
    assign repl_plru = plru_q[repl_set_i] | victim_way_o;

    always_comb
    begin : plru_update_comb
        plru_d = plru_q;

        case (1'b1)
            //  When replacing a cache-line, set the PLRU bit of the new line
            repl_i:
                if (repl_updt_plru_i) begin
                    //  If all PLRU bits of a given would be set, reset them all
                    //  but the currently accessed way
                    if (&repl_plru) begin
                        plru_d[repl_set_i] = victim_way_o;
                    end else begin
                        plru_d[repl_set_i] = repl_plru;
                    end
                end

            //  When accessing a cache-line, set the corresponding PLRU bit
            updt_i:
                //  If all PLRU bits of a given would be set, reset them all
                //  but the currently accessed way
                if (&updt_plru) begin
                    plru_d[updt_set_i] = updt_way_i;
                end else begin
                    plru_d[updt_set_i] = updt_plru;
                end

            default: begin
                //  do nothing
            end
        endcase
    end
    //  }}}

    //  Set state process
    //  {{{
    always_ff @(posedge clk_i or negedge rst_ni)
    begin : lru_ff
        if (!rst_ni) begin
           plru_q <= '0;
        end else begin
           if (updt_i || repl_i) begin
              plru_q <= plru_d;
           end
        end
    end
    //  }}}

endmodule
