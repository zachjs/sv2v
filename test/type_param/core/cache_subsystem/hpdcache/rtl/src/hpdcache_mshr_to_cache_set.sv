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
 *  Description   : HPDcache MSHR set translation table
 *  History       :
 */
module hpdcache_mshr_to_cache_set
import hpdcache_pkg::*;
//  Ports
//  {{{
(
    //  Clock signals
    input  logic          clk_i,

    //  Write interface
    input  logic          write_i,
    input  hpdcache_set_t write_dcache_set_i,
    input  mshr_way_t     write_mshr_way_i,

    //  Read interface
    input  mshr_way_t     read_mshr_way_i,
    input  mshr_set_t     read_mshr_set_i,
    output hpdcache_set_t read_dcache_set_o
);
//  }}}
    //

    generate
        //  Number of HPDcache sets is bigger than the MSHR sets
        //  In this case, a translation table (in flip-flops) is needed
        //  {{{
        //      Write most significant bits of the HPDcache set into the
        //      translation table
        if (HPDCACHE_SETS > HPDCACHE_MSHR_SETS) begin : hpdcache_sets_gt_mshr_sets_gen
            localparam hpdcache_uint TRLT_TAB_ENTRY_WIDTH =
                    HPDCACHE_SET_WIDTH - HPDCACHE_MSHR_SET_WIDTH;
            typedef logic [TRLT_TAB_ENTRY_WIDTH-1:0] trlt_entry_t;


            //  Translation table
            //
            //  This table is used to store the most significant bits of the HPDcache set
            trlt_entry_t [HPDCACHE_MSHR_SETS-1:0][HPDCACHE_MSHR_WAYS-1:0] tab;
            trlt_entry_t tab_wdata;
            mshr_set_t   write_mshr_set;

            //  Write operation
            //  {{{
            //      Write most significant bits of the HPDcache set into the
            //      translation table
            always_ff @(posedge clk_i)
            begin
                if (write_i) begin
                    tab[write_mshr_set][write_mshr_way_i] <= tab_wdata;
                end
            end

            assign tab_wdata        = write_dcache_set_i[HPDCACHE_MSHR_SET_WIDTH +:
                                                         TRLT_TAB_ENTRY_WIDTH],
                   write_mshr_set   = write_dcache_set_i[0 +: HPDCACHE_MSHR_SET_WIDTH];
            //  }}}

            //  Read operation
            //  {{{
            //      Concatenate the mshr set with the most significant bits of the
            //      dcache set stored in the translation table
            assign read_dcache_set_o = {tab[read_mshr_set_i][read_mshr_way_i], read_mshr_set_i};
            //  }}}
        end
        //  }}}

        //  Number of HPDcache sets is smaller or equal than the MSHR sets
        //  In this case, no translation table is needed
        //  {{{
        else begin : hpdcache_sets_le_mshr_sets_gen
           assign read_dcache_set_o = hpdcache_set_t'(read_mshr_set_i);
        end
        //  }}}
    endgenerate

//  Assertions
//  {{{
//  pragma translate_off
//  pragma translate_on
//  }}}
endmodule
