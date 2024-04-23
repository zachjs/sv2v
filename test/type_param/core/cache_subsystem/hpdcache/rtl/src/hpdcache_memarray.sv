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
 *  Description   : HPDcache Directory and Data Memory Arrays
 *  History       :
 */
module hpdcache_memarray
import hpdcache_pkg::*;
    //  Ports
    //  {{{
(
    input  logic                                    clk_i,
    input  logic                                    rst_ni,

    input  hpdcache_dir_addr_t                      dir_addr_i,
    input  hpdcache_way_vector_t                    dir_cs_i,
    input  hpdcache_way_vector_t                    dir_we_i,
    input  hpdcache_dir_entry_t [HPDCACHE_WAYS-1:0] dir_wentry_i,
    output hpdcache_dir_entry_t [HPDCACHE_WAYS-1:0] dir_rentry_o,

    input  hpdcache_data_addr_t                     data_addr_i,
    input  hpdcache_data_enable_t                   data_cs_i,
    input  hpdcache_data_enable_t                   data_we_i,
    input  hpdcache_data_be_entry_t                 data_wbyteenable_i,
    input  hpdcache_data_entry_t                    data_wentry_i,
    output hpdcache_data_entry_t                    data_rentry_o
);
    //  }}}

    //  Memory arrays
    //  {{{
    generate
        genvar x, y, dir_w;

        //  Directory
        //
        for (dir_w = 0; dir_w < int'(HPDCACHE_WAYS); dir_w++) begin : dir_sram_gen
            hpdcache_sram #(
                .DATA_SIZE (HPDCACHE_DIR_RAM_WIDTH),
                .ADDR_SIZE (HPDCACHE_DIR_RAM_ADDR_WIDTH)
            ) dir_sram (
                .clk       (clk_i),
                .rst_n     (rst_ni),
                .cs        (dir_cs_i[dir_w]),
                .we        (dir_we_i[dir_w]),
                .addr      (dir_addr_i),
                .wdata     (dir_wentry_i[dir_w]),
                .rdata     (dir_rentry_o[dir_w])
            );
        end

        //  Data
        //
        for (y = 0; y < int'(HPDCACHE_DATA_RAM_Y_CUTS); y++) begin : data_sram_row_gen
            for (x = 0; x < int'(HPDCACHE_DATA_RAM_X_CUTS); x++) begin : data_sram_col_gen
                if (HPDCACHE_DATA_RAM_WBYTEENABLE) begin : data_sram_wbyteenable_gen
                    hpdcache_sram_wbyteenable #(
                        .DATA_SIZE   (HPDCACHE_DATA_RAM_WIDTH),
                        .ADDR_SIZE   (HPDCACHE_DATA_RAM_ADDR_WIDTH)
                    ) data_sram (
                        .clk         (clk_i),
                        .rst_n       (rst_ni),
                        .cs          (data_cs_i[y][x]),
                        .we          (data_we_i[y][x]),
                        .addr        (data_addr_i[y][x]),
                        .wdata       (data_wentry_i[y][x]),
                        .wbyteenable (data_wbyteenable_i[y][x]),
                        .rdata       (data_rentry_o[y][x])
                    );
                end else begin : data_sram_wmask_gen
                    hpdcache_data_ram_data_t data_wmask;

                    //  build the bitmask from the write byte enable signal
                    always_comb
                    begin : data_wmask_comb
                        for (int w = 0; w < HPDCACHE_DATA_WAYS_PER_RAM_WORD; w++) begin
                            for (int b = 0; b < HPDCACHE_WORD_WIDTH/8; b++) begin
                                data_wmask[w][8*b +: 8] = {8{data_wbyteenable_i[y][x][w][b]}};
                            end
                        end
                    end

                    hpdcache_sram_wmask #(
                        .DATA_SIZE   (HPDCACHE_DATA_RAM_WIDTH),
                        .ADDR_SIZE   (HPDCACHE_DATA_RAM_ADDR_WIDTH)
                    ) data_sram (
                        .clk         (clk_i),
                        .rst_n       (rst_ni),
                        .cs          (data_cs_i[y][x]),
                        .we          (data_we_i[y][x]),
                        .addr        (data_addr_i[y][x]),
                        .wdata       (data_wentry_i[y][x]),
                        .wmask       (data_wmask),
                        .rdata       (data_rentry_o[y][x])
                    );
                end
            end
        end
    endgenerate
    //  }}}
endmodule
