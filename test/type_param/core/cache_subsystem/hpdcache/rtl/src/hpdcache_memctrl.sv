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
 *  Description   : HPDcache Directory and Data Memory RAMs Controller
 *  History       :
 */
module hpdcache_memctrl
import hpdcache_pkg::*;
    //  Ports
    //  {{{
(
    //      Global clock and reset signals
    //      {{{
    input  logic                                clk_i,
    input  logic                                rst_ni,
    //      }}}

    //      Global control signals
    //      {{{
    output logic                                ready_o,
    //      }}}

    //      DIR array access interface
    //      {{{
    input  logic                                dir_match_i,
    input  hpdcache_set_t                       dir_match_set_i,
    input  hpdcache_tag_t                       dir_match_tag_i,
    input  logic                                dir_update_lru_i,
    output hpdcache_way_vector_t                dir_hit_way_o,

    input  logic                                dir_amo_match_i,
    input  hpdcache_set_t                       dir_amo_match_set_i,
    input  hpdcache_tag_t                       dir_amo_match_tag_i,
    input  logic                                dir_amo_update_plru_i,
    output hpdcache_way_vector_t                dir_amo_hit_way_o,

    input  logic                                dir_refill_i,
    input  hpdcache_set_t                       dir_refill_set_i,
    input  hpdcache_dir_entry_t                 dir_refill_entry_i,
    input  logic                                dir_refill_updt_plru_i,
    output hpdcache_way_vector_t                dir_victim_way_o,

    input  logic                                dir_cmo_check_i,
    input  hpdcache_set_t                       dir_cmo_check_set_i,
    input  hpdcache_tag_t                       dir_cmo_check_tag_i,
    output hpdcache_way_vector_t                dir_cmo_check_hit_way_o,

    input  logic                                dir_cmo_inval_i,
    input  hpdcache_set_t                       dir_cmo_inval_set_i,
    input  hpdcache_way_vector_t                dir_cmo_inval_way_i,

    //      }}}

    //      DATA array access interface
    //      {{{
    input  logic                                data_req_read_i,
    input  hpdcache_set_t                       data_req_read_set_i,
    input  hpdcache_req_size_t                  data_req_read_size_i,
    input  hpdcache_word_t                      data_req_read_word_i,
    output hpdcache_req_data_t                  data_req_read_data_o,

    input  logic                                data_req_write_i,
    input  logic                                data_req_write_enable_i,
    input  hpdcache_set_t                       data_req_write_set_i,
    input  hpdcache_req_size_t                  data_req_write_size_i,
    input  hpdcache_word_t                      data_req_write_word_i,
    input  hpdcache_req_data_t                  data_req_write_data_i,
    input  hpdcache_req_be_t                    data_req_write_be_i,

    input  logic                                data_amo_write_i,
    input  logic                                data_amo_write_enable_i,
    input  hpdcache_set_t                       data_amo_write_set_i,
    input  hpdcache_req_size_t                  data_amo_write_size_i,
    input  hpdcache_word_t                      data_amo_write_word_i,
    input  logic [63:0]                         data_amo_write_data_i,
    input  logic  [7:0]                         data_amo_write_be_i,

    input  logic                                data_refill_i,
    input  hpdcache_way_vector_t                data_refill_way_i,
    input  hpdcache_set_t                       data_refill_set_i,
    input  hpdcache_word_t                      data_refill_word_i,
    input  hpdcache_refill_data_t               data_refill_data_i
    //      }}}
);
    //  }}}

    //  Definition of constants
    //  {{{
    localparam int unsigned HPDCACHE_ALL_CUTS = HPDCACHE_DATA_RAM_X_CUTS*HPDCACHE_DATA_RAM_Y_CUTS;
    localparam int unsigned HPDCACHE_DATA_REQ_RATIO = HPDCACHE_ACCESS_WORDS/HPDCACHE_REQ_WORDS;
    //  }}}

    //  Definition of functions
    //  {{{

    //      hpdcache_compute_data_ram_cs
    //
    //      description: This function computes the chip-select signal for data
    //                   RAMs depending on the request size and the word offset
    function automatic hpdcache_data_row_enable_t hpdcache_compute_data_ram_cs(
            input hpdcache_req_size_t size_i,
            input hpdcache_word_t     word_i);

        localparam hpdcache_uint32 off_width =
                HPDCACHE_ACCESS_WORDS > 1 ? $clog2(HPDCACHE_ACCESS_WORDS) : 1;

        hpdcache_data_row_enable_t ret;
        hpdcache_uint32 off;

        case (size_i)
            3'h0,
            3'h1,
            3'h2,
            3'h3:    ret = hpdcache_data_row_enable_t'({ 64/HPDCACHE_WORD_WIDTH{1'b1}});
            3'h4:    ret = hpdcache_data_row_enable_t'({128/HPDCACHE_WORD_WIDTH{1'b1}});
            3'h5:    ret = hpdcache_data_row_enable_t'({256/HPDCACHE_WORD_WIDTH{1'b1}});
            default: ret = hpdcache_data_row_enable_t'({512/HPDCACHE_WORD_WIDTH{1'b1}});
        endcase

        off = HPDCACHE_ACCESS_WORDS > 1 ? hpdcache_uint'(word_i[0 +: off_width]) : 0;
        return hpdcache_data_row_enable_t'(ret << off);
    endfunction

    function automatic hpdcache_data_ram_row_idx_t hpdcache_way_to_data_ram_row(
            input hpdcache_way_vector_t way);
        for (hpdcache_uint i = 0; i < HPDCACHE_WAYS; i++) begin
            if (way[i]) return hpdcache_data_ram_row_idx_t'(i / HPDCACHE_DATA_WAYS_PER_RAM_WORD);
        end
        return 0;
    endfunction

    function automatic hpdcache_data_ram_way_idx_t hpdcache_way_to_data_ram_word(
            input hpdcache_way_vector_t way);
        for (hpdcache_uint i = 0; i < HPDCACHE_WAYS; i++) begin
            if (way[i]) return hpdcache_data_ram_way_idx_t'(i % HPDCACHE_DATA_WAYS_PER_RAM_WORD);
        end
        return 0;
    endfunction

    function automatic hpdcache_data_ram_addr_t hpdcache_set_to_data_ram_addr(
            input hpdcache_set_t set,
            input hpdcache_word_t word);
        hpdcache_uint ret;

        ret = (hpdcache_uint'(set)*(HPDCACHE_CL_WORDS / HPDCACHE_ACCESS_WORDS)) +
              (hpdcache_uint'(word) / HPDCACHE_ACCESS_WORDS);

        return hpdcache_data_ram_addr_t'(ret);
    endfunction
    //  }}}

    //  Definition of internal signals and registers
    //  {{{
    genvar gen_i, gen_j, gen_k;

    //      Directory initialization signals and registers
    logic                                      init_q,     init_d;
    hpdcache_dir_addr_t                        init_set_q, init_set_d;
    hpdcache_way_vector_t                      init_dir_cs;
    hpdcache_way_vector_t                      init_dir_we;
    hpdcache_dir_entry_t                       init_dir_wentry;

    //      Directory valid bit vector (one bit per set and way)
    hpdcache_way_vector_t [HPDCACHE_SETS-1:0]  dir_valid_q, dir_valid_d;
    hpdcache_set_t                             dir_req_set_q, dir_req_set_d;
    hpdcache_dir_addr_t                        dir_addr;
    hpdcache_way_vector_t                      dir_cs;
    hpdcache_way_vector_t                      dir_we;
    hpdcache_dir_entry_t  [HPDCACHE_WAYS-1:0]  dir_wentry;
    hpdcache_dir_entry_t  [HPDCACHE_WAYS-1:0]  dir_rentry;

    hpdcache_data_addr_t                       data_addr;
    hpdcache_data_enable_t                     data_cs;
    hpdcache_data_enable_t                     data_we;
    hpdcache_data_be_entry_t                   data_wbyteenable;
    hpdcache_data_entry_t                      data_wentry;
    hpdcache_data_entry_t                      data_rentry;

    logic                                      data_write;
    logic                                      data_write_enable;
    hpdcache_set_t                             data_write_set;
    hpdcache_req_size_t                        data_write_size;
    hpdcache_word_t                            data_write_word;
    hpdcache_refill_data_t                     data_write_data;
    hpdcache_refill_be_t                       data_write_be;

    hpdcache_refill_data_t                     data_req_write_data;
    hpdcache_refill_be_t                       data_req_write_be;

    hpdcache_refill_data_t                     data_amo_write_data;
    hpdcache_refill_be_t                       data_amo_write_be;

    hpdcache_way_vector_t                      data_way;

    hpdcache_data_ram_row_idx_t                data_ram_row;
    hpdcache_data_ram_way_idx_t                data_ram_word;

    //  }}}

    //  Init FSM
    //  {{{
    always_comb
    begin : init_comb
        init_dir_wentry.tag      = '0;
        init_dir_wentry.reserved = '0;
        init_dir_cs              = '0;
        init_dir_we              = '0;
        init_d                   = init_q;
        init_set_d               = init_set_q;

        case (init_q)
            1'b0: begin
                init_d      = (hpdcache_uint'(init_set_q) == (HPDCACHE_SETS - 1));
                init_set_d  = init_set_q + 1;
                init_dir_cs = '1;
                init_dir_we = '1;
            end

            1'b1: begin
                init_d      = 1'b1;
                init_set_d  = init_set_q;
            end
        endcase
    end

    assign ready_o = init_q;

    always_ff @(posedge clk_i or negedge rst_ni)
    begin : init_ff
        if (!rst_ni) begin
            init_q      <= 1'b0;
            init_set_q  <= 0;
            dir_valid_q <= '0;
        end else begin
            init_q      <= init_d;
            init_set_q  <= init_set_d;
            dir_valid_q <= dir_valid_d;
        end
    end
    //  }}}

    //  Memory arrays
    //  {{{
    hpdcache_memarray hpdcache_memarray_i(
        .clk_i,
        .rst_ni,

        .dir_addr_i         (dir_addr),
        .dir_cs_i           (dir_cs),
        .dir_we_i           (dir_we),
        .dir_wentry_i       (dir_wentry),
        .dir_rentry_o       (dir_rentry),

        .data_addr_i        (data_addr),
        .data_cs_i          (data_cs),
        .data_we_i          (data_we),
        .data_wbyteenable_i (data_wbyteenable),
        .data_wentry_i      (data_wentry),
        .data_rentry_o      (data_rentry)
    );
    //  }}}

    //  Directory RAM request mux
    //  {{{
    always_comb
    begin : dir_ctrl_comb
        case (1'b1)
            //  Cache directory initialization
            ~init_q: begin
                dir_addr    = init_set_q;
                dir_cs      = init_dir_cs;
                dir_we      = init_dir_we;
                dir_wentry  = {HPDCACHE_WAYS{init_dir_wentry}};
            end

            //  Cache directory match tag -> hit
            dir_match_i: begin
                dir_addr    = dir_match_set_i;
                dir_cs      = '1;
                dir_we      = '0;
                dir_wentry  = '0;
            end

            //  Cache directory AMO match tag -> hit
            dir_amo_match_i: begin
                dir_addr    = dir_amo_match_set_i;
                dir_cs      = '1;
                dir_we      = '0;
                dir_wentry  = '0;
            end

            //  Cache directory update
            dir_refill_i: begin
                dir_addr    = dir_refill_set_i;
                dir_cs      = dir_victim_way_o;
                dir_we      = dir_victim_way_o;
                dir_wentry  = {HPDCACHE_WAYS{dir_refill_entry_i}};
            end

            //  Cache directory CMO match tag
            dir_cmo_check_i: begin
                dir_addr    = dir_cmo_check_set_i;
                dir_cs      = '1;
                dir_we      = '0;
                dir_wentry  = '0;
            end

            //  Do nothing
            default: begin
                dir_addr    = '0;
                dir_cs      = '0;
                dir_we      = '0;
                dir_wentry  = '0;
            end
        endcase
    end
    //  }}}

    //  Directory valid logic
    //  {{{
    always_comb
    begin : dir_valid_comb
        dir_valid_d = dir_valid_q;

        unique case (1'b1)
            //  Refill the cache after a miss
            dir_refill_i: begin
                dir_valid_d[dir_refill_set_i]    = dir_valid_q[dir_refill_set_i]    |  dir_victim_way_o;
            end
            //  CMO invalidate a set
            dir_cmo_inval_i: begin
                dir_valid_d[dir_cmo_inval_set_i] = dir_valid_q[dir_cmo_inval_set_i] & ~dir_cmo_inval_way_i;
            end
            default: begin
                // do nothing
            end
        endcase
    end
    //  }}}

    //  Directory hit logic
    //  {{{
    assign dir_req_set_d = dir_match_i     ? dir_match_set_i     :
                           dir_amo_match_i ? dir_amo_match_set_i :
                           dir_cmo_check_i ? dir_cmo_check_set_i :
                                             dir_req_set_q       ;

    generate
        hpdcache_way_vector_t req_hit;
        hpdcache_way_vector_t amo_hit;
        hpdcache_way_vector_t cmo_hit;

        for (gen_i = 0; gen_i < int'(HPDCACHE_WAYS); gen_i++)
        begin : dir_match_tag_gen
            assign req_hit[gen_i] = (dir_rentry[gen_i].tag == dir_match_tag_i),
                   amo_hit[gen_i] = (dir_rentry[gen_i].tag == dir_amo_match_tag_i),
                   cmo_hit[gen_i] = (dir_rentry[gen_i].tag == dir_cmo_check_tag_i);

            assign dir_hit_way_o          [gen_i] = dir_valid_q[dir_req_set_q][gen_i] & req_hit[gen_i],
                   dir_amo_hit_way_o      [gen_i] = dir_valid_q[dir_req_set_q][gen_i] & amo_hit[gen_i],
                   dir_cmo_check_hit_way_o[gen_i] = dir_valid_q[dir_req_set_q][gen_i] & cmo_hit[gen_i];
        end
    endgenerate
    //  }}}

    //  Directory victim select logic
    //  {{{
    logic               plru_updt;
    hpdcache_way_vector_t plru_updt_way;

    assign plru_updt     = dir_update_lru_i | dir_amo_update_plru_i,
           plru_updt_way = dir_update_lru_i ? dir_hit_way_o : dir_amo_hit_way_o;

    hpdcache_plru #(
        .SETS                (HPDCACHE_SETS),
        .WAYS                (HPDCACHE_WAYS)
    ) plru_i (
        .clk_i,
        .rst_ni,

        .updt_i              (plru_updt),
        .updt_set_i          (dir_req_set_q),
        .updt_way_i          (plru_updt_way),

        .repl_i              (dir_refill_i),
        .repl_set_i          (dir_refill_set_i),
        .repl_dir_valid_i    (dir_valid_q[dir_refill_set_i]),
        .repl_updt_plru_i    (dir_refill_updt_plru_i),

        .victim_way_o        (dir_victim_way_o)
    );
    //  }}}

    //  Data RAM request multiplexor
    //  {{{

    //  Upsize the request interface to match the maximum access width of the data RAM
    generate
        if (HPDCACHE_DATA_REQ_RATIO > 1) begin : upsize_data_req_write_gen
            //  demux request DATA
            assign data_req_write_data = {HPDCACHE_DATA_REQ_RATIO{data_req_write_data_i}};

            //  demux request BE
            hpdcache_demux #(
                .NOUTPUT     (HPDCACHE_DATA_REQ_RATIO),
                .DATA_WIDTH  (HPDCACHE_REQ_DATA_WIDTH/8),
                .ONE_HOT_SEL (1'b0)
            ) data_req_write_be_demux_i (
                .data_i      (data_req_write_be_i),
                .sel_i       (data_req_write_word_i[HPDCACHE_REQ_WORD_INDEX_WIDTH +:
                                                    $clog2(HPDCACHE_DATA_REQ_RATIO)]),
                .data_o      (data_req_write_be)
            );
        end else begin
            assign data_req_write_data = data_req_write_data_i,
                   data_req_write_be   = data_req_write_be_i;
        end
    endgenerate

    //  Upsize the AMO data interface to match the maximum access width of the data RAM
    generate
        localparam hpdcache_uint AMO_DATA_RATIO       = HPDCACHE_DATA_RAM_ACCESS_WIDTH/64;
        localparam hpdcache_uint AMO_DATA_INDEX_WIDTH = $clog2(AMO_DATA_RATIO);

        if (AMO_DATA_RATIO > 1) begin
            assign data_amo_write_data = {AMO_DATA_RATIO{data_amo_write_data_i}};

            hpdcache_demux #(
                .NOUTPUT          (AMO_DATA_RATIO),
                .DATA_WIDTH       (8),
                .ONE_HOT_SEL      (1'b0)
            ) amo_be_demux_i (
                .data_i           (data_amo_write_be_i),
                .sel_i            (data_amo_write_word_i[0 +: AMO_DATA_INDEX_WIDTH]),
                .data_o           (data_amo_write_be)
            );
        end else begin
            assign data_amo_write_data = data_amo_write_data_i,
                   data_amo_write_be   = data_amo_write_be_i;
        end
    endgenerate

    //  Multiplex between data write requests
    always_comb
    begin : data_write_comb
        case (1'b1)
            data_refill_i: begin
                data_write        = 1'b1;
                data_write_enable = 1'b1;
                data_write_set    = data_refill_set_i;
                data_write_size   = hpdcache_req_size_t'($clog2(HPDCACHE_DATA_RAM_ACCESS_WIDTH/8));
                data_write_word   = data_refill_word_i;
                data_write_data   = data_refill_data_i;
                data_write_be     = '1;
            end

            data_req_write_i: begin
                data_write        = 1'b1;
                data_write_enable = data_req_write_enable_i;
                data_write_set    = data_req_write_set_i;
                data_write_size   = data_req_write_size_i;
                data_write_word   = data_req_write_word_i;
                data_write_data   = data_req_write_data;
                data_write_be     = data_req_write_be;
            end

            data_amo_write_i: begin
                data_write        = 1'b1;
                data_write_enable = data_amo_write_enable_i;
                data_write_set    = data_amo_write_set_i;
                data_write_size   = data_amo_write_size_i;
                data_write_word   = data_amo_write_word_i;
                data_write_data   = data_amo_write_data;
                data_write_be     = data_amo_write_be;
            end

            default: begin
                data_write        = 1'b0;
                data_write_enable = 1'b0;
                data_write_set    = '0;
                data_write_size   = '0;
                data_write_word   = '0;
                data_write_data   = '0;
                data_write_be     = '0;
            end
        endcase
    end

    //  Multiplex between read and write access on the data RAM
    assign  data_way = data_refill_i    ? data_refill_way_i :
                       data_amo_write_i ? dir_amo_hit_way_o :
                                          dir_hit_way_o;

    //  Decode way index
    assign  data_ram_word = hpdcache_way_to_data_ram_word(data_way),
            data_ram_row  = hpdcache_way_to_data_ram_row(data_way);

    always_comb
    begin : data_ctrl_comb
        case (1'b1)
            //  Select data read inputs
            data_req_read_i: begin
                data_addr = {HPDCACHE_ALL_CUTS{hpdcache_set_to_data_ram_addr(data_req_read_set_i,
                                                                         data_req_read_word_i)}};

                data_we          = '0;
                data_wbyteenable = '0;
                data_wentry      = '0;
                for (int unsigned i = 0; i < HPDCACHE_DATA_RAM_Y_CUTS; i++) begin
                    data_cs[i] = hpdcache_compute_data_ram_cs(data_req_read_size_i,
                                                              data_req_read_word_i);
                end
            end

            //  Select data write inputs
            data_write: begin
                data_addr = {HPDCACHE_ALL_CUTS{hpdcache_set_to_data_ram_addr(data_write_set,
                                                                           data_write_word)}};

                for (int unsigned i = 0; i < HPDCACHE_DATA_RAM_Y_CUTS; i++) begin
                    for (int unsigned j = 0; j < HPDCACHE_DATA_RAM_X_CUTS; j++) begin
                        data_wentry[i][j] = {HPDCACHE_DATA_WAYS_PER_RAM_WORD{data_write_data[j]}};
                    end
                end

                for (int unsigned i = 0; i < HPDCACHE_DATA_RAM_Y_CUTS; i++) begin
                    data_cs[i] = hpdcache_compute_data_ram_cs(data_write_size, data_write_word);

                    if (i == hpdcache_uint'(data_ram_row)) begin
                        data_we[i] = data_write_enable ? data_cs[i] : '0;
                    end else begin
                        data_we[i] = '0;
                    end

                    //  Build the write mask
                    for (int unsigned j = 0; j < HPDCACHE_ACCESS_WORDS; j++) begin
                        for (int unsigned k = 0; k < HPDCACHE_DATA_WAYS_PER_RAM_WORD; k++) begin
                            data_wbyteenable[i][j][k] = (k == hpdcache_uint'(data_ram_word)) ?
                                                        data_write_be[j] : '0;
                        end
                    end
                end
            end

            //  Do nothing
            default: begin
                data_addr        = '0;
                data_cs          = '0;
                data_we          = '0;
                data_wbyteenable = '0;
                data_wentry      = '0;
            end
        endcase
    end
    //  }}}

    //  Data RAM read data multiplexor
    //  {{{
    generate
        hpdcache_req_data_t [HPDCACHE_DATA_REQ_RATIO-1:0][HPDCACHE_WAYS-1:0] data_read_words;
        hpdcache_req_data_t                              [HPDCACHE_WAYS-1:0] data_read_req_word;

        //  Organize the read data by words (all ways for the same word are contiguous)
        for (gen_i = 0; gen_i < int'(HPDCACHE_DATA_REQ_RATIO); gen_i++) begin
            for (gen_j = 0; gen_j < int'(HPDCACHE_WAYS); gen_j++) begin
                for (gen_k = 0; gen_k < int'(HPDCACHE_REQ_WORDS); gen_k++) begin
                    assign data_read_words[gen_i][gen_j][gen_k] =
                            data_rentry[(gen_j / HPDCACHE_DATA_WAYS_PER_RAM_WORD)]
                                       [(gen_i * HPDCACHE_REQ_WORDS     ) + gen_k]
                                       [(gen_j % HPDCACHE_DATA_WAYS_PER_RAM_WORD)];
                end
            end
        end

        //  Mux the data according to the access word
        if (HPDCACHE_DATA_REQ_RATIO > 1) begin : req_width_lt_ram_width
            typedef logic [$clog2(HPDCACHE_DATA_REQ_RATIO)-1:0] data_req_word_t;
            data_req_word_t data_read_req_word_index_q;

            hpdcache_mux #(
                .NINPUT      (HPDCACHE_DATA_REQ_RATIO),
                .DATA_WIDTH  (HPDCACHE_REQ_DATA_WIDTH*HPDCACHE_WAYS)
            ) data_read_req_word_mux_i(
                .data_i      (data_read_words),
                .sel_i       (data_read_req_word_index_q),
                .data_o      (data_read_req_word)
            );

            always_ff @(posedge clk_i)
            begin : data_req_read_word_ff
                data_read_req_word_index_q <=
                        data_req_read_word_i[HPDCACHE_REQ_WORD_INDEX_WIDTH +:
                                             $clog2(HPDCACHE_DATA_REQ_RATIO)];
            end
        end

        //  Request data interface width is equal to the data RAM width
        else begin : req_width_eq_ram_width
            assign data_read_req_word = data_read_words;
        end

        //  Mux the data according to the hit way
        hpdcache_mux #(
            .NINPUT      (HPDCACHE_WAYS),
            .DATA_WIDTH  (HPDCACHE_REQ_DATA_WIDTH),
            .ONE_HOT_SEL (1'b1)
        ) data_read_req_word_way_mux_i(
            .data_i      (data_read_req_word),
            .sel_i       (dir_hit_way_o),
            .data_o      (data_req_read_data_o)
        );
    endgenerate


    //  Delay the accessed set for checking the tag from the directory in the
    //  next cycle (hit logic)
    always_ff @(posedge clk_i)
    begin : req_read_ff
        if (dir_match_i || dir_amo_match_i || dir_cmo_check_i) begin
            dir_req_set_q <= dir_req_set_d;
        end
    end
    //  }}}

    //  Assertions
    //  {{{
    //  pragma translate_off
    concurrent_dir_access_assert: assert property (@(posedge clk_i) disable iff (!rst_ni)
            $onehot0({dir_match_i, dir_amo_match_i, dir_cmo_check_i, dir_refill_i})) else
            $error("hpdcache_memctrl: more than one process is accessing the cache directory");

    concurrent_data_access_assert: assert property (@(posedge clk_i) disable iff (!rst_ni)
            $onehot0({data_req_read_i, data_req_write_i, data_amo_write_i, data_refill_i})) else
            $error("hpdcache_memctrl: more than one process is accessing the cache data");
    //  pragma translate_on
    //  }}}
endmodule
