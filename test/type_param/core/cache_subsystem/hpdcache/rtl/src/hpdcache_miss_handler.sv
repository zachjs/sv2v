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
 *  Description   : HPDcache Miss Handler
 *  History       :
 */
module hpdcache_miss_handler
//  {{{
import hpdcache_pkg::*;
//  Parameters
//  {{{
#(
    parameter  int  HPDcacheMemIdWidth    = 8,
    parameter  int  HPDcacheMemDataWidth  = 512,
    parameter  type hpdcache_mem_req_t    = logic,
    parameter  type hpdcache_mem_resp_r_t = logic,
    localparam type hpdcache_mem_id_t     = logic [HPDcacheMemIdWidth-1:0]
)
//  }}}
//  Ports
//  {{{
(
    input  logic                  clk_i,
    input  logic                  rst_ni,

    //      Global control signals
    //      {{{
    output logic                  mshr_empty_o,
    output logic                  mshr_full_o,
    //      }}}

    //      Configuration signals
    //      {{{
    input  logic                  cfg_prefetch_updt_plru_i,
    //      }}}

    //      CHECK interface
    //      {{{
    input  logic                  mshr_check_i,
    input  mshr_set_t             mshr_check_set_i,
    input  mshr_tag_t             mshr_check_tag_i,
    output logic                  mshr_check_hit_o,
    //      }}}

    //      MISS interface
    //      {{{
    //          MISS request interface
    output logic                  mshr_alloc_ready_o,
    input  logic                  mshr_alloc_i,
    input  logic                  mshr_alloc_cs_i,
    input  hpdcache_nline_t       mshr_alloc_nline_i,
    output logic                  mshr_alloc_full_o,
    input  hpdcache_req_tid_t     mshr_alloc_tid_i,
    input  hpdcache_req_sid_t     mshr_alloc_sid_i,
    input  hpdcache_word_t        mshr_alloc_word_i,
    input  logic                  mshr_alloc_need_rsp_i,
    input  logic                  mshr_alloc_is_prefetch_i,

    //          REFILL MISS interface
    input  logic                  refill_req_ready_i,
    output logic                  refill_req_valid_o,
    output logic                  refill_busy_o,
    output logic                  refill_updt_plru_o,
    output hpdcache_set_t         refill_set_o,
    output hpdcache_dir_entry_t   refill_dir_entry_o,
    input  hpdcache_way_vector_t  refill_victim_way_i,
    output logic                  refill_write_dir_o,
    output logic                  refill_write_data_o,
    output hpdcache_way_vector_t  refill_victim_way_o,
    output hpdcache_refill_data_t refill_data_o,
    output hpdcache_word_t        refill_word_o,
    output hpdcache_nline_t       refill_nline_o,
    output logic                  refill_updt_rtab_o,

    //          REFILL core response interface
    output logic                  refill_core_rsp_valid_o,
    output hpdcache_rsp_t         refill_core_rsp_o,
    //      }}}

    //      MEMORY interface
    //      {{{
    input  logic                  mem_req_ready_i,
    output logic                  mem_req_valid_o,
    output hpdcache_mem_req_t     mem_req_o,

    output logic                  mem_resp_ready_o,
    input  logic                  mem_resp_valid_i,
    input  hpdcache_mem_resp_r_t  mem_resp_i
    //      }}}
);
//  }}}

    //  Declaration of constants and types
    //  {{{
    localparam int unsigned REFILL_REQ_RATIO = HPDCACHE_ACCESS_WORDS/HPDCACHE_REQ_WORDS;

    typedef enum logic {
        MISS_REQ_IDLE = 1'b0,
        MISS_REQ_SEND = 1'b1
    } miss_req_fsm_e;

    typedef enum {
        REFILL_IDLE,
        REFILL_WRITE,
        REFILL_WRITE_DIR
    } refill_fsm_e;

    typedef struct packed {
        hpdcache_mem_error_e r_error;
        hpdcache_mem_id_t    r_id;
    } mem_resp_metadata_t;

    function automatic mshr_set_t get_ack_mshr_set(hpdcache_mem_id_t id);
        return id[0 +: HPDCACHE_MSHR_SET_WIDTH];
    endfunction

    function automatic mshr_way_t get_ack_mshr_way(hpdcache_mem_id_t id);
        return id[HPDCACHE_MSHR_SET_WIDTH +: HPDCACHE_MSHR_WAY_WIDTH];
    endfunction
    //  }}}

    //  Declaration of internal signals and registers
    //  {{{
    miss_req_fsm_e           miss_req_fsm_q, miss_req_fsm_d;
    mshr_way_t               mshr_alloc_way_q, mshr_alloc_way_d;
    mshr_set_t               mshr_alloc_set_q, mshr_alloc_set_d;
    mshr_tag_t               mshr_alloc_tag_q, mshr_alloc_tag_d;

    refill_fsm_e             refill_fsm_q, refill_fsm_d;
    hpdcache_set_t           refill_set_q;
    hpdcache_tag_t           refill_tag_q;
    hpdcache_way_vector_t    refill_way_q;
    hpdcache_req_sid_t       refill_sid_q;
    hpdcache_req_tid_t       refill_tid_q;
    hpdcache_word_t          refill_cnt_q, refill_cnt_d;
    logic                    refill_need_rsp_q;
    logic                    refill_is_prefetch_q;
    hpdcache_word_t          refill_core_rsp_word_q;
    logic                    refill_way_bypass;

    mem_resp_metadata_t      refill_fifo_resp_meta_wdata, refill_fifo_resp_meta_rdata;
    logic                    refill_fifo_resp_meta_w, refill_fifo_resp_meta_wok;
    logic                    refill_fifo_resp_meta_r, refill_fifo_resp_meta_rok;

    logic                    refill_fifo_resp_data_w, refill_fifo_resp_data_wok;
    hpdcache_refill_data_t   refill_fifo_resp_data_rdata;
    logic                    refill_fifo_resp_data_r;

    logic                    refill_core_rsp_valid;
    hpdcache_req_data_t      refill_core_rsp_rdata;
    hpdcache_req_sid_t       refill_core_rsp_sid;
    hpdcache_req_tid_t       refill_core_rsp_tid;
    logic                    refill_core_rsp_error;
    hpdcache_word_t          refill_core_rsp_word;
    hpdcache_rsp_t           refill_core_rsp;

    logic                    refill_is_error;

    logic                    mshr_alloc;
    logic                    mshr_alloc_cs;
    logic                    mshr_ack;
    logic                    mshr_ack_cs;
    mshr_set_t               mshr_ack_set;
    mshr_way_t               mshr_ack_way;
    hpdcache_nline_t         mshr_ack_nline;
    hpdcache_req_sid_t       mshr_ack_src_id;
    hpdcache_req_tid_t       mshr_ack_req_id;
    hpdcache_word_t          mshr_ack_word;
    logic                    mshr_ack_need_rsp;
    logic                    mshr_ack_is_prefetch;
    logic                    mshr_empty;
    //  }}}

    //  Miss Request FSM
    //  {{{
    always_comb
    begin : miss_req_fsm_comb
        mshr_alloc_ready_o = 1'b0;
        mshr_alloc         = 1'b0;
        mshr_alloc_cs      = 1'b0;
        mem_req_valid_o    = 1'b0;

        miss_req_fsm_d     = miss_req_fsm_q;

        case (miss_req_fsm_q)
            MISS_REQ_IDLE: begin
                mshr_alloc_ready_o = 1'b1;
                mshr_alloc         = mshr_alloc_i;
                mshr_alloc_cs      = mshr_alloc_cs_i;
                if (mshr_alloc_i) begin
                    miss_req_fsm_d = MISS_REQ_SEND;
                end else begin
                    miss_req_fsm_d = MISS_REQ_IDLE;
                end
            end
            MISS_REQ_SEND: begin
                mem_req_valid_o = 1'b1;
                if (mem_req_ready_i) begin
                    miss_req_fsm_d = MISS_REQ_IDLE;
                end else begin
                    miss_req_fsm_d = MISS_REQ_SEND;
                end
            end
        endcase
    end

    localparam hpdcache_uint REFILL_REQ_SIZE = $clog2(HPDcacheMemDataWidth/8);
    localparam hpdcache_uint REFILL_REQ_LEN = HPDCACHE_CL_WIDTH/HPDcacheMemDataWidth;

    assign mem_req_o.mem_req_addr = {mshr_alloc_tag_q, mshr_alloc_set_q, {HPDCACHE_OFFSET_WIDTH{1'b0}} },
           mem_req_o.mem_req_len = hpdcache_mem_len_t'(REFILL_REQ_LEN-1),
           mem_req_o.mem_req_size = hpdcache_mem_size_t'(REFILL_REQ_SIZE),
           mem_req_o.mem_req_id = hpdcache_mem_id_t'({mshr_alloc_way_q, mshr_alloc_set_q}),
           mem_req_o.mem_req_command = HPDCACHE_MEM_READ,
           mem_req_o.mem_req_atomic = HPDCACHE_MEM_ATOMIC_ADD,
           mem_req_o.mem_req_cacheable = 1'b1;

    always_ff @(posedge clk_i)
    begin : miss_req_fsm_internal_ff
        if (mshr_alloc) begin
            mshr_alloc_way_q <= mshr_alloc_way_d;
            mshr_alloc_set_q <= mshr_alloc_set_d;
            mshr_alloc_tag_q <= mshr_alloc_tag_d;
        end
    end

    always_ff @(posedge clk_i or negedge rst_ni)
    begin : miss_req_fsm_ff
        if (!rst_ni) begin
            miss_req_fsm_q <= MISS_REQ_IDLE;
        end else begin
            miss_req_fsm_q <= miss_req_fsm_d;
        end
    end
    //  }}}

    //  Refill FSM
    //  {{{

    //      ask permission to the refill arbiter if there is a pending refill
    assign refill_req_valid_o  = refill_fsm_q == REFILL_IDLE ? refill_fifo_resp_meta_rok : 1'b0;

    //      forward the victim way directly from the victim selection logic or
    //      from the internal register
    assign refill_victim_way_o = refill_way_bypass ? refill_victim_way_i : refill_way_q;

    always_comb
    begin : miss_resp_fsm_comb
        automatic hpdcache_uint REFILL_LAST_CHUNK_WORD;
        REFILL_LAST_CHUNK_WORD = HPDCACHE_CL_WORDS - HPDCACHE_ACCESS_WORDS;

        refill_updt_plru_o      = 1'b0;
        refill_set_o            = '0;
        refill_write_dir_o      = 1'b0;
        refill_write_data_o     = 1'b0;
        refill_updt_rtab_o      = 1'b0;
        refill_cnt_d            = refill_cnt_q;
        refill_way_bypass       = 1'b0;

        refill_core_rsp_valid   = 1'b0;
        refill_core_rsp_sid     = '0;
        refill_core_rsp_tid     = '0;
        refill_core_rsp_error   = 1'b0;
        refill_core_rsp_word    = 0;

        refill_fifo_resp_meta_r = 1'b0;
        refill_fifo_resp_data_r = 1'b0;

        mshr_ack_cs             = 1'b0;
        mshr_ack                = 1'b0;

        refill_fsm_d            = refill_fsm_q;

        case (refill_fsm_q)
            //  Wait for refill responses
            //  {{{
            REFILL_IDLE: begin
                if (refill_fifo_resp_meta_rok) begin
                    //  anticipate the activation of the MSHR independently of the grant signal from
                    //  the refill arbiter. This is to avoid the introduction of unnecessary timing
                    //  paths (however there could be a minor augmentation of the power
                    //  consumption).
                    mshr_ack_cs = 1'b1;

                    //  if the permission is granted, start refilling
                    if (refill_req_ready_i) begin
                        refill_fsm_d = REFILL_WRITE;

                        //  read the MSHR and reset the valid bit for the
                        //  corresponding entry
                        mshr_ack = 1'b1;

                        //  initialize the counter for refill words
                        refill_cnt_d = 0;
                    end
                end
            end
            //  }}}

            //  Write refill data into the cache
            //  {{{
            REFILL_WRITE: begin
                automatic logic is_prefetch;

                //  Respond to the core (when needed)
                if (refill_cnt_q == 0) begin
                    automatic hpdcache_uint _core_rsp_word;
                    _core_rsp_word = hpdcache_uint'(mshr_ack_word)/HPDCACHE_ACCESS_WORDS;

                    if (mshr_ack_need_rsp) begin
                        refill_core_rsp_valid = (hpdcache_uint'(_core_rsp_word) == 0);
                    end

                    refill_core_rsp_sid = mshr_ack_src_id;
                    refill_core_rsp_tid = mshr_ack_req_id;
                    refill_core_rsp_error = refill_is_error;
                    refill_core_rsp_word = hpdcache_word_t'(
                        hpdcache_uint'(mshr_ack_word)/HPDCACHE_REQ_WORDS);
                end else begin
                    automatic hpdcache_uint _core_rsp_word;
                    _core_rsp_word = hpdcache_uint'(refill_core_rsp_word_q)/
                                     HPDCACHE_ACCESS_WORDS;

                    if (refill_need_rsp_q) begin
                        automatic hpdcache_uint _refill_cnt;
                        _refill_cnt = hpdcache_uint'(refill_cnt_q)/HPDCACHE_ACCESS_WORDS;
                        refill_core_rsp_valid = (_core_rsp_word == _refill_cnt);
                    end

                    refill_core_rsp_sid = refill_sid_q;
                    refill_core_rsp_tid = refill_tid_q;
                    refill_core_rsp_error = refill_is_error;
                    refill_core_rsp_word = hpdcache_word_t'(
                        hpdcache_uint'(refill_core_rsp_word_q)/HPDCACHE_REQ_WORDS);
                end

                //  Write the the data in the cache data array
                if (refill_cnt_q == 0) begin
                    refill_set_o = mshr_ack_nline[0 +: HPDCACHE_SET_WIDTH];
                    refill_way_bypass = 1'b1;
                    is_prefetch = mshr_ack_is_prefetch;
                end else begin
                    refill_set_o = refill_set_q;
                    refill_way_bypass = 1'b0;
                    is_prefetch = refill_is_prefetch_q;
                end
                refill_write_data_o = ~refill_is_error;

                //  Consume chunk of data from the FIFO buffer in the memory interface
                refill_fifo_resp_data_r = 1'b1;

                //  Update directory on the last chunk of data
                refill_cnt_d = refill_cnt_q + hpdcache_word_t'(HPDCACHE_ACCESS_WORDS);

                if (hpdcache_uint'(refill_cnt_q) == REFILL_LAST_CHUNK_WORD) begin
                    if (REFILL_LAST_CHUNK_WORD == 0) begin
                        //  Special case: if the cache-line data can be written in a single cycle,
                        //  wait an additional cycle to write the directory. This allows to prevent
                        //  a RAM-to-RAM timing path between the MSHR and the DIR.
                        refill_fsm_d = REFILL_WRITE_DIR;
                    end else begin
                        //  Write the new entry in the cache directory
                        refill_write_dir_o  = ~refill_is_error;

                        //  Update the PLRU bits. Only in the following cases:
                        //  - There is no error in response AND
                        //  - It is a prefetch and the cfg_prefetch_updt_plru_i is set OR
                        //  - It is a read miss.
                        refill_updt_plru_o  =  ~refill_is_error &
                                              (~is_prefetch | cfg_prefetch_updt_plru_i);

                        //  Update dependency flags in the retry table
                        refill_updt_rtab_o  = 1'b1;

                        //  consume the response from the network
                        refill_fifo_resp_meta_r = 1'b1;

                        refill_fsm_d = REFILL_IDLE;
                    end
                end
            end
            //  }}}

            //  Write cache directory (this state is only visited when ACCESS_WORDS == CL_WORDS,
            //  this is when the entire cache-line can be written in a single cycle)
            //  {{{
            REFILL_WRITE_DIR: begin
                automatic logic is_prefetch;
                is_prefetch = refill_is_prefetch_q;

                //  Select the target set and way
                refill_set_o = refill_set_q;
                refill_way_bypass = 1'b0;

                //  Write the new entry in the cache directory
                refill_write_dir_o  = ~refill_is_error;

                //  Update the PLRU bits. Only in the following cases:
                //  - There is no error in response AND
                //  - It is a prefetch and the cfg_prefetch_updt_plru_i is set OR
                //  - It is a read miss.
                refill_updt_plru_o  =  ~refill_is_error &
                                      (~is_prefetch | cfg_prefetch_updt_plru_i);

                //  Update dependency flags in the retry table
                refill_updt_rtab_o  = 1'b1;

                //  consume the response from the network
                refill_fifo_resp_meta_r = 1'b1;

                refill_fsm_d = REFILL_IDLE;
            end
            //  }}}

            default: begin
                // pragma translate_off
                $error("Illegal state");
                // pragma translate_on
            end
        endcase
    end

    assign  refill_is_error = (refill_fifo_resp_meta_rdata.r_error == HPDCACHE_MEM_RESP_NOK);

    assign  refill_busy_o  = (refill_fsm_q != REFILL_IDLE),
            refill_nline_o = {refill_tag_q, refill_set_q},
            refill_word_o  = refill_cnt_q;

    assign  mshr_ack_set = get_ack_mshr_set(refill_fifo_resp_meta_rdata.r_id),
            mshr_ack_way = get_ack_mshr_way(refill_fifo_resp_meta_rdata.r_id);

    assign  refill_dir_entry_o.tag      = refill_tag_q,
            refill_dir_entry_o.reserved = '0;

    assign  refill_core_rsp.rdata   = refill_core_rsp_rdata,
            refill_core_rsp.sid     = refill_core_rsp_sid,
            refill_core_rsp.tid     = refill_core_rsp_tid,
            refill_core_rsp.error   = refill_core_rsp_error,
            refill_core_rsp.aborted = 1'b0;

    hpdcache_fifo_reg #(
        .FIFO_DEPTH  (1),
        .FEEDTHROUGH (HPDCACHE_REFILL_CORE_RSP_FEEDTHROUGH),
        .fifo_data_t (hpdcache_rsp_t)
    ) i_refill_core_rsp_buf(
        .clk_i,
        .rst_ni,
        .w_i         (refill_core_rsp_valid),
        .wok_o       (/*unused*/),
        .wdata_i     (refill_core_rsp),
        .r_i         (1'b1),  //  core shall always be ready to consume a response
        .rok_o       (refill_core_rsp_valid_o),
        .rdata_o     (refill_core_rsp_o)
    );

    generate
        //  refill's width is bigger than the width of the core's interface
        if (REFILL_REQ_RATIO > 1) begin : core_rsp_data_mux_gen
            hpdcache_mux #(
                .NINPUT      (REFILL_REQ_RATIO),
                .DATA_WIDTH  (HPDCACHE_REQ_DATA_WIDTH)
            ) data_read_rsp_mux_i(
                .data_i      (refill_data_o),
                .sel_i       (refill_core_rsp_word[0 +: $clog2(REFILL_REQ_RATIO)]),
                .data_o      (refill_core_rsp_rdata)
            );
        end

        //  refill's width is equal to the width of the core's interface
        else begin
            assign refill_core_rsp_rdata = refill_data_o;
        end
    endgenerate

    /* FIXME: when multiple chunks, in case of error, the error bit is not
     *        necessarily set on all chunks */
    assign refill_fifo_resp_meta_wdata = '{
        r_error: mem_resp_i.mem_resp_r_error,
        r_id   : mem_resp_i.mem_resp_r_id
    };

    hpdcache_fifo_reg #(
        .FIFO_DEPTH  (2),
        .fifo_data_t (mem_resp_metadata_t)
    ) i_r_metadata_fifo (
        .clk_i,
        .rst_ni,

        .w_i    (refill_fifo_resp_meta_w),
        .wok_o  (refill_fifo_resp_meta_wok),
        .wdata_i(refill_fifo_resp_meta_wdata),

        .r_i    (refill_fifo_resp_meta_r),
        .rok_o  (refill_fifo_resp_meta_rok),
        .rdata_o(refill_fifo_resp_meta_rdata)
    );

    generate
        if (HPDcacheMemDataWidth < HPDCACHE_REFILL_DATA_WIDTH) begin
            hpdcache_data_upsize #(
                .WR_WIDTH(HPDcacheMemDataWidth),
                .RD_WIDTH(HPDCACHE_REFILL_DATA_WIDTH),
                .DEPTH(2*(HPDCACHE_CL_WIDTH/HPDCACHE_REFILL_DATA_WIDTH))
            ) i_rdata_upsize (
                .clk_i,
                .rst_ni,

                .w_i      (refill_fifo_resp_data_w),
                .wlast_i  (mem_resp_i.mem_resp_r_last),
                .wok_o    (refill_fifo_resp_data_wok),
                .wdata_i  (mem_resp_i.mem_resp_r_data),

                .r_i      (refill_fifo_resp_data_r),
                .rok_o    (/* unused */),
                .rdata_o  (refill_fifo_resp_data_rdata)
            );
        end else if (HPDcacheMemDataWidth > HPDCACHE_REFILL_DATA_WIDTH) begin
            hpdcache_data_downsize #(
                .WR_WIDTH(HPDcacheMemDataWidth),
                .RD_WIDTH(HPDCACHE_REFILL_DATA_WIDTH),
                .DEPTH(2*(HPDCACHE_CL_WIDTH/HPDcacheMemDataWidth))
            ) i_rdata_downsize (
                .clk_i,
                .rst_ni,

                .w_i      (refill_fifo_resp_data_w),
                .wok_o    (refill_fifo_resp_data_wok),
                .wdata_i  (mem_resp_i.mem_resp_r_data),

                .r_i      (refill_fifo_resp_data_r),
                .rok_o    (/* unused */),
                .rdata_o  (refill_fifo_resp_data_rdata)
            );
        end else begin
            hpdcache_fifo_reg #(
                .FIFO_DEPTH  (2),
                .fifo_data_t (hpdcache_refill_data_t)
            ) i_rdata_fifo (
                .clk_i,
                .rst_ni,

                .w_i      (refill_fifo_resp_data_w),
                .wok_o    (refill_fifo_resp_data_wok),
                .wdata_i  (mem_resp_i.mem_resp_r_data),

                .r_i      (refill_fifo_resp_data_r),
                .rok_o    (/* unused */),
                .rdata_o  (refill_fifo_resp_data_rdata)
            );
        end
    endgenerate

    assign           refill_data_o = refill_fifo_resp_data_rdata;

    assign refill_fifo_resp_data_w = mem_resp_valid_i &
                                     (refill_fifo_resp_meta_wok | ~mem_resp_i.mem_resp_r_last),
           refill_fifo_resp_meta_w = mem_resp_valid_i &
                                     (refill_fifo_resp_data_wok &  mem_resp_i.mem_resp_r_last),
                  mem_resp_ready_o = refill_fifo_resp_data_wok &
                                     (refill_fifo_resp_meta_wok | ~mem_resp_i.mem_resp_r_last);

    always_ff @(posedge clk_i or negedge rst_ni)
    begin : miss_resp_fsm_ff
        if (!rst_ni) begin
            refill_fsm_q <= REFILL_IDLE;
        end else begin
            refill_fsm_q <= refill_fsm_d;
        end
    end

    always_ff @(posedge clk_i)
    begin : miss_resp_fsm_internal_ff
        if ((refill_fsm_q == REFILL_WRITE) && (refill_cnt_q == 0)) begin
            refill_set_q <= mshr_ack_nline[0                  +: HPDCACHE_SET_WIDTH];
            refill_tag_q <= mshr_ack_nline[HPDCACHE_SET_WIDTH +: HPDCACHE_TAG_WIDTH];;
            refill_way_q <= refill_victim_way_i;
            refill_sid_q <= mshr_ack_src_id;
            refill_tid_q <= mshr_ack_req_id;
            refill_need_rsp_q <= mshr_ack_need_rsp;
            refill_is_prefetch_q <= mshr_ack_is_prefetch;
            refill_core_rsp_word_q <= mshr_ack_word;
        end
        refill_cnt_q <= refill_cnt_d;
    end
    //  }}}

    //  Miss Status Holding Register component
    //  {{{
    hpdcache_mshr hpdcache_mshr_i (
        .clk_i,
        .rst_ni,

        .empty_o                  (mshr_empty),
        .full_o                   (mshr_full_o),

        .check_i                  (mshr_check_i),
        .check_set_i              (mshr_check_set_i),
        .check_tag_i              (mshr_check_tag_i),
        .hit_o                    (mshr_check_hit_o),
        .alloc_i                  (mshr_alloc),
        .alloc_cs_i               (mshr_alloc_cs),
        .alloc_nline_i            (mshr_alloc_nline_i),
        .alloc_req_id_i           (mshr_alloc_tid_i),
        .alloc_src_id_i           (mshr_alloc_sid_i),
        .alloc_word_i             (mshr_alloc_word_i),
        .alloc_need_rsp_i         (mshr_alloc_need_rsp_i),
        .alloc_is_prefetch_i      (mshr_alloc_is_prefetch_i),
        .alloc_full_o             (mshr_alloc_full_o),
        .alloc_set_o              (mshr_alloc_set_d),
        .alloc_tag_o              (mshr_alloc_tag_d),
        .alloc_way_o              (mshr_alloc_way_d),

        .ack_i                    (mshr_ack),
        .ack_cs_i                 (mshr_ack_cs),
        .ack_set_i                (mshr_ack_set),
        .ack_way_i                (mshr_ack_way),
        .ack_req_id_o             (mshr_ack_req_id),
        .ack_src_id_o             (mshr_ack_src_id),
        .ack_nline_o              (mshr_ack_nline),
        .ack_word_o               (mshr_ack_word),
        .ack_need_rsp_o           (mshr_ack_need_rsp),
        .ack_is_prefetch_o        (mshr_ack_is_prefetch)
    );

    //    Indicate to the cache controller that there is no pending miss. This
    //    is, when the MSHR is empty, and the MISS handler has finished of
    //    processing the last miss response.
    assign mshr_empty_o = mshr_empty & ~refill_busy_o;
    //  }}}

    //  Assertions
    //  {{{
    //  pragma translate_off
    initial assert (HPDcacheMemIdWidth >= (HPDCACHE_MSHR_SET_WIDTH + HPDCACHE_MSHR_WAY_WIDTH)) else
            $error("miss_handler: not enough ID bits in the memory interface");
    //  pragma translate_on
    //  }}}

endmodule
//  }}}
