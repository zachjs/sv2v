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
 *  Description   : HPDcache Write Buffer
 *  History       :
 */
module hpdcache_wbuf
    //  Parameters
    //  {{{
#(
    //  Number of entries in the directory part of the Write Buffer
    parameter int unsigned WBUF_DIR_ENTRIES      = 0,
    //  Number of entries in the data part of the Write Buffer
    parameter int unsigned WBUF_DATA_ENTRIES     = 0,
    //  Width in bits of the write words
    parameter int unsigned WBUF_WORD_WIDTH       = 0,
    //  Number of words per line in the write buffer
    parameter int unsigned WBUF_WORDS            = 0,
    //  Width in bits of the physical address
    parameter int unsigned WBUF_PA_WIDTH         = 0,
    //  Maximum value of the time counter
    parameter int unsigned WBUF_TIMECNT_MAX      = 8,
    //  Number of most significant bits to check for read conflicts
    parameter int unsigned WBUF_READ_MATCH_WIDTH = 0,
    //  Use a feedthrough FIFO on the send interface
    parameter bit WBUF_SEND_FEEDTHROUGH = 0,

    localparam int unsigned WBUF_OFFSET_WIDTH   = $clog2((WBUF_WORD_WIDTH*WBUF_WORDS)/8),
    localparam int unsigned WBUF_TAG_WIDTH      = WBUF_PA_WIDTH - WBUF_OFFSET_WIDTH,
    localparam int unsigned WBUF_WORD_OFFSET    = $clog2(WBUF_WORD_WIDTH/8),
    localparam int unsigned WBUF_DATA_PTR_WIDTH = $clog2(WBUF_DATA_ENTRIES),
    localparam int unsigned WBUF_DIR_PTR_WIDTH  = $clog2(WBUF_DIR_ENTRIES),
    localparam int unsigned WBUF_TIMECNT_WIDTH  = $clog2(WBUF_TIMECNT_MAX),
    localparam type wbuf_addr_t      = logic unsigned [        WBUF_PA_WIDTH-1:0],
    localparam type wbuf_dir_ptr_t   = logic unsigned [   WBUF_DIR_PTR_WIDTH-1:0],
    localparam type wbuf_data_ptr_t  = logic unsigned [  WBUF_DATA_PTR_WIDTH-1:0],
    localparam type wbuf_data_t      = logic          [      WBUF_WORD_WIDTH-1:0],
    localparam type wbuf_be_t        = logic          [    WBUF_WORD_WIDTH/8-1:0],
    localparam type wbuf_data_buf_t  = wbuf_data_t    [           WBUF_WORDS-1:0],
    localparam type wbuf_be_buf_t    = wbuf_be_t      [           WBUF_WORDS-1:0],
    localparam type wbuf_tag_t       = logic unsigned [       WBUF_TAG_WIDTH-1:0],
    localparam type wbuf_match_t     = logic unsigned [WBUF_READ_MATCH_WIDTH-1:0],
    localparam type wbuf_timecnt_t   = logic unsigned [   WBUF_TIMECNT_WIDTH-1:0]
)
    //  }}}
    //  Ports
    //  {{{
(
    //  Clock and reset signals
    input  logic                  clk_i,
    input  logic                  rst_ni,

    //  Global control signals
    output logic                  empty_o,
    output logic                  full_o,
    input  logic                  flush_all_i,

    //  Configuration signals
    //    Timer threshold
    input  wbuf_timecnt_t         cfg_threshold_i,
    //    Reset timer on write
    input  logic                  cfg_reset_timecnt_on_write_i,
    //    Sequentialize write-after-write hazards
    input  logic                  cfg_sequential_waw_i,
    //    Inhibit write coalescing
    input  logic                  cfg_inhibit_write_coalescing_i,

    //  Write interface
    input  logic                  write_i,
    output logic                  write_ready_o,
    input  wbuf_addr_t            write_addr_i,
    input  wbuf_data_t            write_data_i,
    input  wbuf_be_t              write_be_i,  // byte-enable
    input  logic                  write_uc_i,  // uncacheable write

    //  Read hit interface
    input  wbuf_addr_t            read_addr_i,
    output logic                  read_hit_o,
    input  logic                  read_flush_hit_i,

    //  Replay hit interface
    input  wbuf_addr_t            replay_addr_i,
    input  logic                  replay_is_read_i,
    output logic                  replay_open_hit_o,
    output logic                  replay_pend_hit_o,
    output logic                  replay_sent_hit_o,
    output logic                  replay_not_ready_o,

    //  Send interface
    input  logic                  send_meta_ready_i,
    output logic                  send_meta_valid_o,
    output wbuf_addr_t            send_addr_o,
    output wbuf_dir_ptr_t         send_id_o,
    output logic                  send_uc_o,

    input  logic                  send_data_ready_i,
    output logic                  send_data_valid_o,
    output wbuf_addr_t            send_data_tag_o,
    output wbuf_data_buf_t        send_data_o,
    output wbuf_be_buf_t          send_be_o,

    //  Acknowledge interface
    input  logic                  ack_i,
    input  wbuf_dir_ptr_t         ack_id_i,
    input  logic                  ack_error_i
);
    //  }}}

    //  Definition of constants, types and functions
    //  {{{
    localparam int WBUF_SEND_FIFO_DEPTH = WBUF_DATA_ENTRIES;

    typedef logic unsigned [31:0]          wbuf_uint;

    typedef enum logic [1:0] {
        WBUF_FREE = 2'b00, // unused/free slot
        WBUF_OPEN = 2'b01, // there are pending writes in this slot
        WBUF_PEND = 2'b10, // the slot is waiting to be sent
        WBUF_SENT = 2'b11  // the slot is sent and waits for the memory acknowledge
    } wbuf_state_e;

    typedef struct packed {
        wbuf_data_ptr_t ptr;
        wbuf_timecnt_t  cnt;
        wbuf_tag_t      tag;
        logic           uc;
    } wbuf_dir_entry_t;

    typedef struct packed {
        wbuf_data_buf_t data;
        wbuf_be_buf_t   be;
    } wbuf_data_entry_t;

    typedef struct packed {
        wbuf_data_ptr_t send_data_ptr;
        wbuf_tag_t      send_data_tag;
    } wbuf_send_data_t;

    typedef struct packed {
        wbuf_tag_t      send_meta_tag;
        wbuf_dir_ptr_t  send_meta_id;
        logic           send_meta_uc;
    } wbuf_send_meta_t;

    function automatic wbuf_dir_ptr_t wbuf_dir_find_next(
            input wbuf_dir_ptr_t curr_ptr,
            input wbuf_state_e [WBUF_DIR_ENTRIES-1:0] dir_state,
            input wbuf_state_e state);
        automatic wbuf_dir_ptr_t next_ptr;
        for (int unsigned i = 0; i < WBUF_DIR_ENTRIES; i++) begin
            next_ptr = wbuf_dir_ptr_t'((i + int'(curr_ptr) + 1) % WBUF_DIR_ENTRIES);
            if (dir_state[next_ptr] == state) begin
                return next_ptr;
            end
        end
        return curr_ptr;
    endfunction

    function automatic wbuf_data_ptr_t wbuf_data_find_next(
            input wbuf_data_ptr_t curr_ptr,
            input logic [WBUF_DATA_ENTRIES-1:0] data_valid,
            input logic state);
        automatic wbuf_data_ptr_t next_ptr;
        for (int unsigned i = 0; i < WBUF_DATA_ENTRIES; i++) begin
            next_ptr = wbuf_data_ptr_t'((i + int'(curr_ptr) + 1) % WBUF_DATA_ENTRIES);
            if (data_valid[next_ptr] == state) begin
                return next_ptr;
            end
        end
        return curr_ptr;
    endfunction

    function automatic void wbuf_data_write(
            output wbuf_data_buf_t wbuf_ret_data,
            output wbuf_be_buf_t   wbuf_ret_be,
            input  wbuf_data_buf_t wbuf_old_data,
            input  wbuf_be_buf_t   wbuf_old_be,
            input  wbuf_data_buf_t wbuf_new_data,
            input  wbuf_be_buf_t   wbuf_new_be);
        for (int unsigned w = 0; w < WBUF_WORDS; w++) begin
            for (int unsigned b = 0; b < WBUF_WORD_WIDTH/8; b++) begin
                wbuf_ret_data[w][b*8 +: 8] = wbuf_new_be[w][b] ?
                        wbuf_new_data[w][b*8 +: 8] :
                        wbuf_old_data[w][b*8 +: 8];
            end
            wbuf_ret_be[w] = wbuf_old_be[w] | wbuf_new_be[w];
        end
    endfunction

    function automatic wbuf_match_t wbuf_tag_to_match_addr(wbuf_tag_t tag);
        return tag[WBUF_TAG_WIDTH - 1:WBUF_TAG_WIDTH - WBUF_READ_MATCH_WIDTH];
    endfunction
    //  }}}

    //  Definition of internal wires and registers
    //  {{{
    wbuf_state_e      [ WBUF_DIR_ENTRIES-1:0]   wbuf_dir_state_q, wbuf_dir_state_d;
    wbuf_dir_entry_t  [ WBUF_DIR_ENTRIES-1:0]   wbuf_dir_q, wbuf_dir_d;
    logic             [WBUF_DATA_ENTRIES-1:0]   wbuf_data_valid_q, wbuf_data_valid_d;
    wbuf_data_entry_t [WBUF_DATA_ENTRIES-1:0]   wbuf_data_q, wbuf_data_d;

    wbuf_dir_ptr_t                              wbuf_dir_free_ptr_q, wbuf_dir_free_ptr_d;
    logic                                       wbuf_dir_free;
    wbuf_dir_ptr_t                              wbuf_dir_send_ptr_q, wbuf_dir_send_ptr_d;
    wbuf_data_ptr_t                             wbuf_data_free_ptr_q, wbuf_data_free_ptr_d;
    logic                                       wbuf_data_free;

    logic                                       wbuf_write_free;
    logic                                       wbuf_write_hit_open;
    logic                                       wbuf_write_hit_pend;
    logic                                       wbuf_write_hit_sent;
    wbuf_dir_ptr_t                              wbuf_write_hit_open_dir_ptr;
    wbuf_dir_ptr_t                              wbuf_write_hit_pend_dir_ptr;

    logic                                       send_meta_valid;
    logic                                       send_meta_ready;
    wbuf_send_meta_t                            send_meta_wdata, send_meta_rdata;

    logic                                       send_data_wok;
    logic                                       send_data_w;
    wbuf_send_data_t                            send_data_d;
    wbuf_send_data_t                            send_data_q;

    wbuf_tag_t                                  write_tag;
    wbuf_data_buf_t                             write_data;
    wbuf_be_buf_t                               write_be;

    logic [WBUF_DIR_ENTRIES-1:0]                replay_match;
    logic [WBUF_DIR_ENTRIES-1:0]                replay_open_hit;
    logic [WBUF_DIR_ENTRIES-1:0]                replay_pend_hit;
    logic [WBUF_DIR_ENTRIES-1:0]                replay_sent_hit;

    genvar                                      gen_i;
    //  }}}

    //  Global control signals
    //  {{{
    always_comb
    begin : empty_comb
        empty_o = 1'b1;
        for (int unsigned i = 0; i < WBUF_DIR_ENTRIES; i++) begin
            empty_o &= (wbuf_dir_state_q[i] == WBUF_FREE);
        end
    end

    always_comb
    begin : full_comb
        full_o = 1'b1;
        for (int unsigned i = 0; i < WBUF_DIR_ENTRIES; i++) begin
            full_o &= (wbuf_dir_state_q[i] != WBUF_FREE);
        end
    end
    //  }}}

    //  Write control
    //  {{{
    assign write_tag = write_addr_i[WBUF_PA_WIDTH-1:WBUF_OFFSET_WIDTH];

    always_comb
    begin : wbuf_write_data_comb
        for (int unsigned w = 0; w < WBUF_WORDS; w++) begin
            write_data[w] = write_data_i;
        end
    end

    generate
        if (WBUF_OFFSET_WIDTH > WBUF_WORD_OFFSET) begin : wbuf_write_be_gt_gen
            always_comb
            begin : wbuf_write_be_comb
                for (int unsigned w = 0; w < WBUF_WORDS; w++) begin
                    if (w == int'(write_addr_i[WBUF_OFFSET_WIDTH-1:WBUF_WORD_OFFSET])) begin
                        write_be[w] = write_be_i;
                    end else begin
                        write_be[w] = '0;
                    end
                end
            end
        end else begin : wbuf_write_be_le_gen
            always_comb
            begin : wbuf_write_be_comb
                for (int unsigned w = 0; w < WBUF_WORDS; w++) begin
                    write_be[w] = write_be_i;
                end
            end
        end
    endgenerate

    always_comb
    begin : wbuf_free_comb
        wbuf_dir_free_ptr_d = wbuf_dir_free_ptr_q;
        if (ack_i) begin
            wbuf_dir_free_ptr_d = ack_id_i;
        end else if (write_i && wbuf_write_free) begin
            wbuf_dir_free_ptr_d = wbuf_dir_find_next(wbuf_dir_free_ptr_q, wbuf_dir_state_q, WBUF_FREE);
        end

        wbuf_data_free_ptr_d = wbuf_data_free_ptr_q;
        if (send_data_valid_o && send_data_ready_i) begin
            wbuf_data_free_ptr_d = send_data_q.send_data_ptr;
        end else if (write_i && wbuf_write_free) begin
            wbuf_data_free_ptr_d = wbuf_data_find_next(wbuf_data_free_ptr_q, wbuf_data_valid_q, 1'b0);
        end
    end

    assign wbuf_dir_free  = (wbuf_dir_state_q[wbuf_dir_free_ptr_q] == WBUF_FREE);
    assign wbuf_data_free = ~wbuf_data_valid_q[wbuf_data_free_ptr_q];

    always_comb
    begin : wbuf_write_hit_comb
        wbuf_write_hit_open = 1'b0;
        wbuf_write_hit_pend = 1'b0;
        wbuf_write_hit_sent = 1'b0;

        wbuf_write_hit_open_dir_ptr = 0;
        wbuf_write_hit_pend_dir_ptr = 0;
        for (int unsigned i = 0; i < WBUF_DIR_ENTRIES; i++) begin
            if (wbuf_dir_q[i].tag == write_tag) begin
                unique case (wbuf_dir_state_q[i])
                    WBUF_OPEN: begin
                        wbuf_write_hit_open = 1'b1;
                        wbuf_write_hit_open_dir_ptr = wbuf_dir_ptr_t'(i);
                    end
                    WBUF_PEND: begin
                        wbuf_write_hit_pend = 1'b1;
                        wbuf_write_hit_pend_dir_ptr = wbuf_dir_ptr_t'(i);
                    end
                    WBUF_SENT: begin
                        wbuf_write_hit_sent = 1'b1;
                    end
                    default: begin
                        /* do nothing */
                    end
                endcase
            end
        end
    end

    //  Check if there is a match between the read address and the tag of one
    //  of the used slots in the write buffer directory
    always_comb
    begin : read_hit_comb
        automatic logic [WBUF_DIR_ENTRIES-1:0] read_hit;

        for (int unsigned i = 0; i < WBUF_DIR_ENTRIES; i++) begin
            read_hit[i] = 1'b0;
            unique case (wbuf_dir_state_q[i])
                WBUF_OPEN, WBUF_PEND, WBUF_SENT: begin
                    automatic wbuf_addr_t  wbuf_addr;
                    automatic wbuf_match_t wbuf_tag;
                    automatic wbuf_match_t read_tag;

                    wbuf_addr   = wbuf_addr_t'(wbuf_dir_q[i].tag) << WBUF_OFFSET_WIDTH;
                    read_tag    = read_addr_i[WBUF_PA_WIDTH-1:WBUF_PA_WIDTH - WBUF_READ_MATCH_WIDTH];
                    wbuf_tag    = wbuf_addr  [WBUF_PA_WIDTH-1:WBUF_PA_WIDTH - WBUF_READ_MATCH_WIDTH];
                    read_hit[i] = (read_tag == wbuf_tag) ? 1'b1 : 1'b0;
                end
                default: begin
                    /* do nothing */
                end
            endcase
        end

        read_hit_o = |read_hit;
    end

    //  Check if there is a match between the replay address and the tag of one
    //  of the used slots in the write buffer directory
    generate
        for (gen_i = 0; gen_i < WBUF_DIR_ENTRIES; gen_i++) begin : replay_match_gen
            assign replay_match[gen_i] = replay_is_read_i ?
                    /* replay is read: compare address block tag (e.g. cache line) */
                    (wbuf_tag_to_match_addr(wbuf_dir_q[gen_i].tag) ==
                        replay_addr_i[WBUF_PA_WIDTH - 1:WBUF_PA_WIDTH - WBUF_READ_MATCH_WIDTH]) :
                    /* replay is write: compare wbuf tag */
                    (wbuf_dir_q[gen_i].tag ==
                        replay_addr_i[WBUF_PA_WIDTH - 1:WBUF_PA_WIDTH - WBUF_TAG_WIDTH]);

            assign replay_open_hit[gen_i] =
                    replay_match[gen_i] && (wbuf_dir_state_q[gen_i] == WBUF_OPEN);
            assign replay_pend_hit[gen_i] =
                    replay_match[gen_i] && (wbuf_dir_state_q[gen_i] == WBUF_PEND);
            assign replay_sent_hit[gen_i] =
                    replay_match[gen_i] && (wbuf_dir_state_q[gen_i] == WBUF_SENT);
        end
    endgenerate

    assign replay_open_hit_o = |replay_open_hit,
           replay_pend_hit_o = |replay_pend_hit,
           replay_sent_hit_o = |replay_sent_hit;

    always_comb
    begin : replay_wbuf_not_ready_comb
        replay_not_ready_o = 1'b0;
        if (replay_pend_hit_o) begin
            replay_not_ready_o = 1'b1;
        end else if (replay_sent_hit_o && cfg_sequential_waw_i) begin
            replay_not_ready_o = 1'b1;
        end else if (!replay_open_hit_o && (!wbuf_dir_free || !wbuf_data_free)) begin
            replay_not_ready_o = 1'b1;
        end
    end

    assign wbuf_write_free =
                wbuf_dir_free
            &   wbuf_data_free
            &  ~wbuf_write_hit_open
            &  ~wbuf_write_hit_pend
            & ~(wbuf_write_hit_sent & cfg_sequential_waw_i);

    assign write_ready_o = wbuf_write_free
                           | ((wbuf_write_hit_open | wbuf_write_hit_pend)
                             & ~cfg_inhibit_write_coalescing_i);
    //  }}}

    //  Update control
    //  {{{
    always_comb
    begin : wbuf_update_comb
        automatic bit timeout;
        automatic bit write_hit;
        automatic bit read_hit;
        automatic bit match_open_ptr;
        automatic bit match_pend_ptr;
        automatic bit match_free;
        automatic bit send;

        timeout = 1'b0;
        write_hit = 1'b0;
        read_hit = 1'b0;
        match_open_ptr = 1'b0;
        match_pend_ptr = 1'b0;
        match_free = 1'b0;
        send = 1'b0;

        wbuf_dir_state_d = wbuf_dir_state_q;
        wbuf_dir_d = wbuf_dir_q;
        wbuf_data_d = wbuf_data_q;

        send_data_w = 1'b0;
        send_meta_valid = 1'b0;

        for (int unsigned i = 0; i < WBUF_DIR_ENTRIES; i++) begin
            case (wbuf_dir_state_q[i])
                WBUF_FREE: begin
                    match_free = wbuf_write_free && (i == int'(wbuf_dir_free_ptr_q));

                    if (write_i && match_free) begin
                        send = (cfg_threshold_i == 0)
                               | write_uc_i
                               | flush_all_i
                               | cfg_inhibit_write_coalescing_i;

                        wbuf_dir_state_d[i] = send ? WBUF_PEND : WBUF_OPEN;
                        wbuf_dir_d[i].tag = write_tag;
                        wbuf_dir_d[i].cnt = 0;
                        wbuf_dir_d[i].ptr = wbuf_data_free_ptr_q;
                        wbuf_dir_d[i].uc  = write_uc_i;

                        wbuf_data_write(
                            wbuf_data_d[wbuf_data_free_ptr_q].data,
                            wbuf_data_d[wbuf_data_free_ptr_q].be,
                            '0,
                            '0,
                            write_data,
                            write_be
                        );
                    end
                end

                WBUF_OPEN: begin
                    match_open_ptr  = (i == int'(wbuf_write_hit_open_dir_ptr));
                    timeout         = (wbuf_dir_q[i].cnt == (cfg_threshold_i - 1));
                    read_hit        = read_flush_hit_i & wbuf_write_hit_open & match_open_ptr;
                    write_hit       = write_i
                                      & wbuf_write_hit_open
                                      & match_open_ptr
                                      & ~cfg_inhibit_write_coalescing_i;

                    if (!flush_all_i) begin
                        if (write_hit && cfg_reset_timecnt_on_write_i) begin
                            timeout = 1'b0;
                            wbuf_dir_d[i].cnt = 0;
                        end else if (!timeout) begin
                            wbuf_dir_d[i].cnt = wbuf_dir_q[i].cnt + 1;
                        end

                        if (read_hit | timeout | cfg_inhibit_write_coalescing_i) begin
                            wbuf_dir_state_d[i] = WBUF_PEND;
                        end
                    end else begin
                        wbuf_dir_state_d[i] = WBUF_PEND;
                    end

                    if (write_hit) begin
                        wbuf_data_write(
                            wbuf_data_d[wbuf_dir_q[i].ptr].data,
                            wbuf_data_d[wbuf_dir_q[i].ptr].be,
                            wbuf_data_q[wbuf_dir_q[i].ptr].data,
                            wbuf_data_q[wbuf_dir_q[i].ptr].be,
                            write_data,
                            write_be
                        );
                    end
                end

                WBUF_PEND: begin
                    match_pend_ptr = (i == int'(wbuf_write_hit_pend_dir_ptr));
                    write_hit = write_i
                                & wbuf_write_hit_pend
                                & match_pend_ptr
                                & ~cfg_inhibit_write_coalescing_i;

                    if (write_hit) begin
                        wbuf_data_write(
                            wbuf_data_d[wbuf_dir_q[i].ptr].data,
                            wbuf_data_d[wbuf_dir_q[i].ptr].be,
                            wbuf_data_q[wbuf_dir_q[i].ptr].data,
                            wbuf_data_q[wbuf_dir_q[i].ptr].be,
                            write_data,
                            write_be
                        );
                    end

                    if (i == int'(wbuf_dir_send_ptr_q)) begin
                        send_data_w = send_meta_ready;
                        send_meta_valid  = send_data_wok;
                        if (send_meta_ready && send_data_wok) begin
                            wbuf_dir_state_d[i] = WBUF_SENT;
                        end
                    end
                end

                WBUF_SENT: begin
                    if (ack_i && (i == int'(ack_id_i))) begin
                        wbuf_dir_state_d[i] = WBUF_FREE;
                    end
                end
            endcase
        end
    end

    always_comb
    begin : wbuf_data_valid_comb
        wbuf_data_valid_d = wbuf_data_valid_q;

        //  allocate a free data buffer on new write
        if (write_i && wbuf_write_free) begin
            wbuf_data_valid_d[wbuf_data_free_ptr_q] = 1'b1;
        end

        //  de-allocate a data buffer as soon as it is send
        if (send_data_valid_o && send_data_ready_i) begin
            wbuf_data_valid_d[send_data_q.send_data_ptr] = 1'b0;
        end
    end
    //  }}}

    //  Send control
    //  {{{
    //    Data channel
    hpdcache_fifo_reg #(
        .FIFO_DEPTH          (WBUF_SEND_FIFO_DEPTH),
        .FEEDTHROUGH         (WBUF_SEND_FEEDTHROUGH),
        .fifo_data_t         (wbuf_send_data_t)
    ) send_data_ptr_fifo_i (
        .clk_i,
        .rst_ni,
        .w_i                 (send_data_w),
        .wok_o               (send_data_wok),
        .wdata_i             (send_data_d),
        .r_i                 (send_data_ready_i),
        .rok_o               (send_data_valid_o),
        .rdata_o             (send_data_q)
    );

    assign send_data_d.send_data_ptr = wbuf_dir_q[wbuf_dir_send_ptr_q].ptr,
           send_data_d.send_data_tag = wbuf_dir_q[wbuf_dir_send_ptr_q].tag;

    assign send_data_tag_o   = wbuf_addr_t'(send_data_q.send_data_tag),
           send_data_o       = wbuf_data_q[send_data_q.send_data_ptr].data,
           send_be_o         = wbuf_data_q[send_data_q.send_data_ptr].be;

    //    Meta-data channel
    hpdcache_fifo_reg #(
        .FIFO_DEPTH          (WBUF_SEND_FIFO_DEPTH),
        .FEEDTHROUGH         (WBUF_SEND_FEEDTHROUGH),
        .fifo_data_t         (wbuf_send_meta_t)
    ) send_meta_fifo_i (
        .clk_i,
        .rst_ni,
        .w_i                 (send_meta_valid),
        .wok_o               (send_meta_ready),
        .wdata_i             (send_meta_wdata),
        .r_i                 (send_meta_ready_i),
        .rok_o               (send_meta_valid_o),
        .rdata_o             (send_meta_rdata)
    );

    assign send_meta_wdata.send_meta_tag = wbuf_dir_q[wbuf_dir_send_ptr_q].tag,
           send_meta_wdata.send_meta_id  = wbuf_dir_send_ptr_q,
           send_meta_wdata.send_meta_uc  = wbuf_dir_q[wbuf_dir_send_ptr_q].uc;

    assign send_addr_o = { send_meta_rdata.send_meta_tag, {WBUF_OFFSET_WIDTH{1'b0}} },
           send_id_o   = send_meta_rdata.send_meta_id,
           send_uc_o   = send_meta_rdata.send_meta_uc;

    //    Send pointer
    always_comb
    begin : wbuf_send_comb
        wbuf_dir_send_ptr_d = wbuf_dir_find_next(wbuf_dir_send_ptr_q, wbuf_dir_state_q, WBUF_PEND);
        if (wbuf_dir_state_q[wbuf_dir_send_ptr_q] == WBUF_PEND) begin
            if (!send_meta_valid || !send_meta_ready) begin
                wbuf_dir_send_ptr_d = wbuf_dir_send_ptr_q;
            end
        end
    end
    //  }}}

    //  Internal state assignment
    //  {{{
    always_ff @(posedge clk_i) wbuf_data_q <= wbuf_data_d;

    always_ff @(posedge clk_i or negedge rst_ni)
    begin : wbuf_state_ff
        if (!rst_ni) begin
            wbuf_dir_q           <= '0;
            wbuf_dir_state_q     <= {WBUF_DIR_ENTRIES{WBUF_FREE}};
            wbuf_data_valid_q    <= '0;
            wbuf_dir_free_ptr_q  <= 0;
            wbuf_dir_send_ptr_q  <= 0;
            wbuf_data_free_ptr_q <= 0;
        end else begin
            wbuf_dir_q           <= wbuf_dir_d;
            wbuf_dir_state_q     <= wbuf_dir_state_d;
            wbuf_data_valid_q    <= wbuf_data_valid_d;
            wbuf_dir_free_ptr_q  <= wbuf_dir_free_ptr_d;
            wbuf_dir_send_ptr_q  <= wbuf_dir_send_ptr_d;
            wbuf_data_free_ptr_q <= wbuf_data_free_ptr_d;
        end
    end
    //  }}}

    //  Assertions
    //  {{{
    //  pragma translate_off
    initial assert(WBUF_WORDS inside {1, 2, 4, 8, 16}) else
            $error("WBUF: width of data buffers must be a power of 2");
    ack_sent_assert: assert property (@(posedge clk_i) disable iff (!rst_ni)
            (ack_i -> (wbuf_dir_state_q[ack_id_i] == WBUF_SENT))) else
            $error("WBUF: acknowledging a not SENT slot");
    send_pend_assert: assert property (@(posedge clk_i) disable iff (!rst_ni)
            (send_meta_valid -> (wbuf_dir_state_q[wbuf_dir_send_ptr_q] == WBUF_PEND))) else
            $error("WBUF: sending a not PEND slot");
    send_valid_data_assert: assert property (@(posedge clk_i) disable iff (!rst_ni)
            (send_data_valid_o -> (wbuf_data_valid_q[send_data_q.send_data_ptr] == 1'b1))) else
            $error("WBUF: sending a not valid data");
    //  pragma translate_on
    //  }}}
endmodule
