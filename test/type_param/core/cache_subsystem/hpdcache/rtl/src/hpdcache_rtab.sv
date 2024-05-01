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
 *  Creation Date : September, 2021
 *  Description   : HPDcache Replay Table
 *  History       :
 */
module hpdcache_rtab
import hpdcache_pkg::*;
//  Parameters
//  {{{
#(
    parameter type rtab_entry_t = logic
)
//  }}}
//  Ports
//  {{{
(
    //  Clock and reset signals
    input  logic                  clk_i,
    input  logic                  rst_ni,

    //  Global control signals
    output logic                  empty_o,          // RTAB is empty
    output logic                  full_o,           // RTAB is full

    //  Check RTAB signals
    //     This interface allows to check if there is an address-overlapping
    //     request in the RTAB with respect to the given nline.
    input  logic                  check_i,          // Check for hit (nline) in the RTAB
    input  hpdcache_nline_t       check_nline_i,
    output logic                  check_hit_o,

    //  Allocate signals
    //     This interface allows to allocate a new request in a new linked list
    input  logic                  alloc_i,
    input  logic                  alloc_and_link_i,
    input  rtab_entry_t           alloc_req_i,
    input  logic                  alloc_mshr_hit_i,
    input  logic                  alloc_mshr_full_i,
    input  logic                  alloc_mshr_ready_i,
    input  logic                  alloc_wbuf_hit_i,
    input  logic                  alloc_wbuf_not_ready_i,

    //  Pop signals
    //     This interface allows to read (and remove) a request from the RTAB
    output logic                  pop_try_valid_o,  // Request ready to be replayed
    input  logic                  pop_try_i,
    output rtab_entry_t           pop_try_req_o,
    output rtab_ptr_t             pop_try_ptr_o,

    //  Pop Commit signals
    //     This interface allows to actually remove a popped request
    input  logic                  pop_commit_i,
    input  rtab_ptr_t             pop_commit_ptr_i,

    //  Pop Rollback signals
    //     This interface allows to put back a popped request
    input  logic                  pop_rback_i,
    input  rtab_ptr_t             pop_rback_ptr_i,
    input  logic                  pop_rback_mshr_hit_i,
    input  logic                  pop_rback_mshr_full_i,
    input  logic                  pop_rback_mshr_ready_i,
    input  logic                  pop_rback_wbuf_hit_i,
    input  logic                  pop_rback_wbuf_not_ready_i,


    //  Control signals from/to WBUF
    output hpdcache_req_addr_t    wbuf_addr_o,      // address to check against ongoing writes
    output logic                  wbuf_is_read_o,   // monitored request is read
    input  logic                  wbuf_hit_open_i,  // Hit on open entry in the write buf
    input  logic                  wbuf_hit_pend_i,  // Hit on pend entry in the write buf
    input  logic                  wbuf_hit_sent_i,  // Hit on sent entry in the write buf
    input  logic                  wbuf_not_ready_i, // Write buffer cannot accept the write

    //  Control signals from the Miss Handler
    input  logic                  miss_ready_i,     // Miss Handler is ready

    //  Control signals from the Refill Handler
    input  logic                  refill_i,         // Active refill
    input  hpdcache_nline_t       refill_nline_i,   // Cache-line index being refilled

    //  Configuration parameters
    input  logic                  cfg_single_entry_i // Enable only one entry of the table
);
//  }}}

//  Definition of constants, types and functions
//  {{{
    localparam int N = HPDCACHE_RTAB_ENTRIES;

    function automatic rtab_ptr_t rtab_bv_to_index(
            input logic [N-1:0] bv);
        for (int i = 0; i < N; i++) begin
            if (bv[i]) return rtab_ptr_t'(i);
        end
        return 0;
    endfunction

    function automatic logic [N-1:0] rtab_index_to_bv(
            input rtab_ptr_t index);
        logic [N-1:0] bv;

        for (int i = 0; i < N; i++) begin
            bv[i] = (rtab_ptr_t'(i) == index);
        end
        return bv;
    endfunction

    function automatic bit rtab_mshr_set_equal(
            input hpdcache_nline_t x,
            input hpdcache_nline_t y);
        return (x[0 +: HPDCACHE_MSHR_SET_WIDTH] == y[0 +: HPDCACHE_MSHR_SET_WIDTH]);
    endfunction

    function automatic logic [N-1:0] rtab_next(rtab_ptr_t [N-1:0] next, rtab_ptr_t x);
        return rtab_index_to_bv(next[x]);
    endfunction

    typedef enum {
        POP_TRY_HEAD,
        POP_TRY_NEXT,
        POP_TRY_NEXT_WAIT
    } rtab_pop_try_state_e;
//  }}}

//  Internal signals and registers
//  {{{
    rtab_entry_t        [N-1:0]  req_q;
    rtab_ptr_t          [N-1:0]  next_q;

    rtab_pop_try_state_e         pop_try_state_q, pop_try_state_d;
    logic               [N-1:0]  pop_try_next_q, pop_try_next_d;

    logic               [N-1:0]  valid_q;
    logic               [N-1:0]  valid_set, valid_rst;
    logic               [N-1:0]  alloc_valid_set;
    logic               [N-1:0]  pop_commit_valid_rst;

    //  Bits indicating  if the corresponding entry is the head of a linked list
    logic               [N-1:0]  head_q;
    logic               [N-1:0]  head_set, head_rst;
    logic               [N-1:0]  alloc_head_set, alloc_head_rst;
    logic               [N-1:0]  pop_try_head_rst;
    logic               [N-1:0]  pop_commit_head_set;
    logic               [N-1:0]  pop_rback_head_set;

    //  Bits indicating  if the corresponding entry is the tail of a linked list
    logic               [N-1:0]  tail_q;
    logic               [N-1:0]  tail_set, tail_rst;
    logic               [N-1:0]  alloc_tail_set, alloc_tail_rst;

    //  There is a pend ing miss on the target nline
    logic               [N-1:0]  deps_mshr_hit_q;
    logic               [N-1:0]  deps_mshr_hit_set, deps_mshr_hit_rst;
    logic               [N-1:0]  alloc_deps_mshr_hit_set;
    logic               [N-1:0]  pop_rback_deps_mshr_hit_set;

    //  The MSHR has no  available slot for the new miss
    logic               [N-1:0]  deps_mshr_full_q;
    logic               [N-1:0]  deps_mshr_full_set, deps_mshr_full_rst;
    logic               [N-1:0]  alloc_deps_mshr_full_set;
    logic               [N-1:0]  pop_rback_deps_mshr_full_set;

    //  The MSHR is not  ready to send a new miss requests
    logic               [N-1:0]  deps_mshr_ready_q;
    logic               [N-1:0]  deps_mshr_ready_set, deps_mshr_ready_rst;
    logic               [N-1:0]  alloc_deps_mshr_ready_set;
    logic               [N-1:0]  pop_rback_deps_mshr_ready_set;

    //  Hit on an non-e mpty entry of the write buffer
    logic               [N-1:0]  deps_wbuf_hit_q;
    logic               [N-1:0]  deps_wbuf_hit_set, deps_wbuf_hit_rst;
    logic               [N-1:0]  alloc_deps_wbuf_hit_set;
    logic               [N-1:0]  pop_rback_deps_wbuf_hit_set;

    //  Hit on a pend entry of the write buffer
    logic               [N-1:0]  deps_wbuf_not_ready_q;
    logic               [N-1:0]  deps_wbuf_not_ready_set, deps_wbuf_not_ready_rst;
    logic               [N-1:0]  alloc_deps_wbuf_not_ready_set;
    logic               [N-1:0]  pop_rback_deps_wbuf_not_ready_set;

    logic               [N-1:0]  nodeps;
    hpdcache_nline_t    [N-1:0]  nline;
    hpdcache_req_addr_t [N-1:0]  addr;
    logic               [N-1:0]  is_read;
    logic               [N-1:0]  check_hit;
    logic               [N-1:0]  match_check_nline;
    logic               [N-1:0]  match_check_tail;
    logic               [N-1:0]  match_refill_nline;
    logic               [N-1:0]  match_refill_mshr_set;

    logic               [N-1:0]  free;
    logic               [N-1:0]  free_alloc;
    logic                        alloc;

    logic               [N-1:0]  pop_match_next;
    logic               [N-1:0]  pop_rback_ptr_bv;
    logic               [N-1:0]  pop_try_bv;
    logic               [N-1:0]  ready;

    genvar                       gen_i;
//  }}}

//  Compute global control signals
//  {{{
    //  compute if entries are ready to be replayed
    assign nodeps       = ~(deps_mshr_hit_q |
                            deps_mshr_full_q |
                            deps_mshr_ready_q |
                            deps_wbuf_hit_q |
                            deps_wbuf_not_ready_q);

    assign ready        = valid_q & head_q & nodeps;

    assign free         = ~valid_q;

    //  compute the free vector (one-hot signal)
    hpdcache_prio_1hot_encoder #(
        .N         (N)
    ) free_encoder_i (
        .val_i     (free),
        .val_o     (free_alloc)
    );

    //  full and empty signals
    assign empty_o = &(~valid_q);
    assign  full_o = &( valid_q) | (|valid_q & cfg_single_entry_i);
//  }}}

//  Check interface
//  {{{
    generate
        for (gen_i = 0; gen_i < N; gen_i++) begin : check_gen
            assign              addr[gen_i] = {req_q[gen_i].addr_tag, req_q[gen_i].addr_offset},
                               nline[gen_i] = hpdcache_get_req_addr_nline(addr[gen_i]),
                   match_check_nline[gen_i] = (check_nline_i == nline[gen_i]);

            assign is_read[gen_i] =         is_load(req_q[gen_i].op) |
                                    is_cmo_prefetch(req_q[gen_i].op, req_q[gen_i].size);
        end
    endgenerate

    assign check_hit        =  valid_q   & match_check_nline,
           check_hit_o      = |check_hit,
           match_check_tail =  check_hit & tail_q;
//  }}}

//  Allocation process
//  {{{
    assign alloc = alloc_i | alloc_and_link_i;

    //  Set the valid bit-vector of the replay table
    assign alloc_valid_set = free_alloc       & {N{alloc}};

    //  Set of head and tail bit-vectors during an allocation
    //    - The head bit is only set when creating a new linked-list
    //    - The tail bit is always set because new requests are added on the tail.
    assign alloc_head_set  = free_alloc       & {N{alloc_i}},
           alloc_tail_set  = alloc_valid_set;

    //  Reset of head and tail bit-vectors during an allocation
    //    - When doing an allocation and link, head bit shall be reset
    //    - when doing an allocation and link, the "prev" tail shall be reset
    assign alloc_head_rst  = free_alloc       & {N{alloc_and_link_i}},
           alloc_tail_rst  = match_check_tail & {N{alloc_and_link_i}};

    //  Set the dependency bits for the allocated entry
    assign alloc_deps_mshr_hit_set       = alloc_valid_set & {N{      alloc_mshr_hit_i}},
           alloc_deps_mshr_full_set      = alloc_valid_set & {N{     alloc_mshr_full_i}},
           alloc_deps_mshr_ready_set     = alloc_valid_set & {N{    alloc_mshr_ready_i}},
           alloc_deps_wbuf_hit_set       = alloc_valid_set & {N{      alloc_wbuf_hit_i}},
           alloc_deps_wbuf_not_ready_set = alloc_valid_set & {N{alloc_wbuf_not_ready_i}};
//  }}}

//  Update replay table dependencies
//  {{{
    //  Update write buffer hit dependencies
    //  {{{
    //  Build a bit-vector with HEAD requests waiting for a conflict in the wbuf
    logic [N-1:0]  wbuf_rd_pending, wbuf_wr_pending;
    logic [N-1:0]  wbuf_rd_gnt, wbuf_wr_gnt;
    logic [  1:0]  wbuf_pending;
    logic [  1:0]  wbuf_gnt;
    logic          wbuf_ready;
    logic [N-1:0]  wbuf_sel;

    assign wbuf_rd_pending = valid_q & head_q & deps_wbuf_hit_q,
           wbuf_wr_pending = valid_q & head_q & deps_wbuf_not_ready_q;

    //  Choose in a round-robin manner a ready transaction waiting for a conflict in the wbuf
    hpdcache_rrarb #(
        .N              (N)
    ) wbuf_rd_pending_arb_i (
        .clk_i,
        .rst_ni,
        .req_i          (wbuf_rd_pending),
        .gnt_o          (wbuf_rd_gnt),
        .ready_i        (wbuf_gnt[0] & wbuf_ready)
    );

    hpdcache_rrarb #(
        .N              (N)
    ) wbuf_wr_pending_arb_i (
        .clk_i,
        .rst_ni,
        .req_i          (wbuf_wr_pending),
        .gnt_o          (wbuf_wr_gnt),
        .ready_i        (wbuf_gnt[1] & wbuf_ready)
    );

    assign wbuf_pending = {|wbuf_wr_gnt, |wbuf_rd_gnt},
           wbuf_ready   = |(pop_try_bv & (wbuf_rd_gnt | wbuf_wr_gnt));

    hpdcache_fxarb #(
        .N              (2)
    ) wbuf_pending_arb_i (
        .clk_i,
        .rst_ni,
        .req_i          (wbuf_pending),
        .gnt_o          (wbuf_gnt),
        .ready_i        (wbuf_ready)
    );

    assign wbuf_sel = wbuf_gnt[0] ? wbuf_rd_gnt :
                      wbuf_gnt[1] ? wbuf_wr_gnt : '0;

    hpdcache_mux #(
        .NINPUT         (N),
        .DATA_WIDTH     ($bits(hpdcache_req_addr_t)),
        .ONE_HOT_SEL    (1'b1)
    ) wbuf_pending_addr_mux_i (
        .data_i         (addr),
        .sel_i          (wbuf_sel),
        .data_o         (wbuf_addr_o)
    );

    hpdcache_mux #(
        .NINPUT         (N),
        .DATA_WIDTH     (1),
        .ONE_HOT_SEL    (1'b1)
    ) wbuf_pending_is_read_mux_i (
        .data_i         (is_read),
        .sel_i          (wbuf_sel),
        .data_o         (wbuf_is_read_o)
    );

    //  reset write buffer dependency bits with the output from the write buffer
    assign deps_wbuf_hit_rst =
            wbuf_sel & ~{N{wbuf_hit_open_i | wbuf_hit_pend_i | wbuf_hit_sent_i}};
    assign deps_wbuf_not_ready_rst =
            wbuf_sel & ~{N{wbuf_not_ready_i}};
    //  }}}

    //  Update miss handler dependency
    //  {{{
    assign deps_mshr_ready_rst = {N{miss_ready_i}};
    //  }}}

    //  Update refill dependencies
    //  {{{
    generate
        for (gen_i = 0; gen_i < N; gen_i++) begin : match_refill_gen
            assign match_refill_mshr_set[gen_i] =
                    rtab_mshr_set_equal(refill_nline_i, nline[gen_i]);
            assign match_refill_nline[gen_i] =
                    (refill_nline_i == nline[gen_i]);
        end
    endgenerate

    assign deps_mshr_full_rst = {N{refill_i}} & match_refill_mshr_set;
    assign deps_mshr_hit_rst  = {N{refill_i}} & match_refill_nline;
    //  }}}
//  }}}

//  Pop interface
//  {{{
    logic [N-1:0]  pop_sel;
    logic [N-1:0]  pop_commit_bv;

    assign pop_commit_bv = rtab_index_to_bv(pop_commit_ptr_i);

    //  Pop try process
    //  {{{
    logic [N-1:0]  pop_gnt;
    logic          pop_head;

    hpdcache_rrarb #(
        .N              (N)
    ) pop_arb_i (
        .clk_i,
        .rst_ni,
        .req_i          (ready),
        .gnt_o          (pop_gnt),
        .ready_i        (pop_head)
    );

    always_comb
    begin : req_valid_comb
        case(pop_try_state_q)
            POP_TRY_HEAD     : pop_try_valid_o = |ready;
            POP_TRY_NEXT     : pop_try_valid_o = 1'b1;
            POP_TRY_NEXT_WAIT: pop_try_valid_o = 1'b1;
            default          : pop_try_valid_o = 1'b0;
        endcase
    end

    always_comb
    begin : pop_entry_sel_comb
        pop_try_state_d = pop_try_state_q;
        pop_try_next_d = pop_try_next_q;
        pop_head = 1'b0;
        pop_sel = '0;

        case (pop_try_state_q)
            POP_TRY_HEAD: begin
                // This FSM may be in this state after forwarding the tail of
                // a list. In that case, a rollback may arrive in this cycle.
                pop_sel = pop_gnt;
                if (!pop_rback_i && pop_try_valid_o) begin
                    if (pop_try_i) begin
                        //  If the request interface accepts the request, go to the next request
                        //  in the list (if the current request is not the tail). Otherwise, stay in
                        //  the same state to to forward a request from a new list
                        pop_head = 1'b1;
                        if ((pop_gnt & ~tail_q) != 0) begin
                            pop_try_state_d = POP_TRY_NEXT;
                            pop_try_next_d = rtab_next(next_q, pop_try_ptr_o);
                        end
                    end
                end
            end
            POP_TRY_NEXT: begin
                pop_sel     = pop_try_next_q;
                if (pop_rback_i) begin
                    pop_try_state_d = POP_TRY_HEAD;
                end else begin
                    if (pop_try_i) begin
                        //  If the request interface accepts the new request, go to the next request
                        //  in the list (if the current request is not the tail). Otherwise, return
                        //  to the POP_TRY_HEAD state to forward a request from a new list
                        if ((pop_try_next_q & ~tail_q) != 0) begin
                            pop_try_state_d = POP_TRY_NEXT;
                            pop_try_next_d  = rtab_next(next_q, pop_try_ptr_o);
                        end else begin
                            pop_try_state_d = POP_TRY_HEAD;
                        end
                    end else begin
                        //  If the request interface is not ready to consume the new request, wait
                        //  until it is
                        pop_try_state_d = POP_TRY_NEXT_WAIT;
                    end
                end
            end
            POP_TRY_NEXT_WAIT: begin
                //  Wait for the current request to be accepted. Then go to the next request in the
                //  list or to a new list
                pop_sel     = pop_try_next_q;
                if (pop_try_i) begin
                    if ((pop_try_next_q & ~tail_q) != 0) begin
                        pop_try_state_d = POP_TRY_NEXT;
                        pop_try_next_d  = rtab_next(next_q, pop_try_ptr_o);
                    end else begin
                        pop_try_state_d = POP_TRY_HEAD;
                    end
                end
            end
            default: begin
            end
        endcase
    end

    assign pop_commit_head_set  = '0;

    hpdcache_mux #(
        .NINPUT         (N),
        .DATA_WIDTH     ($bits(rtab_entry_t)),
        .ONE_HOT_SEL    (1'b1)
    ) pop_mux_i (
        .data_i         (req_q),
        .sel_i          (pop_sel),
        .data_o         (pop_try_req_o)
    );

    //  Temporarily unset the head bit of the popped request to prevent it to be rescheduled
    assign pop_try_bv       = pop_sel & {N{pop_try_i}},
           pop_try_head_rst = pop_try_bv;


    //  Forward the index of the entry being popped. This is used later by the
    //  commit or rollback operations
    assign pop_try_ptr_o = rtab_bv_to_index(pop_sel);

    //  }}}

    //  Pop commit process
    //  {{{
    //  Invalidate the entry being popped (head of the linked list)
    assign pop_commit_valid_rst = {N{pop_commit_i}} & rtab_index_to_bv(pop_commit_ptr_i);
    //  }}}

    //  Pop rollback process
    //  {{{
    //  Set again the head bit of the rolled-back request
    assign pop_rback_ptr_bv                  = rtab_index_to_bv(pop_rback_ptr_i);

    assign pop_rback_head_set                = {N{pop_rback_i}} & pop_rback_ptr_bv;

    assign pop_rback_deps_mshr_hit_set       = {N{pop_rback_i}} & pop_rback_ptr_bv & {N{pop_rback_mshr_hit_i}},
           pop_rback_deps_mshr_full_set      = {N{pop_rback_i}} & pop_rback_ptr_bv & {N{pop_rback_mshr_full_i}},
           pop_rback_deps_mshr_ready_set     = {N{pop_rback_i}} & pop_rback_ptr_bv & {N{pop_rback_mshr_ready_i}},
           pop_rback_deps_wbuf_hit_set       = {N{pop_rback_i}} & pop_rback_ptr_bv & {N{pop_rback_wbuf_hit_i}},
           pop_rback_deps_wbuf_not_ready_set = {N{pop_rback_i}} & pop_rback_ptr_bv & {N{pop_rback_wbuf_not_ready_i}};
    //  }}}
//  }}}

//  Internal state assignment
//  {{{
    assign head_set                = alloc_head_set | pop_commit_head_set | pop_rback_head_set,
           head_rst                = alloc_head_rst | pop_try_head_rst;

    assign tail_set                = alloc_tail_set,
           tail_rst                = alloc_tail_rst;

    assign valid_set               = alloc_valid_set,
           valid_rst               = pop_commit_valid_rst;

    assign deps_mshr_hit_set       = alloc_deps_mshr_hit_set       | pop_rback_deps_mshr_hit_set,
           deps_mshr_full_set      = alloc_deps_mshr_full_set      | pop_rback_deps_mshr_full_set,
           deps_mshr_ready_set     = alloc_deps_mshr_ready_set     | pop_rback_deps_mshr_ready_set,
           deps_wbuf_hit_set       = alloc_deps_wbuf_hit_set       | pop_rback_deps_wbuf_hit_set,
           deps_wbuf_not_ready_set = alloc_deps_wbuf_not_ready_set | pop_rback_deps_wbuf_not_ready_set;

    always_ff @(posedge clk_i or negedge rst_ni)
    begin : rtab_valid_ff
        if (!rst_ni) begin
            valid_q               <= '0;
            head_q                <= '0;
            tail_q                <= '0;
            deps_mshr_hit_q       <= '0;
            deps_mshr_full_q      <= '0;
            deps_mshr_ready_q     <= '0;
            deps_wbuf_hit_q       <= '0;
            deps_wbuf_not_ready_q <= '0;
            next_q                <= '0;
        end else begin
            valid_q <= (~valid_q &  valid_set) |
                       ( valid_q & ~valid_rst);

            //  update head and tail flags
            head_q <= (~head_q &  head_set) |
                      ( head_q & ~head_rst);

            tail_q <= (~tail_q &  tail_set) |
                      ( tail_q & ~tail_rst);

            //  update dependency flags
            deps_mshr_hit_q       <= (~deps_mshr_hit_q       &  deps_mshr_hit_set) |
                                     ( deps_mshr_hit_q       & ~deps_mshr_hit_rst);
            deps_mshr_full_q      <= (~deps_mshr_full_q      &  deps_mshr_full_set) |
                                     ( deps_mshr_full_q      & ~deps_mshr_full_rst);
            deps_mshr_ready_q     <= (~deps_mshr_ready_q     &  deps_mshr_ready_set) |
                                     ( deps_mshr_ready_q     & ~deps_mshr_ready_rst);
            deps_wbuf_hit_q       <= (~deps_wbuf_hit_q       &  deps_wbuf_hit_set) |
                                     ( deps_wbuf_hit_q       & ~deps_wbuf_hit_rst);
            deps_wbuf_not_ready_q <= (~deps_wbuf_not_ready_q &  deps_wbuf_not_ready_set) |
                                     ( deps_wbuf_not_ready_q & ~deps_wbuf_not_ready_rst);

            //  update the next pointers
            for (int i = 0; i < N; i++) begin
                if (alloc_and_link_i && match_check_tail[i]) begin
                    next_q[i] <= rtab_bv_to_index(free_alloc);
                end
            end
        end
    end

    always_ff @(posedge clk_i or negedge rst_ni)
    begin : pop_try_ff
        if (!rst_ni) begin
            pop_try_state_q <= POP_TRY_HEAD;
            pop_try_next_q  <= '0;
        end else begin
            pop_try_state_q <= pop_try_state_d;
            pop_try_next_q  <= pop_try_next_d;
        end
    end

    always_ff @(posedge clk_i)
    begin : rtab_ff
        for (int i = 0; i < N; i++) begin
            //  update the request array
            if (valid_set[i]) begin
                req_q[i] <= alloc_req_i;
            end
        end
    end
//  }}}

//  Assertions
//  {{{
//  pragma translate_off
    assert property (@(posedge clk_i) disable iff (!rst_ni)
            check_i |-> $onehot0(match_check_tail)) else
                    $error("rtab: more than one entry matching");

    assert property (@(posedge clk_i) disable iff (!rst_ni)
            alloc_and_link_i |-> (check_i & check_hit_o)) else
                    $error("rtab: alloc and link shall be performed in case of check hit");

    assert property (@(posedge clk_i) disable iff (!rst_ni)
            alloc_and_link_i |->
                    ({alloc_req_i.addr_tag, hpdcache_get_req_offset_set(alloc_req_i.addr_offset)} ==
                        check_nline_i)) else
                    $error("rtab: nline for alloc and link shall match the one being checked");

    assert property (@(posedge clk_i) disable iff (!rst_ni)
            alloc_i |-> !alloc_and_link_i) else
                    $error("rtab: only one allocation per cycle is allowed");

`ifndef VERILATOR
    assert property (@(posedge clk_i) disable iff (!rst_ni)
            pop_try_i |-> ##1 (pop_commit_i | pop_rback_i)) else
                    $error("rtab: a pop try shall be followed by a commit or rollback");
`endif

    assert property (@(posedge clk_i) disable iff (!rst_ni)
            pop_commit_i |-> valid_q[pop_commit_ptr_i]) else
                    $error("rtab: commiting an invalid entry");

    assert property (@(posedge clk_i) disable iff (!rst_ni)
            pop_rback_i |-> valid_q[pop_rback_ptr_i]) else
                    $error("rtab: rolling-back an invalid entry");

    assert property (@(posedge clk_i) disable iff (!rst_ni)
            pop_rback_i |-> !pop_try_i) else
                    $error("rtab: cache shall not accept a new request while rolling back");

    assert property (@(posedge clk_i) disable iff (!rst_ni)
            alloc |-> ~full_o) else
                    $error("rtab: trying to allocate while the table is full");

    assert property (@(posedge clk_i) disable iff (!rst_ni)
            alloc_and_link_i |-> ~cfg_single_entry_i) else
                    $error("rtab: trying to link a request in single entry mode");
//  pragma translate_on
//  }}}
endmodule
