// Copyright 2018 ETH Zurich and University of Bologna.
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License.  You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.
//
// Author: Florian Zaruba, ETH Zurich
// Date: 05.05.2017
// Description: CSR Register File as specified by RISC-V


module csr_regfile
  import ariane_pkg::*;
#(
    parameter config_pkg::cva6_cfg_t CVA6Cfg        = config_pkg::cva6_cfg_empty,
    parameter int                    AsidWidth      = 1,
    parameter int unsigned           MHPMCounterNum = 6
) (
    input logic clk_i,  // Clock
    input logic rst_ni,  // Asynchronous reset active low
    input logic time_irq_i,  // Timer threw a interrupt
    // send a flush request out if a CSR with a side effect has changed (e.g. written)
    output logic flush_o,
    output logic halt_csr_o,  // halt requested
    // commit acknowledge
    input  scoreboard_entry_t [CVA6Cfg.NrCommitPorts-1:0] commit_instr_i, // the instruction we want to commit
    input  logic [CVA6Cfg.NrCommitPorts-1:0]              commit_ack_i,   // Commit acknowledged a instruction -> increase instret CSR
    // Core and Cluster ID
    input  logic[riscv::VLEN-1:0] boot_addr_i,                // Address from which to start booting, mtvec is set to the same address
    input  logic[riscv::XLEN-1:0] hart_id_i,                  // Hart id in a multicore environment (reflected in a CSR)
    // we are taking an exception
    input exception_t ex_i,  // We've got an exception from the commit stage, take it

    input fu_op csr_op_i,  // Operation to perform on the CSR file
    input logic [11:0] csr_addr_i,  // Address of the register to read/write
    input logic [riscv::XLEN-1:0] csr_wdata_i,  // Write data in
    output logic [riscv::XLEN-1:0] csr_rdata_o,  // Read data out
    input logic dirty_fp_state_i,  // Mark the FP sate as dirty
    input  logic                  csr_write_fflags_i,         // Write fflags register e.g.: we are retiring a floating point instruction
    input logic dirty_v_state_i,  // Mark the V state as dirty
    input logic [riscv::VLEN-1:0] pc_i,  // PC of instruction accessing the CSR
    output exception_t csr_exception_o,  // attempts to access a CSR without appropriate privilege
                                         // level or to write  a read-only register also
                                         // raises illegal instruction exceptions.
    // Interrupts/Exceptions
    output logic  [riscv::VLEN-1:0] epc_o,                    // Output the exception PC to PC Gen, the correct CSR (mepc, sepc) is set accordingly
    output logic eret_o,  // Return from exception, set the PC of epc_o
    output logic  [riscv::VLEN-1:0] trap_vector_base_o,       // Output base of exception vector, correct CSR is output (mtvec, stvec)
    output riscv::priv_lvl_t priv_lvl_o,  // Current privilege level the CPU is in
    // FP Imprecise exceptions
    input  logic            [4:0] acc_fflags_ex_i,            // Imprecise FP exception from the accelerator (fcsr.fflags format)
    input logic acc_fflags_ex_valid_i,  // An FP exception from the accelerator occurred
    // FPU
    output riscv::xs_t fs_o,  // Floating point extension status
    output logic [4:0] fflags_o,  // Floating-Point Accured Exceptions
    output logic [2:0] frm_o,  // Floating-Point Dynamic Rounding Mode
    output logic [6:0] fprec_o,  // Floating-Point Precision Control
    // Vector extension
    output riscv::xs_t vs_o,  // Vector extension status
    // Decoder
    output irq_ctrl_t irq_ctrl_o,  // interrupt management to id stage
    // MMU
    output logic en_translation_o,  // enable VA translation
    output logic en_ld_st_translation_o,  // enable VA translation for load and stores
    output riscv::priv_lvl_t      ld_st_priv_lvl_o,           // Privilege level at which load and stores should happen
    output logic sum_o,
    output logic mxr_o,
    output logic [riscv::PPNW-1:0] satp_ppn_o,
    output logic [AsidWidth-1:0] asid_o,
    // external interrupts
    input logic [1:0] irq_i,  // external interrupt in
    input logic ipi_i,  // inter processor interrupt -> connected to machine mode sw
    input logic debug_req_i,  // debug request in
    output logic set_debug_pc_o,
    // Virtualization Support
    output logic tvm_o,  // trap virtual memory
    output logic tw_o,  // timeout wait
    output logic tsr_o,  // trap sret
    output logic debug_mode_o,  // we are in debug mode -> that will change some decoding
    output logic single_step_o,  // we are in single-step mode
    // Caches
    output logic icache_en_o,  // L1 ICache Enable
    output logic dcache_en_o,  // L1 DCache Enable
    // Accelerator
    output logic acc_cons_en_o,  // Accelerator memory consistent mode
    // Performance Counter
    output logic [11:0] perf_addr_o,  // read/write address to performance counter module
    output logic [riscv::XLEN-1:0] perf_data_o,  // write data to performance counter module
    input logic [riscv::XLEN-1:0] perf_data_i,  // read data from performance counter module
    output logic perf_we_o,
    // PMPs
    output riscv::pmpcfg_t [15:0] pmpcfg_o,  // PMP configuration containing pmpcfg for max 16 PMPs
    output logic [15:0][riscv::PLEN-3:0] pmpaddr_o,  // PMP addresses
    output logic [31:0] mcountinhibit_o
);
  // internal signal to keep track of access exceptions
  logic read_access_exception, update_access_exception, privilege_violation;
  logic csr_we, csr_read;
  riscv::xlen_t csr_wdata, csr_rdata;
  riscv::priv_lvl_t trap_to_priv_lvl;
  // register for enabling load store address translation, this is critical, hence the register
  logic en_ld_st_translation_d, en_ld_st_translation_q;
  logic mprv;
  logic mret;  // return from M-mode exception
  logic sret;  // return from S-mode exception
  logic dret;  // return from debug mode
  // CSR write causes us to mark the FPU state as dirty
  logic dirty_fp_state_csr;
  riscv::mstatus_rv_t mstatus_q, mstatus_d;
  riscv::xlen_t mstatus_extended;
  riscv::satp_t satp_q, satp_d;
  riscv::dcsr_t dcsr_q, dcsr_d;
  riscv::csr_t csr_addr;
  // privilege level register
  riscv::priv_lvl_t priv_lvl_d, priv_lvl_q;
  // we are in debug
  logic debug_mode_q, debug_mode_d;
  logic mtvec_rst_load_q;  // used to determine whether we came out of reset

  riscv::xlen_t dpc_q, dpc_d;
  riscv::xlen_t dscratch0_q, dscratch0_d;
  riscv::xlen_t dscratch1_q, dscratch1_d;
  riscv::xlen_t mtvec_q, mtvec_d;
  riscv::xlen_t medeleg_q, medeleg_d;
  riscv::xlen_t mideleg_q, mideleg_d;
  riscv::xlen_t mip_q, mip_d;
  riscv::xlen_t mie_q, mie_d;
  riscv::xlen_t mcounteren_q, mcounteren_d;
  riscv::xlen_t mscratch_q, mscratch_d;
  riscv::xlen_t mepc_q, mepc_d;
  riscv::xlen_t mcause_q, mcause_d;
  riscv::xlen_t mtval_q, mtval_d;
  logic fiom_d, fiom_q;

  riscv::xlen_t stvec_q, stvec_d;
  riscv::xlen_t scounteren_q, scounteren_d;
  riscv::xlen_t sscratch_q, sscratch_d;
  riscv::xlen_t sepc_q, sepc_d;
  riscv::xlen_t scause_q, scause_d;
  riscv::xlen_t stval_q, stval_d;
  riscv::xlen_t dcache_q, dcache_d;
  riscv::xlen_t icache_q, icache_d;
  riscv::xlen_t acc_cons_q, acc_cons_d;

  logic wfi_d, wfi_q;

  logic [63:0] cycle_q, cycle_d;
  logic [63:0] instret_q, instret_d;

  riscv::pmpcfg_t [15:0] pmpcfg_q, pmpcfg_d;
  logic [15:0][riscv::PLEN-3:0] pmpaddr_q, pmpaddr_d;
  logic [MHPMCounterNum+3-1:0] mcountinhibit_d, mcountinhibit_q;
  logic [3:0] index;

  localparam riscv::xlen_t IsaCode = (riscv::XLEN'(CVA6Cfg.RVA) <<  0)                // A - Atomic Instructions extension
  | (riscv::XLEN'(CVA6Cfg.RVB) << 1)  // C - Bitmanip extension
  | (riscv::XLEN'(CVA6Cfg.RVC) << 2)  // C - Compressed extension
  | (riscv::XLEN'(CVA6Cfg.RVD) << 3)  // D - Double precision floating-point extension
  | (riscv::XLEN'(CVA6Cfg.RVF) << 5)  // F - Single precision floating-point extension
  | (riscv::XLEN'(1) << 8)  // I - RV32I/64I/128I base ISA
  | (riscv::XLEN'(1) << 12)  // M - Integer Multiply/Divide extension
  | (riscv::XLEN'(0) << 13)  // N - User level interrupts supported
  | (riscv::XLEN'(CVA6Cfg.RVS) << 18)  // S - Supervisor mode implemented
  | (riscv::XLEN'(CVA6Cfg.RVU) << 20)  // U - User mode implemented
  | (riscv::XLEN'(CVA6Cfg.RVV) << 21)  // V - Vector extension
  | (riscv::XLEN'(CVA6Cfg.NSX) << 23)  // X - Non-standard extensions present
  | ((riscv::XLEN == 64 ? 2 : 1) << riscv::XLEN - 2);  // MXL

  assign pmpcfg_o  = pmpcfg_q[15:0];
  assign pmpaddr_o = pmpaddr_q;

  riscv::fcsr_t fcsr_q, fcsr_d;
  // ----------------
  // Assignments
  // ----------------
  assign csr_addr = riscv::csr_t'(csr_addr_i);
  assign fs_o = mstatus_q.fs;
  assign vs_o = mstatus_q.vs;
  // ----------------
  // CSR Read logic
  // ----------------
  assign mstatus_extended = riscv::IS_XLEN64 ? mstatus_q[riscv::XLEN-1:0] :
                              {mstatus_q.sd, mstatus_q.wpri3[7:0], mstatus_q[22:0]};


  always_comb begin : csr_read_process
    // a read access exception can only occur if we attempt to read a CSR which does not exist
    read_access_exception = 1'b0;
    csr_rdata = '0;
    perf_addr_o = csr_addr.address[11:0];
    index = '0;

    if (csr_read) begin
      unique case (csr_addr.address)
        riscv::CSR_FFLAGS: begin
          if (CVA6Cfg.FpPresent) begin
            csr_rdata = {{riscv::XLEN - 5{1'b0}}, fcsr_q.fflags};
          end else begin
            read_access_exception = 1'b1;
          end
        end
        riscv::CSR_FRM: begin
          if (CVA6Cfg.FpPresent) begin
            csr_rdata = {{riscv::XLEN - 3{1'b0}}, fcsr_q.frm};
          end else begin
            read_access_exception = 1'b1;
          end
        end
        riscv::CSR_FCSR: begin
          if (CVA6Cfg.FpPresent) begin
            csr_rdata = {{riscv::XLEN - 8{1'b0}}, fcsr_q.frm, fcsr_q.fflags};
          end else begin
            read_access_exception = 1'b1;
          end
        end
        // non-standard extension
        riscv::CSR_FTRAN: begin
          if (CVA6Cfg.FpPresent) begin
            csr_rdata = {{riscv::XLEN - 7{1'b0}}, fcsr_q.fprec};
          end else begin
            read_access_exception = 1'b1;
          end
        end
        // debug registers
        riscv::CSR_DCSR:
        if (CVA6Cfg.DebugEn) csr_rdata = {{riscv::XLEN - 32{1'b0}}, dcsr_q};
        else read_access_exception = 1'b1;
        riscv::CSR_DPC:
        if (CVA6Cfg.DebugEn) csr_rdata = dpc_q;
        else read_access_exception = 1'b1;
        riscv::CSR_DSCRATCH0:
        if (CVA6Cfg.DebugEn) csr_rdata = dscratch0_q;
        else read_access_exception = 1'b1;
        riscv::CSR_DSCRATCH1:
        if (CVA6Cfg.DebugEn) csr_rdata = dscratch1_q;
        else read_access_exception = 1'b1;
        // trigger module registers
        riscv::CSR_TSELECT: read_access_exception = 1'b1;  // not implemented
        riscv::CSR_TDATA1: read_access_exception = 1'b1;  // not implemented
        riscv::CSR_TDATA2: read_access_exception = 1'b1;  // not implemented
        riscv::CSR_TDATA3: read_access_exception = 1'b1;  // not implemented
        // supervisor registers
        riscv::CSR_SSTATUS: begin
          if (CVA6Cfg.RVS)
            csr_rdata = mstatus_extended & ariane_pkg::SMODE_STATUS_READ_MASK[riscv::XLEN-1:0];
          else read_access_exception = 1'b1;
        end
        riscv::CSR_SIE:
        if (CVA6Cfg.RVS) csr_rdata = mie_q & mideleg_q;
        else read_access_exception = 1'b1;
        riscv::CSR_SIP:
        if (CVA6Cfg.RVS) csr_rdata = mip_q & mideleg_q;
        else read_access_exception = 1'b1;
        riscv::CSR_STVEC:
        if (CVA6Cfg.RVS) csr_rdata = stvec_q;
        else read_access_exception = 1'b1;
        riscv::CSR_SCOUNTEREN:
        if (CVA6Cfg.RVS) csr_rdata = scounteren_q;
        else read_access_exception = 1'b1;
        riscv::CSR_SSCRATCH:
        if (CVA6Cfg.RVS) csr_rdata = sscratch_q;
        else read_access_exception = 1'b1;
        riscv::CSR_SEPC:
        if (CVA6Cfg.RVS) csr_rdata = sepc_q;
        else read_access_exception = 1'b1;
        riscv::CSR_SCAUSE:
        if (CVA6Cfg.RVS) csr_rdata = scause_q;
        else read_access_exception = 1'b1;
        riscv::CSR_STVAL:
        if (CVA6Cfg.RVS) csr_rdata = stval_q;
        else read_access_exception = 1'b1;
        riscv::CSR_SATP: begin
          if (CVA6Cfg.RVS) begin
            // intercept reads to SATP if in S-Mode and TVM is enabled
            if (priv_lvl_o == riscv::PRIV_LVL_S && mstatus_q.tvm) begin
              read_access_exception = 1'b1;
            end else begin
              csr_rdata = satp_q;
            end
          end else begin
            read_access_exception = 1'b1;
          end
        end
        // machine mode registers
        riscv::CSR_MSTATUS: csr_rdata = mstatus_extended;
        riscv::CSR_MSTATUSH:
        if (riscv::XLEN == 32) csr_rdata = '0;
        else read_access_exception = 1'b1;
        riscv::CSR_MISA: csr_rdata = IsaCode;
        riscv::CSR_MEDELEG:
        if (CVA6Cfg.RVS) csr_rdata = medeleg_q;
        else read_access_exception = 1'b1;
        riscv::CSR_MIDELEG:
        if (CVA6Cfg.RVS) csr_rdata = mideleg_q;
        else read_access_exception = 1'b1;
        riscv::CSR_MIE: csr_rdata = mie_q;
        riscv::CSR_MTVEC: csr_rdata = mtvec_q;
        riscv::CSR_MCOUNTEREN: csr_rdata = mcounteren_q;
        riscv::CSR_MSCRATCH: csr_rdata = mscratch_q;
        riscv::CSR_MEPC: csr_rdata = mepc_q;
        riscv::CSR_MCAUSE: csr_rdata = mcause_q;
        riscv::CSR_MTVAL: csr_rdata = mtval_q;
        riscv::CSR_MIP: csr_rdata = mip_q;
        riscv::CSR_MENVCFG: csr_rdata = '0 | fiom_q;
        riscv::CSR_MENVCFGH: begin
          if (riscv::XLEN == 32) csr_rdata = '0;
          else read_access_exception = 1'b1;
        end
        riscv::CSR_MVENDORID: csr_rdata = OPENHWGROUP_MVENDORID;
        riscv::CSR_MARCHID: csr_rdata = ARIANE_MARCHID;
        riscv::CSR_MIMPID: csr_rdata = '0;  // not implemented
        riscv::CSR_MHARTID: csr_rdata = hart_id_i;
        riscv::CSR_MCONFIGPTR: csr_rdata = '0;  // not implemented
        riscv::CSR_MCOUNTINHIBIT:
        csr_rdata = {{(riscv::XLEN - (MHPMCounterNum + 3)) {1'b0}}, mcountinhibit_q};
        // Counters and Timers
        riscv::CSR_MCYCLE: csr_rdata = cycle_q[riscv::XLEN-1:0];
        riscv::CSR_MCYCLEH:
        if (riscv::XLEN == 32) csr_rdata = cycle_q[63:32];
        else read_access_exception = 1'b1;
        riscv::CSR_MINSTRET: csr_rdata = instret_q[riscv::XLEN-1:0];
        riscv::CSR_MINSTRETH:
        if (riscv::XLEN == 32) csr_rdata = instret_q[63:32];
        else read_access_exception = 1'b1;
        riscv::CSR_CYCLE: csr_rdata = cycle_q[riscv::XLEN-1:0];
        riscv::CSR_CYCLEH:
        if (riscv::XLEN == 32) csr_rdata = cycle_q[63:32];
        else read_access_exception = 1'b1;
        riscv::CSR_INSTRET: csr_rdata = instret_q[riscv::XLEN-1:0];
        riscv::CSR_INSTRETH:
        if (riscv::XLEN == 32) csr_rdata = instret_q[63:32];
        else read_access_exception = 1'b1;
        //Event Selector
        riscv::CSR_MHPM_EVENT_3,
                riscv::CSR_MHPM_EVENT_4,
                riscv::CSR_MHPM_EVENT_5,
                riscv::CSR_MHPM_EVENT_6,
                riscv::CSR_MHPM_EVENT_7,
                riscv::CSR_MHPM_EVENT_8,
                riscv::CSR_MHPM_EVENT_9,
                riscv::CSR_MHPM_EVENT_10,
                riscv::CSR_MHPM_EVENT_11,
                riscv::CSR_MHPM_EVENT_12,
                riscv::CSR_MHPM_EVENT_13,
                riscv::CSR_MHPM_EVENT_14,
                riscv::CSR_MHPM_EVENT_15,
                riscv::CSR_MHPM_EVENT_16,
                riscv::CSR_MHPM_EVENT_17,
                riscv::CSR_MHPM_EVENT_18,
                riscv::CSR_MHPM_EVENT_19,
                riscv::CSR_MHPM_EVENT_20,
                riscv::CSR_MHPM_EVENT_21,
                riscv::CSR_MHPM_EVENT_22,
                riscv::CSR_MHPM_EVENT_23,
                riscv::CSR_MHPM_EVENT_24,
                riscv::CSR_MHPM_EVENT_25,
                riscv::CSR_MHPM_EVENT_26,
                riscv::CSR_MHPM_EVENT_27,
                riscv::CSR_MHPM_EVENT_28,
                riscv::CSR_MHPM_EVENT_29,
                riscv::CSR_MHPM_EVENT_30,
                riscv::CSR_MHPM_EVENT_31 :
        csr_rdata = perf_data_i;

        riscv::CSR_MHPM_COUNTER_3,
                riscv::CSR_MHPM_COUNTER_4,
                riscv::CSR_MHPM_COUNTER_5,
                riscv::CSR_MHPM_COUNTER_6,
                riscv::CSR_MHPM_COUNTER_7,
                riscv::CSR_MHPM_COUNTER_8,
                riscv::CSR_MHPM_COUNTER_9,
                riscv::CSR_MHPM_COUNTER_10,
                riscv::CSR_MHPM_COUNTER_11,
                riscv::CSR_MHPM_COUNTER_12,
                riscv::CSR_MHPM_COUNTER_13,
                riscv::CSR_MHPM_COUNTER_14,
                riscv::CSR_MHPM_COUNTER_15,
                riscv::CSR_MHPM_COUNTER_16,
                riscv::CSR_MHPM_COUNTER_17,
                riscv::CSR_MHPM_COUNTER_18,
                riscv::CSR_MHPM_COUNTER_19,
                riscv::CSR_MHPM_COUNTER_20,
                riscv::CSR_MHPM_COUNTER_21,
                riscv::CSR_MHPM_COUNTER_22,
                riscv::CSR_MHPM_COUNTER_23,
                riscv::CSR_MHPM_COUNTER_24,
                riscv::CSR_MHPM_COUNTER_25,
                riscv::CSR_MHPM_COUNTER_26,
                riscv::CSR_MHPM_COUNTER_27,
                riscv::CSR_MHPM_COUNTER_28,
                riscv::CSR_MHPM_COUNTER_29,
                riscv::CSR_MHPM_COUNTER_30,
                riscv::CSR_MHPM_COUNTER_31 :
        csr_rdata = perf_data_i;

        riscv::CSR_MHPM_COUNTER_3H,
                riscv::CSR_MHPM_COUNTER_4H,
                riscv::CSR_MHPM_COUNTER_5H,
                riscv::CSR_MHPM_COUNTER_6H,
                riscv::CSR_MHPM_COUNTER_7H,
                riscv::CSR_MHPM_COUNTER_8H,
                riscv::CSR_MHPM_COUNTER_9H,
                riscv::CSR_MHPM_COUNTER_10H,
                riscv::CSR_MHPM_COUNTER_11H,
                riscv::CSR_MHPM_COUNTER_12H,
                riscv::CSR_MHPM_COUNTER_13H,
                riscv::CSR_MHPM_COUNTER_14H,
                riscv::CSR_MHPM_COUNTER_15H,
                riscv::CSR_MHPM_COUNTER_16H,
                riscv::CSR_MHPM_COUNTER_17H,
                riscv::CSR_MHPM_COUNTER_18H,
                riscv::CSR_MHPM_COUNTER_19H,
                riscv::CSR_MHPM_COUNTER_20H,
                riscv::CSR_MHPM_COUNTER_21H,
                riscv::CSR_MHPM_COUNTER_22H,
                riscv::CSR_MHPM_COUNTER_23H,
                riscv::CSR_MHPM_COUNTER_24H,
                riscv::CSR_MHPM_COUNTER_25H,
                riscv::CSR_MHPM_COUNTER_26H,
                riscv::CSR_MHPM_COUNTER_27H,
                riscv::CSR_MHPM_COUNTER_28H,
                riscv::CSR_MHPM_COUNTER_29H,
                riscv::CSR_MHPM_COUNTER_30H,
                riscv::CSR_MHPM_COUNTER_31H :
        if (riscv::XLEN == 32) csr_rdata = perf_data_i;
        else read_access_exception = 1'b1;

        // Performance counters (User Mode - R/O Shadows)
        riscv::CSR_HPM_COUNTER_3,
                riscv::CSR_HPM_COUNTER_4,
                riscv::CSR_HPM_COUNTER_5,
                riscv::CSR_HPM_COUNTER_6,
                riscv::CSR_HPM_COUNTER_7,
                riscv::CSR_HPM_COUNTER_8,
                riscv::CSR_HPM_COUNTER_9,
                riscv::CSR_HPM_COUNTER_10,
                riscv::CSR_HPM_COUNTER_11,
                riscv::CSR_HPM_COUNTER_12,
                riscv::CSR_HPM_COUNTER_13,
                riscv::CSR_HPM_COUNTER_14,
                riscv::CSR_HPM_COUNTER_15,
                riscv::CSR_HPM_COUNTER_16,
                riscv::CSR_HPM_COUNTER_17,
                riscv::CSR_HPM_COUNTER_18,
                riscv::CSR_HPM_COUNTER_19,
                riscv::CSR_HPM_COUNTER_20,
                riscv::CSR_HPM_COUNTER_21,
                riscv::CSR_HPM_COUNTER_22,
                riscv::CSR_HPM_COUNTER_23,
                riscv::CSR_HPM_COUNTER_24,
                riscv::CSR_HPM_COUNTER_25,
                riscv::CSR_HPM_COUNTER_26,
                riscv::CSR_HPM_COUNTER_27,
                riscv::CSR_HPM_COUNTER_28,
                riscv::CSR_HPM_COUNTER_29,
                riscv::CSR_HPM_COUNTER_30,
                riscv::CSR_HPM_COUNTER_31 :
        csr_rdata = perf_data_i;

        riscv::CSR_HPM_COUNTER_3H,
                riscv::CSR_HPM_COUNTER_4H,
                riscv::CSR_HPM_COUNTER_5H,
                riscv::CSR_HPM_COUNTER_6H,
                riscv::CSR_HPM_COUNTER_7H,
                riscv::CSR_HPM_COUNTER_8H,
                riscv::CSR_HPM_COUNTER_9H,
                riscv::CSR_HPM_COUNTER_10H,
                riscv::CSR_HPM_COUNTER_11H,
                riscv::CSR_HPM_COUNTER_12H,
                riscv::CSR_HPM_COUNTER_13H,
                riscv::CSR_HPM_COUNTER_14H,
                riscv::CSR_HPM_COUNTER_15H,
                riscv::CSR_HPM_COUNTER_16H,
                riscv::CSR_HPM_COUNTER_17H,
                riscv::CSR_HPM_COUNTER_18H,
                riscv::CSR_HPM_COUNTER_19H,
                riscv::CSR_HPM_COUNTER_20H,
                riscv::CSR_HPM_COUNTER_21H,
                riscv::CSR_HPM_COUNTER_22H,
                riscv::CSR_HPM_COUNTER_23H,
                riscv::CSR_HPM_COUNTER_24H,
                riscv::CSR_HPM_COUNTER_25H,
                riscv::CSR_HPM_COUNTER_26H,
                riscv::CSR_HPM_COUNTER_27H,
                riscv::CSR_HPM_COUNTER_28H,
                riscv::CSR_HPM_COUNTER_29H,
                riscv::CSR_HPM_COUNTER_30H,
                riscv::CSR_HPM_COUNTER_31H :
        if (riscv::XLEN == 32) csr_rdata = perf_data_i;
        else read_access_exception = 1'b1;

        // custom (non RISC-V) cache control
        riscv::CSR_DCACHE: csr_rdata = dcache_q;
        riscv::CSR_ICACHE: csr_rdata = icache_q;
        // custom (non RISC-V) accelerator memory consistency mode
        riscv::CSR_ACC_CONS: begin
          if (CVA6Cfg.EnableAccelerator) begin
            csr_rdata = acc_cons_q;
          end else begin
            read_access_exception = 1'b1;
          end
        end
        // PMPs
        riscv::CSR_PMPCFG0: csr_rdata = pmpcfg_q[riscv::XLEN/8-1:0];
        riscv::CSR_PMPCFG1:
        if (riscv::XLEN == 32) csr_rdata = pmpcfg_q[7:4];
        else read_access_exception = 1'b1;
        riscv::CSR_PMPCFG2: csr_rdata = pmpcfg_q[8+:riscv::XLEN/8];
        riscv::CSR_PMPCFG3:
        if (riscv::XLEN == 32) csr_rdata = pmpcfg_q[15:12];
        else read_access_exception = 1'b1;
        // PMPADDR
        riscv::CSR_PMPADDR0,
                riscv::CSR_PMPADDR1,
                riscv::CSR_PMPADDR2,
                riscv::CSR_PMPADDR3,
                riscv::CSR_PMPADDR4,
                riscv::CSR_PMPADDR5,
                riscv::CSR_PMPADDR6,
                riscv::CSR_PMPADDR7,
                riscv::CSR_PMPADDR8,
                riscv::CSR_PMPADDR9,
                riscv::CSR_PMPADDR10,
                riscv::CSR_PMPADDR11,
                riscv::CSR_PMPADDR12,
                riscv::CSR_PMPADDR13,
                riscv::CSR_PMPADDR14,
                riscv::CSR_PMPADDR15: begin
          // index is specified by the last byte in the address
          index = csr_addr.csr_decode.address[3:0];
          // Important: we only support granularity 8 bytes (G=1)
          // -> last bit of pmpaddr must be set 0/1 based on the mode:
          // NA4, NAPOT: 1
          // TOR, OFF:   0
          if (pmpcfg_q[index].addr_mode[1] == 1'b1) csr_rdata = pmpaddr_q[index][riscv::PLEN-3:0];
          else csr_rdata = {pmpaddr_q[index][riscv::PLEN-3:1], 1'b0};
        end
        default: read_access_exception = 1'b1;
      endcase
    end
  end
  // ---------------------------
  // CSR Write and update logic
  // ---------------------------
  riscv::xlen_t mask;
  always_comb begin : csr_update
    automatic riscv::satp_t satp;
    automatic logic [63:0] instret;


    satp            = satp_q;
    instret         = instret_q;

    mcountinhibit_d = mcountinhibit_q;

    // --------------------
    // Counters
    // --------------------
    cycle_d         = cycle_q;
    instret_d       = instret_q;
    if (!debug_mode_q) begin
      // increase instruction retired counter
      for (int i = 0; i < CVA6Cfg.NrCommitPorts; i++) begin
        if (commit_ack_i[i] && !ex_i.valid && !mcountinhibit_q[2]) instret++;
      end
      instret_d = instret;
      // increment the cycle count
      if (!mcountinhibit_q[0]) cycle_d = cycle_q + 1'b1;
      else cycle_d = cycle_q;
    end

    eret_o                  = 1'b0;
    flush_o                 = 1'b0;
    update_access_exception = 1'b0;

    set_debug_pc_o          = 1'b0;

    perf_we_o               = 1'b0;
    perf_data_o             = 'b0;

    fcsr_d                  = fcsr_q;

    priv_lvl_d              = priv_lvl_q;
    debug_mode_d            = debug_mode_q;
    dcsr_d                  = dcsr_q;
    dpc_d                   = dpc_q;
    dscratch0_d             = dscratch0_q;
    dscratch1_d             = dscratch1_q;
    mstatus_d               = mstatus_q;

    // check whether we come out of reset
    // this is a workaround. some tools have issues
    // having boot_addr_i in the asynchronous
    // reset assignment to mtvec_d, even though
    // boot_addr_i will be assigned a constant
    // on the top-level.
    if (mtvec_rst_load_q) begin
      mtvec_d = {{riscv::XLEN - riscv::VLEN{1'b0}}, boot_addr_i} + 'h40;
    end else begin
      mtvec_d = mtvec_q;
    end

    medeleg_d              = medeleg_q;
    mideleg_d              = mideleg_q;
    mip_d                  = mip_q;
    mie_d                  = mie_q;
    mepc_d                 = mepc_q;
    mcause_d               = mcause_q;
    mcounteren_d           = mcounteren_q;
    mscratch_d             = mscratch_q;
    mtval_d                = mtval_q;
    fiom_d                 = fiom_q;
    dcache_d               = dcache_q;
    icache_d               = icache_q;
    acc_cons_d             = acc_cons_q;

    sepc_d                 = sepc_q;
    scause_d               = scause_q;
    stvec_d                = stvec_q;
    scounteren_d           = scounteren_q;
    sscratch_d             = sscratch_q;
    stval_d                = stval_q;
    satp_d                 = satp_q;

    en_ld_st_translation_d = en_ld_st_translation_q;
    dirty_fp_state_csr     = 1'b0;

    pmpcfg_d               = pmpcfg_q;
    pmpaddr_d              = pmpaddr_q;

    // check for correct access rights and that we are writing
    if (csr_we) begin
      unique case (csr_addr.address)
        // Floating-Point
        riscv::CSR_FFLAGS: begin
          if (CVA6Cfg.FpPresent) begin
            dirty_fp_state_csr = 1'b1;
            fcsr_d.fflags = csr_wdata[4:0];
            // this instruction has side-effects
            flush_o = 1'b1;
          end else begin
            update_access_exception = 1'b1;
          end
        end
        riscv::CSR_FRM: begin
          if (CVA6Cfg.FpPresent) begin
            dirty_fp_state_csr = 1'b1;
            fcsr_d.frm    = csr_wdata[2:0];
            // this instruction has side-effects
            flush_o = 1'b1;
          end else begin
            update_access_exception = 1'b1;
          end
        end
        riscv::CSR_FCSR: begin
          if (CVA6Cfg.FpPresent) begin
            dirty_fp_state_csr = 1'b1;
            fcsr_d[7:0] = csr_wdata[7:0];  // ignore writes to reserved space
            // this instruction has side-effects
            flush_o = 1'b1;
          end else begin
            update_access_exception = 1'b1;
          end
        end
        riscv::CSR_FTRAN: begin
          if (CVA6Cfg.FpPresent) begin
            dirty_fp_state_csr = 1'b1;
            fcsr_d.fprec = csr_wdata[6:0];  // ignore writes to reserved space
            // this instruction has side-effects
            flush_o = 1'b1;
          end else begin
            update_access_exception = 1'b1;
          end
        end
        // debug CSR
        riscv::CSR_DCSR: begin
          if (CVA6Cfg.DebugEn) begin
            dcsr_d           = csr_wdata[31:0];
            // debug is implemented
            dcsr_d.xdebugver = 4'h4;
            // currently not supported
            dcsr_d.nmip      = 1'b0;
            dcsr_d.stopcount = 1'b0;
            dcsr_d.stoptime  = 1'b0;
          end else begin
            update_access_exception = 1'b1;
          end
        end
        riscv::CSR_DPC:
        if (CVA6Cfg.DebugEn) dpc_d = csr_wdata;
        else update_access_exception = 1'b1;
        riscv::CSR_DSCRATCH0:
        if (CVA6Cfg.DebugEn) dscratch0_d = csr_wdata;
        else update_access_exception = 1'b1;
        riscv::CSR_DSCRATCH1:
        if (CVA6Cfg.DebugEn) dscratch1_d = csr_wdata;
        else update_access_exception = 1'b1;
        // trigger module CSRs
        riscv::CSR_TSELECT: update_access_exception = 1'b1;  // not implemented
        riscv::CSR_TDATA1: update_access_exception = 1'b1;  // not implemented
        riscv::CSR_TDATA2: update_access_exception = 1'b1;  // not implemented
        riscv::CSR_TDATA3: update_access_exception = 1'b1;  // not implemented
        // sstatus is a subset of mstatus - mask it accordingly
        riscv::CSR_SSTATUS: begin
          if (CVA6Cfg.RVS) begin
            mask = ariane_pkg::SMODE_STATUS_WRITE_MASK[riscv::XLEN-1:0];
            mstatus_d = (mstatus_q & ~{{64-riscv::XLEN{1'b0}}, mask}) | {{64-riscv::XLEN{1'b0}}, (csr_wdata & mask)};
            // hardwire to zero if floating point extension is not present
            if (!CVA6Cfg.FpPresent) begin
              mstatus_d.fs = riscv::Off;
            end
            // hardwire to zero if vector extension is not present
            if (!CVA6Cfg.RVV) begin
              mstatus_d.vs = riscv::Off;
            end
            // this instruction has side-effects
            flush_o = 1'b1;
          end else begin
            update_access_exception = 1'b1;
          end
        end
        // even machine mode interrupts can be visible and set-able to supervisor
        // if the corresponding bit in mideleg is set
        riscv::CSR_SIE: begin
          if (CVA6Cfg.RVS) begin
            // the mideleg makes sure only delegate-able register (and therefore also only implemented registers) are written
            mie_d = (mie_q & ~mideleg_q) | (csr_wdata & mideleg_q);
          end else begin
            update_access_exception = 1'b1;
          end
        end

        riscv::CSR_SIP: begin
          if (CVA6Cfg.RVS) begin
            // only the supervisor software interrupt is write-able, iff delegated
            mask  = riscv::MIP_SSIP & mideleg_q;
            mip_d = (mip_q & ~mask) | (csr_wdata & mask);
          end else begin
            update_access_exception = 1'b1;
          end
        end

        riscv::CSR_STVEC:
        if (CVA6Cfg.RVS) stvec_d = {csr_wdata[riscv::XLEN-1:2], 1'b0, csr_wdata[0]};
        else update_access_exception = 1'b1;
        riscv::CSR_SCOUNTEREN:
        if (CVA6Cfg.RVS) scounteren_d = {{riscv::XLEN - 32{1'b0}}, csr_wdata[31:0]};
        else update_access_exception = 1'b1;
        riscv::CSR_SSCRATCH:
        if (CVA6Cfg.RVS) sscratch_d = csr_wdata;
        else update_access_exception = 1'b1;
        riscv::CSR_SEPC:
        if (CVA6Cfg.RVS) sepc_d = {csr_wdata[riscv::XLEN-1:1], 1'b0};
        else update_access_exception = 1'b1;
        riscv::CSR_SCAUSE:
        if (CVA6Cfg.RVS) scause_d = csr_wdata;
        else update_access_exception = 1'b1;
        riscv::CSR_STVAL:
        if (CVA6Cfg.RVS) stval_d = csr_wdata;
        else update_access_exception = 1'b1;
        // supervisor address translation and protection
        riscv::CSR_SATP: begin
          if (CVA6Cfg.RVS) begin
            // intercept SATP writes if in S-Mode and TVM is enabled
            if (priv_lvl_o == riscv::PRIV_LVL_S && mstatus_q.tvm) update_access_exception = 1'b1;
            else begin
              satp      = riscv::satp_t'(csr_wdata);
              // only make ASID_LEN - 1 bit stick, that way software can figure out how many ASID bits are supported
              satp.asid = satp.asid & {{(riscv::ASIDW - AsidWidth) {1'b0}}, {AsidWidth{1'b1}}};
              // only update if we actually support this mode
              if (riscv::vm_mode_t'(satp.mode) == riscv::ModeOff ||
                                riscv::vm_mode_t'(satp.mode) == riscv::MODE_SV)
                satp_d = satp;
            end
            // changing the mode can have side-effects on address translation (e.g.: other instructions), re-fetch
            // the next instruction by executing a flush
            flush_o = 1'b1;
          end else begin
            update_access_exception = 1'b1;
          end
        end

        riscv::CSR_MSTATUS: begin
          mstatus_d    = {{64 - riscv::XLEN{1'b0}}, csr_wdata};
          mstatus_d.xs = riscv::Off;
          if (!CVA6Cfg.FpPresent) begin
            mstatus_d.fs = riscv::Off;
          end
          if (!CVA6Cfg.RVV) begin
            mstatus_d.vs = riscv::Off;
          end
          mstatus_d.wpri3 = 9'b0;
          mstatus_d.wpri1 = 1'b0;
          mstatus_d.wpri2 = 1'b0;
          mstatus_d.wpri0 = 1'b0;
          mstatus_d.ube   = 1'b0;  // CVA6 is little-endian
          // this register has side-effects on other registers, flush the pipeline
          flush_o         = 1'b1;
        end
        riscv::CSR_MSTATUSH: if (riscv::XLEN != 32) update_access_exception = 1'b1;
        // MISA is WARL (Write Any Value, Reads Legal Value)
        riscv::CSR_MISA: ;
        // machine exception delegation register
        // 0 - 15 exceptions supported
        riscv::CSR_MEDELEG: begin
          if (CVA6Cfg.RVS) begin
            mask = (1 << riscv::INSTR_ADDR_MISALIGNED) |
                             (1 << riscv::BREAKPOINT) |
                             (1 << riscv::ENV_CALL_UMODE) |
                             (1 << riscv::INSTR_PAGE_FAULT) |
                             (1 << riscv::LOAD_PAGE_FAULT) |
                             (1 << riscv::STORE_PAGE_FAULT);
            medeleg_d = (medeleg_q & ~mask) | (csr_wdata & mask);
          end else begin
            update_access_exception = 1'b1;
          end
        end
        // machine interrupt delegation register
        // we do not support user interrupt delegation
        riscv::CSR_MIDELEG: begin
          if (CVA6Cfg.RVS) begin
            mask = riscv::MIP_SSIP | riscv::MIP_STIP | riscv::MIP_SEIP;
            mideleg_d = (mideleg_q & ~mask) | (csr_wdata & mask);
          end else begin
            update_access_exception = 1'b1;
          end
        end
        // mask the register so that unsupported interrupts can never be set
        riscv::CSR_MIE: begin
          mask = riscv::MIP_SSIP | riscv::MIP_STIP | riscv::MIP_SEIP | riscv::MIP_MSIP | riscv::MIP_MTIP | riscv::MIP_MEIP;
          mie_d = (mie_q & ~mask) | (csr_wdata & mask); // we only support supervisor and M-mode interrupts
        end

        riscv::CSR_MTVEC: begin
          mtvec_d = {csr_wdata[riscv::XLEN-1:2], 1'b0, csr_wdata[0]};
          // we are in vector mode, this implementation requires the additional
          // alignment constraint of 64 * 4 bytes
          if (csr_wdata[0]) mtvec_d = {csr_wdata[riscv::XLEN-1:8], 7'b0, csr_wdata[0]};
        end
        riscv::CSR_MCOUNTEREN: mcounteren_d = {{riscv::XLEN - 32{1'b0}}, csr_wdata[31:0]};

        riscv::CSR_MSCRATCH: mscratch_d = csr_wdata;
        riscv::CSR_MEPC: mepc_d = {csr_wdata[riscv::XLEN-1:1], 1'b0};
        riscv::CSR_MCAUSE: mcause_d = csr_wdata;
        riscv::CSR_MTVAL: mtval_d = csr_wdata;
        riscv::CSR_MIP: begin
          mask  = riscv::MIP_SSIP | riscv::MIP_STIP | riscv::MIP_SEIP;
          mip_d = (mip_q & ~mask) | (csr_wdata & mask);
        end
        riscv::CSR_MENVCFG: if (CVA6Cfg.RVS) fiom_d = csr_wdata[0];
        riscv::CSR_MENVCFGH: begin
          if (riscv::XLEN != 32) update_access_exception = 1'b1;
        end
        riscv::CSR_MCOUNTINHIBIT:
        mcountinhibit_d = {csr_wdata[MHPMCounterNum+2:2], 1'b0, csr_wdata[0]};
        // performance counters
        riscv::CSR_MCYCLE: cycle_d[riscv::XLEN-1:0] = csr_wdata;
        riscv::CSR_MCYCLEH:
        if (riscv::XLEN == 32) cycle_d[63:32] = csr_wdata;
        else update_access_exception = 1'b1;
        riscv::CSR_MINSTRET: instret_d[riscv::XLEN-1:0] = csr_wdata;
        riscv::CSR_MINSTRETH:
        if (riscv::XLEN == 32) instret_d[63:32] = csr_wdata;
        else update_access_exception = 1'b1;
        //Event Selector
        riscv::CSR_MHPM_EVENT_3,
                riscv::CSR_MHPM_EVENT_4,
                riscv::CSR_MHPM_EVENT_5,
                riscv::CSR_MHPM_EVENT_6,
                riscv::CSR_MHPM_EVENT_7,
                riscv::CSR_MHPM_EVENT_8,
                riscv::CSR_MHPM_EVENT_9,
                riscv::CSR_MHPM_EVENT_10,
                riscv::CSR_MHPM_EVENT_11,
                riscv::CSR_MHPM_EVENT_12,
                riscv::CSR_MHPM_EVENT_13,
                riscv::CSR_MHPM_EVENT_14,
                riscv::CSR_MHPM_EVENT_15,
                riscv::CSR_MHPM_EVENT_16,
                riscv::CSR_MHPM_EVENT_17,
                riscv::CSR_MHPM_EVENT_18,
                riscv::CSR_MHPM_EVENT_19,
                riscv::CSR_MHPM_EVENT_20,
                riscv::CSR_MHPM_EVENT_21,
                riscv::CSR_MHPM_EVENT_22,
                riscv::CSR_MHPM_EVENT_23,
                riscv::CSR_MHPM_EVENT_24,
                riscv::CSR_MHPM_EVENT_25,
                riscv::CSR_MHPM_EVENT_26,
                riscv::CSR_MHPM_EVENT_27,
                riscv::CSR_MHPM_EVENT_28,
                riscv::CSR_MHPM_EVENT_29,
                riscv::CSR_MHPM_EVENT_30,
                riscv::CSR_MHPM_EVENT_31 :     begin
          perf_we_o   = 1'b1;
          perf_data_o = csr_wdata;
        end

        riscv::CSR_MHPM_COUNTER_3,
                riscv::CSR_MHPM_COUNTER_4,
                riscv::CSR_MHPM_COUNTER_5,
                riscv::CSR_MHPM_COUNTER_6,
                riscv::CSR_MHPM_COUNTER_7,
                riscv::CSR_MHPM_COUNTER_8,
                riscv::CSR_MHPM_COUNTER_9,
                riscv::CSR_MHPM_COUNTER_10,
                riscv::CSR_MHPM_COUNTER_11,
                riscv::CSR_MHPM_COUNTER_12,
                riscv::CSR_MHPM_COUNTER_13,
                riscv::CSR_MHPM_COUNTER_14,
                riscv::CSR_MHPM_COUNTER_15,
                riscv::CSR_MHPM_COUNTER_16,
                riscv::CSR_MHPM_COUNTER_17,
                riscv::CSR_MHPM_COUNTER_18,
                riscv::CSR_MHPM_COUNTER_19,
                riscv::CSR_MHPM_COUNTER_20,
                riscv::CSR_MHPM_COUNTER_21,
                riscv::CSR_MHPM_COUNTER_22,
                riscv::CSR_MHPM_COUNTER_23,
                riscv::CSR_MHPM_COUNTER_24,
                riscv::CSR_MHPM_COUNTER_25,
                riscv::CSR_MHPM_COUNTER_26,
                riscv::CSR_MHPM_COUNTER_27,
                riscv::CSR_MHPM_COUNTER_28,
                riscv::CSR_MHPM_COUNTER_29,
                riscv::CSR_MHPM_COUNTER_30,
                riscv::CSR_MHPM_COUNTER_31 :  begin
          perf_we_o   = 1'b1;
          perf_data_o = csr_wdata;
        end

        riscv::CSR_MHPM_COUNTER_3H,
                riscv::CSR_MHPM_COUNTER_4H,
                riscv::CSR_MHPM_COUNTER_5H,
                riscv::CSR_MHPM_COUNTER_6H,
                riscv::CSR_MHPM_COUNTER_7H,
                riscv::CSR_MHPM_COUNTER_8H,
                riscv::CSR_MHPM_COUNTER_9H,
                riscv::CSR_MHPM_COUNTER_10H,
                riscv::CSR_MHPM_COUNTER_11H,
                riscv::CSR_MHPM_COUNTER_12H,
                riscv::CSR_MHPM_COUNTER_13H,
                riscv::CSR_MHPM_COUNTER_14H,
                riscv::CSR_MHPM_COUNTER_15H,
                riscv::CSR_MHPM_COUNTER_16H,
                riscv::CSR_MHPM_COUNTER_17H,
                riscv::CSR_MHPM_COUNTER_18H,
                riscv::CSR_MHPM_COUNTER_19H,
                riscv::CSR_MHPM_COUNTER_20H,
                riscv::CSR_MHPM_COUNTER_21H,
                riscv::CSR_MHPM_COUNTER_22H,
                riscv::CSR_MHPM_COUNTER_23H,
                riscv::CSR_MHPM_COUNTER_24H,
                riscv::CSR_MHPM_COUNTER_25H,
                riscv::CSR_MHPM_COUNTER_26H,
                riscv::CSR_MHPM_COUNTER_27H,
                riscv::CSR_MHPM_COUNTER_28H,
                riscv::CSR_MHPM_COUNTER_29H,
                riscv::CSR_MHPM_COUNTER_30H,
                riscv::CSR_MHPM_COUNTER_31H :  begin
          perf_we_o = 1'b1;
          if (riscv::XLEN == 32) perf_data_o = csr_wdata;
          else update_access_exception = 1'b1;
        end

        riscv::CSR_DCACHE: dcache_d = {{riscv::XLEN - 1{1'b0}}, csr_wdata[0]};  // enable bit
        riscv::CSR_ICACHE: icache_d = {{riscv::XLEN - 1{1'b0}}, csr_wdata[0]};  // enable bit
        riscv::CSR_ACC_CONS: begin
          if (CVA6Cfg.EnableAccelerator) begin
            acc_cons_d = {{riscv::XLEN - 1{1'b0}}, csr_wdata[0]};  // enable bit
          end else begin
            update_access_exception = 1'b1;
          end
        end
        // PMP locked logic
        // 1. refuse to update any locked entry
        // 2. also refuse to update the entry below a locked TOR entry
        // Note that writes to pmpcfg below a locked TOR entry are valid
        riscv::CSR_PMPCFG0:
        for (int i = 0; i < (riscv::XLEN / 8); i++)
        if (!pmpcfg_q[i].locked) pmpcfg_d[i] = csr_wdata[i*8+:8];
        riscv::CSR_PMPCFG1: begin
          if (riscv::XLEN == 32) begin
            for (int i = 0; i < 4; i++)
            if (!pmpcfg_q[i+4].locked) pmpcfg_d[i+4] = csr_wdata[i*8+:8];
          end else begin
            update_access_exception = 1'b1;
          end
        end
        riscv::CSR_PMPCFG2:
        for (int i = 0; i < (riscv::XLEN / 8); i++)
        if (!pmpcfg_q[i+8].locked) pmpcfg_d[i+8] = csr_wdata[i*8+:8];
        riscv::CSR_PMPCFG3: begin
          if (riscv::XLEN == 32) begin
            for (int i = 0; i < 4; i++)
            if (!pmpcfg_q[i+12].locked) pmpcfg_d[i+12] = csr_wdata[i*8+:8];
          end else begin
            update_access_exception = 1'b1;
          end
        end
        riscv::CSR_PMPADDR0,
                riscv::CSR_PMPADDR1,
                riscv::CSR_PMPADDR2,
                riscv::CSR_PMPADDR3,
                riscv::CSR_PMPADDR4,
                riscv::CSR_PMPADDR5,
                riscv::CSR_PMPADDR6,
                riscv::CSR_PMPADDR7,
                riscv::CSR_PMPADDR8,
                riscv::CSR_PMPADDR9,
                riscv::CSR_PMPADDR10,
                riscv::CSR_PMPADDR11,
                riscv::CSR_PMPADDR12,
                riscv::CSR_PMPADDR13,
                riscv::CSR_PMPADDR14,
                riscv::CSR_PMPADDR15:  begin
          // index is specified by the last byte in the address
          automatic logic [3:0] index = csr_addr.csr_decode.address[3:0];
          // check if the entry or the entry above is locked
          if (!pmpcfg_q[index].locked && !(pmpcfg_q[index+1].locked && pmpcfg_q[index].addr_mode == riscv::TOR)) begin
            pmpaddr_d[index] = csr_wdata[riscv::PLEN-3:0];
          end
        end
        default: update_access_exception = 1'b1;
      endcase
    end

    mstatus_d.sxl = riscv::XLEN_64;
    mstatus_d.uxl = riscv::XLEN_64;

    // mark the floating point extension register as dirty
    if (CVA6Cfg.FpPresent && (dirty_fp_state_csr || dirty_fp_state_i)) begin
      mstatus_d.fs = riscv::Dirty;
    end
    // mark the vector extension register as dirty
    if (CVA6Cfg.RVV && dirty_v_state_i) begin
      mstatus_d.vs = riscv::Dirty;
    end
    // hardwired extension registers
    mstatus_d.sd = (mstatus_q.xs == riscv::Dirty) | (mstatus_q.fs == riscv::Dirty);

    // reserve PMPCFG bits 5 and 6 (hardwire to 0)
    for (int i = 0; i < CVA6Cfg.NrPMPEntries; i++) pmpcfg_d[i].reserved = 2'b0;

    // write the floating point status register
    if (CVA6Cfg.FpPresent && csr_write_fflags_i) begin
      fcsr_d.fflags = csr_wdata_i[4:0] | fcsr_q.fflags;
    end

    // ----------------------------
    // Accelerator FP imprecise exceptions
    // ----------------------------

    // Update fflags as soon as a FP exception occurs in the accelerator
    // The exception is imprecise, and the fcsr.fflags update always happens immediately
    if (CVA6Cfg.EnableAccelerator) begin
      fcsr_d.fflags |= acc_fflags_ex_valid_i ? acc_fflags_ex_i : 5'b0;
    end

    // ---------------------
    // External Interrupts
    // ---------------------
    // Machine Mode External Interrupt Pending
    mip_d[riscv::IRQ_M_EXT] = irq_i[0];
    // Machine software interrupt
    mip_d[riscv::IRQ_M_SOFT] = ipi_i;
    // Timer interrupt pending, coming from platform timer
    mip_d[riscv::IRQ_M_TIMER] = time_irq_i;

    // -----------------------
    // Manage Exception Stack
    // -----------------------
    // update exception CSRs
    // we got an exception update cause, pc and stval register
    trap_to_priv_lvl = riscv::PRIV_LVL_M;
    // Exception is taken and we are not in debug mode
    // exceptions in debug mode don't update any fields
    if ((CVA6Cfg.DebugEn && !debug_mode_q && ex_i.cause != riscv::DEBUG_REQUEST && ex_i.valid) || (!CVA6Cfg.DebugEn && ex_i.valid)) begin
      // do not flush, flush is reserved for CSR writes with side effects
      flush_o = 1'b0;
      // figure out where to trap to
      // a m-mode trap might be delegated if we are taking it in S mode
      // first figure out if this was an exception or an interrupt e.g.: look at bit (XLEN-1)
      // the cause register can only be $clog2(riscv::XLEN) bits long (as we only support XLEN exceptions)
      if (CVA6Cfg.RVS && ((ex_i.cause[riscv::XLEN-1] && mideleg_q[ex_i.cause[$clog2(
              riscv::XLEN
          )-1:0]]) || (~ex_i.cause[riscv::XLEN-1] && medeleg_q[ex_i.cause[$clog2(
              riscv::XLEN
          )-1:0]]))) begin
        // traps never transition from a more-privileged mode to a less privileged mode
        // so if we are already in M mode, stay there
        if (priv_lvl_o == riscv::PRIV_LVL_M) trap_to_priv_lvl = riscv::PRIV_LVL_M;
        else trap_to_priv_lvl = riscv::PRIV_LVL_S;
      end

      // trap to supervisor mode
      if (CVA6Cfg.RVS && trap_to_priv_lvl == riscv::PRIV_LVL_S) begin
        // update sstatus
        mstatus_d.sie = 1'b0;
        mstatus_d.spie = mstatus_q.sie;
        // this can either be user or supervisor mode
        mstatus_d.spp = priv_lvl_q[0];
        // set cause
        scause_d = ex_i.cause;
        // set epc
        sepc_d = {{riscv::XLEN - riscv::VLEN{pc_i[riscv::VLEN-1]}}, pc_i};
        // set mtval or stval
        stval_d        = (ariane_pkg::ZERO_TVAL
                                  && (ex_i.cause inside {
                                    riscv::ILLEGAL_INSTR,
                                    riscv::BREAKPOINT,
                                    riscv::ENV_CALL_UMODE,
                                    riscv::ENV_CALL_SMODE,
                                    riscv::ENV_CALL_MMODE
                                  } || ex_i.cause[riscv::XLEN-1])) ? '0 : ex_i.tval;
        // trap to machine mode
      end else begin
        // update mstatus
        mstatus_d.mie = 1'b0;
        mstatus_d.mpie = mstatus_q.mie;
        // save the previous privilege mode
        mstatus_d.mpp = priv_lvl_q;
        mcause_d = ex_i.cause;
        // set epc
        mepc_d = {{riscv::XLEN - riscv::VLEN{pc_i[riscv::VLEN-1]}}, pc_i};
        // set mtval or stval
        mtval_d        = (ariane_pkg::ZERO_TVAL
                                  && (ex_i.cause inside {
                                    riscv::ILLEGAL_INSTR,
                                    riscv::BREAKPOINT,
                                    riscv::ENV_CALL_UMODE,
                                    riscv::ENV_CALL_SMODE,
                                    riscv::ENV_CALL_MMODE
                                  } || ex_i.cause[riscv::XLEN-1])) ? '0 : ex_i.tval;
      end

      priv_lvl_d = trap_to_priv_lvl;
    end

    // ------------------------------
    // Debug
    // ------------------------------
    // Explains why Debug Mode was entered.
    // When there are multiple reasons to enter Debug Mode in a single cycle, hardware should set cause to the cause with the highest priority.
    // 1: An ebreak instruction was executed. (priority 3)
    // 2: The Trigger Module caused a breakpoint exception. (priority 4)
    // 3: The debugger requested entry to Debug Mode. (priority 2)
    // 4: The hart single stepped because step was set. (priority 1)
    // we are currently not in debug mode and could potentially enter
    if (!debug_mode_q) begin
      dcsr_d.prv = priv_lvl_o;
      // trigger module fired

      // caused by a breakpoint
      if (CVA6Cfg.DebugEn && ex_i.valid && ex_i.cause == riscv::BREAKPOINT) begin
        dcsr_d.prv = priv_lvl_o;
        // check that we actually want to enter debug depending on the privilege level we are currently in
        unique case (priv_lvl_o)
          riscv::PRIV_LVL_M: begin
            debug_mode_d   = dcsr_q.ebreakm;
            set_debug_pc_o = dcsr_q.ebreakm;
          end
          riscv::PRIV_LVL_S: begin
            if (CVA6Cfg.RVS) begin
              debug_mode_d   = dcsr_q.ebreaks;
              set_debug_pc_o = dcsr_q.ebreaks;
            end
          end
          riscv::PRIV_LVL_U: begin
            if (CVA6Cfg.RVU) begin
              debug_mode_d   = dcsr_q.ebreaku;
              set_debug_pc_o = dcsr_q.ebreaku;
            end
          end
          default: ;
        endcase
        // save PC of next this instruction e.g.: the next one to be executed
        dpc_d = {{riscv::XLEN - riscv::VLEN{pc_i[riscv::VLEN-1]}}, pc_i};
        dcsr_d.cause = ariane_pkg::CauseBreakpoint;
      end

      // we've got a debug request
      if (CVA6Cfg.DebugEn && ex_i.valid && ex_i.cause == riscv::DEBUG_REQUEST) begin
        dcsr_d.prv = priv_lvl_o;
        // save the PC
        dpc_d = {{riscv::XLEN - riscv::VLEN{pc_i[riscv::VLEN-1]}}, pc_i};
        // enter debug mode
        debug_mode_d = 1'b1;
        // jump to the base address
        set_debug_pc_o = 1'b1;
        // save the cause as external debug request
        dcsr_d.cause = ariane_pkg::CauseRequest;
      end

      // single step enable and we just retired an instruction
      if (CVA6Cfg.DebugEn && dcsr_q.step && commit_ack_i[0]) begin
        dcsr_d.prv = priv_lvl_o;
        // valid CTRL flow change
        if (commit_instr_i[0].fu == CTRL_FLOW) begin
          // we saved the correct target address during execute
          dpc_d = {
            {riscv::XLEN - riscv::VLEN{commit_instr_i[0].bp.predict_address[riscv::VLEN-1]}},
            commit_instr_i[0].bp.predict_address
          };
          // exception valid
        end else if (ex_i.valid) begin
          dpc_d = {{riscv::XLEN - riscv::VLEN{1'b0}}, trap_vector_base_o};
          // return from environment
        end else if (eret_o) begin
          dpc_d = {{riscv::XLEN - riscv::VLEN{1'b0}}, epc_o};
          // consecutive PC
        end else begin
          dpc_d = {
            {riscv::XLEN - riscv::VLEN{commit_instr_i[0].pc[riscv::VLEN-1]}},
            commit_instr_i[0].pc + (commit_instr_i[0].is_compressed ? 'h2 : 'h4)
          };
        end
        debug_mode_d   = 1'b1;
        set_debug_pc_o = 1'b1;
        dcsr_d.cause   = ariane_pkg::CauseSingleStep;
      end
    end
    // go in halt-state again when we encounter an exception
    if (CVA6Cfg.DebugEn && debug_mode_q && ex_i.valid && ex_i.cause == riscv::BREAKPOINT) begin
      set_debug_pc_o = 1'b1;
    end

    // ------------------------------
    // MPRV - Modify Privilege Level
    // ------------------------------
    // Set the address translation at which the load and stores should occur
    // we can use the previous values since changing the address translation will always involve a pipeline flush
    if (ariane_pkg::MMU_PRESENT && mprv && CVA6Cfg.RVS && riscv::vm_mode_t'(satp_q.mode) == riscv::MODE_SV && (mstatus_q.mpp != riscv::PRIV_LVL_M))
      en_ld_st_translation_d = 1'b1;
    else  // otherwise we go with the regular settings
      en_ld_st_translation_d = en_translation_o;

    ld_st_priv_lvl_o = (mprv) ? mstatus_q.mpp : priv_lvl_o;
    en_ld_st_translation_o = en_ld_st_translation_q;
    // ------------------------------
    // Return from Environment
    // ------------------------------
    // When executing an xRET instruction, supposing xPP holds the value y, xIE is set to xPIE; the privilege
    // mode is changed to y; xPIE is set to 1; and xPP is set to U
    if (mret) begin
      // return from exception, IF doesn't care from where we are returning
      eret_o         = 1'b1;
      // return to the previous privilege level and restore all enable flags
      // get the previous machine interrupt enable flag
      mstatus_d.mie  = mstatus_q.mpie;
      // restore the previous privilege level
      priv_lvl_d     = mstatus_q.mpp;
      // set mpp to user mode
      mstatus_d.mpp  = riscv::PRIV_LVL_U;
      // set mpie to 1
      mstatus_d.mpie = 1'b1;
    end

    if (CVA6Cfg.RVS && sret) begin
      // return from exception, IF doesn't care from where we are returning
      eret_o         = 1'b1;
      // return the previous supervisor interrupt enable flag
      mstatus_d.sie  = mstatus_q.spie;
      // restore the previous privilege level
      priv_lvl_d     = riscv::priv_lvl_t'({1'b0, mstatus_q.spp});
      // set spp to user mode
      mstatus_d.spp  = 1'b0;
      // set spie to 1
      mstatus_d.spie = 1'b1;
    end

    // return from debug mode
    if (CVA6Cfg.DebugEn && dret) begin
      // return from exception, IF doesn't care from where we are returning
      eret_o       = 1'b1;
      // restore the previous privilege level
      priv_lvl_d   = riscv::priv_lvl_t'(dcsr_q.prv);
      // actually return from debug mode
      debug_mode_d = 1'b0;
    end
  end

  // ---------------------------
  // CSR OP Select Logic
  // ---------------------------
  always_comb begin : csr_op_logic
    csr_wdata = csr_wdata_i;
    csr_we    = 1'b1;
    csr_read  = 1'b1;
    mret      = 1'b0;
    sret      = 1'b0;
    dret      = 1'b0;

    unique case (csr_op_i)
      CSR_WRITE: csr_wdata = csr_wdata_i;
      CSR_SET:   csr_wdata = csr_wdata_i | csr_rdata;
      CSR_CLEAR: csr_wdata = (~csr_wdata_i) & csr_rdata;
      CSR_READ:  csr_we = 1'b0;
      MRET: begin
        // the return should not have any write or read side-effects
        csr_we   = 1'b0;
        csr_read = 1'b0;
        mret     = 1'b1;  // signal a return from machine mode
      end
      default: begin
        if (CVA6Cfg.RVS && csr_op_i == SRET) begin
          // the return should not have any write or read side-effects
          csr_we   = 1'b0;
          csr_read = 1'b0;
          sret     = 1'b1;  // signal a return from supervisor mode
        end else if (CVA6Cfg.DebugEn && csr_op_i == DRET) begin
          // the return should not have any write or read side-effects
          csr_we   = 1'b0;
          csr_read = 1'b0;
          dret     = 1'b1;  // signal a return from debug mode
        end else begin
          csr_we   = 1'b0;
          csr_read = 1'b0;
        end
      end
    endcase
    // if we are violating our privilges do not update the architectural state
    if (privilege_violation) begin
      csr_we   = 1'b0;
      csr_read = 1'b0;
    end
  end

  assign irq_ctrl_o.mie = mie_q;
  assign irq_ctrl_o.mip = mip_q;
  assign irq_ctrl_o.sie = mstatus_q.sie;
  assign irq_ctrl_o.mideleg = mideleg_q;
  assign irq_ctrl_o.global_enable = (~debug_mode_q)
      // interrupts are enabled during single step or we are not stepping
      // No need to check interrupts during single step if we don't support DEBUG mode
      & (~CVA6Cfg.DebugEn | (~dcsr_q.step | dcsr_q.stepie))
                                    & ((mstatus_q.mie & (priv_lvl_o == riscv::PRIV_LVL_M))
                                    | (priv_lvl_o != riscv::PRIV_LVL_M));

  always_comb begin : privilege_check
    // -----------------
    // Privilege Check
    // -----------------
    privilege_violation = 1'b0;
    // if we are reading or writing, check for the correct privilege level this has
    // precedence over interrupts
    if (csr_op_i inside {CSR_WRITE, CSR_SET, CSR_CLEAR, CSR_READ}) begin
      if ((riscv::priv_lvl_t'(priv_lvl_o & csr_addr.csr_decode.priv_lvl) != csr_addr.csr_decode.priv_lvl)) begin
        privilege_violation = 1'b1;
      end
      // check access to debug mode only CSRs
      if ((!CVA6Cfg.DebugEn && csr_addr_i[11:4] == 8'h7b) || (CVA6Cfg.DebugEn && csr_addr_i[11:4] == 8'h7b && !debug_mode_q)) begin
        privilege_violation = 1'b1;
      end
      // check counter-enabled counter CSR accesses
      // counter address range is C00 to C1F
      if (csr_addr_i inside {[riscv::CSR_CYCLE : riscv::CSR_HPM_COUNTER_31]}) begin
        if (priv_lvl_o == riscv::PRIV_LVL_S && CVA6Cfg.RVS) begin
          privilege_violation = ~mcounteren_q[csr_addr_i[4:0]];
        end else if (priv_lvl_o == riscv::PRIV_LVL_U && CVA6Cfg.RVU) begin
          privilege_violation = ~mcounteren_q[csr_addr_i[4:0]] | ~scounteren_q[csr_addr_i[4:0]];
        end else if (priv_lvl_o == riscv::PRIV_LVL_M) begin
          privilege_violation = 1'b0;
        end
      end
    end
  end
  // ----------------------
  // CSR Exception Control
  // ----------------------
  always_comb begin : exception_ctrl
    csr_exception_o = {{riscv::XLEN{1'b0}}, {riscv::XLEN{1'b0}}, 1'b0};
    // ----------------------------------
    // Illegal Access (decode exception)
    // ----------------------------------
    // we got an exception in one of the processes above
    // throw an illegal instruction exception
    if (update_access_exception || read_access_exception) begin
      csr_exception_o.cause = riscv::ILLEGAL_INSTR;
      // we don't set the tval field as this will be set by the commit stage
      // this spares the extra wiring from commit to CSR and back to commit
      csr_exception_o.valid = 1'b1;
    end

    if (privilege_violation) begin
      csr_exception_o.cause = riscv::ILLEGAL_INSTR;
      csr_exception_o.valid = 1'b1;
    end
  end

  // -------------------
  // Wait for Interrupt
  // -------------------
  always_comb begin : wfi_ctrl
    // wait for interrupt register
    wfi_d = wfi_q;
    // if there is any (enabled) interrupt pending un-stall the core
    // also un-stall if we want to enter debug mode
    if (|(mip_q & mie_q) || (CVA6Cfg.DebugEn && debug_req_i) || irq_i[1]) begin
      wfi_d = 1'b0;
      // or alternatively if there is no exception pending and we are not in debug mode wait here
      // for the interrupt
    end else if (((CVA6Cfg.DebugEn && !debug_mode_q) && csr_op_i == WFI && !ex_i.valid) || (!CVA6Cfg.DebugEn && csr_op_i == WFI && !ex_i.valid)) begin
      wfi_d = 1'b1;
    end
  end

  // output assignments dependent on privilege mode
  always_comb begin : priv_output
    trap_vector_base_o = {mtvec_q[riscv::VLEN-1:2], 2'b0};
    // output user mode stvec
    if (CVA6Cfg.RVS && trap_to_priv_lvl == riscv::PRIV_LVL_S) begin
      trap_vector_base_o = {stvec_q[riscv::VLEN-1:2], 2'b0};
    end

    // if we are in debug mode jump to a specific address
    if (CVA6Cfg.DebugEn && debug_mode_q) begin
      trap_vector_base_o = CVA6Cfg.DmBaseAddress[riscv::VLEN-1:0] + CVA6Cfg.ExceptionAddress[riscv::VLEN-1:0];
    end

    // check if we are in vectored mode, if yes then do BASE + 4 * cause we
    // are imposing an additional alignment-constraint of 64 * 4 bytes since
    // we want to spare the costly addition. Furthermore check to which
    // privilege level we are jumping and whether the vectored mode is
    // activated for _that_ privilege level.
    if (ex_i.cause[riscv::XLEN-1] &&
                ((((CVA6Cfg.RVS || CVA6Cfg.RVU) && trap_to_priv_lvl == riscv::PRIV_LVL_M && mtvec_q[0]) || (!CVA6Cfg.RVS && !CVA6Cfg.RVU && mtvec_q[0]))
               || (CVA6Cfg.RVS && trap_to_priv_lvl == riscv::PRIV_LVL_S && stvec_q[0]))) begin
      trap_vector_base_o[7:2] = ex_i.cause[5:0];
    end

    epc_o = mepc_q[riscv::VLEN-1:0];
    // we are returning from supervisor mode, so take the sepc register
    if (CVA6Cfg.RVS && sret) begin
      epc_o = sepc_q[riscv::VLEN-1:0];
    end
    // we are returning from debug mode, to take the dpc register
    if (CVA6Cfg.DebugEn && dret) begin
      epc_o = dpc_q[riscv::VLEN-1:0];
    end
  end

  // -------------------
  // Output Assignments
  // -------------------
  always_comb begin
    // When the SEIP bit is read with a CSRRW, CSRRS, or CSRRC instruction, the value
    // returned in the rd destination register contains the logical-OR of the software-writable
    // bit and the interrupt signal from the interrupt controller.
    csr_rdata_o = csr_rdata;

    unique case (csr_addr.address)
      riscv::CSR_MIP:
      csr_rdata_o = csr_rdata | ({{riscv::XLEN - 1{1'b0}}, irq_i[1]} << riscv::IRQ_S_EXT);
      // in supervisor mode we also need to check whether we delegated this bit
      riscv::CSR_SIP: begin
        if (CVA6Cfg.RVS) begin
          csr_rdata_o = csr_rdata
                              | ({{riscv::XLEN-1{1'b0}}, (irq_i[1] & mideleg_q[riscv::IRQ_S_EXT])} << riscv::IRQ_S_EXT);
        end
      end
      default: ;
    endcase
  end

  // in debug mode we execute with privilege level M
  assign priv_lvl_o = (CVA6Cfg.DebugEn && debug_mode_q) ? riscv::PRIV_LVL_M : priv_lvl_q;
  // FPU outputs
  assign fflags_o = fcsr_q.fflags;
  assign frm_o = fcsr_q.frm;
  assign fprec_o = fcsr_q.fprec;
  // MMU outputs
  assign satp_ppn_o = satp_q.ppn;
  assign asid_o = satp_q.asid[AsidWidth-1:0];
  assign sum_o = mstatus_q.sum;
  // we support bare memory addressing and SV39
  assign en_translation_o = ((CVA6Cfg.RVS && riscv::vm_mode_t'(satp_q.mode) == riscv::MODE_SV) &&
                               priv_lvl_o != riscv::PRIV_LVL_M)
                              ? 1'b1
                              : 1'b0;
  assign mxr_o = mstatus_q.mxr;
  assign tvm_o = mstatus_q.tvm;
  assign tw_o = mstatus_q.tw;
  assign tsr_o = mstatus_q.tsr;
  assign halt_csr_o = wfi_q;
`ifdef PITON_ARIANE
  assign icache_en_o = icache_q[0];
`else
  assign icache_en_o = icache_q[0] & (~debug_mode_q);
`endif
  assign dcache_en_o = dcache_q[0];
  assign acc_cons_en_o = CVA6Cfg.EnableAccelerator ? acc_cons_q[0] : 1'b0;

  // determine if mprv needs to be considered if in debug mode
  assign mprv = (CVA6Cfg.DebugEn && debug_mode_q && !dcsr_q.mprven) ? 1'b0 : mstatus_q.mprv;
  assign debug_mode_o = debug_mode_q;
  assign single_step_o = dcsr_q.step;
  assign mcountinhibit_o = {{29 - MHPMCounterNum{1'b0}}, mcountinhibit_q};

  // sequential process
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (~rst_ni) begin
      priv_lvl_q   <= riscv::PRIV_LVL_M;
      // floating-point registers
      fcsr_q       <= '0;
      // debug signals
      debug_mode_q <= 1'b0;
      if (CVA6Cfg.DebugEn) begin
        dcsr_q           <= '0;
        dcsr_q.prv       <= riscv::PRIV_LVL_M;
        dcsr_q.xdebugver <= 4'h4;
        dpc_q            <= '0;
        dscratch0_q      <= {riscv::XLEN{1'b0}};
        dscratch1_q      <= {riscv::XLEN{1'b0}};
      end
      // machine mode registers
      mstatus_q        <= 64'b0;
      // set to boot address + direct mode + 4 byte offset which is the initial trap
      mtvec_rst_load_q <= 1'b1;
      mtvec_q          <= '0;
      mip_q            <= {riscv::XLEN{1'b0}};
      mie_q            <= {riscv::XLEN{1'b0}};
      mepc_q           <= {riscv::XLEN{1'b0}};
      mcause_q         <= {riscv::XLEN{1'b0}};
      mcounteren_q     <= {riscv::XLEN{1'b0}};
      mscratch_q       <= {riscv::XLEN{1'b0}};
      mtval_q          <= {riscv::XLEN{1'b0}};
      fiom_q           <= '0;
      dcache_q         <= {{riscv::XLEN - 1{1'b0}}, 1'b1};
      icache_q         <= {{riscv::XLEN - 1{1'b0}}, 1'b1};
      mcountinhibit_q  <= '0;
      acc_cons_q       <= {{riscv::XLEN - 1{1'b0}}, CVA6Cfg.EnableAccelerator};
      // supervisor mode registers
      if (CVA6Cfg.RVS) begin
        medeleg_q    <= {riscv::XLEN{1'b0}};
        mideleg_q    <= {riscv::XLEN{1'b0}};
        sepc_q       <= {riscv::XLEN{1'b0}};
        scause_q     <= {riscv::XLEN{1'b0}};
        stvec_q      <= {riscv::XLEN{1'b0}};
        scounteren_q <= {riscv::XLEN{1'b0}};
        sscratch_q   <= {riscv::XLEN{1'b0}};
        stval_q      <= {riscv::XLEN{1'b0}};
        satp_q       <= {riscv::XLEN{1'b0}};
      end
      // timer and counters
      cycle_q                <= 64'b0;
      instret_q              <= 64'b0;
      // aux registers
      en_ld_st_translation_q <= 1'b0;
      // wait for interrupt
      wfi_q                  <= 1'b0;
      // pmp
      for (int i = 0; i < 16; i++) begin
        if (i < CVA6Cfg.NrPMPEntries) begin
          pmpcfg_q[i]  <= riscv::pmpcfg_t'(CVA6Cfg.PMPCfgRstVal[i]);
          pmpaddr_q[i] <= CVA6Cfg.PMPAddrRstVal[i][riscv::PLEN-3:0];
        end else begin
          pmpcfg_q[i]  <= '0;
          pmpaddr_q[i] <= '0;
        end
      end
    end else begin
      priv_lvl_q <= priv_lvl_d;
      // floating-point registers
      fcsr_q     <= fcsr_d;
      // debug signals
      if (CVA6Cfg.DebugEn) begin
        debug_mode_q <= debug_mode_d;
        dcsr_q       <= dcsr_d;
        dpc_q        <= dpc_d;
        dscratch0_q  <= dscratch0_d;
        dscratch1_q  <= dscratch1_d;
      end
      // machine mode registers
      mstatus_q        <= mstatus_d;
      mtvec_rst_load_q <= 1'b0;
      mtvec_q          <= mtvec_d;
      mip_q            <= mip_d;
      mie_q            <= mie_d;
      mepc_q           <= mepc_d;
      mcause_q         <= mcause_d;
      mcounteren_q     <= mcounteren_d;
      mscratch_q       <= mscratch_d;
      mtval_q          <= mtval_d;
      fiom_q           <= fiom_d;
      dcache_q         <= dcache_d;
      icache_q         <= icache_d;
      mcountinhibit_q  <= mcountinhibit_d;
      acc_cons_q       <= acc_cons_d;
      // supervisor mode registers
      if (CVA6Cfg.RVS) begin
        medeleg_q    <= medeleg_d;
        mideleg_q    <= mideleg_d;
        sepc_q       <= sepc_d;
        scause_q     <= scause_d;
        stvec_q      <= stvec_d;
        scounteren_q <= scounteren_d;
        sscratch_q   <= sscratch_d;
        stval_q      <= stval_d;
        satp_q       <= satp_d;
      end
      // timer and counters
      cycle_q                <= cycle_d;
      instret_q              <= instret_d;
      // aux registers
      en_ld_st_translation_q <= en_ld_st_translation_d;
      // wait for interrupt
      wfi_q                  <= wfi_d;
      // pmp
      for (int i = 0; i < 16; i++) begin
        if (i < CVA6Cfg.NrPMPEntries) begin
          // We only support >=8-byte granularity, NA4 is disabled
          if(!CVA6Cfg.PMPEntryReadOnly[i] && pmpcfg_d[i].addr_mode != riscv::NA4 && !(pmpcfg_d[i].access_type.r == '0 && pmpcfg_d[i].access_type.w == '1)) begin
            pmpcfg_q[i] <= pmpcfg_d[i];
          end else begin
            pmpcfg_q[i] <= pmpcfg_q[i];
          end
          if (!CVA6Cfg.PMPEntryReadOnly[i]) begin
            pmpaddr_q[i] <= pmpaddr_d[i];
          end else begin
            pmpaddr_q[i] <= pmpaddr_q[i];
          end
        end else begin
          pmpcfg_q[i]  <= '0;
          pmpaddr_q[i] <= '0;
        end
      end
    end
  end

  //-------------
  // Assertions
  //-------------
  //pragma translate_off
  // check that eret and ex are never valid together
  assert property (@(posedge clk_i) disable iff (!rst_ni !== '0) !(eret_o && ex_i.valid))
  else begin
    $error("eret and exception should never be valid at the same time");
    $stop();
  end
  //pragma translate_on
endmodule
