`default_nettype none

`define CACHE_SETS 8
`define CACHE_BLOCK_SIZE 'd16

`define WORD_POISON 32'hBADF00D
`define CACHE_PHYSICAL_TAG_POISON 22'h277BAD

// typedef logic [5:0] CacheIndex_t;
// typedef logic [3:0] CacheBlockOffset_t;
// typedef logic [19:0] CacheVirtualTag_t;
// typedef logic [21:0] CachePhysicalTag_t;
// typedef logic [$clog2(`CACHE_SETS)-1:0] CacheSetNumber_t;

`define CACHE_REQUEST_WIDTH 104

// typedef struct packed {
//     CacheIndex_t             index      ;
//     CacheBlockOffset_t       blockOffset;
//     // This field is only valid if requestType is a write
//     CachePhysicalTag_t            tag        ;
//     Word_t                   writeData     ;
//     CacheRequestType_t       requestType;
//     logic                    isValid    ;
//     logic              [3:0] writeEnable;
//     // The user must ensure that this set is safe to write to using the memory
//     // locking system.
//     CacheSetNumber_t writeSet;
// } CacheRequest_t;

module CacheHelper (
    input wire [5:0] index,
    input wire [3:0] offset,
    output wire [`CACHE_REQUEST_WIDTH-1:0] flatRequest
);
    // CacheRequestType_t
    parameter CACHE_READ = 32'd0;
    parameter CACHE_WRITE = 32'd1;
    parameter CACHE_DRAM_FILL = 32'd2;

    function [`CACHE_REQUEST_WIDTH-1:0] readRequestManual(input [5:0] index, input [3:0] offset);
    begin
        readRequestManual[2:0] = 4'b0; // writeSet: 32-bit literal in SV truncated to 4 bits in Verilog
        readRequestManual[6:3] = 4'b0; // writeEnable
        readRequestManual[7] = 1'b1; // isValid
        readRequestManual[39:8] = CACHE_READ; // requestType
        readRequestManual[71:40] = `WORD_POISON; // writeData
        readRequestManual[93:72] = `CACHE_PHYSICAL_TAG_POISON; // tag
        readRequestManual[97:94] = offset; // blockOffset
        readRequestManual[103:98] = index; // index

    end
    endfunction

    wire [`CACHE_REQUEST_WIDTH-1:0] request;

    assign request = readRequestManual(index, offset);

    assign flatRequest = request;

endmodule