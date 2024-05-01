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
 *  Authors       : Riccardo Alidori, Cesar Fuguet
 *  Creation Date : June, 2021
 *  Description   : Snooper used by the hardware memory prefetcher
 *  History       :
 */
module hwpf_stride_snooper
import hpdcache_pkg::*;
(
    input  logic            en_i,           // Snooper enable bit.
    input  hpdcache_nline_t base_nline_i,   // Address to check
    input  hpdcache_nline_t snoop_addr_i,   // Input address to snoop
    output snoop_match_o   // If high, the Snoopers matched the snoop_address
);

    // The snooper match if enabled and the two addresses are equal
    assign snoop_match_o = en_i && ( base_nline_i == snoop_addr_i );

endmodule
