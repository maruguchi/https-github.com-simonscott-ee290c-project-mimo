% Script to calculate the required symbol rate (and clock freq) to meet the
% LTE and 802.11ac specs

% Note: convergence time not an issue. Just 4 cycles, which we use.

%%%%%%%%%%%%% LTE %%%%%%%%%%%%%%%%

lte_max_streams = 4;
lte_max_mod_order = 64;
lte_our_mod_order = 4;

lte_subc_per_resblock = 12;
lte_slot_len = 0.5e-3;          % seconds
lte_sym_per_slot = 7;
lte_subc_bw = 15e3;             % Hz
lte_max_chan_bw = 20e6;            % Hz
lte_num_subc = lte_max_chan_bw / lte_subc_bw;

lte_sym_rate_per_subc = lte_sym_per_slot / lte_slot_len
lte_clock_rate_subc_seq = lte_sym_rate_per_subc * lte_num_subc
lte_our_data_rate = lte_clock_rate_subc_seq * lte_our_mod_order * lte_max_streams

%%%%%%%%%%%%% 802.11 ac %%%%%%%%%%%%%%%%

ac802_max_subc = 234;           % these subcarriers occupy single channel (80MHz mode)
ac802_max_chan_width = 80e6;    % max is actually 160MHz, but Sriram said we don't need to support it
ac802_max_streams = 4;          % max is actually 8, but only 4 needed for this class
ac802_max_mod_order = 256;      % 256-QAM
ac802_our_mod_order = 4;
ac802_max_code_rate = 5/6;      % code rate used for 256-QAM
ac802_our_code_rate = 3/4;      % the spec defines this for QPSK (MCS-2)

bps_qpsk_20mhz_1stream = 21.7e6;

% The following assumes 4 data streams, all computed in parallel
ac802_sym_rate_per_subc = bps_qpsk_20mhz_1stream * 4.5 / ac802_max_subc / ...
                            ac802_our_code_rate / log2(ac802_our_mod_order)

ac802_clock_rate_subc_seq = ac802_sym_rate_per_subc * ac802_max_subc
ac802_our_throughput = ac802_clock_rate_subc_seq * log2(ac802_our_mod_order) * ac802_max_streams

% Note: multiples of these packets can be aggregated together
ac802_pkt_len_bytes = 11454;
ac802_pkt_len_syms = ac802_pkt_len_bytes * 8 / log2(ac802_our_mod_order)
