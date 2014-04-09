function [H, h] = get_channel(Nt, Nr, fd, model)

% Returns
% - H: one channel matrix
% - h: comm.MIMOChannel object

% Nt = Number of transmit antennas
% Nr = Number of receive antennas
% fd = Doppler frequency (determines coherence time)

H = zeros(Nr, Nt);

%% IEEE 802.11n Channel Models
% This example shows how to simulate multiple-input multiple-output (MIMO)
% multipath fading channels based on the IEEE(R) 802.11n channel models for
% indoor wireless local area networks (WLAN). The example uses a MIMO
% multipath fading channel System object with two transmit antennas, two
% receive antennas, and a bell Doppler spectrum object.

% Copyright 2008-2012 The MathWorks, Inc.
% $Revision: 1.1.6.5 $ $Date: 2012/04/14 03:43:22 $

%% IEEE 802.11n Channel Models: Overview
% The IEEE 802.11n channel models [ <#10 1> ] are designed for indoor
% wireless local area networks for bandwidths of up to 100 MHz, at
% frequencies of 2 and 5 GHz. The channel models comprise a set of 6
% profiles, labeled A to F, which cover the scenarios of flat fading,
% residential, residential/small office, typical office, large office, and
% large space (indoors and outdoors). Each channel model has a path loss
% model including shadowing, and a MIMO multipath fading model, which
% describes the multipath delay profile, the spatial properties, the
% K-factor distribution, and the Doppler spectrum.
%
% Each channel model has a certain number of taps (one for model A, and 9
% to 18 for models B-F). Each tap is characterized by a relative delay
% (with respect to the first path delay). Each model further comprises a
% number of clusters, which correspond to overlapping subsets of the tap
% delays. For example, model B has two clusters: cluster 1 corresponds to
% tap delays 0 to 40 ns (in steps of 10 ns), while cluster 2 corresponds to
% tap delays 20 to 80 ns (also in steps of 10 ns). Hence, clusters 1 and 2
% comprise 5 and 7 tap delays, respectively, and they overlap in 3 tap
% delays (20, 30 and 40 ns). Each cluster is assigned a set of spatial
% properties: a mean angle of arrival (AoA), a mean angle of departure
% (AoD), an angular spread (AS) at the receiver, and an angular spread at
% the transmitter. These parameters assume the same values for all tap
% delays pertaining to a given cluster. These parameters determine the
% transmit and correlation matrices associated with each tap delay.
%
% The IEEE 802.11n channel models make the following assumptions: 1) The 
% power azimuth spectrum (PAS) and the power delay spectrum (PDS) are 
% separable: each tap is modeled independently. 2) The PAS and the Doppler
% spectrum for each tap are separable: the spatial correlation (correlation
% matrices) and temporal correlation (Doppler spectrum) for each tap are
% modeled independently. 3) Each tap is modeled using the Kronecker model
% for Rician channels, hence it is assumed that the transmit and receive
% correlation matrices are separable for each tap.

%% Initialization of Simulation-Specific Parameters 
% The simulation sampling rate is specified, and kept the same for the
% remainder of the example. The input to the channel simulator is
% oversampled by a factor of four.

S = RandStream('swb2712', 'Seed', 12345); % Set a local random number stream
M = 2;                      % Modulation order
hModem = modem.pskmod(M);   % 2-PSK modulator object

Rsym = 10e3;                % Input symbol rate
Rbit = Rsym * log2(M);      % Input bit rate
Nos  = 4;                   % Oversampling factor
Rs   = Rbit * Nos;          % Input sample rate 

%% Channel Model B
% The code below constructs a MIMO channel System object according to
% channel model B of [ <#10 1> ], in non-line-of-sight (NLOS) mode.
%
% This channel model has 9 Rayleigh-fading paths, and each path has a bell
% Doppler spectrum, with a parameter as specified in the default
% doppler.bell object. 
%
% We use two transmit antennas and two receive antennas. For each path, the
% transmit and receive correlation matrices are calculated according to the
% procedure given in [ <#10 1> ], [ <#10 2> ]. 

[tau, pdb, AS_Tx, AoD, AS_Rx, AoA] = get_802_11n_channel_params(model);

pdb_sum = 10*log10(sum(10.^(pdb/10),1));

% fd = 3;             % Maximum Doppler shift for all paths (identical)
ds = doppler.bell;  % Bell doppler spectrum, with default parameters

% Element spacing at the transmit and receive antennas (normalized by the
% wavelength)
TxSpacing = 0.5;
RxSpacing = 0.5;

% Calculation of transmit and receive correlation arrays
[TxCorrelationMatrix, RxCorrelationMatrix] = ...
    correlationMatrix(Nt, Nr, pdb, TxSpacing, RxSpacing, ...
    AS_Tx, AoD, AS_Rx, AoA);

h = comm.MIMOChannel( ...
        'SampleRate',                Rs, ...
        'PathDelays',                tau, ...
        'AveragePathGains',          pdb_sum, ...
        'MaximumDopplerShift',       fd, ...               
        'DopplerSpectrum',           ds, ...    
        'NumTransmitAntennas',       Nt, ...                 
        'NumReceiveAntennas',        Nr, ...                 
        'TransmitCorrelationMatrix', TxCorrelationMatrix, ...
        'ReceiveCorrelationMatrix',  RxCorrelationMatrix); %, ...   
%         'RandomStream',              'mt19937ar with seed', ... 
%         'Seed',                      99, ...
%         'PathGainsOutputPort',       true);
    
randWait = randi(1e2);

while (randWait > 0)
    step(h,ones(1,Nt));
    randWait = randWait - 1;
end
    
for colInd = 1:Nt
    H(:, colInd) = step(h, [zeros(1, (colInd-1)) 1 zeros(1, Nt-colInd)]);    
end

end