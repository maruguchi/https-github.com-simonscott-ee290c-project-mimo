function [decodedData, errors] = ML_Decode(Ntx, Nrx, rx, H_est)

% Compute transmission length
transmitLength = size(rx, 2);

% Build matrix of all possible transmit combinations (in a single symbol
% interval)
symbols = 1/sqrt(2) * [1 + 1j, 1 - 1j, -1 + 1j, -1 - 1j];
s_comb = combn(symbols, Ntx);

% Calculate all possible received sequences
Hs = H_est * s_comb.';
num_poss_rcvd = size(Hs, 2);

% For each instant of time at the receiver
dist = zeros(1, num_poss_rcvd);
for time = 1:transmitLength
    
    % Find s that minimizes (y - Hs)
    for i = 1:num_poss_rcvd
        dist(i) = norm(rx(:, time) - Hs(:, i));
    end
    
    [min_val min_idx] = min(dist);
    
    decodedData(:, time) = s_comb(min_idx, :);
        
end

errors = 0;

end