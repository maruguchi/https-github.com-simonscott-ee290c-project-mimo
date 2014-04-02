function [decodedData] = direct_inverse(Ntx, Nrx, rx, H_est, SNR)

% Compute data lengths
dataLength = size(rx, 2);

% W is the MIMO decoder matrix at the receiver
W = mmseWeights(H_est, Nrx, SNR);

equalized = W*rx;
decodedData = 1/sqrt(2)*(sign(real(equalized)) + 1j*sign(imag(equalized)));
% decodedData = equalized;

end