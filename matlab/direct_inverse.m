function [decodedData, errors] = direct_inverse(Ntx, Nrx, rx, training, H_est, SNR)

% Compute transmission, training and data lengths
trainLength = size(training, 2);
transmitLength = size(rx, 2);
dataLength = transmitLength - trainLength;

% W is the MIMO decoder matrix at the receiver
kernel = H_est'*H_est + 1/SNR*eye(Nrx);
W = kernel\H_est'; %temporary function uses direct MATLAB inverse
decodedData = zeros(Ntx,dataLength);

equalized = W*rx;
decodedData = 1/sqrt(2)*(sign(real(equalized)) + 1j*sign(imag(equalized)));
% decodedData = equalized;

errors = rx(:, 1:trainLength) - training;

end