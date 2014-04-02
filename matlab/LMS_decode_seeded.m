function [decodedData] = LMS_decode_seeded(Ntx, Nrx, rx, mu, seed)

% Compute transmission, training and data lengths
dataLength = size(rx, 2);

% W is the MIMO decoder matrix at the receiver
W = seed;
decodedData = zeros(Ntx,dataLength);

% For each instant of time at the receiver
for time = 1:dataLength
    xn = rx(:,time);
    
    rn = W*xn;
    yn = 1/sqrt(2)*(sign(real(rn)) + 1j*sign(imag(rn)));

    en = rn - yn;
    
    for k = 1:size(W,1)
         W(k,:) = W(k,:) - mu*en(k)*xn';
    end
    
    decodedData(:, time) = yn;
end

end