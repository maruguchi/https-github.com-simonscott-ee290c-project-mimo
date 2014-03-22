function [decodedData, errors] = LMS_Decode(Ntx, Nrx, rx, training, mu)

% Compute transmission, training and data lengths
trainLength = size(training, 2);
transmitLength = size(rx, 2);
dataLength = transmitLength - trainLength;

% W is the MIMO decoder matrix at the receiver
W = eye(Ntx,Nrx);
decodedData = zeros(Ntx,dataLength);

errors = zeros(Nrx,transmitLength);

% For each instant of time at the receiver
for time = 1:transmitLength
    xn = rx(:,time);
    
    rn = W*xn;
    yn = sign(real(rn)) + 1j*sign(imag(rn));
    if (time <= trainLength)
        en = rn - training(:,time);
    else 
        en = rn - yn;
    end
    
    for k = 1:size(W,1)
         W(k,:) = W(k,:) - mu*en(k)*xn';
    end
    
    errors(:,transmitLength) = abs(en);
    decodedData(:, time) = yn;
        
end

end