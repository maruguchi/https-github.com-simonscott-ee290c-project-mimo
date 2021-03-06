function decodedData = LMS_MIMO(mu)

Ntx = 4;
trainLength = 1000;
dataLength = 1000;
transmitLength = trainLength + dataLength;

Nrx = 4;
SNR = 10; %dB
nse = 10^(-SNR/20);

% mu = 0.01;
errors = zeros(Nrx,transmitLength);

training = randint(Ntx,trainLength)*2-1 + 1j*(randint(Ntx,trainLength)*2-1);
data = randint(Ntx,dataLength)*2-1 + 1j*(randint(Ntx,dataLength)*2-1);

transmitted = 1/sqrt(2)*[training data];
global H;
H = 1/sqrt(2)*(randn(Nrx,Ntx) + 1j*randn(Nrx,Ntx));
% H = eye(Ntx);
noise = 1/sqrt(2)*(normrnd(0, nse, Nrx, transmitLength) + 1j*normrnd(0, nse, Nrx, transmitLength));
% noise = zeros(Nrx,transmitLength);
rx = H*transmitted + noise;

W = eye(Ntx,Nrx);
decodedData = zeros(Ntx,dataLength);

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
    
%     errors(:,transmitLength) = abs(en);
    decodedData(:, time) = yn;
        
end
xn
rn
yn

figure;
stem(sum(abs(decodedData-transmitted)/2,1));

% figure;
% plot(errors);

end