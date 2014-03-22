function decodedData = MIMO_sim(mu, decoder_type)

% Transmit parameters
Ntx = 4;
trainLength = 1000;
dataLength = 1000;
transmitLength = trainLength + dataLength;

% Receive parameters
Nrx = 4;
SNR = 10; %dB
nse = 10^(-SNR/20);

% The data that is transmitted
training = randint(Ntx,trainLength)*2-1 + 1j*(randint(Ntx,trainLength)*2-1);
data = randint(Ntx,dataLength)*2-1 + 1j*(randint(Ntx,dataLength)*2-1);
transmitted = 1/sqrt(2)*[training data];

% H is random channel matrix
% Each element is a random number, with normal distribution with
% mu=0 std_dev=1, for both real and imag parts
% Normalize by 1/sqrt(2) to make mean magnitude of H = 1
global H;
H = 1/sqrt(2)*(randn(Nrx,Ntx) + 1j*randn(Nrx,Ntx));

% Add noise
noise_type = 'normal';     % choose uniform or normal
if strcmp(noise_type, 'uniform')
    noise = (2*rand(Nrx, transmitLength) - 1) * sqrt(2) * nse;
else
    noise = 1/sqrt(2)*(normrnd(0, nse, Nrx, transmitLength) + 1j*normrnd(0, nse, Nrx, transmitLength));
end
rx = H*transmitted + noise;

% Decode the received signal
if(strcmp(decoder_type, 'LMS'))
    [decodedData, errors] = LMS_decode(Ntx, Nrx, rx, training, mu);
    
elseif (strcmp(decoder_type, 'sphere'))
     % add other decoders here
     
end

figure;
stem(sum(abs(decodedData-transmitted)/2,1));

% figure;
% plot(errors);

end