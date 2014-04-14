function [decodedData, SER] = MIMO_sim(mu_LMS, mu_LMS_seeded, SNR_dB, decoder_type, plotEnable, channel_model)

% Transmit parameters
Ntx = 2;
trainLength = 1000;
dataLength = 1000;
transmitLength = trainLength + dataLength;

% Receive parameters
Nrx = 2;
% SNR = 20; %dB
nse = 10^(-SNR_dB/20);

% Channel time-varying parameters
time_varying = 1;
doppler_f = 3;

% The data that is transmitted
training = 1/sqrt(2)*(randi(2,Ntx,trainLength)*2-3 + 1j*(randi(2,Ntx,trainLength)*2-3));
data = 1/sqrt(2)*(randi(2,Ntx,dataLength)*2-3 + 1j*(randi(2,Ntx,dataLength)*2-3));
transmitted = [training data];

% H is random channel matrix
% Each element is a random number, with normal distribution with
% mu=0 std_dev=1/sqrt(2), for both real and imag parts
% Modeled version of H uses 802.11n channel model B
global H;
channel_type = 'modeled';
if strcmp(channel_type, 'random')
    H = 1/sqrt(2)*(randn(Nrx,Ntx) + 1j*randn(Nrx,Ntx));
elseif strcmp(channel_type, 'modeled')
    [H, H_dynamic] = get_channel(Nrx, Ntx, doppler_f, channel_model);
end

% Add noise
noise_type = 'normal';     % choose uniform or normal
if strcmp(noise_type, 'uniform')
    noise = (2*rand(Nrx, transmitLength) - 1) * sqrt(2) * nse;
else
    noise = 1/sqrt(2)*(normrnd(0, nse, Nrx, transmitLength) + 1j*normrnd(0, nse, Nrx, transmitLength));
end

if (strcmp(channel_type, 'modeled') && (time_varying == 1))
    rx = (step(H_dynamic,transmitted.')).' + noise;
else
    rx = H*transmitted + noise;
end

[trainIn, trainOut] = train(H, Nrx, SNR_dB);
H_estimate = estimate_channel(trainIn, trainOut);
W_estimate = mmseWeights(H_estimate, Nrx, SNR_dB);

if(strcmp(decoder_type, 'all'))
    numDecoders = 5;
    decodedData = zeros(Ntx, transmitLength, numDecoders);
else
    numDecoders = 1;
    decodedData = zeros(Ntx, transmitLength);
end

% Decode the received signal
if(strcmp(decoder_type, 'LMS'))
    decodedData = LMS_decode(Ntx, Nrx, rx, training, mu_LMS);
    
elseif(strcmp(decoder_type, 'LMS_seeded'))
    decodedData = LMS_decode_seeded(Ntx, Nrx, rx, mu_LMS_seeded, W_estimate);

elseif (strcmp(decoder_type, 'ML'))
    decodedData = ML_decode(Ntx, Nrx, rx, H_estimate);
    
elseif (strcmp(decoder_type, 'sphere'))
    decodedData = sphere_decode(rx, H_estimate);

elseif (strcmp(decoder_type, 'direct'))
    decodedData = direct_inverse(Ntx, Nrx, rx, H_estimate, SNR_dB);
    
elseif (strcmp(decoder_type, 'all'))
    [decodedData(:,:,1)] = LMS_decode(Ntx, Nrx, rx, training, mu_LMS);
    [decodedData(:,:,2)] = LMS_decode_seeded(Ntx, Nrx, rx, mu_LMS_seeded, W_estimate);
    [decodedData(:,:,3)] = ML_decode(Ntx, Nrx, rx, H_estimate);
    [decodedData(:,:,4)] = sphere_decode(rx, H_estimate);
    [decodedData(:,:,5)] = direct_inverse(Ntx, Nrx, rx, H_estimate, SNR_dB);
        
end

bitErrors = decodedData ~= repmat(transmitted, [1 1 numDecoders]);
SER = sum(sum(bitErrors,1),2)/(transmitLength*Ntx);

if(strcmp(decoder_type, 'LMS'))
    LMSdata = decodedData(:, trainLength+1:end);
    SER = sum(LMSdata(:) ~= data(:))/(dataLength*Ntx);
elseif(strcmp(decoder_type, 'all'))
    LMSdata = decodedData(:, trainLength+1:end,1);
    SER(1) = sum(LMSdata(:) ~= data(:))/(dataLength*Ntx);
end

if(plotEnable == 1)
    figure;
    stem(sum(abs(decodedData-transmitted)/2,1));
end

end