function [trainIn, trainOut] = train(H, Nrx, SNR)

trainLength = size(H,2);

nse = 10^(-SNR/20);
noise_type = 'normal';     % choose uniform or normal
if strcmp(noise_type, 'uniform')
    noise = (2*rand(Nrx, trainLength) - 1) * sqrt(2) * nse;
else
    noise = 1/sqrt(2)*(normrnd(0, nse, Nrx, trainLength) + 1j*normrnd(0, nse, Nrx, trainLength));
end

noise_extra = 1/sqrt(2)*(normrnd(0, nse, Nrx, 1) + 1j*normrnd(0, nse, Nrx, 1));
  
if (trainLength == 2)
    trainIn = [1 1;1 -1];
elseif (trainLength == 3)
    trainIn = [1 1 1 1;
               1 1 -1 -1;
               1 -1 1 -1];
    noise = [noise noise_extra];
elseif (trainLength == 4)
    trainIn = [1 1 1 1;
               1 1 -1 -1;
               1 -1 1 -1;
               1 -1 -1 1];
end
       
trainOut = H*trainIn + noise;

end