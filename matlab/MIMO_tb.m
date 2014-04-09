channel_model = 'E';
decoder_type = 'direct';
plotEnable = 0;
numAvg = 10;
mu_LMS = 0.01;
mu_LMS_seeded = 1e-3;

if(strcmp(decoder_type, 'all'))
    numDecoders = 5;
else
    numDecoders = 1;
end

SNR_dB = linspace(0,30,11);
% SNR_dB = 20;

SER = zeros(numAvg,length(SNR_dB),numDecoders);

for index = 1:length(SNR_dB)
    tic
    for avg = 1:numAvg
        [dat, SER(avg,index, :)] = MIMO_sim(mu_LMS, mu_LMS_seeded, SNR_dB(index), decoder_type, plotEnable, channel_model);
    end
    toc
end

SER_averaged = squeeze(mean(SER,1));

SER_averaged((SER_averaged == 0)) = 1e-6;

figure;
semilogy(SNR_dB, SER_averaged);
legend('LMS','LMS seeded','ML','sphere', 'direct');

global H;
cond(H);

% Unseeded LMS should use mu ~ 0.01
% Seeded LMS should use mu <= 10^-3