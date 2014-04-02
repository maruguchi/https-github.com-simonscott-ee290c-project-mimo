function W = mmseWeights(H_est, Nrx, SNR)

kernel = H_est'*H_est + 1/10^(SNR/10)*eye(Nrx);
W = kernel\H_est';

end