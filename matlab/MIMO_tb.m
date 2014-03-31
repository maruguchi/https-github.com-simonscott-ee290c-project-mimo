mu = 0.002;
decoder_type = 'sphere';
dat = MIMO_sim(mu, decoder_type);

global H;
cond(H)