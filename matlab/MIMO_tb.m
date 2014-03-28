mu = 0.002;
decoder_type = 'ML';
dat = MIMO_sim(mu, decoder_type);

global H;
cond(H)