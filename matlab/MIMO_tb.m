mu = 0.02;
decoder_type = 'LMS';
dat = MIMO_sim(mu, decoder_type);

global H;
cond(H)