mu = 0.005;
decoder_type = 'LMS';
dat = MIMO_sim(mu, decoder_type);

global H;
cond(H)