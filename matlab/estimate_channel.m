function H_estimate = estimate_channel(trainIn, trainOut)

H_estimate = 1/4*trainOut*trainIn';

end