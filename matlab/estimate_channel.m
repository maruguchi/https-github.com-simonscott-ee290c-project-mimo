function H_estimate = estimate_channel(trainIn, trainOut)

if (size(trainIn,1) == 2)
    col1 = 1/2*sum(trainOut,2);
    col2 = 1/2*(trainOut(:,1) - trainOut(:,2));
    H = [col1 col2];
elseif (size(trainIn,1) == 3)
    col1 = 1/4*sum(trainOut,2);
    col2 = 1/4*(sum(trainOut(:,1:2),2) - sum(trainOut(:,3:4),2));
    col3 = 1/4*(sum(trainOut(:,[1 3]),2) - sum(trainOut(:,[2 4]),2));
    H = [col1 col2 col3];
elseif (size(trainIn,1) == 4)
    col1 = 1/4*sum(trainOut,2);
    col2 = 1/4*(sum(trainOut(:,1:2),2) - sum(trainOut(:,3:4),2));
    col3 = 1/4*(sum(trainOut(:,[1 3]),2) - sum(trainOut(:,[2 4]),2));
    col4 = 1/4*(sum(trainOut(:,[1 4]),2) - sum(trainOut(:,2:3),2));
    H = [col1 col2 col3 col4];
end

H_estimate = H;

end