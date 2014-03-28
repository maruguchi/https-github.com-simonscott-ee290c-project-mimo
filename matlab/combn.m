function M = combn(symbols, N)

    % create a list of all possible combinations of N elements
    X = 1:numel(symbols);
    [Y{N:-1:1}] = ndgrid(X);

    % concatenate into one matrix, reshape into 2D and flip columns
    IND = reshape(cat(N+1,Y{:}),[],N);

    M = symbols(IND);

end