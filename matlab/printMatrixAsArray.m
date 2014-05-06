function printMatrixAsArray(m)

disp('REAL');
fprintf(1, 'Array( ');

% If vector
if(size(m, 2) == 1)
    
    for i=1:length(m)

        fprintf(1, '%4.3f', real(m(i)));
        if(i ~= length(m))
            fprintf(1, ', ');
        end
    end

% Else if array
else

    for i=1:size(m, 1)

        fprintf(1, 'Array(');

        for j=1:size(m, 2)
            fprintf(1, '%4.3f', real(m(i,j)));
            if(j ~= size(m, 2))
                fprintf(1, ', ');
            end
        end

        if(i ~= size(m, 1))
            fprintf(1, '), ');
        else
            fprintf(1, ') ');
        end
    end
end

disp(')');

disp('IMAG');
fprintf(1, 'Array( ');

% If vector
if(size(m, 2) == 1)
    
    for i=1:length(m)

        fprintf(1, '%4.3f', imag(m(i)));
        if(i ~= length(m))
            fprintf(1, ', ');
        end
    end

% Else if array
else

    for i=1:size(m, 1)

        fprintf(1, 'Array(');

        for j=1:size(m, 2)
            fprintf(1, '%4.3f', imag(m(i,j)));
            if(j ~= size(m, 2))
                fprintf(1, ', ');
            end
        end

        if(i ~= size(m, 1))
            fprintf(1, '), ');
        else
            fprintf(1, ') ');
        end
    end
end

disp(')');