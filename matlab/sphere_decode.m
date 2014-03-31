function [r, err] = sphere_decode(y, H)
%
%  SPHDEC: sphere decoder for linear STBC
%     
%  R = SPHDEC(H, Y, SYMBSET, RADIUS)
%
%  H:        equivalent channel matrix
%  Y:        received signal vector
%  SYMBSET:  constellation
%  RADIUS:   search radius
%  
%  author:  Xiaoyong Guo
%  website: http://www.wordeazy.com
%
%  Goto to my website to download the C-MEX version 
%  of this program. Please Send BUG report to
%              
%           guo.xiaoyong@gmail.com
%
%  COPYRIGHT (c) 2006-2009,  Xiaoyong Guo
%

% Search radius parameter
radius = 2;

symbset = 1/sqrt(2) * [1 + 1j, 1 - 1j, -1 + 1j, -1 - 1j];

if size(H, 1) < size(H, 2)
	H = [H; zeros(size(H, 2) - size(H, 1), size(H, 2))];
end

[Q, R] = qr(H, 0);
z = Q'*y;
n = size(H,2);

% add examine this variable before make it global
global SPHDEC_RADIUS;
global RETVAL;
global TMPVAL;
global SYMBSETSIZE;
global SEARCHFLAG;

transmitLength = size(y, 2);
r = zeros(n, transmitLength);

% Do the decoding at each instant of time
for t = 1:1:transmitLength

    RETVAL        = zeros(n, 1);
    TMPVAL        = zeros(n, 1);
    SYMBSETSIZE   = length(symbset(:));
    SEARCHFLAG    = 0;
    SPHDEC_RADIUS = radius;

    sphdec_core(z(:,t), R, symbset, n, 0);

    if SEARCHFLAG > 0
        r(:,t) = RETVAL;
    else
        r(:,t) = 0;
    end
    
end


err = 0;

clear SPHDEC_RADIUS RETVAL SYMBSETSIZE SEARCHFLAG;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function sphdec_core(z, R, symbset, layer, dist)

global SPHDEC_RADIUS;
global RETVAL;
global TMPVAL;
global SYMBSETSIZE;
global SEARCHFLAG;

if (layer == 1)
    for ii = 1:SYMBSETSIZE
        TMPVAL(1) = symbset(ii);
        d = abs(z(1) - R(1,:)*TMPVAL)^2 + dist;
        if (d <= SPHDEC_RADIUS)
            RETVAL        =  TMPVAL;
            SPHDEC_RADIUS =  d;
            SEARCHFLAG    =  SEARCHFLAG + 1;
        end
    end
else
    for ii = 1:SYMBSETSIZE
        TMPVAL(layer) = symbset(ii);
        d = abs(z(layer) - R(layer,[layer:end])*TMPVAL(layer:end))^2 + dist;
        if (d <= SPHDEC_RADIUS)
            sphdec_core(z, R, symbset, layer-1, d);
        end
    end
end



