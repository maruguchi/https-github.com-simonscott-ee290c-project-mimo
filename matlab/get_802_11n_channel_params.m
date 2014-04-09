function [tau, pdb, AS_Tx, AoD, AS_Rx, AoA] = get_802_11n_channel_params(model)

if (strcmp(model, 'A'))
    
    tau = 0;
    pdb = 0;
    AoA = 45;
    AS_Rx = 40;
    AoD = 45;
    AS_Tx = 40;
    
elseif (strcmp(model, 'B'))
    
    tau = [0 10 20 30 40 50 60 70 80] * 1e-9; % Path delays, in seconds
    
    % Average path gains of cluster, in dB
    pdb1 = [0 -5.4 -10.8 -16.2 -21.7 -inf -inf -inf -inf];      
    pdb2 = [-inf -inf -3.2 -6.3 -9.4 -12.5 -15.6 -18.7 -21.8];
    %   Angular spreads
    AS_Tx_C1 = [14.4 14.4 14.4 14.4 14.4 -inf -inf -inf -inf];        
    AS_Tx_C2 = [-inf -inf 25.4 25.4 25.4 25.4 25.4 25.4 25.4];        
    %   Mean angles of departure
    AoD_C1 = [225.1 225.1 225.1 225.1 225.1 -inf -inf -inf -inf];     
    AoD_C2 = [-inf -inf 106.5 106.5 106.5 106.5 106.5 106.5 106.5];   

    % Spatial parameters on receiver side:
    %   Angular spreads
    AS_Rx_C1 = [14.4 14.4 14.4 14.4 14.4 -inf -inf -inf -inf];        
    AS_Rx_C2 = [-inf -inf 25.2 25.2 25.2 25.2 25.2 25.2 25.2];        
    %   Mean angles of arrival
    AoA_C1 = [4.3 4.3 4.3 4.3 4.3 -inf -inf -inf -inf];               
    AoA_C2 = [-inf -inf 118.4 118.4 118.4 118.4 118.4 118.4 118.4];
    
    pdb = [pdb1;pdb2];
    AS_Tx = [AS_Tx_C1; AS_Tx_C2];
    AoD = [AoD_C1; AoD_C2];
    AS_Rx = [AS_Rx_C1; AS_Rx_C2];
    AoA = [AoA_C1; AoA_C2];
    
elseif (strcmp(model, 'C'))
    
    tau = [0 10 20 30 40 50 60 70 80 90 110 140 170 200] * 1e-9;
    
    pdb1 = [0 -2.1 -4.3 -6.5 -8.6 -10.8 -13.0 -15.2 -17.3 -19.5];
    AoA1 = 290.3*ones(size(pdb1));
    AS_Rx1 = 24.6*ones(size(pdb1));
    AoD1 = 13.5*ones(size(pdb1));
    AS_Tx1 = 24.7*ones(size(pdb1));
    
    pdb2 = [-5.0 -7.2 -9.3 -11.5 -13.7 -15.8 -18.0 -20.2];
    AoA2 = 332.3*ones(size(pdb2));
    AS_Rx2 = 22.4*ones(size(pdb2));
    AoD2 = 56.4*ones(size(pdb2));
    AS_Tx2 = 22.5*ones(size(pdb2));
    
    pdb1 = [pdb1 -Inf*ones(1,4)];
    AoA1 = [AoA1 -Inf*ones(1,4)];
    AS_Rx1 = [AS_Rx1 -Inf*ones(1,4)];
    AoD1 = [AoD1 -Inf*ones(1,4)];
    AS_Tx1 = [AS_Tx1 -Inf*ones(1,4)];
    
    pdb2 = [-Inf*ones(1,6) pdb2];
    AoA2 = [-Inf*ones(1,6) AoA2];
    AS_Rx2 = [-Inf*ones(1,6) AS_Rx2];
    AoD2 = [-Inf*ones(1,6) AoD2];
    AS_Tx2 = [-Inf*ones(1,6) AS_Tx2];
    
    pdb = [pdb1; pdb2];
    AoA = [AoA1; AoA2];
    AS_Rx = [AS_Rx1; AS_Rx2];
    AoD = [AoD1; AoD2];
    AS_Tx = [AS_Tx1; AS_Tx2];
    
elseif (strcmp(model, 'D'))
        
    tau = [0 10 20 30 40 50 60 70 80 90 110 140 170 200 240 290 340 390] * 1e-9;
    
    pdb1 = [0 -0.9 -1.7 -2.6 -3.5 -4.3 -5.2 -6.1 -6.9 -7.8 -9.0 -11.1 -13.7 -16.3 -19.3 -23.2];
    pdb2 = [-6.6 -9.5 -12.1 -14.7 -17.4 -21.9 -25.5];
    pdb3 = [-18.8 -23.2 -25.2 -26.7]; % path losses vector
    
    ASt1 = 27.4*ones(size(pdb1));
    ASt2 = 32.1*ones(size(pdb2));
    ASt3 = 36.8*ones(size(pdb3));
    
    ASt1 = [ASt1 -inf -inf];
    ASt2 = [-inf*ones(1,10) ASt2 -inf];
    ASt3 = [-inf*ones(1,14) ASt3];
    AS_Tx = [ASt1; ASt2; ASt3]; % Tx angular spread vector
    
    ASr1 = 27.7*ones(size(pdb1));
    ASr2 = 31.4*ones(size(pdb2));
    ASr3 = 37.4*ones(size(pdb3));
    
    ASr1 = [ASr1 -inf -inf];
    ASr2 = [-inf*ones(1,10) ASr2 -inf];
    ASr3 = [-inf*ones(1,14) ASr3];
    AS_Rx = [ASr1; ASr2; ASr3]; % Rx angular spread vector
    
    AoD1 = 332.1*ones(size(pdb1));
    AoD2 = 49.3*ones(size(pdb2));
    AoD3 = 275.9*ones(size(pdb3));
    
    AoD1 = [AoD1 -inf -inf];
    AoD2 = [-inf*ones(1,10) AoD2 -inf];
    AoD3 = [-inf*ones(1,14) AoD3];
    AoD = [AoD1; AoD2; AoD3]; % Tx angles of departure
    
    AoA1 = 158.9*ones(size(pdb1));
    AoA2 = 320.2*ones(size(pdb2));
    AoA3 = 276.1*ones(size(pdb3));
    
    AoA1 = [AoA1 -inf -inf];
    AoA2 = [-inf*ones(1,10) AoA2 -inf];
    AoA3 = [-inf*ones(1,14) AoA3];
    AoA = [AoA1; AoA2; AoA3]; % Rx angles of arrival
    
    pdb1 = [pdb1 -inf -inf];
    pdb2 = [-inf*ones(1,10) pdb2 -inf];
    pdb3 = [-inf*ones(1,14) pdb3];
    pdb = [pdb1;pdb2;pdb3]; % path loss vector
    
elseif (strcmp(model, 'E'))
    
    tau = [0 10 20 30 50 80 110 140 180 230 280 330 380 430 490 560 640 730] * 1e-9;
    
    pdb1 = [-2.6 -3.0 -3.5 -3.9 -4.5 -5.6 -6.9 -8.2 -9.8 -11.7 -13.9 -16.1 -18.3 -20. -22.9];
    AoA1 = 163.7*ones(size(pdb1));
    AS_Rx1 = 35.8*ones(size(pdb1));
    AoD1 = 105.6*ones(size(pdb1));
    AS_Tx1 = 36.1*ones(size(pdb1));
    
    pdb2 = [-1.8 -3.2 -4.5 -5.8 -7.1 -9.9 -10.3 -14.3 -14.7 -18.7 -19.9 -22.4];
    AoA2 = 251.8*ones(size(pdb2));
    AS_Rx2 = 41.6*ones(size(pdb2));
    AoD2 = 293.1*ones(size(pdb2));
    AS_Tx2 = 42.5*ones(size(pdb2));
    
    pdb3 = [-7.9 -9.6 -14.2 -13.8 -18.6 -18.1 -22.8];
    AoA3 = 80.0*ones(size(pdb3));
    AS_Rx3 = 37.4*ones(size(pdb3));
    AoD3 = 61.9*ones(size(pdb3));
    AS_Tx3 = 38.0*ones(size(pdb3));
    
    pdb4 = [-20.6 -20.5 -20.7 -24.6];
    AoA4 = 182.0*ones(size(pdb4));
    AS_Rx4 = 40.3*ones(size(pdb4));
    AoD4 = 275.7*ones(size(pdb4));
    AS_Tx4 = 38.7*ones(size(pdb4));
    
    pdb1 = [pdb1 -Inf*ones(1,3)];
    AoA1 = [AoA1 -Inf*ones(1,3)];
    AS_Rx1 = [AS_Rx1 -Inf*ones(1,3)];
    AoD1 = [AoD1 -Inf*ones(1,3)];
    AS_Tx1 = [AS_Tx1 -Inf*ones(1,3)];
    
    pdb2 = [-Inf*ones(1,4) pdb2 -Inf*ones(1,2)];
    AoA2 = [-Inf*ones(1,4) AoA2 -Inf*ones(1,2)];
    AS_Rx2 = [-Inf*ones(1,4) AS_Rx2 -Inf*ones(1,2)];
    AoD2 = [-Inf*ones(1,4) AoD2 -Inf*ones(1,2)];
    AS_Tx2 = [-Inf*ones(1,4) AS_Tx2 -Inf*ones(1,2)];
    
    pdb3 = [-Inf*ones(1,8) pdb3 -Inf*ones(1,3)];
    AoA3 = [-Inf*ones(1,8) AoA3 -Inf*ones(1,3)];
    AS_Rx3 = [-Inf*ones(1,8) AS_Rx3 -Inf*ones(1,3)];
    AoD3 = [-Inf*ones(1,8) AoD3 -Inf*ones(1,3)];
    AS_Tx3 = [-Inf*ones(1,8) AS_Tx3 -Inf*ones(1,3)];
    
    pdb4 = [-Inf*ones(1,14) pdb4];
    AoA4 = [-Inf*ones(1,14) AoA4];
    AS_Rx4 = [-Inf*ones(1,14) AS_Rx4];
    AoD4 = [-Inf*ones(1,14) AoD4];
    AS_Tx4 = [-Inf*ones(1,14) AS_Tx4];
    
    pdb = [pdb1; pdb2; pdb3; pdb4];
    AoA = [AoA1; AoA2; AoA3; AoA4];
    AS_Rx = [AS_Rx1; AS_Rx2; AS_Rx3; AS_Rx4];
    AoD = [AoD1; AoD2; AoD3; AoD4];
    AS_Tx = [AS_Tx1; AS_Tx2; AS_Tx3; AS_Tx4];
    
elseif (strcmp(model, 'F'))
    
end