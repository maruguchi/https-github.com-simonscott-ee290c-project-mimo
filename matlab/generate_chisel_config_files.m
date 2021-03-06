function [] = generate_chisel_config_files(Nrx, Ntx, SNR_db, trainIn, rx_data, symbols)

commands_file = 'C:\Users\apuglielli\Documents\MATLAB\EE290C\matlab\chisel_configs\snr_adaptive_0db\configTrainBusCmds.txt';
commands_file_id = fopen(commands_file,'wt');
data_file = 'C:\Users\apuglielli\Documents\MATLAB\EE290C\matlab\chisel_configs\snr_adaptive_0db\receiveData.txt';
data_file_id = fopen(data_file,'wt');
symbols_file = 'C:\Users\apuglielli\Documents\MATLAB\EE290C\matlab\chisel_configs\snr_adaptive_0db\decodedData.txt';
symbols_file_id = fopen(symbols_file,'wt');

SNR_lin = 10^(SNR_db/10);

train_row = [];
commands_data_payload = [];
for i = 1:size(trainIn,1)
    for j = 1:size(trainIn,2)
        train_row = [train_row trainIn(i,j) 0];
    end
    commands_data_payload = [commands_data_payload 5+i 7+i train_row];
    train_row = [];
end

commands_header = [0 0 Ntx 1 1 Nrx 2 2 size(trainIn,2) 3 3 0];
commands_snr =  [4 4 1/SNR_lin];
commands_final = [10,5,1];
               
commands_header_format = '%i,%i,%i\n';
commands_snr_format = '%i,%i,%1.5f\n';
commands_data_format = '%i,%i';
for i = 1:Nrx
    commands_data_format = [commands_data_format ',%2.4f,%2.4f'];
end
commands_data_format = [commands_data_format '\n'];

fprintf(commands_file_id, commands_header_format, commands_header);
fprintf(commands_file_id, commands_snr_format, commands_snr);
fprintf(commands_file_id, commands_data_format, commands_data_payload);
fprintf(commands_file_id, commands_header_format, commands_final);

% generates the receive data payload by reorganizing shit
receive_data_payload = zeros(1,2*length(rx_data(:))+size(rx_data,2));
for i = 1:size(rx_data,2)
    data = rx_data(:,i);
    row_data = zeros(1,2*Nrx);
    row_data(1:2:end) = real(data);
    row_data(2:2:end) = imag(data);
    receive_data_payload(((i-1)*(1+2*Nrx)+1):(i*(2*Nrx)+i)) = [i+10 row_data];
end

% generate the receive data format
receive_data_format = '%i';
for i = 1:Nrx
    receive_data_format = [receive_data_format ',%2.4f,%2.4f'];
end
receive_data_format = [receive_data_format '\n'];

fprintf(data_file_id, receive_data_format, receive_data_payload);


symbols_payload = zeros(1,length(symbols(:)));
for i = 1:size(symbols,2)
    data = symbols(:,i).';
    data0 = 0*((real(data)>0) & (imag(data)>0));
    data1 = 1*((real(data)<0) & (imag(data)>0));
    data2 = 2*((real(data)<0) & (imag(data)<0));
    data3 = 3*((real(data)>0) & (imag(data)<0));
    data = data0 + data1 + data2 + data3;
    symbols_payload((i-1)*Nrx+1:i*(Nrx)) = data;
end

symbols_data_format = [];
for i = 1:Nrx
    symbols_data_format = [symbols_data_format '%i,'];
end
symbols_data_format = [symbols_data_format(1:end-1), '\n'];

fprintf(symbols_file_id, symbols_data_format, symbols_payload);