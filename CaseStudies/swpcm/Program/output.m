%% Output Format Module
% This module takes the parameter and output values produced and writes 
% them to a file
%
% Authors: Thulasi Jegatheesan, Spencer Smith and Ned Nedialkov
%
% Date Last Revised: May 20, 2015
%
% Uses: params (Input Parameters Module)
%
% State Variables: none
%
% Environment Variable: the file associated with filename
%
function[] = output(file, time, Temp, eWat, ePCM, Etot, params)
    
    s       = struct(params);
    fileID  = fopen(file, 'w');
    fields  = fieldnames(s);
    
    fprintf(fileID, 'Date: %s\n\n', date);
    for i = 1:numel(fields)
        parameter = s.(fields{i});
        paramName = fields{i};
        fprintf(fileID, '%15s\t %e\n', paramName, parameter);
    end

    fprintf(fileID, '\n');
    fprintf(fileID, '%s\t\t\t %s\t\t\t %s\t\t\t %s\t\t %s\t\t %s\n','TIME', 'TWATER', 'TPCM', 'ENERGY-PCM', 'ENERGY-WATER', 'ENERGY-TOTAL');
    table = [time Temp ePCM eWat Etot];
    fprintf(fileID, '%f\t\t %f\t\t %f\t\t %f\t\t %f\t\t %f\n', transpose(table));
    
    fclose(fileID);
