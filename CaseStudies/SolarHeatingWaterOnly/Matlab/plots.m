%% Plot Module
% This module takes the output values produced and plots them
% in a pdf file and png file
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
function[] = plots(time, Temp)
    %Temperature plot
    set(0,'defaultlinelinewidth',1) ;
    fig = figure('visible', 'off');

    plot(time, Temp)
    xlabel('Time')
    ylabel('Temperature')
    title('Temperature vs. time');

    saveas(fig, 'NewTemp', 'pdf');
    print(fig, '-dpng', 'NewTemp.png');