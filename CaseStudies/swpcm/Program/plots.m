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
function[] = plots(time,Temp,eWat,ePCM)
    %Temperature plot
    set(0,'defaultlinelinewidth',1) ;
    fig = figure('visible', 'off');

    subplot(2,2,1)
    plot(time,Temp(:,1),time,Temp(:,2), '--')
    xlabel('Time')
    ylabel('Temperature')
    legend('Water', 'PCM','Location','NorthWest')
    title('Temperature vs. time');

    %Energy plot
    subplot(2,2,2)
    plot(time,eWat,time,ePCM,'--')
    xlabel('Time')
    ylabel('Energy')
    %legend('Water Energy', 'PCM Energy', 'Location', 'SouthEast')
    title('Energy vs. time');

    saveas(fig, 'NewTemp', 'pdf');
    print(fig, '-dpng', 'NewTemp.png');