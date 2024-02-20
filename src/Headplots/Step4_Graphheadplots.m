clear;
clc;
close all
%% Load data
load DataForHeadPlots.mat
datWISham = DataForHeadPlots;
datWISham.value = datWISham.RSAOutput;
load chaninfo.mat

%% Headplots Normalize each side(positive/negative) within itself
titleStr = "Within PACs";
datHC = datWISham(datWISham.Stim == "Sham" & datWISham.Health == "HC" & datWISham.Medication == "Off",:);
datPD = datWISham(datWISham.Stim == "Sham" & datWISham.Health == "PD" & datWISham.Medication == "Off",:);

for pacStr = unique(datHC.PAC)'
    f = figure;
    set(f,'color','w');

    values_HC = table2array(datHC(datHC.PAC==pacStr,["Channel","value","Side"]));
    [channelsHC,indexesHC] = sort(values_HC(:,1));
    Side_HC = values_HC(indexesHC,3);
    values_HC = values_HC(indexesHC,2);

    values_HC = abs(values_HC).*Side_HC;
    v = values_HC;
    v(v<0) = v(v<0)/-min(v(v<0));
    v(v>0) = v(v>0)/max(v(v>0));
    values_HC = v;

    % values_HC = abs(values_HC).*Side_HC;

    chanInfo = chanlocs(channelsHC);
    topoplot(values_HC, chanInfo,'electrodes','labels','style','map','conv','on','whitebk','on');
    title(strcat("HC: RSA ",string(pacStr)),'Interpreter', 'none')
    % colorbar;
    clim([-1.1,1.1])

    f = figure;
    set(f,'color','w');

    values_PD = table2array(datPD(datPD.PAC==pacStr,["Channel","value","Side"]));
    [channelsPD,indexesPD] = sort(values_PD(:,1));
    Side_PD = values_PD(indexesPD,3);
    values_PD = values_PD(indexesPD,2);

    values_PD = abs(values_PD).*Side_PD;
    v = values_PD;
    v(v<0) = v(v<0)/-min(v(v<0));
    v(v>0) = v(v>0)/max(v(v>0));
    values_PD = v;

    % values_PD = abs(values_PD).*Side_PD;

    chanInfo = chanlocs(channelsPD);
    topoplot(values_PD, chanInfo,'electrodes','labels','style','map','conv','on','whitebk','on');
    title(strcat("PD: RSA ",string(pacStr)),'Interpreter', 'none')
    % colorbar;
    clim([-1.1,1.1])
end
