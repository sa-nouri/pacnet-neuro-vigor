clear;
clc;
close all
%% Load data
dat = ImportMatlabDataPAC("HeadPlots_BetweenPACs_PD.csv");
datBWSham = dat(dat.stimulus == "Sham",:);

dat = ImportMatlabDataPAC("HeadPlots_WithinPACs_PD.csv");
datWISham = dat(dat.stimulus == "Sham",:);

load chaninfo.mat


%% Headplots
titleStr = "Within PACs";
healthStr = "HC";
dat = datWISham;

for pacStr = unique(dat.PAC)'
    values = table2array(dat(dat.PAC==pacStr,["channel","value"]));
    [channels,indexes] = sort(values(:,1));
    values = values(indexes,2);
    if(sum(~isnan(values))~=0)
        values = zscore(values);
        figure
        chanInfo = chanlocs(channels);
        topoplot(values, chanInfo,'electrodes','labels');
        title(strcat(titleStr,"---> Health: ", healthStr,"PAC: ",string(pacStr)),'Interpreter', 'none')
        colorbar;
    end
end
