clear
clc

dat = readtable("ForMatlabNormalization_2023.csv");
dat.Stim = categorical(dat.Stim);

variableIndexes = 7:86;
for sid = unique(dat.SID)'
    for med = unique(dat.Medication)'
        for stim = unique(dat.Stim)'
            for ch = unique(dat.Channel)'
                temp = table2array(dat(dat.SID==sid & dat.Medication == med & dat.Stim==stim & dat.Channel==ch,variableIndexes));
                temp = (temp-repmat(min(temp),size(temp,1),1))./(repmat(max(temp),size(temp,1),1)-repmat(min(temp),size(temp,1),1));
                dat(dat.SID==sid & dat.Medication == med & dat.Stim==stim & dat.Channel==ch,variableIndexes) = array2table(temp);
            end
        end
    end
end

writetable(dat,"MMnormalizedFeats.csv");
%%
clear
clc

dat = readtable("ForMatlabNormalization_2023.csv");
dat.Stim = categorical(dat.Stim);
dat.Vigor0 = dat.Vigor;
variableIndexes = 7:86;
for sid = unique(dat.SID)'
    for med = unique(dat.Medication)'
        for stim = unique(dat.Stim)'
            for ch = unique(dat.Channel)'
                temp = table2array(dat(dat.SID==sid & dat.Medication == med & dat.Stim==stim & dat.Channel==ch,variableIndexes));
                temp = (temp-repmat(mean(temp),size(temp,1),1))./(repmat(std(temp),size(temp,1),1));
                dat(dat.SID==sid & dat.Medication == med & dat.Stim==stim & dat.Channel==ch,variableIndexes) = array2table(temp);
            end
        end
    end
end

writetable(dat,"zScoreNormalizedFeats.csv");