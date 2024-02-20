clear;
clc
desiredFeats = [70:79]; %only PACs
channelNum = 27;
T = [];
SubBand_str = "BroadBand";
subbandIdx = 1;

load FeatLabels.mat
CH_Lab = CH_Lab(desiredFeats);
featLabels = [];
for chIdx = 1:channelNum
    for featIdx = 1:length(CH_Lab)
        featLabels = cat(1,featLabels,"Channel:"+chIdx+"  "+CH_Lab{featIdx});
    end
end

desiredFeats = desiredFeats+7;
rng(11*18*2023);
%% PD ZScore
load TestOutIDs_PD.mat
dat = readtable("zScoreNormalizedFeats.csv");
dat.Stim = categorical(dat.Stim);

Health_str = "PD";
Normalization_str = "ZScore";

Stim_strS = ["Sham"];%,"GVS7","GVS8"];
MedS = [0];%,1];

Health = [];
Stim  = [];
Medication = [];
Run = [];
FeatIdx = [];
% Channel = [];
MAE = [];
CorVal = [];
Normalization = [];
Subband = [];
for stimIdx = 1:length(Stim_strS)
    for medIdx = 1:length(MedS)
        normalizedFeats = dat;
        
        Stim_str = Stim_strS(stimIdx);
        Med = MedS(medIdx);
        disp(Health_str+" -- "+Stim_str+" -- Med: "+Med+" -- "+Normalization_str)

        normalizedFeats = normalizedFeats(normalizedFeats.Stim==Stim_str &...
                                          normalizedFeats.Health==Health_str &...
                                          normalizedFeats.Medication==Med,:);
        [CorrLasso,ACCs_Lasso,~,FeatIdx2] = LassoRegression(normalizedFeats,testOutIDs,desiredFeats);
%         FeatIdx2 = squeeze(sum(reshape(sign(abs(FeatIdx2)),size(testOutIDs,1),[],channelNum),2));
        Run = cat(1,Run,(1:size(testOutIDs,1))');
        Health = cat(1,Health,repmat(Health_str,size(testOutIDs,1),1));
        Stim  = cat(1,Stim,repmat(Stim_str,size(testOutIDs,1),1));
        Medication = cat(1,Medication,repmat(Med,size(testOutIDs,1),1));
        Normalization = cat(1,Normalization,repmat(Normalization_str,size(testOutIDs,1),1));
        MAE = cat(1,MAE,ACCs_Lasso(:,2));
        CorVal = cat(1,CorVal,CorrLasso(:,2));
%         Channel = cat(1,Channel,FeatIdx2);
        FeatIdx = cat(1,FeatIdx,FeatIdx2);
        Subband = cat(1,Subband,repmat(SubBand_str(subbandIdx),size(testOutIDs,1),1));
    end
end

T_temp = table(Health,Medication,Stim,Normalization,Run,Subband,MAE,CorVal);
T1 = array2table(FeatIdx);
T1.Properties.VariableNames = featLabels;

% T1 = array2table(Channel);

T_temp = cat(2,T_temp,T1);
T = cat(1,T,T_temp);

%% HC ZScore
load TestOutIDs_HC.mat
dat = readtable("zScoreNormalizedFeats.csv");
dat.Stim = categorical(dat.Stim);

Health_str = "HC";
Normalization_str = "ZScore";

Stim_strS = ["Sham"];%,"GVS7","GVS8"];
MedS = 0;

Health = [];
Stim  = [];
Medication = [];
Run = [];
FeatIdx = [];
% Channel = [];
MAE = [];
CorVal = [];
Normalization = [];
Subband = [];
for stimIdx = 1:length(Stim_strS)
    for medIdx = 1:length(MedS)
        normalizedFeats = dat;
        
        Stim_str = Stim_strS(stimIdx);
        Med = MedS(medIdx);
        disp(Health_str+" -- "+Stim_str+" -- Med: "+Med+" -- "+Normalization_str)

        normalizedFeats = normalizedFeats(normalizedFeats.Stim==Stim_str &...
                                          normalizedFeats.Health==Health_str &...
                                          normalizedFeats.Medication==Med,:);
        [CorrLasso,ACCs_Lasso,~,FeatIdx2] = LassoRegression(normalizedFeats,testOutIDs,desiredFeats);
%         FeatIdx2 = squeeze(sum(reshape(sign(abs(FeatIdx2)),size(testOutIDs,1),[],channelNum),2));
        Run = cat(1,Run,(1:size(testOutIDs,1))');
        Health = cat(1,Health,repmat(Health_str,size(testOutIDs,1),1));
        Stim  = cat(1,Stim,repmat(Stim_str,size(testOutIDs,1),1));
        Medication = cat(1,Medication,repmat(Med,size(testOutIDs,1),1));
        Normalization = cat(1,Normalization,repmat(Normalization_str,size(testOutIDs,1),1));
        MAE = cat(1,MAE,ACCs_Lasso(:,2));
        CorVal = cat(1,CorVal,CorrLasso(:,2));
%         Channel = cat(1,Channel,FeatIdx2);
        FeatIdx = cat(1,FeatIdx,FeatIdx2);
        Subband = cat(1,Subband,repmat(SubBand_str(subbandIdx),size(testOutIDs,1),1));
    end
end
T_temp = table(Health,Medication,Stim,Normalization,Run,Subband,MAE,CorVal);
T1 = array2table(FeatIdx);
T1.Properties.VariableNames = featLabels;

% T1 = array2table(Channel);
T_temp = cat(2,T_temp,T1);
T = cat(1,T,T_temp);

writetable(T,"LassoRegResults_OffSham_OnlyPACS.csv")

