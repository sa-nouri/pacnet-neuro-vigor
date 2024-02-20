Labind = 0;
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Delta1";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Delta2";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Theta1";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Theta2";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Alpha1";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Alpha2";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Sigma1";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Sigma2";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Beta1";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Beta2";
Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": RSP_Gamma";

%----> Fc    computation
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_FC_Delta";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_FC_Theta";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_FC_Alpha";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_FC_Sigma";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_FC_Beta";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_FC_Gamma";

%----> Fsigma    computation
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_Fsigma_Delta";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_Fsigma_Theta";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_Fsigma_Alpha";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_Fsigma_Sigma";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_Fsigma_Beta";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_Fsigma_Gamma";

%----> S(fc) computation
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_S(fc)_Delta";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_S(fc)_Theta";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_S(fc)_Alpha";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_S(fc)_Sigma";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_S(fc)_Beta";
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": HP_S(fc)_Gamma";

%----> DSI    computation
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": SWI_DSI";
%----> TSI    computation
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": SWI_TSI";
%----> ASI    computation
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": SWI_ASI";
%% Hjorth features
%----> Hjorth_Activity
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": Hjorth_Activity";
%----> Mobility
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": Hjorth_Mobility";
%----> Complexity
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": Hjorth_Complexity";


%----> Skewness
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": Skewness";
%----> Kurtosis
Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": Kurtosis";

%% Bispectrum Time Series
desiredFreqs = ["Delta","Delta","Theta","Theta","Alpha","Alpha","Sigma","Sigma","Beta","Beta","Gamma","Gamma"];
for i =1:length(desiredFreqs)
    Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": BTS_Amplitude."+desiredFreqs(i);
end
for i= 1:length(desiredFreqs)
    Labind = Labind+1; CH_Lab{Labind} =   "Feature_"+Labind+": BTS_Angle."+desiredFreqs(i);
end

%% Wavelet Coefficients
for i=1:8
    Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": Wavelet_EstPower."+num2str(i);
end
CH_Lab = CH_Lab';

%% PAC Features
PACNames = ["Beta_Gamma","Alpha_Gamma","Theta_Gamma","Delta_Gamma","Alpha_Beta","Theta_Beta","Delta_Beta","Theta_Alpha","Delta_Alpha","Delta_Theta"];

for i=1:10
    Labind = Labind+1; CH_Lab{Labind} =  "Feature_"+Labind+": PAC."+PACNames(i);
end
CH_Lab = CH_Lab';
save('FeatLabels.mat','CH_Lab')