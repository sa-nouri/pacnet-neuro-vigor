% run through MainRunLassoRegression
function [CorrLasso,ACCs_Lasso,FeatIdx1,FeatIdx2] = LassoRegression(normalizedFeats,testOutIDs,desiredFeats)

testOutIDs = testOutIDs+1;

SIDs = normalizedFeats.SID;
uniqueSIDs = unique(normalizedFeats.SID);
channels = normalizedFeats.Channel;
trials = normalizedFeats.Trial;
runs = size(testOutIDs,1);

CorrLasso = [];
FeatIdx1 = [];
FeatIdx2 = [];
channelNum = 27;

Feats = table2array(normalizedFeats(:,desiredFeats));

%% Main loop lasso ALL channels

ACCs_Lasso = zeros(runs,2);

f = waitbar(0,'Lasso ALL Channel Evaluation');
for runIdx = 1:runs
    waitbar(runIdx/runs,f,sprintf('Lasso ALL Channel Run = %d',runIdx));
    testIdx = false(size(Feats,1),1);
    for sIDx = 1:length(uniqueSIDs)
        testIdx = testIdx | (SIDs==uniqueSIDs(sIDx) & trials==testOutIDs(runIdx,sIDx));
    end
    trainIdx = ~testIdx;
    
    
    trainX = [];
    testX = [];
    for chIdx = 1:channelNum
        chIdxTemp = channels==chIdx;
        trainX = cat(2,trainX,Feats(trainIdx & chIdxTemp, :));
        testX = cat(2,testX,Feats(testIdx & chIdxTemp, :));
    end
    trainY  = normalizedFeats.Vigor(trainIdx & chIdxTemp);
    testY   = normalizedFeats.Vigor(testIdx & chIdxTemp);
    
    % This one resulted in overfit models
    % lambda = 1e-03;
    % [B,FitInfo] = lasso(trainX,trainY,'Lambda',lambda,'Options',statset('UseParallel',true));
    % Y_p = testX*B+FitInfo.Intercept;
    % C1 = corrcoef(Y_p,testY);
    % C1 = C1(1,2);
    % ACCs_Lasso(runIdx,1) = mean(abs(Y_p-testY));
    % Fidx1 = B; 
    % FeatIdx1 = cat(1,FeatIdx1,Fidx1');
    C1 = 0;

    B = lasso(trainX,trainY,'CV',10,'Alpha',.75,'Options',statset('UseParallel',true));
    maxItt = 50;
    idxLambdaMinMSE=1;
    itt = 1;
    while (idxLambdaMinMSE<2 || idxLambdaMinMSE>(size(B,2)-2))  % Make sure we don't end up with an over/under fitted model
        itt = itt+1;
        if(itt>maxItt)
            idxLambdaMinMSE = 2;
            break;
        end
        [B,FitInfo] = lasso(trainX,trainY,'CV',10,'Alpha',.75,'Options',statset('UseParallel',true));
        idxLambdaMinMSE = FitInfo.IndexMinMSE;
    end


    % [B,FitInfo] = lasso(trainX,trainY,'Alpha',1,'Options',statset('UseParallel',true));
    % [B,FitInfo] = lasso(trainX,trainY,'UseCovariance',true,'Alpha',.75,'Options',statset('UseParallel',true));
    % [~,idxLambdaMinMSE] = min(FitInfo.MSE);
    Y_p = testX*B(:,idxLambdaMinMSE)+FitInfo.Intercept(idxLambdaMinMSE);
    C2 = corrcoef(Y_p,testY);
    C2 = C2(1,2);
    ACCs_Lasso(runIdx,2) = mean(abs(Y_p-testY));
    Fidx2 = B(:,idxLambdaMinMSE);
    FeatIdx2 = cat(1,FeatIdx2,Fidx2');    
    CorrLasso = cat(1,CorrLasso,[C1,C2]);
 
end
close(f)
end