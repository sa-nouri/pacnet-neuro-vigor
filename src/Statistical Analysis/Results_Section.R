library(pacman)
p_load(reshape2,
       ez,
       lme4,
       ggplot2,
       grid,
       tidyr,
       plyr,
       dplyr,
       effects,
       gridExtra,
       DescTools,
       Cairo, #alternate image writing package with superior performance.
       corrplot,
       knitr,
       PerformanceAnalytics,
       afex,
       ggpubr,
       readxl,
       # officer,
       psych,
       rstatix,
       emmeans,
       standardize,
       performance,stringr,
       scatterplot3d,
       plotrix,
       rgl,RColorBrewer,export,car)

fontSize = 16
gDefault =   theme(
  text = element_text(family = "serif"),
  strip.text = element_text(size=fontSize-2, face="bold"),
  plot.title = element_text(size = fontSize),
  axis.title = element_text(size = fontSize),
  legend.title = element_text(size=fontSize),
  legend.text=element_text(size=fontSize-4),
  axis.text = element_text(size = fontSize-4),
  # panel.grid.major.y = element_line(color = "gray",
  #                           linewidth = 0.75,
  #                           linetype = 3),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "transparent",color = "black", 
                                  linewidth = 0.5, linetype =  "solid"),
  plot.background = element_rect(fill = "transparent"),
  axis.line = element_line(linewidth = .5, linetype =  "solid")
)
###################################################### Functions ------
PostHocT_Tests <- function(data, CompList = NULL, Paired = T, PAdjustM = "bonferroni", VarEqual = T,
                           Detailed = T, PoolSD = F, Alternative = "two.sided",PrintOutput = T, roundDigit = 2){
  results <- data  %>%
    pairwise_t_test(value ~ groups, pool.sd = PoolSD, var.equal=VarEqual, paired = Paired,
                    detailed = Detailed,
                    alternative = Alternative,
                    comparisons = CompList,
                    p.adjust.method = PAdjustM) %>%
    mutate_if(is.numeric, round,digits = 3) %>%
    as.data.frame()
  
  CD <- data %>%
    cohens_d(value~groups, var.equal = VarEqual,paired = Paired,
             comparisons = CompList) %>%
    as.data.frame()
  
  marginals <- data %>%
    group_by(groups)%>%
    summarise(M = mean(value, na.rm=T),
              SD = sd(value, na.rm=T)) %>%
    mutate_if(is.numeric, round,digits = (roundDigit+1)) %>%
    as.data.frame()
  
  results = results[,c("group1", "group2", "statistic", "df", "p", "p.adj", "p.adj.signif", "conf.low", "conf.high")]
  results = results %>% mutate_at(c("statistic", "df", "conf.low", "conf.high"), round,digits = roundDigit)
  results$cohenD = round(CD$effsize,roundDigit)
  if(PrintOutput){
    print(results)
    print(marginals)
  }
  out = list(results,marginals)
}

WriteANOVAResults_APA <- function(file = "ANOVAoutput.doc", results , boldFlag = T){
  #Requires library(rtf)
  rtfFile <- RTF(file)
  results = results[results$Effect!="(Intercept)",]
  results$interaction = grepl(":",results$Effect)
  mainEffects = results$Effect[results$interaction==0]
  for (factorName in mainEffects){
    tempRes = results[results$Effect==factorName,]
    addText(rtfFile,"There was ")
    if (tempRes$p<0.05){
      addText(rtfFile,"a ")
      addText(rtfFile,"significant " , bold = boldFlag)
    }
    else{
      addText(rtfFile,"no " , bold = boldFlag)
    }
    addText(rtfFile,paste("main effect of ",factorName,", ",sep = "") )
    addText(rtfFile,"f", italic = T)
    addText(rtfFile,paste("(",tempRes$DFn,", ",tempRes$DFd,") = ", tempRes$F,", ",sep = "") )
    addText(rtfFile,"p ", italic = T)
    if (tempRes$p<0.001){
      addText(rtfFile," < 0.001, ")
    }
    else{
      addText(rtfFile,paste(" = ",tempRes$p,", ",sep = "") )
    }
    addText(rtfFile,paste("#pareta = ",tempRes$pareta,"\n\n",sep = "") )
  }
  
  
  interEffects = results$Effect[results$interaction==1]
  for (factorName in interEffects){
    tempRes = results[results$Effect==factorName,]
    facNames = gsub(":"," and ",factorName)
    addText(rtfFile,"There was ")
    if (tempRes$p<0.05){
      addText(rtfFile,"a ")
      addText(rtfFile,"significant " , bold = boldFlag)
    }
    else{
      addText(rtfFile,"no " , bold = boldFlag)
    }
    addText(rtfFile,paste("interaction effect between ", facNames,", ",sep = ""))
    addText(rtfFile,"f", italic = T)
    addText(rtfFile,paste("(",tempRes$DFn,", ",tempRes$DFd,") = ", tempRes$F,", ",sep = "") )
    addText(rtfFile,"p ", italic = T)
    if (tempRes$p<0.001){
      addText(rtfFile,"< 0.001, ")
    }
    else{
      addText(rtfFile,paste(" = ",tempRes$p,", ",sep = "") )
    }
    addText(rtfFile,paste("#pareta = ",tempRes$pareta,"\n\n",sep = "") )
  }
  done(rtfFile)
}

WriteTTestResults_APA <- function(file = "Ttestoutput.doc", results,marginals, boldFlag = T){
  #Requires library(rtf)
  rtfFile <- RTF(file)
  
  for (rIdx in 1:nrow(results)){
    d = results[rIdx,]
    mG1 = marginals[marginals$groups==d$group1,]
    mG2 = marginals[marginals$groups==d$group2,]
    
    addText(rtfFile,paste(d$group1,", (",sep = "") )
    addText(rtfFile,"M ", italic = T)
    addText(rtfFile,paste("= ",mG1$M,", ",sep = "") )
    addText(rtfFile,"SD ", italic = T)
    addText(rtfFile,paste("= ",mG1$SD,"), were ",sep = "") )
    if(d$p.adj>0.05){
      addText(rtfFile,"not significantly ", bold = boldFlag)
    }else{
      addText(rtfFile,"significantly ", bold = boldFlag)
    }
    addText(rtfFile,paste("different from ",d$group2,", (",sep = "") )
    addText(rtfFile,"M ", italic = T)
    addText(rtfFile,paste("= ",mG2$M,", ",sep = "") )
    addText(rtfFile,"SD ", italic = T)
    addText(rtfFile,paste("= ",mG2$SD,"); ",sep = "") )
    addText(rtfFile,"t", italic = T)
    addText(rtfFile,paste("(",d$df,") = ",d$statistic,", ",sep = "") )
    addText(rtfFile,"p ", italic = T)
    if (d$p<0.001){
      addText(rtfFile,"< 0.001, ")
    }
    else{
      addText(rtfFile,paste(" = ",d$p,", ",sep = "") )
    }
    
    addText(rtfFile,"Cohen's ")
    addText(rtfFile,"d ", italic = T)
    addText(rtfFile,paste(" = ",d$cohenD,", ",sep = "") )
    
    addText(rtfFile,"#padj ", italic = T)
    if (d$p.adj<0.001){
      addText(rtfFile,"< 0.001, ")
    }
    else{
      addText(rtfFile,paste(" = ",d$p.adj,", ",sep = "") )
    }
    addText(rtfFile,"95% CI ", italic = T)
    addText(rtfFile,paste("[",d$conf.low,", ",d$conf.high,"]\n\n",sep = "") )
    
  }
  done(rtfFile)
}

###################################################### Behavioral Performance Figure 3-----
d = read.table("ForMatlabNormalization_2023.csv", header=TRUE, sep=",", strip.white = TRUE)
names(d)
d = unique(d[,c("SID", "Health", "Medication", "Stim", "Trial", "Vigor")])
graphDat = d %>%
  group_by(SID,Health,Medication,Stim) %>%
  summarise(MV = mean(Vigor,na.rm=T))%>%
  as.data.frame()

graphDat$Stim = factor(graphDat$Stim, levels = c("Sham","GVS7","GVS8"), labels =  c("Sham","GVS1","GVS2"))
graphDat = graphDat[graphDat$Medication ==0,]

ggplot(graphDat, aes(x = Stim, y = MV, color = Health)) + 
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge", linewidth = 1)+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1,
                                              jitter.height = 0,
                                              dodge.width = .9),shape = 21,fill="grey",aes(color = Health))+
  scale_color_brewer(palette="Set2")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  gDefault+
  xlab("Stimulus")+
  ylab("Motor Vigor")


sDat = graphDat[graphDat$Health %in% c("HC","PD"),]
results=as.data.frame(ezANOVA(data=sDat, dv="MV", wid=.("SID"), within=.("Stim"),
                              between = c("Health"), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 3)
results

as.data.frame(summarise(group_by(sDat,Stim), M = formatC(mean(MV), format = "e", digits = 2), 
                        SD = formatC(sd(MV), format = "e", digits = 2)))

t.test(sDat$MV[sDat$Stim=="Sham"],
       sDat$MV[sDat$Stim=="GVS1"],paired = T)

t.test(sDat$MV[sDat$Stim=="Sham"],
       sDat$MV[sDat$Stim=="GVS2"],paired = T)

###################################################### Preliminary Analysis and PACNET Baseline Performance Figure 4-----
graphdat = read.csv(file="AccuracyData.csv")

names(graphdat)
graphdat = melt(graphdat, id.vars = c("Health","Run","PACFeatures"),
                variable.name = "Metric", value.name = "value")
graphdat = graphdat[graphdat$Metric == "CorVal",]
graphdat$PACFeatures = factor(graphdat$PACFeatures,levels = c("DNN","No PACs","With PACs", "Only PACs" ))

ggplot(graphdat, aes(x=Health, y=value, fill=PACFeatures)) + 
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.3,
                                              jitter.height = 0,
                                              dodge.width = .75),
              shape = 21,color = "#696969", alpha = 0.3, size = 1.5)+
  ylab("Performance (Correlation)")+
  xlab("Health")+
  gDefault

as.data.frame(summarise(group_by(graphdat,Health,PACFeatures,Metric), M = round(mean(value),2), SD = round(sd(value),2)))

graphdat %>%
  group_by(Health,PACFeatures,Metric) %>%
  summarise(value = mean(value)) %>%
  group_by(PACFeatures,Metric) %>%
  summarise(M = round(mean(value),2), SD = round(sd(value),2)) %>%
  as.data.frame()

#---------------- F test for Variance
sDat = graphdat[graphdat$Metric=="CorVal",]
sDat$value = FisherZ(sDat$value)
head(sDat)
sDat = reshape2::dcast(sDat,Run+Health~PACFeatures, value.var="value")
sDat = melt(sDat, id.vars = c("Run","Health"),
            variable.name = "PACFeatures")
sDat$value[is.na(sDat$value)]=0

sDat %>%
  group_by(Health,PACFeatures) %>%
  summarise(M = round(mean(value),2), SD = round(sd(value),2)) %>%
  as.data.frame()

sDat %>%
  group_by(PACFeatures) %>%
  summarise(M = round(mean(value),2), SD = round(sd(value),2)) %>%
  as.data.frame()

idDNN = sDat$PACFeatures == "DNN" & sDat$Health=="HC"
idNoPac = sDat$PACFeatures == "No PACs" & sDat$Health=="HC"
idWPac = sDat$PACFeatures == "With PACs" & sDat$Health=="HC"
idPac = sDat$PACFeatures == "Only PACs" & sDat$Health=="HC"
var.test(sDat$value[idDNN],sDat$value[idNoPac])
var.test(sDat$value[idDNN],sDat$value[idWPac])
var.test(sDat$value[idDNN],sDat$value[idPac])
var.test(sDat$value[idNoPac],sDat$value[idWPac])

idDNN = sDat$PACFeatures == "DNN" & sDat$Health=="PD"
idNoPac = sDat$PACFeatures == "No PACs" & sDat$Health=="PD"
idWPac = sDat$PACFeatures == "With PACs" & sDat$Health=="PD"
idPac = sDat$PACFeatures == "Only PACs" & sDat$Health=="PD"
var.test(sDat$value[idDNN],sDat$value[idNoPac])
var.test(sDat$value[idDNN],sDat$value[idWPac])
var.test(sDat$value[idDNN],sDat$value[idPac])
var.test(sDat$value[idNoPac],sDat$value[idWPac])


#---------------- Levene's test for Variance
sDat = graphdat[graphdat$Metric=="CorVal",]
sDat$value = FisherZ(sDat$value)
head(sDat)
sDat = reshape2::dcast(sDat,Run+Health~PACFeatures, value.var="value")
sDat = melt(sDat, id.vars = c("Run","Health"),
            variable.name = "PACFeatures")
sDat$value[is.na(sDat$value)]=0

leveneTest(value ~ Health*PACFeatures, sDat)

###################################################### Frequency of Features Selected by Lasso -----
d = read.table("LassoRegResults.csv", header=TRUE, sep=",", strip.white = TRUE)
d$CorVal[d$CorVal=="NaN"] = NA
d = d[complete.cases(d),]
d$CorVal = as.numeric(d$CorVal)

Sdat = melt(d[d$Normalization=="ZScore" & d$Medication==0,], id.vars = c("Health", "Medication","Stim","Normalization", "Run","Subband"),
            variable.name = "Feature")

Sdat = Sdat[!(Sdat$Feature %in% c("MAE","CorVal")),]


Sdat$value[abs(Sdat$value)<=0.01] = NA
Sdat = Sdat[complete.cases(Sdat),]
Sdat$value = sign(abs(Sdat$value))


Sdat$FType = case_when(grepl("RSP",Sdat$Feature)~"RSP",
                       grepl("HP",Sdat$Feature)~"HP",
                       grepl("BTS_Amplitude",Sdat$Feature)~"BTS_Amplitude",
                       grepl("BTS_Angle",Sdat$Feature)~"BTS_Angle",
                       grepl("PAC",Sdat$Feature)~"PAC")

Sdat$Stim = factor(Sdat$Stim, levels = c("Sham","GVS7","GVS8"))
Sdat$Channel = str_extract(Sdat$Feature,"Channel.[0-9]+")
head(Sdat)

FeatDat = as.data.frame(summarise(group_by(Sdat,Health,Stim,FType,Run),N=n(),value = sum(value,na.rm=T)))
FeatDat = as.data.frame(mutate(group_by(FeatDat,Health,Stim,Run),fNum=sum(N,na.rm = T)) )
FeatDat$value = FeatDat$value/FeatDat$fNum


ggplot(FeatDat, aes(x=Health, y=value, fill=FType)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",size=.5)+
  facet_grid(~Stim)
###################################################### Features Selected by Lasso Beta average-----
d = read.table("LassoRegResults.csv", header=TRUE, sep=",", strip.white = TRUE)
d$CorVal[d$CorVal=="NaN"] = NA
d = d[complete.cases(d),]
d$CorVal = as.numeric(d$CorVal)

Sdat = melt(d[d$Normalization=="ZScore" & d$Medication==0,], id.vars = c("Health", "Medication","Stim","Normalization", "Run","Subband"),
            variable.name = "Feature")

Sdat = Sdat[!(Sdat$Feature %in% c("MAE","CorVal")),]


Sdat$value[abs(Sdat$value)<=0.01] = NA
Sdat = Sdat[complete.cases(Sdat),]

Sdat$FType = case_when(grepl("RSP",Sdat$Feature)~"RSP",
                       grepl("HP",Sdat$Feature)~"HP",
                       grepl("BTS_Amplitude",Sdat$Feature)~"BTS_Amplitude",
                       grepl("BTS_Angle",Sdat$Feature)~"BTS_Angle",
                       grepl("PAC",Sdat$Feature)~"PAC")

Sdat$Stim = factor(Sdat$Stim, levels = c("Sham","GVS7","GVS8"))
Sdat$Channel = str_extract(Sdat$Feature,"Channel.[0-9]+")
head(Sdat)


FeatDat = as.data.frame(summarise(group_by(Sdat,Health,Stim,FType,Run),N=n(),value = mean(value,na.rm=T)))

ggplot(FeatDat, aes(x=Health, y=value, fill=FType)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",size=.5)+
  facet_grid(~Stim)

conds = FeatDat$FType=="PAC" & FeatDat$Health=="PD"
t.test(FeatDat$value[conds])

conds = FeatDat$FType=="PAC" & FeatDat$Health=="HC"
t.test(FeatDat$value[conds])


########################################## PAC Informativeness ---------------
dat = read.csv("regions_stats.csv")

unique(dat$medication)
dat$Health = dat$medication
dat$Health = factor(dat$Health, levels = c("hc","pd_off","pd_on"),
                    labels = c("HC","PD","PD"))

dat$Medication = ifelse(dat$medication=="pd_on",1,0)

dat$Stim = factor(dat$stim, levels = c("sham","stim7","stim8"),
                  labels = c("Sham","GVS7","GVS8"))
dat$Region = factor(dat$Region, 
                    levels = c("Delta_Theta", "Delta_Alpha", "Delta_Beta", "Delta_Gamma",
                               "Theta_Alpha", "Theta_Beta", "Theta_Gamma", "Alpha_Beta", 
                               "Alpha_Gamma", "Beta_Gamma","Overall"))

datOverall = dat[dat$Region=="Overall",c("Stim","Medication","Health","Region","mean","median")]
names(datOverall) = c("Stim", "Medication", "Health", "Region", "Mean_Overall", "Med_Overall")
datOverall = datOverall[,c("Stim", "Medication", "Health", "Mean_Overall", "Med_Overall")]

dat = dat[dat$Region!="Overall",c("Stim","Medication","Health","Region","mean","median")]
names(dat) = c("Stim", "Medication", "Health", "Region", "Mean", "Median")
dat = merge(dat,datOverall)


dat$Exclude = ifelse(dat$Mean<=max(dat$Med_Overall,dat$Mean_Overall),1,0)
unique(dat$Region[dat$Exclude==1])

datOverall$Med_Overall = max(datOverall$Med_Overall)
datOverall$Mean_Overall = max(datOverall$Mean_Overall)
datOverall = unique(datOverall[,c("Med_Overall","Mean_Overall")])
ggplot(dat,aes(x=Region,y=Mean)) + 
  geom_bar(stat="summary",fun="mean",position="dodge",fill="#8fa8ab")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  geom_hline(data = datOverall, aes(yintercept = Med_Overall,linetype = "Median"),color = "#921713")+
  geom_hline(data = datOverall, aes(yintercept = Mean_Overall,linetype = "Mean"),color = "#921713")+
  scale_linetype_manual(name = NULL,
                        limits = c("Median","Mean"),
                        values = c("solid","dotdash"),
                        labels = c("Overall Median","Overall Mean"))+
  gDefault+
  scale_x_discrete(guide = guide_axis(angle = -25))+
  ylab("Region Mean")


for (i in 1:length(regionNames)){
  X = dat$Mean[dat$Region == regionNames[i]]
  res = t.test(X-datOverall$Mean_Overall,alternative = c("greater"))
  res$p.value = res$p.value*10
  if(res$p.value<0.05){
    starStr  = " ****"
  }else{
    starStr  = ""
  }
  
  print(paste("Average for ",regionNames[i]," = ", mean(X),", and P-Value = ", res$p.value, starStr, sep = ""))
}


for (i in 1:length(regionNames)){
  X = dat$Median[dat$Region == regionNames[i]]
  res = t.test(X-datOverall$Med_Overall,alternative = c("greater"))
  res$p.value = res$p.value*10
  if(res$p.value<0.05){
    starStr  = " ****"
  }else{
    starStr  = ""
  }
  print(paste("Median for ",regionNames[i]," = ", mean(X),", and P-Value = ", res$p.value, starStr, sep = ""))
}



####################################################### Overall PACs Informativeness Graph 6 -----
datWithin = read.csv("RSAWithinPACs.csv")
datWithin$PAC = factor(datWithin$PAC, 
                       levels = c("Delta_Theta", "Delta_Alpha", "Delta_Beta", "Delta_Gamma",
                                  "Theta_Alpha", "Theta_Beta", "Theta_Gamma", "Alpha_Beta", "Alpha_Gamma", "Beta_Gamma"))
dat = datWithin
dat$Stim = factor(dat$Stim, levels = c("Sham","GVS7", "GVS8"), labels = c("Sham","GVS1","GVS2"))
dat = dat[!(dat$PAC %in% ExcludedPACs),]
head(dat)

sDat = dat
sDat$value = FisherZ(sDat$RSADiff) 
sDat$Test = paste(sDat$Health,sDat$Medication,sDat$Stim,sDat$Channel,sDat$PAC,sep = "_")
Result <- sDat %>%
  group_by(Test) %>%                       
  summarise(res = list(tidy(t.test(value, mu=0))), 
            RSAOutput = mean(RSAOutput, na.rm=T), 
            RSADiff = mean(RSADiff, na.rm=T),
            RSAOutput_scaled = mean(RSAOutput_scaled, na.rm=T),
            N=n()) %>%
  unnest(cols = c(res))%>%
  as.data.frame()


Result$p.value = round(Result$p.value,3)
Result$Sig = unique(0)
Result$Sig[Result$p.value<0.05] = 1
Result = as.data.frame(Result)
Result$Side = sign(Result$statistic)*Result$Sig
Result = Result[,c("Test","Sig","Side")]

dat = dat[,c("itt","Stim","Health","Medication","Channel","PAC","RSAOutput","RSADiff","RSAOutput_scaled")]
dat$Test = paste(dat$Health,dat$Medication,dat$Stim,dat$Channel,dat$PAC,sep = "_")
dat = merge(dat,Result, by = "Test")
dat = dat[,c("itt","Stim","Health","Medication","Channel","PAC","Sig","Side","RSAOutput","RSADiff","RSAOutput_scaled")]
dat = dat[dat$Side>=0,]
dat = dat %>% group_by(itt,PAC,Stim,Health,Medication) %>%
  summarise(RSAOutput = mean(RSAOutput, na.rm = T),
            RSADiff = mean(RSADiff, na.rm = T),
            RSAOutput_scaled = mean(RSAOutput_scaled, na.rm = T),
            N=n()) %>% as.data.frame()
dat$PAC = factor(dat$PAC, 
                 levels = c("Delta_Theta", "Delta_Alpha", "Delta_Beta", "Delta_Gamma",
                            "Theta_Alpha", "Theta_Beta", "Theta_Gamma", "Alpha_Beta", "Alpha_Gamma", "Beta_Gamma"))
dat = dat[!(dat$PAC %in% c("Alpha_Beta","Theta_Alpha")),]
dat = dat[dat$Medication == "Off" & dat$Stim == "Sham",]

graphdat = dat
graphdat$value = graphdat$RSAOutput


fontSize = 16
gDefault1 =   theme(
  text = element_text(family = "serif"),
  strip.text = element_text(size=fontSize-2, face="bold"),
  plot.title = element_text(size = fontSize),
  axis.title = element_text(size = fontSize),
  legend.title = element_text(size=fontSize),
  legend.text=element_text(size=fontSize-4),
  axis.text = element_text(size = fontSize-4),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "transparent",color = "black", 
                                  linewidth = 0.5, linetype =  "solid"),
  plot.background = element_rect(fill = "transparent"),
  axis.line = element_line(linewidth = .5, linetype =  "solid")
)

ggplot(graphdat,aes(x=PAC, y=value, fill = PAC)) + 
  stat_summary(fun.data = "mean_sd", geom="errorbar",
               position = position_dodge(width = .9),width = 0.5)+
  geom_point(stat="summary",fun="mean",position = position_dodge(width = .9),
             size = 2,shape = 23)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  gDefault1+
  facet_wrap(~Health)+
  scale_x_discrete(guide = guide_axis(angle = -25))+
  ylab("Average Similarity Score")

#------------Overall PACs Informativeness

sDat = dat
sDat$value = FisherZ(sDat$RSAOutput)

results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("PAC"),
                              between = c("Health"), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 3)
results

#------------ Health
phDat = as.data.frame(summarise(group_by(sDat,itt,Health),value = mean(value)))
head(phDat)
phDat$groups = paste(phDat$Health,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = F)
WriteTTestResults_APA(file = "ResultAPA1.doc", A[[1]],A[[2]])

phDat = sDat
head(phDat)
phDat$groups = paste(phDat$Health,phDat$PAC,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = F,CompList = list(c("HC_Alpha_Gamma","PD_Alpha_Gamma"),
                                                            c("HC_Delta_Beta","PD_Delta_Beta"),
                                                            c("HC_Theta_Gamma","PD_Theta_Gamma"),
                                                            c("HC_Beta_Gamma","PD_Beta_Gamma")))

A = PostHocT_Tests(data = phDat, Paired = T,CompList = list(c("HC_Delta_Beta","HC_Theta_Gamma"),
                                                            c("HC_Delta_Beta","HC_Alpha_Gamma"),
                                                            c("HC_Delta_Beta","HC_Beta_Gamma"),
                                                            c("HC_Theta_Gamma","HC_Alpha_Gamma"),
                                                            c("HC_Theta_Gamma","HC_Beta_Gamma"),
                                                            c("HC_Alpha_Gamma","HC_Beta_Gamma"),
                                                            c("PD_Delta_Beta","PD_Theta_Gamma"),
                                                            c("PD_Delta_Beta","PD_Alpha_Gamma"),
                                                            c("PD_Delta_Beta","PD_Beta_Gamma"),
                                                            c("PD_Theta_Gamma","PD_Alpha_Gamma"),
                                                            c("PD_Theta_Gamma","PD_Beta_Gamma"),
                                                            c("PD_Alpha_Gamma","PD_Beta_Gamma")))
WriteTTestResults_APA(file = "ResultAPA2.doc", A[[1]],A[[2]])

###################################################### Stimulation and Medication Effects Figure 7 ----
datWithin = read.csv("RSAWithinPACs.csv")
datWithin$PAC = factor(datWithin$PAC, 
                       levels = c("Delta_Theta", "Delta_Alpha", "Delta_Beta", "Delta_Gamma",
                                  "Theta_Alpha", "Theta_Beta", "Theta_Gamma", "Alpha_Beta", "Alpha_Gamma", "Beta_Gamma"))
dat = datWithin
dat$Stim = factor(dat$Stim, levels = c("Sham","GVS7", "GVS8"), labels = c("Sham","GVS1","GVS2"))
dat = dat[!(dat$PAC %in% ExcludedPACs),]
head(dat)

sDat = dat
sDat$value = FisherZ(sDat$RSADiff) 
sDat$Test = paste(sDat$Health,sDat$Medication,sDat$Stim,sDat$Channel,sDat$PAC,sep = "_")
Result <- sDat %>%
  group_by(Test) %>%                       
  summarise(res = list(tidy(t.test(value, mu=0))), 
            RSAOutput = mean(RSAOutput, na.rm=T), 
            RSADiff = mean(RSADiff, na.rm=T),
            RSAOutput_scaled = mean(RSAOutput_scaled, na.rm=T),
            N=n()) %>%
  unnest(cols = c(res))%>%
  as.data.frame()


Result$p.value = round(Result$p.value,3)
Result$Sig = unique(0)
Result$Sig[Result$p.value<0.05] = 1
Result = as.data.frame(Result)
Result$Side = sign(Result$statistic)*Result$Sig
Result = Result[,c("Test","Sig","Side")]

dat = dat[,c("itt","Stim","Health","Medication","Channel","PAC","RSAOutput","RSADiff","RSAOutput_scaled")]
dat$Test = paste(dat$Health,dat$Medication,dat$Stim,dat$Channel,dat$PAC,sep = "_")
dat = merge(dat,Result, by = "Test")
dat = dat[,c("itt","Stim","Health","Medication","Channel","PAC","Sig","Side","RSAOutput","RSADiff","RSAOutput_scaled")]
dat = dat[dat$Side>=0,]
dat = dat %>% group_by(itt,PAC,Stim,Health,Medication) %>%
  summarise(RSAOutput = mean(RSAOutput, na.rm = T),
            RSADiff = mean(RSADiff, na.rm = T),
            RSAOutput_scaled = mean(RSAOutput_scaled, na.rm = T),
            N=n()) %>% as.data.frame()
dat$PAC = factor(dat$PAC, 
                 levels = c("Delta_Theta", "Delta_Alpha", "Delta_Beta", "Delta_Gamma",
                            "Theta_Alpha", "Theta_Beta", "Theta_Gamma", "Alpha_Beta", "Alpha_Gamma", "Beta_Gamma"))
dat = dat[!(dat$PAC %in% c("Alpha_Beta","Theta_Alpha")),]

graphdat = dat
graphdat$value = graphdat$RSAOutput
graphdat$Medication[graphdat$Health=="HC"] = ""
graphdat$Health = paste(graphdat$Health,graphdat$Medication,sep = " ")
dat = graphdat

graphdat = dat[(dat$PAC %in% c("Delta_Beta")),]
ggplot(graphdat,aes(x=Health, y=value, fill=Stim, color = Stim)) + 
  stat_summary(fun.data = "mean_sd", geom="errorbar",
               position = position_dodge(width = .9),width = 0.5,linewidth = 1)+
  geom_point(stat="summary",fun="mean",position = position_dodge(width = .9),
             size = 2,shape = 23,fill = "black")+
  facet_wrap(~PAC)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  gDefault

graphdat = dat[(dat$PAC %in% c("Theta_Gamma")),]
ggplot(graphdat,aes(x=Health, y=value, fill=Stim, color = Stim)) + 
  stat_summary(fun.data = "mean_sd", geom="errorbar",
               position = position_dodge(width = .9),width = 0.5,size = 1)+
  geom_point(stat="summary",fun="mean",position = position_dodge(width = .9),
             size = 2,shape = 23,fill = "black")+
  facet_wrap(~PAC)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  gDefault


graphdat = dat[(dat$PAC %in% c("Beta_Gamma")),]
ggplot(graphdat,aes(x=Health, y=value, fill=Stim, color = Stim)) + 
  stat_summary(fun.data = "mean_sd", geom="errorbar",
               position = position_dodge(width = .9),width = 0.5,size = 1)+
  geom_point(stat="summary",fun="mean",position = position_dodge(width = .9),
             size = 2,shape = 23,fill = "black")+
  facet_wrap(~PAC)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  gDefault


graphdat = dat[(dat$PAC %in% c("Alpha_Gamma")),]
ggplot(graphdat,aes(x=Health, y=value, fill=Stim, color = Stim)) + 
  stat_summary(fun.data = "mean_sd", geom="errorbar",
               position = position_dodge(width = .9),width = 0.5,size = 1)+
  geom_point(stat="summary",fun="mean",position = position_dodge(width = .9),
             size = 2,shape = 23,fill = "black")+
  facet_wrap(~PAC)+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  gDefault


###################################################### Anova tests ----
#--------------------------> Delta_Beta: HC PD off-----
sDat = dat[dat$PAC == "Delta_Beta" & dat$Health %in% c("HC ","PD Off"),]
sDat$value = FisherZ(sDat$value)
head(sDat)

results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("Stim"),
                              between = c("Health"), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 2)
results

WriteANOVAResults_APA("0_Anova.doc",results = results)

ggplot(sDat,aes(x=Health, y=value, fill=Stim)) + 
  geom_bar(stat="summary",fun="mean",position = position_dodge(width = .9),
           width = .8)+
  stat_summary(fun.data = "mean_se", geom="errorbar",position = position_dodge(width = .9),
               width = .8)+
  scale_fill_brewer(palette="Set2")+
  gDefault

#------------ Health
phDat = as.data.frame(summarise(group_by(sDat,itt,Health),value = mean(value)))
head(phDat)
phDat$groups = paste(phDat$Health,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = F)
WriteTTestResults_APA(file = "1_ttest.doc", A[[1]],A[[2]])
#------------ Stim PD
phDat = sDat[sDat$Health == "PD Off",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T)
WriteTTestResults_APA(file = "2_ttest.doc", A[[1]],A[[2]])
#------------ Stim HC
phDat = sDat[sDat$Health == "HC ",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T)
WriteTTestResults_APA(file = "3_ttest.doc", A[[1]],A[[2]])

#--------------------------> Delta_Beta: PD off and On -----
sDat = dat[dat$PAC == "Delta_Beta" & dat$Health %in% c("PD Off","PD On"),]

sDat$value = FisherZ(sDat$value)
head(sDat)

results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("Health","Stim"),
                              type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 2)
results

WriteANOVAResults_APA("0_Anova.doc",results = results)

ggplot(sDat,aes(x=Health, y=value, fill=Stim)) + 
  geom_bar(stat="summary",fun="mean",position = position_dodge(width = .9),
           width = .8)+
  stat_summary(fun.data = "mean_se", geom="errorbar",position = position_dodge(width = .9),
               width = .8)+
  scale_fill_brewer(palette="Set2")+
  gDefault

#------------ Medication
phDat = as.data.frame(summarise(group_by(sDat,itt,Health),value = mean(value)))
head(phDat)
phDat$groups = paste(phDat$Health,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T)
WriteTTestResults_APA(file = "1_ttest.doc", A[[1]],A[[2]])
#------------ Stim PD
phDat = sDat[sDat$Health == "PD On",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T,roundDigit = 3)
WriteTTestResults_APA(file = "2_ttest.doc", A[[1]],A[[2]])


#--------------------------> Theta_Gamma: HC PD off-----
sDat = dat[dat$PAC == "Theta_Gamma" & dat$Health %in% c("HC ","PD Off"),]

sDat$value = FisherZ(sDat$value)
head(sDat)

results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("Stim"),
                              between = c("Health"), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 2)
results

WriteANOVAResults_APA("0_Anova.doc",results = results)

ggplot(sDat,aes(x=Health, y=value, fill=Stim)) + 
  geom_bar(stat="summary",fun="mean",position = position_dodge(width = .9),
           width = .8)+
  stat_summary(fun.data = "mean_se", geom="errorbar",position = position_dodge(width = .9),
               width = .8)+
  scale_fill_brewer(palette="Set2")+
  gDefault

#------------ Health
phDat = as.data.frame(summarise(group_by(sDat,itt,Health),value = mean(value)))
head(phDat)
phDat$groups = paste(phDat$Health,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = F, roundDigit = 3)
WriteTTestResults_APA(file = "1_ttest.doc", A[[1]],A[[2]])
#------------ Stim PD
phDat = sDat[sDat$Health == "PD Off",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T, roundDigit = 3)
WriteTTestResults_APA(file = "2_ttest.doc", A[[1]],A[[2]])
#------------ Stim HC
phDat = sDat[sDat$Health == "HC ",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T, roundDigit = 3)
WriteTTestResults_APA(file = "3_ttest.doc", A[[1]],A[[2]])

#--------------------------> Theta_Gamma: PD off and On -----
sDat = dat[dat$PAC == "Theta_Gamma" & dat$Health %in% c("PD Off","PD On"),]

sDat$value = FisherZ(sDat$value)
head(sDat)

results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("Health","Stim"),
                              type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 2)
results

WriteANOVAResults_APA("0_Anova.doc",results = results)

ggplot(sDat,aes(x=Health, y=value, fill=Stim)) + 
  geom_bar(stat="summary",fun="mean",position = position_dodge(width = .9),
           width = .8)+
  stat_summary(fun.data = "mean_se", geom="errorbar",position = position_dodge(width = .9),
               width = .8)+
  scale_fill_brewer(palette="Set2")+
  gDefault

#------------ Medication
phDat = as.data.frame(summarise(group_by(sDat,itt,Health),value = mean(value)))
head(phDat)
phDat$groups = paste(phDat$Health,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T)
WriteTTestResults_APA(file = "1_ttest.doc", A[[1]],A[[2]])
#------------ Stim PD On
phDat = sDat[sDat$Health == "PD On",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T,roundDigit = 3)
WriteTTestResults_APA(file = "2_ttest.doc", A[[1]],A[[2]])



#--------------------------> Alpha_Gamma: HC PD off-----
sDat = dat[dat$PAC == "Alpha_Gamma" & dat$Health %in% c("HC ","PD Off"),]

sDat$value = FisherZ(sDat$value)
head(sDat)

results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("Stim"),
                              between = c("Health"), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 2)
results

WriteANOVAResults_APA("0_Anova.doc",results = results)

ggplot(sDat,aes(x=Health, y=value, fill=Stim)) + 
  geom_bar(stat="summary",fun="mean",position = position_dodge(width = .9),
           width = .8)+
  stat_summary(fun.data = "mean_se", geom="errorbar",position = position_dodge(width = .9),
               width = .8)+
  scale_fill_brewer(palette="Set2")+
  gDefault

#------------ Health
phDat = as.data.frame(summarise(group_by(sDat,itt,Health),value = mean(value)))
head(phDat)
phDat$groups = paste(phDat$Health,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = F, roundDigit = 3)
WriteTTestResults_APA(file = "1_ttest.doc", A[[1]],A[[2]])
#------------ Stim PD
phDat = sDat[sDat$Health == "PD Off",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T, roundDigit = 3)
WriteTTestResults_APA(file = "2_ttest.doc", A[[1]],A[[2]])
#------------ Stim HC
phDat = sDat[sDat$Health == "HC ",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T, roundDigit = 3)
WriteTTestResults_APA(file = "3_ttest.doc", A[[1]],A[[2]])

#--------------------------> Alpha_Gamma: PD off and On -----
sDat = dat[dat$PAC == "Alpha_Gamma" & dat$Health %in% c("PD Off","PD On"),]

sDat$value = FisherZ(sDat$value)
head(sDat)

results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("Health","Stim"),
                              type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 2)
results

WriteANOVAResults_APA("0_Anova.doc",results = results)

ggplot(sDat,aes(x=Health, y=value, fill=Stim)) + 
  geom_bar(stat="summary",fun="mean",position = position_dodge(width = .9),
           width = .8)+
  stat_summary(fun.data = "mean_se", geom="errorbar",position = position_dodge(width = .9),
               width = .8)+
  scale_fill_brewer(palette="Set2")+
  gDefault

#------------ Medication
phDat = as.data.frame(summarise(group_by(sDat,itt,Health),value = mean(value)))
head(phDat)
phDat$groups = paste(phDat$Health,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T)
WriteTTestResults_APA(file = "1_ttest.doc", A[[1]],A[[2]])
#------------ Stim PD On
phDat = sDat[sDat$Health == "PD On",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T,roundDigit = 3)
WriteTTestResults_APA(file = "2_ttest.doc", A[[1]],A[[2]])



#--------------------------> Beta_Gamma: HC PD off-----
sDat = dat[dat$PAC == "Beta_Gamma" & dat$Health %in% c("HC ","PD Off"),]

sDat$value = FisherZ(sDat$value)
head(sDat)

results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("Stim"),
                              between = c("Health"), type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 2)
results

WriteANOVAResults_APA("0_Anova.doc",results = results)

ggplot(sDat,aes(x=Health, y=value, fill=Stim)) + 
  geom_bar(stat="summary",fun="mean",position = position_dodge(width = .9),
           width = .8)+
  stat_summary(fun.data = "mean_se", geom="errorbar",position = position_dodge(width = .9),
               width = .8)+
  scale_fill_brewer(palette="Set2")+
  gDefault

#------------ Health
phDat = as.data.frame(summarise(group_by(sDat,itt,Health),value = mean(value)))
head(phDat)
phDat$groups = paste(phDat$Health,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = F, roundDigit = 3)
WriteTTestResults_APA(file = "_ttest.doc", A[[1]],A[[2]])
#------------ Stim PD
phDat = sDat[sDat$Health == "PD Off",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T, roundDigit = 3)
WriteTTestResults_APA(file = "_ttest.doc", A[[1]],A[[2]])
#------------ Stim HC
phDat = sDat[sDat$Health == "HC ",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T, roundDigit = 3)
WriteTTestResults_APA(file = "_ttest.doc", A[[1]],A[[2]])

#--------------------------> Beta_Gamma: PD off and On -----
sDat = dat[dat$PAC == "Beta_Gamma" & dat$Health %in% c("PD Off","PD On"),]

sDat$value = FisherZ(sDat$value)
head(sDat)

results=as.data.frame(ezANOVA(data=sDat, dv="value", wid=.("itt"), within=.("Health","Stim"),
                              type=3,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 2)
results

WriteANOVAResults_APA("0_Anova.doc",results = results)

ggplot(sDat,aes(x=Health, y=value, fill=Stim)) + 
  geom_bar(stat="summary",fun="mean",position = position_dodge(width = .9),
           width = .8)+
  stat_summary(fun.data = "mean_se", geom="errorbar",position = position_dodge(width = .9),
               width = .8)+
  scale_fill_brewer(palette="Set2")+
  gDefault

#------------ Medication
phDat = as.data.frame(summarise(group_by(sDat,itt,Health),value = mean(value)))
head(phDat)
phDat$groups = paste(phDat$Health,sep = "_")
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T)
WriteTTestResults_APA(file = "_ttest.doc", A[[1]],A[[2]])
#------------ Stim PD On
phDat = sDat[sDat$Health == "PD On",]
head(phDat)
phDat$groups = phDat$Stim
unique(phDat$groups)
A = PostHocT_Tests(data = phDat, Paired = T,roundDigit = 3)
WriteTTestResults_APA(file = "_ttest.doc", A[[1]],A[[2]])


