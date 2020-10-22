rm(list=ls())  


if(!library(shiny, logical.return=TRUE)) {
    install.packages("shiny")
    library(shiny)
}

if(!library(shinythemes, logical.return=TRUE)) {
    install.packages("shinythemes")
    library(shinythemes)
}

if(!library(tidyverse, logical.return=TRUE)) {
    install.packages("tidyverse")
    library(tidyverse)
}
# ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats

if(!library(lubridate, logical.return=TRUE)) {
    install.packages("lubridate")
    library(lubridate)
}

if(!library(htmltools, logical.return=TRUE)) {
    install.packages("htmltools")
    library(htmltools)
}

if(!library(modelr, logical.return=TRUE)) {
    install.packages("modelr")
    library(modelr)
}
# 
# if(!library(readxl, logical.return=TRUE)) {
#     install.packages("readxl")
#     library(ggplot2)
# }
# 
# if(!library(magrittr, logical.return=TRUE)) {
#     install.packages("magrittr")
#     library(magrittr)
# }
# 
# if(!library(plyr, logical.return=TRUE)) {
#     install.packages("plyr")
#     library(plyr)
# }
# 
# if(!library(dplyr, logical.return=TRUE)) {
#     install.packages("dplyr")
#     library(dplyr)
# }
# 
if(!library(reshape2, logical.return=TRUE)) {
    install.packages("reshape2")
    library(reshape2)
}
# 
# if(!library(tidyr, logical.return=TRUE)) {
#     install.packages("tidyr")
#     library(tidyr)
# }
# 
# if(!library(stringr, logical.return=TRUE)) {
#     install.packages("stringr")
#     library(stringr)
# }
# 
if(!library(flexdashboard, logical.return=TRUE)) {
    install.packages("flexdashboard")
    library(flexdashboard)
}

if(!library(ggiraph, logical.return=TRUE)) {
    install.packages("ggiraph")
    library(ggiraph)
}
# 
# 
# #install.packages("ggiraph")
# #library(ggiraph)
# 
# if(!library(randomForest, logical.return=TRUE)) {
#     install.packages("randomForest")
#     library(randomForest)
# }
# 
# if(!library(broom, logical.return=TRUE)) {
#     install.packages("broom")
#     library(broom)
# }
# 
# if(!library(xtable, logical.return=TRUE)) {
#     install.packages("xtable")
#     library(xtable)
# }
# 
# if(!library(gridExtra, logical.return=TRUE)) {
#     install.packages("gridExtra")
#     library(gridExtra)
# }
# 
if(!library(party, logical.return=TRUE)) {
    install.packages("party")
    library(party)
}

# if(!library(jsonlite, logical.return=TRUE)) {
#     install.packages("jsonlite")
#     library(jsonlite)
# }
# 
if(!library(shinyjs, logical.return=TRUE)) {
    install.packages("shinyjs")
    library(shinyjs)
}
if(!library(readxl, logical.return=TRUE)) {
    install.packages("readxl")
    library(readxl)
}

# if(!library(Rtools, logical.return=TRUE)) {
#     install.packages("Rtools")
#     library(Rtools)
# }

if(!library(sticky, logical.return=TRUE)) {
    install.packages("sticky")
    library(sticky)
}

# if(!library(Hmisc, logical.return=TRUE)) {
#     install.packages("Hmisc")
#     library(Hmisc)
# }

if(!require(DT)) install.packages("DT")
if(!require(data.table)) install.packages("data.table")
if(!require(modelr)) install.packages("modelr")

### shinyio 에서 필요 ###
# if(!require(Rttf2pt1)) install.packages("Rttf2pt1")
# if(!require(checkmate)) install.packages("checkmate")
# if(!require(extrafont)) install.packages("extrafont")
# if(!require(extrafontdb)) install.packages("extrafontdb")
# if(!require(gridExtra)) install.packages("gridExtra")
# if(!require(hrbrthemes)) install.packages("hrbrthemes")
# if(!require(magick)) install.packages("magick")
# if(!require(pander)) install.packages("pander")
# if(!require(pryr)) install.packages("pryr")
# if(!require(rapportools)) install.packages("rapportools")
# if(!require(summarytools)) install.packages("summarytools")
# if(!require(viridis)) install.packages("viridis")
### shinyio 에서 필요 ###






# theme_update(legend.position="right")
# 

# 
# load("initEnv.Rdata", .GlobalEnv)
# 
# IDModel <- "empty"
# 
# nameVecVF <- c("empty1", "empty2")
# 
# 
# DFsample <- data.frame()
# sampleModelList <- data.frame()
# 
# 
# reactNewModel <- reactiveVal("")
# 
# 
# 
# 
# DFpm2 <- data.frame()
# DFpmMean2 <- data.frame()
# xLabel <- character(0)
# yLabel <- character(0)
# PredSummary <- data.frame()
# 
# 
# loadSource <- function() {}
# 
# numVFDirectRaw <- ""
# numVFIndirectRaw <- ""
# numVFTrans <- ""
# 
# 
# 
# source("sourcing/PredictEx.R", encoding="UTF-8")
# CompVarName <- c("C","Si","Mn","P","S","Cu","Ni","Cr","Mo","V","Nb","B","TotalAl","SolAl","Ti", "N2")
# #CompVarName <- c("C","Si","Mn","P","S","Cu","Ni","Cr","Mo","V","Nb","B")

######################## global area  ##############################
Sys.setlocale( "LC_ALL", "Korean_Korea.949")
# Sys.setlocale( "LC_ALL", "C")
source("Base/function/globalModal.R", encoding="UTF-8")
source("Base/function/globalFunc.R", encoding="UTF-8")
source("Base/function/commonPlotReport.R", encoding="UTF-8")
source("Base/function/commonDescriptiveReport.R", encoding="UTF-8")
source("Base/function/modalTreatVar.R", encoding="UTF-8")
source("Base/modal/modalGraphOptionGlobal.R", encoding="UTF-8")
source("Base/modal/modalGraphOptionScatter.R", encoding="UTF-8")
source("Base/modal/modalGraphOptionBoxplot.R", encoding="UTF-8")
source("Base/modal/modalGraphOptionBar.R", encoding="UTF-8")
source("Base/modal/modalGraphOptionHistogram.R", encoding="UTF-8")
dfReportCommon <- NULL
fromReportCommon <- NULL
curSelModelResultReport <- NULL
reactAesList <- reactiveVal(NA)
stringList <- list(predict=NA)
stringList[["predict"]] <- list(predictTab1=NA)
stringList[["predict"]][["predictTab1"]] <- list(pred=NA)
stringList[["predict"]][["predictTab1"]][["pred"]] <- "예측값 : "
digitList <- list(predict=NA)
digitList[["predict"]] <- list(predictTab1=NA)
digitList[["predict"]][["predictTab1"]] <- list(pred=NA)
digitList[["predict"]][["predictTab1"]][["pred"]] <- 0
dateVarNames <- NULL

######################## tidying Tab  ##############################
source("main/tidyingMain.R", encoding="UTF-8")


######################## sourcing Tab  ##############################
source("main/sourcingMain.R", encoding="UTF-8")
# source("function/renderReportSourcing.R", encoding="UTF-8")
reactDFSource <- reactiveVal(NA)
reactDFSource(NA)
# renderReportSourcing <- function(input, output, session) {
#       alert("현재 Sourcing의 Sourcing 리포트는 미개발 상태입니다.")
# }



# ######################## Sampling Tab #################################
DomainTable1Names <- NULL
DomainTable1NamesLabel <- NULL
DomainTable2Names <- NULL
DomainTable2NamesLabel <- NULL
DomainTable3Names <- NULL
DomainTable3NamesLabel <- NULL
catVarWithoutModal <- NULL
catVarWithModal <- NULL
selCatDomainExplore <- NULL
MinReqExplore <<- NULL
MaxReqExplore <<- NULL

source("Base/main/samplingMain.R", encoding="UTF-8")
source("Base/function/revampDomainUI.R", encoding="UTF-8")
source("Base/function/sampleRelatedModal.R", encoding="UTF-8")
source("Base/function/sampleRelatedFunc.R", encoding="UTF-8")






# ######################## Explore Tab #################################
# ######################## Explore Tab #################################
# ######################## Explore Tab #################################
source("Base/main/exploreMain.R", encoding="UTF-8")
# source("function/ExploreRelatedFunc.R", encoding="UTF-8")
# source("function/ExploreRelatedModal.R", encoding="UTF-8")
source("Base/tab/scatter1TabModuleUI.R", encoding="UTF-8")
source("Base/tab/scatterGirafe1TabModuleUI.R", encoding="UTF-8")
source("Base/graphFunction/graphFunc.R", encoding="UTF-8")
source("Base/tab/boxplot1TabModuleUI.R", encoding="UTF-8")
source("Base/tab/boxplot2TabModuleUI.R", encoding="UTF-8")
source("Base/tab/boxplot3TabModuleUI.R", encoding="UTF-8")
source("Base/tab/violin1TabModuleUI.R", encoding="UTF-8")
source("Base/tab/bar1TabModuleUI.R", encoding="UTF-8")
source("Base/tab/histogram1TabModuleUI.R", encoding="UTF-8")
source("Base/tab/scatter2TabModuleUI.R", encoding="UTF-8")
clustersK <- NULL
source("Base/tab/clusteringTabModuleUI.R", encoding="UTF-8")
source("Base/tab/baseTabModule.R", encoding="UTF-8")
source("Base/tab/cTreeTabModuleUI.R", encoding="UTF-8")

reactCurSampleClustering <- reactiveVal(NA)




# # source("module/GroupDifferenceTabModuleUI.R", encoding="UTF-8")
# source("module/ReqSampleTabModuleUI.R", encoding="UTF-8")
# reactCurSampleExplore <- reactiveVal(NA)
# IDSourceExplore <- "empty"
# DFSourceExplore <- data.frame()
# reactIDSourceExplore <- reactiveVal(IDSourceExplore)
# IDSampleListExplore <- c("empty1", "empty2", "empty3")
# curSampleExplore <- data.frame()
# numVarExplore <- c("")
# 
# revampTTSE  <- function(input, output, session) {}
# # InsertReqInputUI  <- function(input, output, session) {}  # 흔적
# # RemoveReqInputUI <- function(input,output, session) {
# #     removeUI( selector ="div[title='ReqInput']")
# # }  # 흔적
#   
# 
# MinReqExplore <- NULL
# MaxReqExplore <- NULL
# selNameFileDomain <<- "kkk.Rdata"
# 
# RenderReportTTSE <- function(input, output, session) {
#     alert("현재 Sourcing의 TTSE 리포트는 미개발 상태입니다.")
# }
# 

######################### ExploreTable Tab #################################
######################### ExploreTable Tab #################################
######################### ExploreTable Tab #################################
source("Base/main/exploreTableMain.R", encoding="UTF-8")
source("Base/tab/exploreTable1TabModuleUI.R", encoding="UTF-8")
source("Base/tab/exploreTable2TabModuleUI.R", encoding="UTF-8")
curSampleExploreTable <- NA



# ######################## New Modeling Tab ###########################
source("main/modelingMain.R", encoding="UTF-8")
source("Base/function/modelingModal.R", encoding="UTF-8")
source("Base/tab/modeling/predMeas1TabModuleUI.R", encoding="UTF-8")
source("Base/tab/modeling/modYVF1TabModuleUI.R", encoding="UTF-8")
# source("function/NewModelRelatedModal.R", encoding="UTF-8")
# source("function/modelingTab.R", encoding="UTF-8")
# source("function/predMeasTab.R", encoding="UTF-8")
# source("module/predMeasMeanTab.R", encoding="UTF-8")
# source("module/VFMeasTab.R", encoding="UTF-8")
# source("module/VFMeasMeanTab.R", encoding="UTF-8")
# IDSourceNewModel <- "empty"
# DFSourceNewModel <- data.frame()
# reactIDSourceNewModel <- reactiveVal(IDSourceNewModel)
# IDSampleNewModel <- "empty"
# IDSampleListNewModel <- c("empty1", "empty2", "empty3")
# nameVecVF <- NULL
# ModelNew <- list()
# DFpm <- data.frame()
# DFpmMean <- data.frame()
# DFpmm <- data.frame()
# DFpmmMean <- data.frame()
# InsertSampleSelUINewModel  <- function(input, output, session) {}
# RemoveSampleSelUINewModel <- function(input,output, session) {
#     removeUI( selector ="div[title='SampleSelNewModel']")
# }
# dfDomainNumNewModel <- NULL
# developer <- "익명"
# observeEventSampleNewModel <- function(input, output, session) {}
# observeEventIDModelAlgorithm <- function(input, output, session) {}
# updateModelList <- function(input, output, session) {}
# renderPredY1 <- function(DFmodel, finalModel, IDSource) {return (DFmodel)}
# RenderReportNewModel <- function(input, output, session) {}
# glanceModel <- data.frame(a=c(1,2), b=c("A", "B"))
# renderModel <- function(input, output, session, DFmodel, IDModeling) {}
# 
########################### Predict Tab #############################
DesignTable1Names <- NULL
DesignTable1NamesLabel <- NULL
DesignTable2Names <- NULL
DesignTable2NamesLabel <- NULL
DesignTable3Names <- NULL
DesignTable3NamesLabel <- NULL
catVarWithoutModalPredict <- NULL
catVarWithModalPredict <- NULL
selCatDesignPredict <- NULL
source("Base/main/predictMain.R", encoding="UTF-8")
source("Base/function/revampDesignUI.R", encoding="UTF-8")
source("Base/function/predictRelatedModal.R", encoding="UTF-8")
source("Base/tab/predPlotSeed1TabModuleUI.R", encoding="UTF-8")
source("Base/tab/predPlot1TabModuleUI.R", encoding="UTF-8")
ggObjPred1 <- NULL
predPlot1TabTitle <- "predPlot1"
source("Base/tab/predPlot2TabModuleUI.R", encoding="UTF-8")
ggObjPred2 <- NULL
predPlot2TabTitle <- "predPlot2"
source("Base/tab/predPlot3TabModuleUI.R", encoding="UTF-8")
ggObjPred3 <- NULL
predPlot3TabTitle <- "predPlot3"
# source("function/VFPredTab.R", encoding="UTF-8")
# source("function/RenderPredYSummary.R", encoding="UTF-8")
# source("module/PredSummaryTab.R", encoding="UTF-8")
# IDSourcePredict <- "empty"
# DFSourcePredict <- data.frame()
reactPredVal <- reactiveVal(NA)
# reactIDSourcePredict <- reactiveVal(IDSourcePredict)
# reactPredYSummary <- reactiveVal(NA)
# 
# IDSamplePredict <- "empty"
curDesignDF <- data.frame()
# ModelPred_VFY_selY <- NULL
# ModelPred_VFY_selY2 <- NULL
# ModelListPredict <- NULL
# UnitListPredict <- NULL
# curSamplePredict <- data.frame()
renderStrPred <- function(df) {
    alert("개발자(developer) 문의 필요 : 예측 결과 표시 함수를 갱신해야 합니다.")
}
# InsertSampleSelUIPred  <- function(input, output, session) {}
# RemoveSampleSelUIPred <- function(input,output, session) {
#     removeUI( selector ="div[title='SampleSelPred']")
# }
# revampDesignInputUI  <- function(input, output, session) {}
# RemoveCatVarDesignUI <- function(input,output, session) {
#     removeUI( selector ="div[title='catVarDesignContent']")
# }
# CalcPredict <- function(input, output, session)  {}
# updateModelListPred <- function(input, output, session, property, modelPredNo) {}
# updateVFListPredLeft <- function(input, output, session) {}
# updateVFListPredRight <- function(input, output, session) {}
# observeEventPredict <- function(input, output, session) {}
# revampPredSummaryUI <- function(input, output, session) {}
# 
# PredYSummary <- NULL
# selVF2Predict <- NULL
# numVarLevelPred <- NULL
# catVarLevelPred <- NULL
# 
# InsertDesignInputUI  <- function(input, output, session) {}
# 
# #source("sourcing/CoilPipe/module/DesignInput.R", encoding="UTF-8")
# 


########################### Optimize Tab #############################
OptimizeTable1Names <- NULL
OptimizeTable1NamesLabel <- NULL
OptimizeTable2Names <- NULL
OptimizeTable2NamesLabel <- NULL
OptimizeTable3Names <- NULL
OptimizeTable3NamesLabel <- NULL
catVarWithoutModalOptimize <- NULL
catVarWithModalOptimize <- NULL
selCatOptimizeOptimize <- NULL
source("Base/main/optimizeMain.R", encoding="UTF-8")
source("Base/function/revampOptimizeUI.R", encoding="UTF-8")
source("Base/function/optimizeRelatedModal.R", encoding="UTF-8")


reactPredValOpt <- reactiveVal(NA)


# 
# dfRefStateReactive <- reactive({
#     reactNewModel()
#     # Reference State로 사용하기 위한 평균 구하기
#     func1 <- function(x) {
#         if (is.numeric(DFpm[,x])) {  mean(DFpm[,x], na.rm=TRUE) }
#         else {
#             sort(unique(DFpm[,x]))[1]
#         }
#         
#     }
#     
#     meanDF <- as.data.frame(lapply(1:ncol(DFpm), FUN=func1))
#     names(meanDF) <- names(DFpm)
#     
#     #dfRefState의 첫번째 컬럼 생성
#     colTempo <- rep(meanDF[[1]], nrow(DFpm))
#     dfRefState <- data.frame(colTempo)
#     names(dfRefState)[1] <- names(DFpm)[1]
#     
#     #dfRefState의 나머지 컬럼 생성
#     for ( i in 2:ncol(DFpm)) {
#         colTempo <- rep(meanDF[[i]], nrow(DFpm))
#         dfRefState <- cbind(dfRefState, colTempo)
#         names(dfRefState)[i] <- names(DFpm)[i]
#     }
#     
#     dfRefState
# })
# 

