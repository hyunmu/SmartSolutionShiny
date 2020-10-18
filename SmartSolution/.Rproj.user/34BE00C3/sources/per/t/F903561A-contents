############################################ mtcars ##############################
loadSource <- function() {
  DFSource <<- read_rds("../SourceData/mtcars/mtcars_전처리.rds")
  
  # DFSource <<- select(DFSource, -sampleCode, -bHOT, -clusterGr)
  # DFSource$carb <- as.factor(DFSource$carb)
  # write_rds(DFSource, "../SourceData/mtcars/mtcars_전처리.rds")
  
  chosenDFSourceFile <<- "mtcars"
  # source("sourcing/mtcars/function/revampDomainUI_2.R", encoding="UTF-8")
#     load("../SmartDesignPOSCORawData/CoilKWTensile/data/CoilKWTensile.Rdata", .GlobalEnv)
# #    load("./sourcing/CoilTensile/data/IDSampleList.Rdata", .GlobalEnv) 
#     source("sourcing/CoilTensile/function/MutateSampleCode.R", encoding="UTF-8")
#     source("sourcing/CoilTensile/function/SamplingFunc.R", encoding="UTF-8")
#     source("sourcing/CoilTensile/function/revampDesignInputUI.R", encoding="UTF-8")
#     source("sourcing/CoilTensile/function/CalcPredict.R", encoding="UTF-8")
#     source("sourcing/CoilTensile/function/ModelingFunc.R", encoding="UTF-8")

#     source("sourcing/CoilTensile/function/revampTTSE.R", encoding="UTF-8")
#     source("sourcing/CoilTensile/function/InsertSampleSelUI.R", encoding="UTF-8")
#     source("sourcing/CoilTensile/function/NewModelRelatedFunc.R", encoding="UTF-8")
#     source("sourcing/CoilTensile/function/PredictRelatedFunc.R", encoding="UTF-8")
#     source("sourcing/CoilTensile/function/revampPredSummaryUI.R", encoding="UTF-8")
# #    source("sourcing/CoilTensile/function/DesignInputFunc.R", encoding="UTF-8")

  # 이하는 리포트 생성용

  ############## 전체 관련 고유 정보 ################
  pathHTMLReport <<- "../USER/mtcars/output"
  
  ############## Sourcing 관련 고유 정보 ################
  pathFileRmdSourcingReport <<- c("sourcing/mtcars/Rmd/SourcingReport1.Rmd",
                                  "sourcing/mtcars/Rmd/SourcingReport2.Rmd"
  )

  outputFileNamesSourcingReport <<- c("SourcingReport1.html",   ### 한글명은 안됨, ggplot 포함시 pandoc에서 에러 발생 ###
                                      "SourcingReport2.html")
  outputFileFinalNamesSourcingReport <<- c("mtcars 리포트1.html",   
                                           "mtcars 리포트2.html")
  ############## Sampling 관련 고유 정보 ################
  pathFileRmdSamplingReport <<- c("sourcing/mtcars/Rmd/SamplingReport1.Rmd",
                                  "sourcing/mtcars/Rmd/SamplingReport2.Rmd",
                                  "sourcing/mtcars/Rmd/SamplingReportTable.Rmd"
  )
  outputFileNamesSamplingReport <<- c("SamplingReport1.html",
                                      "SamplingReport2.html",
                                      "SamplingReportTable.html")

  DomainTable1Names <<- c("mpg","cyl","disp","hp","wt")
  DomainTable1NamesLabel <<- c("Miles/G","No Cylinder","배기량","마력","무게")
  DomainTable2Names <<- NULL
  DomainTable2NamesLabel <<- NULL
  DomainTable3Names <<- NULL
  DomainTable3NamesLabel <<- NULL
  # chemCompName <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb","Ti", "SolAl", "B",  "N2", "Ca")
  # chemCompNameLabel <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb", "Ti","SolAl", "B(ppm)", "N2(ppm)",  "Ca(ppm)")
  
  pathNameFileDomain <<- "../USER/mtcars/Domain"
  pathNameFileDesign <<- "../USER/mtcars/Design"
  pathNameFileNewModel <<- "../Model/mtcars"


#     
#     numVarExploreSamplingHard <<- "CoilT"
#     
    ############## dfDomainCat 계산 ################
  catVarWithoutModal <<- c("am", "vs")  # Modal을 이용하지 않고 Category를 선택하는 변수
  catVarWithModal <<- c("gear", "carb")  # Modal을 이용하여 Category를 선택하는 변수

  catVarNameExplore <- c(catVarWithoutModal, catVarWithModal)
  dfDomainCatExplore <- list("am", "vs", "gear", "carb" )
  names(dfDomainCatExplore) <- catVarNameExplore

  for(i in 1:NROW(catVarNameExplore)) {
    if(is.factor(DFSource[,catVarNameExplore[i]][[1]])) {
      dfDomainCatExplore[[catVarNameExplore[i]]] <-
        attr(DFSource[,catVarNameExplore[i]][[1]], "levels")
    } else {
      dfDomainCatExplore[[catVarNameExplore[i]]] <-
        as.character(unique(DFSource[,catVarNameExplore[i]]))
    }
  }

  selCatDomainExplore <<- dfDomainCatExplore


}

