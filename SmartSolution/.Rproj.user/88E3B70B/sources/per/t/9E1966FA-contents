############################################ EXCEL ##############################
MTVCodeList <- list(del=NA, remain=NA, mutate=NA, change=NA)

loadSource <- function() {
  projectWD <- getwd()
  setwd("..")
  dirPath <- paste0(getwd(),"/SourceData/EXCEL")
  setwd(dirPath)
  # Tell R to sleep until the current directory matches the expected directory
  while(getwd() != normalizePath(dirPath, winslash="/")) {
    Sys.sleep(0.02)
  }
  filePath <- file.choose()

  chosenDFSourceFileExt <<- str_split(filePath, "\\\\")[[1]][length( str_split(filePath, "\\\\")[[1]])]
  chosenDFSourceFile <<- str_split(chosenDFSourceFileExt, "\\.")[[1]][1]
  chosenDFSourceExt <- str_split(chosenDFSourceFileExt, "\\.")[[1]][2]

  setwd(projectWD)
  switch(chosenDFSourceExt,
         xlsx = {
           DFSource <<- read_xlsx_meta(filePath)
         },
         csv = {
           DFSource <<- read_csv_meta(filePath)
         },
         rds = {
           DFSource <<- read_rds(filePath)
         })

  # setwd(projectWD)
  # # Tell R to sleep until the current directory matches the expected directory
  # while(getwd() != normalizePath(projectWD, winslash="/")) {
  #   Sys.sleep(0.02)
  # }
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
  pathHTMLReport <<- "../USER/EXCEL/output"
  
  hide("renderReportSourcing")
  # ############## Sourcing 관련 고유 정보 ################
  # pathFileRmdSourcingReport <<- c("sourcing/mtcars/Rmd/SourcingReport1.Rmd",
  #                                 "sourcing/mtcars/Rmd/SourcingReport2.Rmd"
  # )
  # outputFileNamesSourcingReport <<- c("SourcingReport1.html",
  #                                     "SourcingReport2.html")
  # ############## Sampling 관련 고유 정보 ################
  # pathFileRmdSamplingReport <<- c("sourcing/mtcars/Rmd/SamplingReport1.Rmd",
  #                                 "sourcing/mtcars/Rmd/SamplingReport2.Rmd",
  #                                 "sourcing/mtcars/Rmd/SamplingReportTable.Rmd"
  # )
  # outputFileNamesSamplingReport <<- c("SamplingReport1.html",
  #                                     "SamplingReport2.html",
  #                                     "SamplingReportTable.html")


  
  pathNameFileDomain <<- "../USER/EXCEL/Domain"
  pathNameFileDesign <<- "../USER/EXCEL/Design"
  # pathNameFileNewModel <<- "../Model/mtcars"


   

}

renderAttrSamplingUI <- function() {
  
  ############## 변수 분리 ################
  numVar <- extractNumVarName(DFSource)
  catVar <- extractCatVarName(DFSource)
  
  func1 <- function(x) {
    if(length(unique(DFSource[,x])) > 3 ) {
      TRUE
    } else {
      FALSE
    }
    
  }
  
  boolTrueNum <- vapply(numVar, func1, logical(1))
  numVarTrue <- numVar[boolTrueNum]
  
  DomainTable1Names <<- numVarTrue[1:12]
  # DomainTable1Names <<- c("Sepal.Length", "Sepal.Width")
  DomainTable1NamesLabel <<- DomainTable1Names
  
  DomainTable2Names <<- numVarTrue[13:24]
  DomainTable2NamesLabel <<- DomainTable2Names
  DomainTable3Names <<- numVarTrue[25:36]
  DomainTable3NamesLabel <<- DomainTable3Names
  # chemCompName <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb","Ti", "SolAl", "B",  "N2", "Ca")
  # chemCompNameLabel <- c("C","Si","Mn","P","S", "Cu", "Ni", "Cr", "Mo", "V", "Nb", "Ti","SolAl", "B(ppm)", "N2(ppm)",  "Ca(ppm)")
  
  ############## dfDomainCat 계산 ################
  func1 <- function(x) {
    if( length(unique(DFSource[,x])) == 2 || length(unique(DFSource[,x])) == 3) {
      TRUE
    } else {
      FALSE
    }
  }
  boolTwoThreeLevels <- vapply(catVar, func1, logical(1))
  catVarTwoThreeLevels <- union(catVar[boolTwoThreeLevels], numVar[!boolTrueNum])
  catVarWithoutModal <<- catVarTwoThreeLevels # Modal을 이용하지 않고 Category를 선택하는 변수
  
  func1 <- function(x) {
    if( length(unique(DFSource[,x])) < 4) {
      TRUE
    } else {
      FALSE
    }
  }
  boolFewLevels <- vapply(catVar, func1, logical(1))
  catVarWithModal <<- setdiff(catVar[!boolFewLevels], "rowNoSource")  # Modal을 이용하여 Category를 선택하는 변수
  
  
  
  catVarNameExplore <- c(catVarWithoutModal, catVarWithModal)
  dfDomainCatExplore <- vector("list",length(catVarNameExplore))
  
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


read_xlsx_meta <- function(filePath) {
  DFSource1 <- read_xlsx(filePath, sheet=1,
                        skip=4, col_names=FALSE)
  DFSource1 <- as.data.frame(DFSource1)
  labelVec <- read_xlsx(filePath,  sheet=1,
                       range=cell_rows(2))
  xxx <- as.data.frame(labelVec)
  yyy <- t(xxx)
  labelVec <- attr(yyy,"dimnames")[[1]]
  for(i in 1: NCOL(DFSource1)) {
    str <- paste0("...",i)
    attr(DFSource1[,str],"label") <- labelVec[i]
  }
  
  
  colNameVec <- read_xlsx(filePath,
                         sheet=1, range=cell_rows(1))
  xxx <- as.data.frame(colNameVec)
  yyy <- t(xxx)
  colNameVec <- attr(yyy,"dimnames")[[1]]
  colnames(DFSource1) <- colNameVec
  
  
  ### valid Min, Max 행에는 보이지 않지만 enter가 포함되어야 함 ###
  validMaxVec <- read_xlsx(filePath,
                          sheet=1, range=cell_rows(3), col_types="numeric", col_names=FALSE)
  xxx <- as.data.frame(validMaxVec)
  for(i in 1: NCOL(DFSource1)) {
    attr(DFSource1[,i],"validMax") <- xxx[1,i]
  }
  
  validMinVec <- read_xlsx(filePath,
                          sheet=1, range=cell_rows(4), col_types="numeric", col_names=FALSE)
  xxx <- as.data.frame(validMinVec)
  for(i in 1: NCOL(DFSource1)) {
    attr(DFSource1[,i],"validMin") <- xxx[1,i]
  }
  
  numVarConst <- extractNumVarNameAndConst(DFSource1)
  decimalVec <- vapply(numVarConst, renderDigitVector,  FUN.VALUE=numeric(1), DFSource1)
  decimalVec <- as.vector(decimalVec)
  for(x in numVarConst) {
    # numVarConst <- "thick"
    attr(DFSource1[,x], "max") <- max(DFSource1[,x], na.rm=TRUE)
    attr(DFSource1[,x], "mean") <- mean(DFSource1[,x], na.rm=TRUE)
    attr(DFSource1[,x], "min") <- min(DFSource1[,x], na.rm=TRUE)
    attr(DFSource1[,x], "digit") <- decimalVec[which(numVarConst==x)]
  }
  
  DFSource1 <- sticky_all(DFSource1)
  
  return(DFSource1)
}
