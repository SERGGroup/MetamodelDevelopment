# Chargement des packages
library(lhs)
library(DiceDesign)
library(triangle)
library(DiceKriging)
library(rgl)
library(sensitivity)
library(plotly)
library(rsm)
library(xml2)

# ------------------------------------------------------------ #
# ------------------------------------------------------------ #
# ------------------------------------------------------------ #

#                       FUNCTIONS

# ------------------------------------------------------------ #
# ------------------------------------------------------------ #
# ------------------------------------------------------------ #

#<----------------------- DATA SETTINGS FUNCTION ------------------------>

getPrameterSet <- function(getNameAndDimensionOnly = FALSE, getAlsoFirstTryValues = FALSE){
  
  namesArray = "T_in"
  dimensionArray = "[ C ]"
  lob = 0;
  upb = 50;
  meanValue = 15;
  ignoreInSensitivity = FALSE
  
  namesArray = c(namesArray, "P_in")
  dimensionArray = c(dimensionArray, "[ MPa ]")
  lob = c(lob, 8);
  upb = c(upb, 15);
  meanValue = c(meanValue, 10);
  ignoreInSensitivity = c(ignoreInSensitivity, FALSE);
  
  namesArray = c(namesArray, "Re_ext")
  dimensionArray = c(dimensionArray, "[ - ]")
  lob = c(lob, 5000);
  upb = c(upb, 100000);
  meanValue = c(meanValue, 40000);
  ignoreInSensitivity = c(ignoreInSensitivity, FALSE);
  
  if (getNameAndDimensionOnly) {
    
    dataMatrix = character(2*length(namesArray))
    dataMatrix = matrix(dataMatrix, nrow = length(namesArray), ncol = 2)
    colnames(dataMatrix) = c("dataNamesArray", "dimensionArray")
    dataMatrix = data.frame(dataMatrix)
    
    dataMatrix$dataNamesArray = namesArray
    dataMatrix$dimensionArray = dimensionArray
    
  } else {
    
    dataMatrix = character(5*length(namesArray))
    dataMatrix = matrix(dataMatrix, nrow = length(namesArray), ncol = 5)
    colnames(dataMatrix) = c("dataNamesArray", "dataLob", "dataUpb", "meanValue", "ignoreInSensitivity")
    dataMatrix = data.frame(dataMatrix)
    
    dataMatrix$dataNamesArray = namesArray
    dataMatrix$dataLob = lob
    dataMatrix$dataUpb = upb
    dataMatrix$meanValue = meanValue
    dataMatrix$ignoreInSensitivity = ignoreInSensitivity
    
  }
  
  return(dataMatrix)
  
}

getPrameterValues <- function(xmlFileName, getNameAndDimensionOnly = FALSE){
  
  try({
    
    currentDir = dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(currentDir)
    
  })
  
  namesArray = vector()
  dimensionArray = vector()
  lob = vector()
  upb = vector()
  meanValue = vector()
  ignoreInSensitivity = vector()
  
  outputData = read_xml(xmlFileName)
  inputs = xml_child(outputData, "inputs")
  
  
  for (input in xml_children(inputs)){
    
    namesArray = c(namesArray, xml_attr(input, "name"))
    dimensionArray = c(dimensionArray, xml_attr(input, "measure_unit"))
    
    lob = c(lob, as.double(xml_attr(input, "min")))
    upb = c(upb, as.double(xml_attr(input, "max")))
    meanValue = c(meanValue, as.double(xml_attr(input, "mean")))
    
    ignoreInSensitivity = c(ignoreInSensitivity, FALSE)
    
  }
  
  if (getNameAndDimensionOnly) {
    
    dataMatrix = character(2*length(namesArray))
    dataMatrix = matrix(dataMatrix, nrow = length(namesArray), ncol = 2)
    colnames(dataMatrix) = c("dataNamesArray", "dimensionArray")
    dataMatrix = data.frame(dataMatrix)
    
    dataMatrix$dataNamesArray = namesArray
    dataMatrix$dimensionArray = dimensionArray
    
  } else {
    
    dataMatrix = character(5*length(namesArray))
    dataMatrix = matrix(dataMatrix, nrow = length(namesArray), ncol = 5)
    colnames(dataMatrix) = c("dataNamesArray", "dataLob", "dataUpb", "meanValue", "ignoreInSensitivity")
    dataMatrix = data.frame(dataMatrix)
    
    dataMatrix$dataNamesArray = namesArray
    dataMatrix$dataLob = lob
    dataMatrix$dataUpb = upb
    dataMatrix$meanValue = meanValue
    dataMatrix$ignoreInSensitivity = ignoreInSensitivity
    
  }
  
  return(dataMatrix)
  
}

getMetamodelNames <- function(xmlFileName){
  
  metamodelParamName = vector()
  metamodelDimension = vector()
  metamodelModifier = vector()
  
  try({
    
    currentDir = dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(currentDir)
    
  })
  
  outputData = read_xml(xmlFileName)
  outputs = xml_child(outputData, "outputs")
  
  for (output in xml_children(outputs)){
    
    metamodelParamName = c(metamodelParamName, xml_attr(output, "name"))
    metamodelDimension = c(metamodelDimension, xml_attr(output, "measure_unit"))
    metamodelModifier = c(metamodelModifier, "null")
    
  }
  
  dataMatrix = character(3*length(metamodelParamName))
  dataMatrix = matrix(dataMatrix, nrow = length(metamodelParamName), ncol = 3)
  colnames(dataMatrix) = c("metamodelParamName", "metamodelDimension", "metamodelModifier")
  dataMatrix = data.frame(dataMatrix)
  
  dataMatrix$metamodelParamName = metamodelParamName
  dataMatrix$metamodelDimension = metamodelDimension
  dataMatrix$metamodelModifier = metamodelModifier
  
  return(dataMatrix)
  
}



#<------------------ METAMODEL GENERATION FUNCTION -------------------->

optimalExperimentDesign <- function(nSimu, parameter = vector(), lob = vector(), upb = vector(), namesArray = vector(), displayGraph = FALSE, returnOnlyModifiedValues = FALSE, returnDataInParameterRange = TRUE){
  
  if (length(parameter) != 0){
    
    namesArray = parameter$dataNamesArray
    lob = parameter$dataLob
    upb = parameter$dataUpb
    
  }
  
  n = length(lob)
  
  new_lob <- vector()
  new_upb <- vector()
  newNamesArray <- vector()
  positionIndex <- rep(-1, n)
  
  j = 1
  for (i in 1:n){
    
    if (lob[i] != upb[i]){
      
      new_lob[j] <- lob[i]
      new_upb[j] <- upb[i]
      newNamesArray[j] <- namesArray[i]
      
      positionIndex[i] = j
      j = j + 1
      
    }
    
  }
  
  n = length(new_lob)
  
  #Creation of Basic LHS with values included in [0, 1]
  set.seed(0)
  X_init = randomLHS(nSimu,n)
  
  #Optimization of Basic LHS
  set.seed(0)
  X0 = discrepESE_LHS(X_init, T0 = 0.005*discrepancyCriteria(X_init,type='C2')[[1]], inner_it=100, J=50, it=2, criterion="C2")$design
  
  colnames(X0) = newNamesArray                                          
  
  #Settings of values between [upb, lob]
  if(returnDataInParameterRange){
    
    deltaMatrix = matrix(rep((new_upb - new_lob),nSimu), nSimu, n, byrow = TRUE)
    lobMatrix = matrix(rep(new_lob, nSimu), nSimu, n, byrow = TRUE)
    
    X1 = deltaMatrix * X0 + lobMatrix;
    
  } else {
    
    X1 = X0
    
  }
  
  if (displayGraph){
    
    pairs(X1)
    #Histogram Creation
    par(mfrow=c(1,n))
    
    for(i in 1:n) {
      
      hist(X1[,i],main="",xlab = newNamesArray[i])
      
    }
    
  }
  
  if (!returnOnlyModifiedValues){
    
    n = length(lob)
    returnMatrix <- vector()
    
    j = 1
    for (i in 1:n){
      
      if (positionIndex[i] != -1){
        
        newVector = X1[, positionIndex[i]]
        
      }else{
        
        newVector = rep(lob[i], nSimu)
        
      }
      
      returnMatrix[((i - 1)*nSimu + 1):(i*nSimu)] = newVector
      
    }
    
    returnMatrix = matrix(returnMatrix, ncol = n, nrow = nSimu)
    colnames(returnMatrix) = namesArray
    return(returnMatrix)
    
  } else {
    
    colnames(X1) = newNamesArray
    return(X1)
    
  }
  
}

generateInputPytonFile <- function(X) {
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  fileName = "inputDataPython.txt"
  copyfileName = "inputDataCopy.txt"
  
  n_simu = nrow(X)
  d = ncol(X)
  
  write.table(X, file = fileName, append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE)
  write.table(X, file = copyfileName, append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE)
  
}

importXMLOutputFile <- function(xmlFileName){
  
  try({
    
    currentDir = dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(currentDir)
    
  })
  
  outputData = read_xml(xmlFileName)
  
  
  
  # inputs extraction
  
  inputs = xml_child(outputData, "inputs")
  input_values = vector()
  input_names = vector()
  
  for (input in xml_children(inputs)){
    
    input_names = c(input_names, xml_attr(input, "name"))
    input_values = c(input_values, strsplit(xml_attr(input, "values"), ";")[[1]])
    
  }
  
  n_rows = length(strsplit(xml_attr(input, "values"), ";")[[1]])
  n_col = length(input_names)
  
  input_values = as.double(input_values)
  input_values = matrix(input_values, nrow = n_rows, ncol = n_col, byrow = FALSE)
  input_df = data.frame(input_values)
  
  names(input_df) = input_names
  
  
  
  # outputs extraction
  
  outputs = xml_child(outputData, "outputs")
  output_values = vector()
  output_names = vector()
  
  for (output in xml_children(outputs)){
    
    output_names = c(output_names, xml_attr(output, "name"))
    output_values = c(output_values, strsplit(xml_attr(output, "values"), ";")[[1]])
    
  }
  
  n_rows = length(strsplit(xml_attr(output, "values"), ";")[[1]])
  n_col = length(output_names)
  
  output_values = as.double(output_values)
  output_values = matrix(output_values, nrow = n_rows, ncol = n_col, byrow = FALSE)
  output_df = data.frame(output_values)
  
  names(output_df) = output_names
  
  return_df = list("inputs" = input_df, "outputs" = output_df)
  
  return(return_df)
  
}

importTXTOutputFile <- function(outputfileName = "outputData.txt", conditionCheck = -1){
  
  try({
    
    currentDir = dirname(rstudioapi::getActiveDocumentContext()$path)
    setwd(currentDir)
    
  })
  
  returnData = read.table(outputfileName, header = FALSE)
  expInputNames = (getPrameterSet(getAlsoFirstTryValues = getAlsoFirstTryValues))["dataNamesArray"]
  
  updatedReturnData <- vector()
  updatedExpData <- vector()
  
  validExpInputNames <- vector()
  nData = nrow(returnData)
  
  nRetData = 0
  nExpData = 0
  
  for (i in 1:(ncol(returnData) - 2) ){
    
    if (i <= nrow(expInputNames)){
      
      if (sd(returnData[[i]]) != 0){
        
        updatedExpData = c(updatedExpData, returnData[[i]])
        validExpInputNames = c(validExpInputNames, expInputNames[[1]][i])
        nExpData = nExpData + 1
        
      }
      
      
    } else {
      
      updatedReturnData = c(updatedReturnData, returnData[[i]])
      nRetData = nRetData + 1
      
    }
    
  }
  
  conditionArray_Tout = returnData[[i + 1]]
  conditionArray_HE_eta = returnData[[i + 2]]
  
  returnData <- matrix(data = as.numeric(updatedReturnData), nrow = nData, ncol = nRetData, byrow = FALSE)
  expInputData <- matrix(data = as.numeric(updatedExpData), nrow = nData, ncol = nExpData, byrow = FALSE)
  
  #display condition
  colnames(expInputData) = validExpInputNames
  optionalParameter <- list(colourScaleStyle = viridisLite::cividis)
  
  conditionMatrix = matrix(conditionArray_Tout, nrow = nData, ncol = 1)
  plotOverPairs(expInputData, conditionMatrix, optionalParameter)
  
  conditionMatrix = matrix(conditionArray_HE_eta, nrow = nData, ncol = 1)
  plotOverPairs(expInputData, conditionMatrix, optionalParameter)
  
  updatedReturnData <- vector()
  updatedExpData <- vector()
  
  j = 1
  nExpData = ncol(expInputData)
  nRetData = ncol(returnData)
  
  for (i in 1:nrow(returnData)){
    
    if ((conditionArray_Tout < 91) & (conditionArray_HE_eta < 1)){
      
      updatedReturnData[((j - 1)*nRetData + 1):(j*nRetData)] <- returnData[i,]
      updatedExpData[((j - 1)*nExpData + 1):(j*nExpData)] <- expInputData[i,]
      
      j = j + 1
      
    }
    
  }
  
  
  print(paste(j, " Data accepted over ", nrow(returnData)))
  
  updatedReturnData <- matrix(data = as.numeric(updatedReturnData), nrow = (j - 1), ncol = nRetData, byrow = TRUE)
  updatedExpData <- matrix(data = as.numeric(updatedExpData), nrow = (j - 1), ncol = nExpData, byrow = TRUE)
  
  colnames(updatedExpData) = validExpInputNames
  updatedExpData <- convertPermeabiltyInDarcy(updatedExpData)
  updatedExpData <- as.data.frame(updatedExpData)
  
  metamodelParamName <- getMetamodelNames(isProductionWell, isPeacemanAnalysis)$metamodelParamName
  
  colnames(updatedReturnData) = metamodelParamName
  updatedReturnData = applyMetamodelModifier(updatedReturnData, isProductionWell, isPeacemanAnalysis)
  
  return(list("inputs" = updatedExpData, "outputs" = updatedReturnData))
  
}

generateMetamodels <- function(data, metamodelType = "km", metamodelFormula = ~1, residualSigmaMax = -1){
  
  eliminateStrangeResidual = (residualSigmaMax >= 1)
  nRetData = ncol(data$outputs)
  
  metamodelName <- names(data$outputs)
  dataNames <- names(data$inputs)
  
  metamodelArray = list()
  
  for (i in 1:nRetData){
    
    if (metamodelType == "km"){
      
      ## KRIGING METAMODEL
      newMetamodel <- km(formula=~1, design = data$inputs, response = (data$outputs)[,i], covtype="matern3_2")
      colnames(newMetamodel@y) = metamodelName[i]
      
      par(mar = rep(2, 4))
      plot(newMetamodel)
      
      
    } else { 
      
      ## RESPONSE SURFACE MODEL
      forumlaAsCharacter = as.character(metamodelFormula)
      
      if (forumlaAsCharacter[2] == 1){
        
        ## FORMULA GENERATION
        
        formulaString = metamodelName[i]
        formulaString = paste(formulaString, " ~ SO(")
        
        nData <- length(dataNames)
        
        if (nData > 1){
          
          for (k in 1:(nData - 1)){
            
            formulaString = paste(formulaString, dataNames[k], ", ")
            
          }
          
        }
        
        formulaString = paste(formulaString, dataNames[nData], ")")
        metamodelFormula = as.formula(formulaString)
        
      } else {
        
        ## FORMULA UPDATE
        
        formulaString = metamodelName[i]
        formulaString = paste(formulaString, " ~ ")
        formulaString = paste(formulaString, forumlaAsCharacter[3])
        
        metamodelFormula = as.formula(formulaString)
        
      }
      
      ## METAMODEL GENERATION
      metamodelData = data$inputs
      metamodelData[metamodelName[i]] <- (data$outputs)[,i]
      newMetamodel <- rsm(formula = metamodelFormula, data = metamodelData)
      
      
      ## ELIMINATE NON STANDARD RESIDUAL
      
      if(eliminateStrangeResidual){
        
        res = calculateResidual(newMetamodel)
        res = res/abs(metamodelData[, ncol(metamodelData)])
        
        meanResidual = abs(mean(res))
        sdResidual = sd(res)
        
        updatedMetamodelData = vector()
        
        j = 1
        nParam = ncol(metamodelData)
        
        for (k in 1:nrow(metamodelData)){
          
          if (abs(res[k] - meanResidual) < residualSigmaMax*sdResidual){
            
            updatedMetamodelData[((j - 1)*nParam + 1):(j*nParam)] <- metamodelData[k,]
            j = j + 1
            
          }
          
        }
        
        updatedMetamodelData <- matrix(data = as.numeric(updatedMetamodelData), nrow = (j - 1), ncol = nParam, byrow = TRUE)
        colnames(updatedMetamodelData) = colnames(metamodelData)
        updatedMetamodelData <- as.data.frame(updatedMetamodelData)
        
        print(metamodelFormula)
        newMetamodel <- rsm(formula = metamodelFormula, data = updatedMetamodelData)
        
        print(paste(nrow(metamodelData) - (j - 1), "experimental results with strange residual eliminated", " "))
        
      }
      
      ## DISPLAY RESIDUALS
      
      res = calculateResidual(newMetamodel)
      newMetamodelData = newMetamodel$data
      res = abs(res/newMetamodelData[, ncol(newMetamodelData)])
      
      par(mfrow=c(1,1))
      hist(res)
      
      ## DISPLAY VALUES
      
      dataToPlot = newMetamodel$data
      
      colorIntensity = dataToPlot[metamodelName[i]]
      colorIntensity$residual = res
      
      dataToPlot[metamodelName[i]] <- NULL
      
      plotOverPairs(dataToPlot, as.matrix(colorIntensity))
      
    }
    
    metamodelArray[[i]] <- newMetamodel
    
    Q2_LOO1 <- Q2Analysis(newMetamodel)
    print(paste('Q2 value for metamodel ', metamodelName[i], ': ', Q2_LOO1))
    
  }
  
  return (metamodelArray)
  
}



#<------------------- SENSITIVITY ANALYSIS FUNCTION --------------------->

generateSample <- function(xmlFileName, getAlsoFirstTryValues = FALSE, gaussianDistribution = FALSE, sampleNumber = 10000, getMeanValues = FALSE, getDeltaValue = FALSE){
  
  parameterValue = getPrameterValues(xmlFileName)
  samplesMatrix <- matrix()
  samplesNameArray <- array()
  
  limitsMatrix = matrix(c(parameterValue$dataLob, parameterValue$dataUpb, parameterValue$meanValue), ncol = length(parameterValue$dataLob), nrow = 3, byrow = TRUE)
  
  colnames(limitsMatrix) = parameterValue$dataNamesArray
  rownames(limitsMatrix) = c("lob", "upb", "meanValue")
  
  limitsMatrix = as.data.frame(transposeMatrix(limitsMatrix))
  
  j = 0
  for (i in 1:nrow(parameterValue)){
    
    lob = limitsMatrix$lob[i]
    upb = limitsMatrix$upb[i]
    meanValue = limitsMatrix$meanValue[i]
    ignoreValue = parameterValue$ignoreInSensitivity[i]
    
    if (upb < lob){
      
      tmp = upb
      upb = lob
      lob = tmp
      
    }
    
    if (lob != upb){
      
      samplesNameArray[j + 1] = parameterValue["dataNamesArray"][i,]
      
      if(getDeltaValue){
        
        sampleNumber = 1
        samplesMatrix[j + 1] = upb - lob
        
      } else {
        
        if(ignoreValue | getMeanValues){
          
          samplesMatrix[(j*sampleNumber + 1):((j + 1)*sampleNumber)] = rep(meanValue, sampleNumber)
          
        } else {
          
          if (gaussianDistribution){
            
            meanValue = (upb + lob)/2
            sdValue = abs((upb - lob)/4)
            
            samplesMatrix[(j*sampleNumber + 1):((j + 1)*sampleNumber)] = rnorm(sampleNumber, mean = meanValue, sd = sdValue)
            
          } else {
            
            samplesMatrix[(j*sampleNumber + 1):((j + 1)*sampleNumber)] = runif(sampleNumber, lob, upb)
            
          }
          
        }
        
      }
      
      j = j + 1
      
    }
    
  }
  
  samplesMatrix = matrix(data = samplesMatrix, nrow = sampleNumber, ncol = j, byrow = FALSE)
  colnames(samplesMatrix) = samplesNameArray 
  
  return(data.frame(samplesMatrix))
  
}

performSensitivityAnalysis <- function(metamodel, xmlFileName, getAlsoFirstTryValues = FALSE, calculateSobolIndex = TRUE, nSample = 10000, calculateOnlyInMeanPoint = FALSE, parameterValue = data.frame(), optionalAdditionalParameter = list(), plotRelativeValues = TRUE){
  
  metamodelClass = (class(metamodel))[1]
  metamodelName = getMetamodelName(metamodel)
  metamodelDimension = getMetamodelDimension(metamodel, xmlFileName)
  
  if("printData" %in% names(optionalAdditionalParameter)){
    
    printData = optionalAdditionalParameter$printData
    
  } else {
    
    printData = TRUE
    
  }
  
  if(calculateSobolIndex){
    
    if (metamodelClass == "km"){
      
      f1 <- function(x) {
        
        metamodelName = colnames(metamodel@y)
        
        returnArray <- predict(metamodel, x,type='UK',checkNames=FALSE)$mean
        returnArray = matrix(returnArray, length(returnArray), 1)
        colnames(returnArray) = metamodelName
        
        return(applyMetamodelModifier(returnArray, xmlFileName, TRUE))
        
      }
      
      sample1 <- generateSample(xmlFileName, gaussianDistribution = FALSE, sampleNumber = nSample)
      sample2 <- generateSample(xmlFileName, gaussianDistribution = FALSE, sampleNumber = nSample)
      
      indices.sobol <- sobol2007(f1,sample1,sample2) # utilisation de la fonction sobol2007
      
      print('Extimation Index from Sobol2007')
      print(indices.sobol)
      
      if (printData){
        
        mainEffect = indices.sobol[["S"]]
        totalEffect = indices.sobol[["T"]]
        
        mainEffect = matrix(as.matrix(mainEffect), nrow = 1)
        totalEffect = matrix(as.matrix(totalEffect), nrow = 1)
        
        colnames(mainEffect) = rownames(indices.sobol[["S"]])
        colnames(totalEffect) = rownames(indices.sobol[["T"]])
        
        rownames(mainEffect) = "mainEffect"
        rownames(totalEffect) = "totalEffect"
        
        plotSensitivityIndex(mainEffect,
                             deltaSensitivityArray = totalEffect,
                             metamodelName = metamodelName,
                             metamodelDimension = metamodelDimension,
                             plotRelativeValues = FALSE)
        
        
      }
      
      
    } else {
      
      linearIndex = metamodel$b
      B = metamodel$B
      deltaValues = generateSample(xmlFileName, getAlsoFirstTryValues, getDeltaValue = TRUE)
      
      if (length(parameterValue) == 0){
        
        if (calculateOnlyInMeanPoint){
          
          nSample = 1
          parameterValue <- generateSample(xmlFileName, getAlsoFirstTryValues, sampleNumber = nSample, getMeanValues = TRUE)
          
        } else {
          
          parameterSet = getPrameterValues(xmlFileName)
          expData <- optimalExperimentDesign(nSample, parameter = parameterSet, displayGraph = FALSE)
          parameterValue <- vector()
          j = 0
          
          for(i in 1:ncol(expData)){
            
            if (mean(expData[,i]) != 0){
              
              checkValue = sd(expData[,i])/mean(expData[,i])
              
            } else {
              
              checkValue = sd(expData[,i])
              
            }
            
            if (checkValue != 0 ) {
              
              parameterValue[(j*nSample + 1):((j + 1)*nSample)] = expData[,i]
              j = j + 1
              
            }
            
          }
          
          parameterValue = matrix(data = parameterValue, nrow = nSample, ncol = j, byrow = FALSE)
          
        }
        
      } else {
        
        nSample = nrow(parameterValue)
        
      }
      
      nParam = ncol(parameterValue)
      tmpVector <- matrix(numeric(nParam*nSample), nrow = nSample, ncol = nParam)
      
      for (i in 1:nSample){
        
        for (j in 1:nParam){
          
          tmpVector[i, j] = as.numeric(parameterValue[i, j])
          
        }
        
      }
      
      dotResult = dotProduct(tmpVector, B)
      quadraticIndex = numeric(nParam)
      sdQuadraticIndex = numeric(nParam)
      
      for (i in 1:nParam){
        
        quadraticIndex[i]  = mean(dotResult[, i])
        sdQuadraticIndex[i] = abs(sd(dotResult[, i])/mean(dotResult[, i]))
        
      }
      
      names(quadraticIndex) = names(linearIndex)
      names(sdQuadraticIndex) = names(linearIndex)
      
      deltaValues = as.numeric(deltaValues)
      
      linearIndex = linearIndex*deltaValues
      quadraticIndex = quadraticIndex*deltaValues
      
      globalIndex = linearIndex + 2*quadraticIndex
      
      relLinearIndex = getRelativeVector(linearIndex)
      relQuadraticIndex = getRelativeVector(quadraticIndex)
      relGlobalIndex = getRelativeVector(globalIndex)
      
      if (printData) {
        
        colorArray = vector()
        colorArray[3] = "brown"
        titleArray = c("Linear Variation Index", "Quadratic Variation Index", "Global Variation Index")
        
        sensitivityMatrix = matrix(c(linearIndex, quadraticIndex, globalIndex), nrow = 3, byrow = TRUE)
        deltaSensitivityMatrix = matrix()
        colnames(sensitivityMatrix) = names(linearIndex)
        
        plotSensitivityIndex(sensitivityMatrix, 
                             deltaSensitivityArray = deltaSensitivityMatrix,
                             titleArray = titleArray,
                             colors = colorArray, 
                             plotRelativeValues = plotRelativeValues, 
                             metamodelName = metamodelName, 
                             metamodelDimension = metamodelDimension,
                             plotBeside = FALSE)
        
      } else {
        
        if("indexElement" %in% names(optionalAdditionalParameter)){
          
          indexElement = optionalAdditionalParameter$indexElement
          
        } else {
          
          indexElement = "P_res"
          
        }
        if("indexType" %in% names(optionalAdditionalParameter)){
          
          indexType = optionalAdditionalParameter$indexType
          
        } else {
          
          indexType = "Global"
          
        }
        
        
        if(indexType == "Global"){
          
          return(relGlobalIndex[[indexElement]])
          
        } else {
          
          return(relQuadraticIndex[[indexElement]])
          
        }
        
        
        
      }
      
      
    }
    
  } else {
    
    set.seed(0)
    sample1 <- generateSample(xmlFileName, gaussianDistribution = TRUE, sampleNumber = nSample)
    z1<-f1(sample1)
    
    if (plotData) {
      
      h<-hist(z1, breaks=10, col="red", xlab=matemodelName, 
              main="Histogram with Normal Curve") 
      
      zfit<-seq(min(z1),max(z1),length=40) 
      yfit<-dnorm(zfit,mean=mean(z1),sd=sd(z1)) 
      yfit <- yfit*diff(h$mids[1:2])*length(z1) 
      lines(zfit, yfit, col="blue", lwd=2)
      
      d <- density(z1)
      plot(d, xlab = matemodelName, main=paste0("Kernel Density of", matemodelName,", mean = ",round(mean(z1), digits=4),", sd = ", round(sd(z1), digits=4)))
      polygon(d, col="blue", border="blue")   
      
    }
    
  }
  
}

calculateMetamodelDerivative <- function(metamodel, x){
  
  linearIndex = matrix(metamodel$b, 1, length(metamodel$b))
  
  B = metamodel$B
  dotResult = dotProduct(x, B)
  
  for (i in 1:nrow(x)){
    
    dotResult[i,] = linearIndex[1,] + 2*dotResult[i,]
    
  }
  
  
  
  return(dotResult)
  
} 



#<------------------------- PLOTTING FUNCTION --------------------------->

calculatePlotData <- function(metamodelArray, metamodelIndex, xVariableName, parameterName, numberOfPoint, numberOfParamCurves, xmlFileName, evaluationFunction, optionalAdditionalParameter, is2Dplot = TRUE){
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  #                                   INITIAL SETTINGS
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  parameterValue = getPrameterValues(xmlFileName)
  nParameter = nrow(parameterValue)
  
  metamodelNames = getMetamodelNames(xmlFileName)
  
  namesArray =  parameterValue$dataNamesArray
  lob = parameterValue$dataLob
  upb = parameterValue$dataUpb
  meanValueArray = parameterValue$meanValue
  
  metamodelDimension = metamodelNames$metamodelDimension
  metamodelNames = metamodelNames$metamodelParamName
  
  new_lob <- vector()
  new_upb <- vector()
  newNamesArray <- vector()
  newMean <- vector()
  
  j = 1
  for (i in 1:nParameter){
    
    if (lob[i] != upb[i]){
      
      new_lob[j] <- lob[i]
      new_upb[j] <- upb[i]
      newNamesArray[j] <- namesArray[i]
      newMean[j] <- meanValueArray[i]
      
      j = j + 1
      
    }
    
  }
  
  nParameter = length(new_lob)
  
  valueMax =  vector()
  valueMin =  vector()
  valueMean = vector()
  
  for (k in 1:nParameter){
    
    valueMax[k] = new_upb[k]
    valueMin[k] = new_lob[k]
    valueMean[k] = newMean[k]
    
  }
  
  if (parameterName == "nothing"){
    
    numberOfParamCurves = 1
    
    if(!is2Dplot){
      
      stop("ERROR: for 3D plot you must pass and data name!!")
      
    }
    
  }
  
  #<---------------------- GET DELTA VALUES LIST ------------------------->
  
  deltaValuesList = list()
  
  for (k in 1:nParameter){
    
    if (newNamesArray[k] == xVariableName){
      
      deltaValuesList[[xVariableName]] = (valueMax[k] - valueMin[k])/numberOfPoint
      
    } else if (newNamesArray[k] == parameterName){
      
      deltaValuesList[[parameterName]] = (valueMax[k] - valueMin[k])/numberOfParamCurves
      
    } 
    
  }
  
  if("deltaValuesList" %in% names(optionalAdditionalParameter)){
    
    optionalAdditionalParameter$deltaValuesList = deltaValuesList
    
  } else {
    
    optionalAdditionalParameter = c(optionalAdditionalParameter, deltaValuesList = deltaValuesList)
    
  }
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  #                                   FIELD INITIALIZATION
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  dataList <- list()
  metamodel = list(metamodelArray[[metamodelIndex]])
  minResult = 0
  maxResult = 0
  
  if(is2Dplot){
    
    #<-------------------------- 2D PLOT DATA ---------------------------------->
    
    for (i in 1:numberOfParamCurves){
      
      subExpData = numeric(numberOfPoint * (nParameter))
      subExpData = matrix(subExpData, nrow = numberOfPoint, ncol = (nParameter))
      subExpData = data.frame(subExpData)
      colnames(subExpData) = newNamesArray
      
      subExpDataResult = numeric(numberOfPoint)
      
      for (j in 1:numberOfPoint){
        
        for (k in 1:nParameter){
          
          if (newNamesArray[k] == xVariableName){
            
            subExpData[j, k] = (valueMax[k] - valueMin[k])/numberOfPoint*j + valueMin[k]
            
          } else if (newNamesArray[k] == parameterName){
            
            subExpData[j, k] = (valueMax[k] - valueMin[k])/numberOfParamCurves*i + valueMin[k]
            
          } else {
            
            subExpData[j, k] = valueMean[k]
            
          }
          
        }
        
      }
      
      subExpData <- convertPermeabiltyInDarcy(subExpData)
      results = (evaluateFunction(metamodelArray, subExpData, metamodelIndex, evaluationFunction, optionalAdditionalParameter))[[1]]
      subExpData[metamodelNames[metamodelIndex]] <- results
      
      dataList[[i]] <- subExpData
      
      if (i == 1){
        
        minResult = min(results)
        maxResult = max(results)
        
      } else {
        
        if (minResult > min(results)){
          
          minResult = min(results)
          
        }
        
        if (maxResult < max(results)){
          
          maxResult = max(results)
          
        }
        
      }
      
    }
    
  } else {
    
    #<-------------------------- 3D PLOT DATA ---------------------------------->
    
    if (numberOfPoint > 100){
      
      numberOfPoint = 100
      
    }
    
    subExpData = numeric((numberOfPoint ^ 2) * nParameter)
    subExpData = matrix(subExpData, nrow = (numberOfPoint ^ 2), ncol = (nParameter))
    subExpData = data.frame(subExpData)
    colnames(subExpData) = newNamesArray
    
    xVector = numeric(numberOfPoint)
    yVector = numeric(numberOfPoint)
    
    for (i in 1:numberOfPoint){
      
      for (j in 1:numberOfPoint){
        
        for (k in 1:nParameter){
          
          iRow = (i - 1)*numberOfPoint + j
          
          if (newNamesArray[k] == xVariableName){
            
            xVector[j] = (valueMax[k] - valueMin[k])/numberOfPoint*j + valueMin[k]
            subExpData[iRow, k] = xVector[j]
            
          } else if (newNamesArray[k] == parameterName){
            
            yVector[i] = (valueMax[k] - valueMin[k])/numberOfPoint*i + valueMin[k]
            subExpData[iRow, k] = yVector[i]
            
          } else {
            
            subExpData[iRow, k] = valueMean[k]
            
          }
          
        }
        
      }
      
    }
    
    subExpData <- convertPermeabiltyInDarcy(subExpData)
    results = (evaluateFunction(metamodelArray, subExpData, metamodelIndex, evaluationFunction, optionalAdditionalParameter))[[1]]
    
    results = matrix(results, ncol = numberOfPoint, nrow = numberOfPoint, byrow = TRUE)
    
    if (xVariableName == "k_res"){
      
      xVector = convertPermeabiltyInDarcy(xVector, isSimpleVector = TRUE)
      
    } else if (parameterName == "k_res"){
      
      yVector = convertPermeabiltyInDarcy(yVector, isSimpleVector = TRUE)
      
    }
    
    dataList[[1]] <- results
    dataList[[2]] <- xVector
    dataList[[3]] <- yVector
    
    minResult = min(results)
    maxResult = max(results)
    
  }
  
  returnList <- list()
  
  returnList[[1]] <- dataList
  returnList[[2]] <- minResult
  returnList[[3]] <- maxResult
  
  return(returnList)
  
}

plot2DGraph <- function(xmlFileName, metamodelArray, metamodelIndex, xVariableName, parameterName = "nothing", numberOfPoint = 40, numberOfParamCurves = 10, evaluationFunction = evaluateMetamodel, optionalAdditionalParameter = list()){
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  #                                   INITIAL SETTINGS
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  if("xmlFileName" %in% names(optionalAdditionalParameter)){
    
    optionalAdditionalParameter$xmlFileName = xmlFileName
    
  } else {
    
    optionalAdditionalParameter = c(optionalAdditionalParameter, xmlFileName = xmlFileName)
    
  }
  
  variablesNamesArray = getPrameterValues(xmlFileName, getNameAndDimensionOnly = TRUE)
  variablesDimension = "[ - ]"
  
  for (i in 1:nrow(variablesNamesArray)){
    
    if ((variablesNamesArray$dataNamesArray)[i] == xVariableName){
      
      variablesDimension = (variablesNamesArray$dimensionArray)[i]
      break
      
    }
    
  }
  
  metamodelNames = getMetamodelNames(xmlFileName)
  metamodelDimension = metamodelNames$metamodelDimension
  metamodelNames = metamodelNames$metamodelParamName
  
  returnList = calculatePlotData(metamodelArray, metamodelIndex, xVariableName, parameterName, numberOfPoint, numberOfParamCurves, xmlFileName, evaluationFunction, optionalAdditionalParameter)
  
  dataList = returnList[[1]]
  minResult = returnList[[2]]
  maxResult = returnList[[3]]
  
  if (parameterName == "nothing"){
    
    numberOfParamCurves = 1
    
  }
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  #                                       PLOTTING 
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  par(mfrow=c(1,1))
  
  subExpData = dataList[[1]]
  
  xValues = (subExpData[xVariableName])[,]
  yValues = (subExpData[metamodelNames[metamodelIndex]])[,]
  
  xLabel = paste(xVariableName, variablesDimension, sep = "    ")
  yLabel = paste(metamodelNames[metamodelIndex], metamodelDimension[metamodelIndex], sep = "  ")
  
  plot(xValues, yValues, type="l", col="blue", lwd=1, xlab=xLabel, ylab = yLabel, ylim = c(minResult, maxResult))
  
  i = 2
  while (i <= numberOfParamCurves){
    
    subExpData = dataList[[i]]
    
    xValues = (subExpData[xVariableName])[,]
    yValues = (subExpData[metamodelNames[metamodelIndex]])[,]
    
    lines(xValues, yValues, pch = 18, col = "blue", type = "l", lty = i)
    
    i = i + 1
    
  }
  
}

plot3DGraph <- function(xmlFileName, metamodelArray, metamodelIndex, xVariableName, yVariableName, numberOfPoint = 40, evaluationFunction = evaluateMetamodel, optionalAdditionalParameter = list()){
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  #                                   INITIAL SETTINGS
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  if("xmlFileName" %in% names(optionalAdditionalParameter)){
    
    optionalAdditionalParameter$xmlFileName = xmlFileName
    
  } else {
    
    optionalAdditionalParameter = c(optionalAdditionalParameter, xmlFileName = xmlFileName)
    
  }
  
  variablesNamesArray = getPrameterValues(xmlFileName, getNameAndDimensionOnly = TRUE)
  xVariableDimension = "[ - ]"
  yVariableDimension = "[ - ]"
  
  for (i in 1:nrow(variablesNamesArray)){
    
    if ((variablesNamesArray$dataNamesArray)[i] == xVariableName){
      
      xVariableDimension = (variablesNamesArray$dimensionArray)[i]
      
    } else if ((variablesNamesArray$dataNamesArray)[i] == yVariableName) {
      
      yVariableDimension = (variablesNamesArray$dimensionArray)[i]
      
    }
    
  }
  
  metamodelNames = getMetamodelNames(xmlFileName)
  metamodelDimension = metamodelNames$metamodelDimension
  metamodelNames = metamodelNames$metamodelParamName
  
  returnList = calculatePlotData(metamodelArray, metamodelIndex, xVariableName, yVariableName, numberOfPoint, 1, xmlFileName, evaluationFunction, optionalAdditionalParameter, is2Dplot = FALSE)
  
  dataList  <- returnList[[1]]
  minResult <- returnList[[2]]
  maxResult <- returnList[[3]]
  
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  #                                       PLOTTING 
  
  #<--------------------------------------------------------------------------------------------->#
  #<--------------------------------------------------------------------------------------------->#
  
  par(mfrow=c(1,1))
  
  resultsMatrix <- dataList[[1]]
  xVector       <- dataList[[2]]
  yVector       <- dataList[[3]]
  
  xLabel = paste(xVariableName, xVariableDimension, sep = "    ")
  yLabel = paste(yVariableName, yVariableDimension, sep = "    ")
  
  xLabel <- list(title = xLabel)
  yLabel <- list(title = yLabel)
  
  
  
  #<---------------- CHECK FOR ADDITIONAL PARAMETERS ----------------->
  
  if("xRange" %in% names(optionalAdditionalParameter)){
    
    xLabel <- c(xLabel, range = optionalAdditionalParameter$xRange)
    
  }
  
  if("yRange" %in% names(optionalAdditionalParameter)){
    
    yLabel <- c(yLabel, range = optionalAdditionalParameter$yRange)
    
  }
  
  if("title" %in% names(optionalAdditionalParameter)){
    
    title =  optionalAdditionalParameter$title
    
  } else {
    
    title = paste(metamodelNames[metamodelIndex], 
                  metamodelDimension[metamodelIndex], 
                  sep = "  ")
    
  }
  
  if("contourList" %in% names(optionalAdditionalParameter)){
    
    contourList =  optionalAdditionalParameter$contourList
    
    if(!("showlabels" %in% names(optionalAdditionalParameter))){
      
      contourList = c(contourList, showlabels = TRUE)
      
    }
    
  } else {
    
    contourList = list(showlabels = TRUE)
    
  }
  
  
  #<-------------------------- PLOT DATA ---------------------------->
  
  fig <- plot_ly(
    
    x = xVector, 
    y = yVector, 
    z = resultsMatrix, 
    type = "contour",
    contours = contourList
    
  )
  
  fig <- fig %>% layout(title = title, xaxis = xLabel, yaxis = yLabel)
  fig
  
  
}

plotOverPairs <- function(data, colorIntensityMatrix, optionalAdditionalParameter = list()){
  
  
  #<--------------------------- SETTINGS --------------------------------->
  
  if("colourScaleStyle" %in% names(optionalAdditionalParameter)){
    
    colourScaleStyle = optionalAdditionalParameter$colourScaleStyle
    
  } else {
    
    colourScaleStyle = viridisLite::viridis
    
  }
  if("colourScaleNumber" %in% names(optionalAdditionalParameter)){
    
    colourScaleNumber = optionalAdditionalParameter$colourScaleNumber
    
  } else {
    
    colourScaleNumber = 400
    
  }
  if("pointType" %in% names(optionalAdditionalParameter)){
    
    pointType = optionalAdditionalParameter$pointType
    
  } else {
    
    pointType = 19
    
  }
  if("pointDimension" %in% names(optionalAdditionalParameter)){
    
    pointDimension = optionalAdditionalParameter$pointDimension
    
  } else {
    
    pointDimension = 4
    
  }
  
  
  colourScale = colourScaleStyle(colourScaleNumber)
  
  upper.panel <- function(x, y){
    
    colorIntensityVector = colorIntensityMatrix[, 1]
    
    maxIntensity = max(colorIntensityVector)
    minIntensity = min(colorIntensityVector)
    
    colorVector <- vector()
    colorPosition <- vector()
    
    for (j in 1:length(colorIntensityVector)){
      
      colorPosition[j] = colourScaleNumber - as.integer((colorIntensityVector[j] - minIntensity)/(maxIntensity - minIntensity)*(colourScaleNumber - 1))
      colorVector[j] = colourScale[colorPosition[j]]
      
    }
    
    points(x, y, col = colorVector, pch = pointType, cex = pointDimension)
    
  }
  
  lower.panel <- function(x, y){
    
    if (ncol(colorIntensityMatrix) > 1){
      
      colorIntensityVector = colorIntensityMatrix[, 2]
      
      
    } else {
      
      colorIntensityVector = colorIntensityMatrix[, 1]
      
    }
    
    maxIntensity = max(colorIntensityVector)
    minIntensity = min(colorIntensityVector)
    
    colorVector <- vector()
    colorPosition <- vector()
    
    for (j in 1:length(colorIntensityVector)){
      
      colorPosition[j] = colourScaleNumber - as.integer((colorIntensityVector[j] - minIntensity)/(maxIntensity - minIntensity)*(colourScaleNumber - 1))
      colorVector[j] = colourScale[colorPosition[j]]
      
    }
    
    points(x, y, col = colorVector, pch = pointType, cex = pointDimension)
    
  }
  
  
  #<--------------------------- PLOTTING --------------------------------->
  
  pairs(data, upper.panel = upper.panel, lower.panel = lower.panel)
  
  print("")
  
  for (k in 1:ncol(colorIntensityMatrix)){
    
    colorIntensityVector = colorIntensityMatrix[, k]
    maxIntensity = max(colorIntensityVector)
    minIntensity = min(colorIntensityVector)
    
    print(paste(colnames(colorIntensityMatrix)[k], " Intensity Range = [", maxIntensity, ", ", minIntensity, "]", sep = ""))
    
  }
  
  
}

displayErrorData <- function(xmlFileName){
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  outputfileName = "outputDataPython.txt"
  copyInputfileName = "inputDataCopy.txt"
  
  returnData = read.table(outputfileName, header = FALSE)
  expInputData = read.table(copyInputfileName, header = FALSE)
  expInputNames = (getPrameterValues(xmlFileName))["dataNamesArray"]
  
  validExpInputData <- vector()
  validExpInputNames <- vector()
  nData = nrow(expInputData)
  j = 0
  
  for(i in 1:ncol(expInputData)){
    
    if (mean(expInputData[,i]) != 0){
      
      checkValue = sd(expInputData[,i])/mean(expInputData[,i])
      
    } else {
      
      checkValue = sd(expInputData[,i])
      
    }
    
    if (checkValue != 0 ) {
      
      validExpInputNames[j + 1] = expInputNames[i,]
      validExpInputData[(j*nData + 1):((j + 1)*nData)] = expInputData[,i]
      j = j + 1
      
    }
    
  }
  
  validExpInputData = matrix(data = validExpInputData, nrow = nData, ncol = j, byrow = FALSE)
  expInputData <- as.data.frame(validExpInputData)
  colnames(expInputData) <- validExpInputNames
  
  nRetData = nrow(returnData)
  colorVector <- numeric(nRetData)
  
  for (i in 1:nRetData){
    
    if (returnData[i, 1] == -100){
      
      colorVector[i] = "firebrick2"
      
    } else {
      
      colorVector[i] = "springgreen3"
      
    }
    
  }
  
  pairs(expInputData, col = colorVector)
  
}

plotSensitivityIndex <- function(sensitivityArray, deltaSensitivityArray = vector(), titleArray = vector(),colors = vector(), plotRelativeValues = TRUE, plotBeside = TRUE, metamodelName = "", metamodelDimension = ""){
  
  nPlot = nrow(sensitivityArray)
  nProperties = ncol(sensitivityArray)
  propertiesName = colnames(sensitivityArray)
  
  par(mfrow=c(nPlot,1))
  
  for (i in 1:nPlot){
    
    arrayToPlot = sensitivityArray[i, ]
    legendName = rownames(sensitivityArray)[i]
    ylabel = paste("variation", metamodelDimension, sep = " ")
    
    if(!is.na(deltaSensitivityArray[i])){
      
      otherArray = deltaSensitivityArray[i, ]
      otherLegendName = rownames(deltaSensitivityArray)[i]
      
      if (plotRelativeValues){ 
        
        arrayToPlot = getRelativeVector(arrayToPlot, otherArray)
        
      } else {
        
        arrayToPlot = matrix(c(arrayToPlot, otherArray), ncol = nProperties, nrow = 2, byrow = TRUE)
        
      }
      
      rownames(arrayToPlot) = c(legendName, otherLegendName)
      colnames(arrayToPlot) = propertiesName
      plotLegend = TRUE
      
    } else {
      
      if (plotRelativeValues){
        
        arrayToPlot = getRelativeVector(arrayToPlot)
        ylabel = "relative variation"
        
      }
      
      plotLegend = FALSE
      
    }
    
    
    if(is.na(colors[i])){
      
      barplot(arrayToPlot,
              main = titleArray[i],
              xlab = "Properties",
              ylab = ylabel,
              beside = plotBeside,
              legend.text = plotLegend)
      
    } else {
      
      barplot(arrayToPlot,
              main = titleArray[i],
              xlab = "Properties",
              ylab = ylabel,
              col = colors[i],
              beside = plotBeside, 
              legend.text = plotLegend)
      
    }
    
    
  }
  
  par(mfrow=c(1,1))
  
}



#<------------------------ EVALUATION FUNCTION ------------------------->

evaluateFunction <- function(metamodelArray, x, metamodelIndex = -1, functionToEvaluate =  evaluateMetamodel, optionalAdditionalParameter = list()){
  
  returnData = list()
  
  if("metamodelDifference" %in% names(optionalAdditionalParameter)){
    
    metamodelDifference = optionalAdditionalParameter$metamodelDifference
    
  } else {
    
    metamodelDifference = FALSE
    
  }
  
  if (metamodelDifference){
    
    metamodelIndex = -1
    
  }
  
  if (metamodelIndex == -1){
    
    for (i in 1:length(metamodelArray)){
      
      returnData[[i]] <- (evaluateFunction(metamodelArray, x, i, functionToEvaluate, optionalAdditionalParameter))[[1]]
      
    }
    
  } else {
    
    metamodel = metamodelArray[[metamodelIndex]]
    returnData[[1]] <- functionToEvaluate(metamodel, x, optionalAdditionalParameter)[[1]]
    
  }
  
  if(metamodelDifference & length(returnData) > 1){
    
    newReturnData = list()
    newReturnData[[1]] <- abs((returnData[[2]] - returnData[[1]])/returnData[[1]])
    returnData = newReturnData
    
  }
  return(returnData)
  
}

evaluateMetamodel <- function(metamodel, x, optionalAdditionalParameter){
  
  invisible("evaluateMetamodel")
  
  if("xmlFileName" %in% names(optionalAdditionalParameter)){
    
    xmlFileName = optionalAdditionalParameter$xmlFileName
    
  } else {
    
    xmlFileName = "data.xml"
    
  }
  
  returnData = list()
  metamodelClass = (class(metamodel))[1]
  
  if (metamodelClass == "km"){
    
    metamodelName = colnames(metamodel@y)
    returnArray <- predict(metamodel, x,type='UK',checkNames=FALSE)$mean
    
  } else {
    
    metamodelName = as.character(metamodel$terms[[2]])
    returnArray <- predict(metamodel, x)
    
  }
  
  returnArray = matrix(returnArray, length(returnArray), 1)
  colnames(returnArray) = metamodelName
  returnData[[1]] <- applyMetamodelModifier(returnArray, xmlFileName, TRUE)
  
  return(returnData)
  
}

evaluateMetamodelDerivative <- function(metamodel, x, optionalAdditionalParameter){
  
  invisible("evaluateMetamodelDerivative")
  
  if("derivateIndex" %in% names(optionalAdditionalParameter)){
    
    derivateIndex = optionalAdditionalParameter$derivateIndex
    
  } else {
    
    derivateIndex = 1
    
  }
  
  returnData = list()
  metamodelClass = (class(metamodel))[1]
  
  if (metamodelClass != "km"){
    
    metamodelName = as.character(metamodel$terms[[2]])
    returnArray <- calculateMetamodelDerivative(metamodel, x)
    
  }
  
  returnArray = matrix(returnArray[, derivateIndex], nrow(returnArray), 1)
  colnames(returnArray) = metamodelName
  returnData[[1]] <- returnArray
  
  return(returnData)
  
}

getResidualInRange <- function(metamodel, x, optionalAdditionalParameter){
  
  invisible("getResidualInRange")
  
  returnData = list()
  metamodelClass = (class(metamodel))[1]
  
  if (metamodelClass == "km"){
    
    returnData[[1]] <- numeric(nrow(x))
    
  } else {
    
    if("performCalculationInMetamodelPoints" %in% names(optionalAdditionalParameter)){
      
      performCalculationInMetamodelPoints <- optionalAdditionalParameter$performCalculationInMetamodelPoints
      
    } else {
      
      performCalculationInMetamodelPoints = FALSE
      
    }
    
    if(performCalculationInMetamodelPoints){
      
      residuals = abs(metamodel$residuals)
      metamodelData = metamodel$data
      
      returnData[[1]] <- residuals/metamodelData[, ncol(metamodelData)]
      
    } else {
      
      variatedData = retrunVariatedData(x)$variatedData
      nData = nrow(variatedData)
      nVariatedParam = ncol(variatedData)
      
      metamodelData = metamodel$data
      metamodelResidual = metamodel$residual
      nResidual = length(residuals)
      
      for (i in range(nData - 1)){
        
        upList = list()
        downList = list()
        
        for (j in range(nVariatedParam)){
          
          upList[[j]] = variatedData[(i + 1), j]
          downList[[j]] = variatedData[i, j]
          
        }
        
        paramDict = data.frame(up = upList, down = downList)
        
        sum = 0
        counter = 0
        
        for (k in range(nResidual)){
          
          insideRange = TRUE
          
          for (j in range(nVariatedParam)){
            
            #...
            
          }
          
        }
        
      }
      
    }
    
    
  }
  
  return(returnData)
  
}

getLocalSensitivityIndex <- function(metamodel, x, optionalAdditionalParameter){
  
  invisible("getLocalSensitivityIndex")
  
  returnData = list()
  metamodelClass = (class(metamodel))[1]
  nPoints = nrow(x)
  
  if (metamodelClass == "km"){
    
    returnData[[1]] <- numeric(nPoints)
    
  } else {
    
    returnArray <- numeric(nPoints)
    
    #<----- IMPORT PARAMETERS ----->
    
    if("xmlFileName" %in% names(optionalAdditionalParameter)){
      
      xmlFileName = optionalAdditionalParameter$xmlFileName
      
    } else {
      
      xmlFileName = "data.xml"
      
    }
    if("nSimulationPerRow" %in% names(optionalAdditionalParameter)){
      
      nSimulationPerRow <- optionalAdditionalParameter$nSimulationPerRow
      
    } else {
      
      nSimulationPerRow = 10
      
    }
    if("performCalculationInMetamodelPoints" %in% names(optionalAdditionalParameter)){
      
      performCalculationInMetamodelPoints <- optionalAdditionalParameter$performCalculationInMetamodelPoints
      
    } else {
      
      performCalculationInMetamodelPoints = FALSE
      
    }
    
    if(!("indexElement" %in% names(optionalAdditionalParameter))){
      
      optionalAdditionalParameter = c(optionalAdditionalParameter, indexElement = "P_res")
      
    } 
    if(!("indexType" %in% names(optionalAdditionalParameter))){
      
      optionalAdditionalParameter = c(optionalAdditionalParameter, indexType = "Global")
      
    }
    if("printData" %in% names(optionalAdditionalParameter)){
      
      optionalAdditionalParameter$printData = FALSE
      
    } else {
      
      optionalAdditionalParameter = c(optionalAdditionalParameter, printData = FALSE)
      
    }
    
    if (performCalculationInMetamodelPoints){
      
      for (i in 1:nrow(x)){
        
        returnArray[i] <- performSensitivityAnalysis(metamodel, 1, 
                                                     xmlFileName, 
                                                     calculateOnlyInMeanPoint = TRUE, 
                                                     parameterValue = x[i,], 
                                                     optionalAdditionalParameter = optionalAdditionalParameter)
        
        
      }
      
    } else {
      
      parameters = getPrameterValues(xmlFileName)
      parametersNames = parameters$dataNamesArray
      variableNames = colnames(x)
      nParameter = ncol(x)
      
      
      #<----- MODIFY PARAMETERS ----->
      
      returnedList = retrunVariatedData(x, parameters)
      
      variatedData = returnedList$variatedData
      variatedDataPosition = returnedList$variatedDataPosition
      variatedDataName = colnames(variatedData)
      
      costantData = returnedList$costantData
      costantDataName = names(costantData)
      costantDataPosition = returnedList$costantDataPosition
      
      parameters = returnedList$parameters
      
      
      #<------- INITIALIZE EXP DATA -------->
      
      expData <- optimalExperimentDesign(nSimulationPerRow, 
                                         parameter = parameters, 
                                         returnOnlyModifiedValues = TRUE, 
                                         returnDataInParameterRange = FALSE)
      
      n = 0
      delta = vector()
      lob = vector()
      
      for (name in variatedDataName){
        
        name = paste("deltaValuesList", name, sep = ".")
        delta[n + 1] = as.numeric(optionalAdditionalParameter[[name]])
        lob[n + 1] = -delta[n + 1]/2
        n = n + 1
        
      }
      
      
      deltaMatrix = matrix(rep(delta,nSimulationPerRow), nSimulationPerRow, n, byrow = TRUE)
      expData = deltaMatrix * expData
      
      lobMatrix = matrix(rep(lob, nSimulationPerRow), nSimulationPerRow, n, byrow = TRUE)
      newLobMatrix = matrix(numeric(nSimulationPerRow), nSimulationPerRow, n)
      
      
      
      #<------- INITIALIZE CALC MATRIX -------->
      
      calculationMatrix = numeric(nSimulationPerRow * nParameter)
      calculationMatrix = matrix(calculationMatrix, nrow = nSimulationPerRow, ncol = nParameter)
      colnames(calculationMatrix) = variableNames
      
      for (i in 1:ncol(calculationMatrix)){
        
        for (j in 1:length(costantData)){
          
          k = costantDataPosition[j]
          calculationMatrix[, k] = rep(costantData[j], nSimulationPerRow)
          
        }
        
      }
      
      
      #<--- PERFORM CALCULATION ---->
      
      for (i in 1:nrow(x)){
        
        for (j in 1:ncol(variatedData)){
          
          newLobMatrix[, j] = lobMatrix[, j] + rep(variatedData[i, j], nSimulationPerRow)
          
        }
        
        calculationPoints = expData + newLobMatrix
        
        for (j in 1:ncol(variatedData)){
          
          k = variatedDataPosition[j]
          calculationMatrix[, k] = calculationPoints[, j]
          
        }
        
        calculationMatrix <- convertPermeabiltyInDarcy(calculationMatrix)
        
        returnArray[i] <- performSensitivityAnalysis(metamodel, 1, 
                                                     isProductionWell, 
                                                     isPeacemanAnalysis, 
                                                     calculateOnlyInMeanPoint = TRUE, 
                                                     parameterValue = data.frame(calculationMatrix), 
                                                     optionalAdditionalParameter = optionalAdditionalParameter)
        
        
        
      }
      
    }
    
    returnData[[1]] = returnArray
    
  }
  
  return(returnData)
  
}



#<-------------------- METAMODEL HANDLING FUNCTION ---------------------->

getMetamodelName <- function(metamodel){
  
  metamodelClass = (class(metamodel))[1]
  
  if (metamodelClass == "km"){
    
    metamodelName = colnames(metamodel@y)
    
  } else {
    
    metamodelName = as.character(metamodel$terms[[2]])
    
  }
  
  return(metamodelName)
  
}

getMetamodelDimension <- function(metamodel, xmlFileName){
  
  metamodelName = getMetamodelName(metamodel)
  metamodelNames = getMetamodelNames(xmlFileName)$metamodelParamName
  metamodelDimensions = getMetamodelNames(xmlFileName)$metamodelDimension
  
  k = match(metamodelName, metamodelNames)
  
  if (!is.na(k)){
    
    return(metamodelDimensions[k])
    
  } else {
    
    return("")
    
  }
  
}

getMetamodelModifier <- function(metamodel, xmlFileName){
  
  metamodelName = getMetamodelName(metamodel)
  metamodelNames = getMetamodelNames(xmlFileName)
  metamodelModifiers = metamodelNames$metamodelModifier
  metamodelNames = metamodelNames$metamodelParamName
  
  k = match(metamodelName, metamodelNames)
  
  if (!is.na(k)){
    
    return(metamodelModifiers[k])
    
  } else {
    
    return("")
    
  }
  
}

generateMetamodelFiles <- function(metamodel, xmlFileName, fileFolder){
  
  outputfileName = "t = 0.txt"
  outputfileName <- file.path(fileFolder, outputfileName)
  
  a = (metamodel$coefficients)[1]
  b = metamodel$b
  B = metamodel$B
  
  n_input = length(b)
  output_vector = matrix(, nrow = (n_input + 3), ncol = n_input)
  
  output_vector[1,1] = a
  output_vector[2,] = b
  output_vector[4:nrow(output_vector),] = B
  
  write.table(output_vector, file = outputfileName, na = "", append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE)
  
  
}

generateMetamodelInfoFile <- function(metamodelArr, xmlFileName, fileFolder = ""){
  
  currentDir = dirname(rstudioapi::getActiveDocumentContext()$path)
  
  outputfileName = "info.txt"
  
  if (fileFolder != ""){
    
    currentDir <- file.path(currentDir, fileFolder)
    
    if (!dir.exists(currentDir)){
      
      dir.create(currentDir)
      
    }
    
    outputfileName <- file.path(currentDir, outputfileName)
    
  }
  
  result_string = "Result Names: {"
  modifiers_string = "Result Modifiers: {"
  
  is_first = TRUE
  
  for (metamodel in metamodelArr){
    
    
    metamodelName = getMetamodelName(metamodel)
    metamodelModifier = getMetamodelModifier(metamodel, xmlFileName)
    
    if (is_first){
      
      is_first = FALSE
      result_string = paste(result_string, metamodelName, sep = "")
      modifiers_string = paste(modifiers_string, metamodelModifier, sep = "")
      
    } else {
      
      result_string = paste(result_string, ", ", metamodelName, sep = "")
      modifiers_string = paste(modifiers_string, ", ", metamodelModifier, sep = "")
      
    }
    
    metamodel_directory = file.path(currentDir, metamodelName)
    
    if (!dir.exists(metamodel_directory)) {
      
      dir.create(metamodel_directory)
      
    }
    
    generateMetamodelFiles(metamodel, xmlFileName, metamodel_directory)
    
  }
  
  result_string = paste(result_string, "}", sep = "")
  modifiers_string = paste(modifiers_string, "}", sep = "")
  arrayStringToWrite = c(result_string, modifiers_string, "Times Calculated [year]: {0}")
  
  x = getPrameterValues(xmlFileName)
  names = x$dataNamesArray
  meanValues = x$meanValue
  
  lineToWrite = "Parameter Names and default values: {"
  
  for (i in 1:length(names)){
    
    newDataString = paste("[", names[i], " = ", as.character(meanValues[i]), "]", sep = "")
    
    if (i != 1){
      
      dataString = paste(dataString, newDataString, sep = ", ")
      
    } else {
      
      dataString = newDataString
      
    }
    
  }
  
  lineToWrite = paste(lineToWrite, dataString, "}", sep = "")
  arrayStringToWrite = c(arrayStringToWrite, lineToWrite)
  
  fileConn<-file(outputfileName)
  writeLines(arrayStringToWrite, fileConn)
  close(fileConn)
  
}



#<-------------------------- SUPPORT FUNCTION ------------------------->

calculateResidual <- function(metamodel){
  
  metamodelClass = (class(metamodel))[1]
  
  if (metamodelClass == "km"){
    
    ## Leave One Out Prediction
    metamodelPrediction <- leaveOneOut.km(metamodel,'UK')$mean
    expResult = metamodel@y
    
  } else {
    
    metamodelName = as.character(metamodel$terms[[2]])
    metamodelData = as.matrix((metamodel$data))
    
    expResult = as.matrix((metamodel$data)[metamodelName])
    nExpPoints = ncol(metamodelData) - 1
    expPointValues = metamodelData[, 1:nExpPoints]
    
    metamodelPrediction <- predict(metamodel, as.data.frame(expPointValues))
    
  }
  
  residual = expResult - metamodelPrediction
  return(residual)
  
}

Q2Analysis <- function(metamodel){
  
  metamodelClass = (class(metamodel))[1]
  
  if (metamodelClass == "km"){
    
    expResult = metamodel@y
    
  } else {
    
    metamodelName = as.character(metamodel$terms[[2]])
    expResult = as.matrix((metamodel$data)[metamodelName])
    
  }
  
  residual = calculateResidual(metamodel)
  SR = residual^2
  
  Q2 = 1 - mean(SR)/var(expResult)
  return(Q2)
  
}

dotProduct <- function(matrix1, matrix2){
  
  nRow1 = nrow(matrix1)
  nCol1 = ncol(matrix1)
  nRow2 = nrow(matrix2)
  nCol2 = ncol(matrix2)
  
  if (nCol1 == nRow2) {
    
    outputMatrix = numeric(nRow1*nCol2)
    outputMatrix = matrix(outputMatrix, nrow = nRow1, ncol = nCol2)
    
    for (i in 1:nRow1){
      
      for (j in 1:nCol2){
        
        sum = 0
        
        for (k in 1:nCol1){
          
          sum = sum + matrix1[i, k]*matrix2[k, j]  
          
        }
        
        outputMatrix[i, j] =  sum
        
      }
      
    }
    
  } else {
    
    stop('ncol(matrix1) != nrow(matrix2): dotProduct could not be performed!!')
    
  }
  
  return(outputMatrix)
  
}

getRelativeVector <- function(inputVector, maxValuesVector = vector()){
  
  if (is.na(maxValuesVector[1])){
    
    relativeVector = abs(inputVector)
    relativeVector = relativeVector/sum(relativeVector)
    return(relativeVector)
    
  } else {
    
    nValue = length(inputVector)
    
    checkCrossZero = sign(inputVector*maxValuesVector)
    inputVector = abs(inputVector) 
    maxValuesVector = abs(maxValuesVector) 
    
    minValues = vector()
    maxValues = vector()
    
    for (i in 1:nValue){
      
      if (checkCrossZero[i] == -1 | checkCrossZero[i] == 0){
        
        minValues[i] = 0
        maxValues[i] = max(c(inputVector[i], maxValuesVector[i]))
        
      } else {
        
        minValues[i] = min(c(inputVector[i], maxValuesVector[i]))
        maxValues[i] = max(c(inputVector[i], maxValuesVector[i]))
        
      }
      
    }
    
    for (i in 1:nValue){
      
      if (maxValues[i] < minValues[i]){
        
        tmp = maxValues[i]
        maxValues[i] = minValues[i]
        minValues[i] = tmp
        
      }
      
    }
    
    deltaVector = maxValues - minValues
    deltaVector = deltaVector/sum(maxValues)
    minValues = minValues/sum(maxValues)
    
    returnMatrix = matrix(c(minValues, deltaVector), nrow = 2, byrow = TRUE)
    colnames(returnMatrix) = names(inputVector)
    
    return(returnMatrix)
    
  }
  
  
}

convertPermeabiltyInDarcy <- function(expData, isSimpleVector = FALSE){
  
  if (isSimpleVector){
    
    #expData = 1/(expData*1e12)
    expData = expData*1e12
    
  } else {
    
    if (! is.null(colnames(expData))){
      
      for (i in 1:ncol(expData)){
        
        if (colnames(expData)[i] == "k_res"){
          
          #expData[,i] = 1/(expData[,i]*1e12)
          expData[,i] = expData[,i]*1e12
          
        }
        
        if (colnames(expData)[i] == "flowRate"){
          
          expData[,i] = abs(expData[,i])
          
        }
        
      }
      
    }
    
  }
  
  return(expData)
  
}

retrunVariatedData <- function(data, parameters = data.frame()){
  
  hasParameter = !(length(parameters) == 0)
  
  if(hasParameter){
    
    parametersNames = parameters$dataNamesArray
    
  }
  
  j = 0
  variatedData = vector()
  variatedDataName = vector()
  variatedDataPosition = vector()
  
  m = 0
  costantData = vector()
  costantDataName = vector()
  costantDataPosition = vector()
  
  nPoints = nrow(data)
  variableNames = colnames(data)
  
  for (i in 1:ncol(data)){
    
    if(sd(data[, i]) != 0){
      
      #save data in a vector
      variatedData[(j*nPoints + 1):((j + 1)*nPoints)] = data[, i]
      variatedDataName[j + 1] = variableNames[i]
      variatedDataPosition[j + 1] = i
      j = j + 1
      
    } else {
      
      if (hasParameter) {
        
        #find position in parameter
        k = match(variableNames[i], parametersNames)
        
        #eliminate data from parameter
        parameters$dataLob[k] = -1
        parameters$dataUpb[k] = -1
        
      }
      
      costantData[m + 1] = data[1, i]
      costantDataName[m + 1] = variableNames[i]
      costantDataPosition[m + 1] = i
      m = m + 1
      
    }
    
  }
  
  variatedData = matrix(variatedData, nrow = nPoints, ncol = j)
  colnames(variatedData) = variatedDataName
  names(costantData) = costantDataName
  
  
  if (hasParameter) {
    
    returnList = list(variatedData = variatedData,
                      variatedDataPosition = variatedDataPosition,
                      costantData = costantData,
                      costantDataPosition = costantDataPosition,
                      parameters = parameters)
    
  }else{
    
    returnList = list(variatedData = variatedData,
                      variatedDataPosition = variatedDataPosition,
                      costantData = costantData,
                      costantDataPosition = costantDataPosition)
    
  }
  return(returnList)
  
}

applyMetamodelModifier <- function(values, xmlFileName, revertCalculation = FALSE, byRow = FALSE){
  
  if (byRow){
    
    values = transposeMatrix(values)
    
  } 
  
  valueNames = colnames(values)
  names = getMetamodelNames(xmlFileName)$metamodelParamName
  modifiers = getMetamodelNames(xmlFileName)$metamodelModifier
  
  for (k in 1:ncol(values)) {
    
    i = match(valueNames[k], names)
    
    if (!is.na(i)){
      
      modifier = modifiers[i]
      
      if (modifier == "log"){
        
        if (revertCalculation){
          
          values[, k] = 10**values[, k]
          
        } else {
          
          values[, k] = log10(abs(values[, k]))
          
        }
        
      } else if (modifier == "ln"){
        
        if (revertCalculation){
          
          values[, k] = exp(values[, k])
          
        } else {
          
          values[, k] = log(abs(values[, k]))
          
        }
        
      } else if (modifier == "exp"){
        
        if (revertCalculation){
          
          values[, k] = log(abs(values[, k]))
          
        } else {
          
          values[, k] = exp(values[, k])
          
        }
        
      } else if (modifier == "abs"){
        
        if (!revertCalculation){
          
          values[, k] = abs(values[, k])
          
        }
        
      }
      
    }
    
  }
  
  if (byRow){
    
    values = transposeMatrix(values)
    
  }
  
  return(values)
  
}

transposeMatrix <- function(inputMatrix){
  
  newColNum = nrow(inputMatrix) 
  newRowNum = ncol(inputMatrix)
  
  newColNames = rownames(inputMatrix)
  newRowNames = colnames(inputMatrix)
  
  newMatrix = matrix(inputMatrix, ncol = newColNum, nrow = newRowNum, byrow = TRUE)
  
  colnames(newMatrix) = newColNames
  rownames(newMatrix) = newRowNames
  
  return(newMatrix)
  
}




# ------------------------------------------------------------ #
# ------------------------------------------------------------ #
# ------------------------------------------------------------ #

#                       CALCULATION

# ------------------------------------------------------------ #
# ------------------------------------------------------------ #
# ------------------------------------------------------------ #


# <-------------- EXPERIMENTAL DESIGN GENERATION -------------->

n_simu = 300;

parameterSet = getPrameterSet()
expData <- optimalExperimentDesign(n_simu, parameter = parameterSet, displayGraph = TRUE)
generateInputPytonFile(expData)
head(expData)


# <----------------- METAMODEL GENERATION -------------------->

data = importXMLOutputFile("data.xml")
metamodelArr    <- generateMetamodels(data, metamodelType = "rsm", residualSigmaMax = 2)
metamodelArr_km <- generateMetamodels(data, metamodelType = "km")

displayErrorData()


# <------------------- SENSITIVITY ANALYSIS ------------------>

performSensitivityAnalysis(metamodelArr[[1]], "data.xml", calculateSobolIndex = TRUE,  nSample = 100)
performSensitivityAnalysis(metamodelArr_km[[1]], "data.xml", calculateSobolIndex = TRUE, nSample = 10000)



# <-------------------- 2D PLOT GENERATION ------------------->

plot2DGraph("data.xml", metamodelArr_km, 3, "P_in", "P_out")
plot2DGraph("data.xml", metamodelArr, 4, "m_brine", "x_in")


# <-------------------- 3D PLOT GENERATION ------------------->


plot3DGraph("data.xml", metamodelArr_km, 1, 
            "P_in", "Re")

plot3DGraph("data.xml",metamodelArr, 1, 
            "P_in", "Re")


# <----------------- SPECIAL PLOT GENERATION ----------------->


optionalParameter <- list(title = "d P valve / d Depth valve  [ bar/m ]", derivateIndex = 3)

plot3DGraph(metamodelArr, 1, 
            "PSI", 
            "P_bh",
            evaluationFunction = evaluateMetamodelDerivative,
            optionalAdditionalParameter = optionalParameter)

generateMetamodelInfoFile(metamodelArr, "data.xml", "metamodel")