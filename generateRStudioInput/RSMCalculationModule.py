#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 30 15:08:15 2020

@author: PietroUngar
"""

import os
import numpy as np
import math
import warnings
import tkinter
from tkinter import filedialog
import shutil


class RSMData:
    
    def __init__(self, resultName, time, workingDirectory):
        
        self.resultName = resultName
        self.time = time
        
        filePath = os.path.join(workingDirectory, resultName, "t = " + time + ".txt")

        with open(filePath, "r") as file:
            
            self.a = float(file.readline())
            self.b = self.convertLineInFloatList(file.readline())
            file.readline()
            
            
            self.B = list()
            line = file.readline()
            
            while line:
                
                newList = self.convertLineInFloatList(line)
                self.B.append(newList)
                line = file.readline()
              
    def convertLineInFloatList(self, line):
        
        returnList = list()
        
        for element in line.split("\t"):
            
            returnList.append(float(element))
            
        
        return(returnList)
    
    def calculateResult(self, inputValueArray):
        
        result = np.dot(inputValueArray, self.B)
        result = self.b + result
        
        result = np.dot(result, inputValueArray)
        result = result + self.a
        
        return(result)
    

class RSMCalculator:
    
    def __init__(self, optionalWorkingDirectory = ""):

        if not os.path.isdir(optionalWorkingDirectory):
          
          self.workingDirectory = os.path.dirname(__file__)
        
        else:
        
          self.workingDirectory = optionalWorkingDirectory
        
        
        self.readInfoFile()
        self.RSMDataValues = dict()

        for resultName in self.resultNamesArray:

            resultNameDataList = list()

            for time in self.timesArray:
                
                resultNameDataList.append(RSMData(resultName, time, self.workingDirectory))
            
            self.RSMDataValues.update({resultName:resultNameDataList})
    
    def calculateValues(self, dataValue):
       
        
        #<-------------------- IMPORT DATA VALUE -------------------->
        
        dataKeys = list(dataValue.keys())
        nData = len(dataValue[dataKeys[0]])
        nParameter = len(self.parameterNamesArray)
        nInputParameter = len(dataKeys)
        
        parameterData = list()
        
        
        #<------- CHECK IF DATA EXIST OR PUT DEFAULT INSTEAD ------->
        
        for names in self.parameterNamesArray:
        
            calculationParameterList = list()
            
            if (names in dataKeys):
                
                calculationParameterList = dataValue[names]
                nInputParameter = nInputParameter - 1
                
            else:
                
                defaultValue = self.defaultValueArray[names]
                
                for i in range(nData):
                    
                    calculationParameterList.append(defaultValue)
        
            
            parameterData.append(calculationParameterList)
            
            
        #<------------ CHECK IF ALL DATA HAS BEEN USED ------------>
            
        if not(nInputParameter == 0):
            self.checkKeys(dataKeys)
            
            
            
        #<------------- PREPARE DATA FOR CALCULATION ------------->
        
        inputValueArrayList = list()
        
        for i in range(nData):
            
            inputValueArray = list()
            
            for j in range(nParameter):
                
                inputValueArray.append(parameterData[j][i])
                
            
            inputValueArrayList.append(inputValueArray)
            
            
            
        #<---------------- PERFORM CALCULATION ------------------>
        
        returnData = dict()
        
        for key in self.RSMDataValues.keys():
            
            calculationResult = dict()
            
            for RSMData in self.RSMDataValues[key]:
            
                resultlist = list()
                
                for i in range(nData):
                    
                    resultValue = RSMData.calculateResult(inputValueArrayList[i])
                    resultlist.append(self.updateReturnValueWithModifier(resultValue, key))
                    
                calculationResult.update({RSMData.time:resultlist})
            
            returnData.update({key:calculationResult})

        return(returnData)

    def updateReturnValueWithModifier(self, value, key):

        modifier = self.modifierDict[key]

        if modifier == "log":
                    
            newValue = pow(10, value)
            
        elif modifier == "ln":

            newValue = math.exp(value)
            
        elif modifier == "exp":

            newValue = np.log(value)

        else:

            newValue = value

        return(newValue)
            
    def checkKeys(self, dataValueKeys):
        
        wrongKeys = list()
        
        for key in dataValueKeys:
            
            if not (key in self.parameterNamesArray):
                
                wrongKeys.append(key)
        
        
        if not(len(wrongKeys) == 0):
               
            warningString = ""
            warningString = warningString + "\n\n<-------------------- !!! WARNING !!! -------------------->\n\n"
            
            warningString = warningString + "Some datas in dataValue dicitionary have WRONG KEYS:\n\n"
            
            warningString = warningString + "WRONG KEYS:\n"
            warningString = warningString +  str(wrongKeys) + "\n\n"
            
            warningString = warningString + "ACCEPTED KEYS:\n"
            warningString = warningString +  str(self.parameterNamesArray)
            
            warningString = warningString + "\n\n<-------------------- !!! WARNING !!! -------------------->\n\n"
            
            warnings.warn(UserWarning(warningString))
           
    def readInfoFile(self):
        
        filePath = os.path.join(self.workingDirectory, "info.txt")
        nLines = self.countLines(filePath)

        with open(filePath, "r") as infoFile:
            
            resultNamesString = infoFile.readline()

            if nLines == 4:
                modifierNamesString = infoFile.readline()

            timesString = infoFile.readline()
            parameterNamesAndDefaultString = infoFile.readline()
     
        self.resultNamesArray = self.getNameArrayInBracket(resultNamesString)
        self.timesArray = self.getNameArrayInBracket(timesString)

        if nLines == 4:
            modifierArray = self.getNameArrayInBracket(modifierNamesString)

        else:
            modifierArray = list()
            for i in range(len(self.resultNamesArray)):
                modifierArray.append("null")

        i = 0
        self.modifierDict = dict()
        
        for name in self.resultNamesArray:

            self.modifierDict.update({name:modifierArray[i]})
            i = i + 1

        self.parameterNamesArray = list()
        self.defaultValueArray = dict()
        
        parameterNamesAndDefaultString = self.getNameArrayInBracket(parameterNamesAndDefaultString)
        
        for nameAndDefaultValue in parameterNamesAndDefaultString:

            result = self.getNameArrayInBracket(nameAndDefaultValue, sep = ["[", "]", " = "])
            self.parameterNamesArray.append(result[0])
            self.defaultValueArray.update({result[0]:float(result[1])})
        
    @staticmethod
    def getNameArrayInBracket(line, sep=None):
       
        if sep is None:

            sep = ["{", "}", ", "]

        try:
          
          line = line.split(sep[0])
          line = line[1].split(sep[1])
        
        except:
          
          print(line)
          raise
        
        resultArray = line[0].split(sep[2])
        
        return(resultArray)

    @staticmethod
    def countLines(filePath):

        with open(filePath, "r") as file:

            counter = 0
            line = file.readline()

            while line:

                counter = counter + 1
                line = file.readline()

        return(counter)


class RSMParameterFilesGeneration:
    
    def __init__(self, parameterName, filePath = "", timeInYear = True, updateInfoFile = False):
        
        self.parameterName = parameterName
        self.timeInYear = timeInYear
        
        try:
            
            self.dataFile = open(filePath)
          
        except:

            root = tkinter.Tk()
            root.withdraw()
            currentDirectory = os.path.dirname(__file__)
            
            filePath = filedialog.askopenfilename(initialdir = currentDirectory, filetypes=(('text files', '*.txt'),))
            
            self.dataFile = open(filePath)

        self.extractDataFromFile()
        self.dataFile.close()

        self.generateCoefficientFiles()
        
        if (updateInfoFile):
            self.generateInfoFile()
        
        print("\nCoefficients Imported Correctly")
        
    def extractDataFromFile(self):

        #<------------------- EXTRACT TIME -------------------->
        
        timeLine = self.dataFile.readline()
        self.timeArray = list()
        
        for time in timeLine.split("\t"):

            if(self.timeInYear):

                timeSecond = float(time)
                timeYear = int(timeSecond/(8760*3600))
                
                self.timeArray.append(timeYear)

            else:

                self.timeArray.append(float(time))



        #<---------- GENERATE COEFFICIENT DICTIONARY ---------->

        self.RSMData = dict()

        for time in self.timeArray:

            a = 0.
            b = list()
            B = list()

            dataDictionary = dict()
            dataDictionary.update({"a":a})
            dataDictionary.update({"b":b})
            dataDictionary.update({"B":B})
            
            self.RSMData.update({time:dataDictionary})
            


        #<------- EXPORT INTERCPET LINE COEFFICIENTS -------->

        i = 0
        interceptLine = self.dataFile.readline()

        for interceptParameter in interceptLine.split("\t"):

            if i > 0:

                currentTime = self.timeArray[i - 1]
                self.RSMData[currentTime]["a"] = float(interceptParameter)
                
                
            i = i + 1
        


        #<-------- EXPORT PARAMETER NAMES AND LINEAR COEFFICIENTS -------->

        j = 0
        self.parametersIndex = dict()
        
        line = self.dataFile.readline()
        lineSplitted = line.split("\t")
        lineSplittedCheck = lineSplitted[0].split(":")

        while len(lineSplittedCheck) < 2:
        
            i = 0
            
            for parameter in lineSplitted:

                if i > 0:

                    currentTime = self.timeArray[i - 1]
                    self.RSMData[currentTime]["b"].append(float(parameter))

                else:

                    self.parametersIndex.update({parameter:j})
                    j = j + 1
                    
                    
                i = i + 1


            line = self.dataFile.readline()
            lineSplitted = line.split("\t")
            lineSplittedCheck = lineSplitted[0].split(":")

        
        #<---------- INITIALIZE QUADRATIC COEFFICIENTS MATRIX ---------->
            
        elementNumber = len(self.parametersIndex.keys())
        
        for time in self.timeArray:
            
            for i in range(elementNumber):
                
                rowList = list()
                
                for j in range(elementNumber):
                    
                    rowList.append(0.)
            
                
                self.RSMData[time]["B"].append(rowList)
            
    
    
        #<------- IMPORT QUADRATIC COEFFICIENTS MATRIX ELEMENTS -------->
        
        while line:
            
            k = -1
            lineSplitted = line.split("\t")
            
            for element in lineSplitted:
                
                if k < 0:
                    
                    lineSplittedCheckDiagonal = element.split("^")
                    elementsNames = element.split(":")
                    
                    if len(lineSplittedCheckDiagonal) == 2:
                        
                        elementsNames = [lineSplittedCheckDiagonal[0], lineSplittedCheckDiagonal[0]]
                        
                    i = self.parametersIndex[elementsNames[0]]
                    j = self.parametersIndex[elementsNames[1]]
                    
                else:
            
                    currentTime = self.timeArray[k]
                    self.RSMData[currentTime]["B"][i][j] += float(element)/2
                    self.RSMData[currentTime]["B"][j][i] += float(element)/2
                
                k = k + 1
            
            line = self.dataFile.readline()
      
    def generateCoefficientFiles(self):
        
        #<------------------- CHECK FILES DIRECTORY -------------------->
        
        currentDirectory = os.path.dirname(__file__)
        newDirectoryPath = os.path.join(currentDirectory, self.parameterName)
        
        if os.path.isdir(newDirectoryPath):
            shutil.rmtree(newDirectoryPath)
            
        os.makedirs(newDirectoryPath)

    
        #<------------- GENERATE FILES INISDE DIRECTORY -------------->
        
        for time in self.timeArray:
            
            filePath = os.path.join(newDirectoryPath, "t = " + str(time) + ".txt")
        
            stringToWtrite = str(self.RSMData[time]["a"]) + "\n"
            stringToWtrite = self.appendListElementToString(stringToWtrite, self.RSMData[time]["b"])
           
            stringToWtrite = stringToWtrite + "\n"
        
            for Bsublist in self.RSMData[time]["B"]:
                
                stringToWtrite = self.appendListElementToString(stringToWtrite, Bsublist)
            
            
            with open(filePath, "w") as file:
                file.write(stringToWtrite)
            
    def generateInfoFile(self):
        
        stringToWtrite = "Result Names: {" + self.parameterName + "}\n"
       
        stringToWtrite = stringToWtrite + "Times Calculated [year]: {" 
        stringToWtrite = self.appendListElementToString(stringToWtrite, self.timeArray, separator = ", ", endStringCharacter = "}\n")
   
        parameterNameList = list()
        for parameterName in self.parametersIndex.keys():
            
            stringToAppend = "[" + parameterName + " = defaultValue]"
            parameterNameList.append(stringToAppend)
    
        stringToWtrite = stringToWtrite + "Parameter Names and default values: {" 
        stringToWtrite = self.appendListElementToString(stringToWtrite, parameterNameList, separator = ", ", endStringCharacter = "}")
        
        currentDirectory = os.path.dirname(__file__)
        filePath = os.path.join(currentDirectory, "Info.txt")
        
        with open(filePath, "w") as file:
            file.write(stringToWtrite)
    
    def appendListElementToString(self, inputString, inputList, separator = "\t", endStringCharacter = "\n"):
        
        i = 0
        for element in inputList:
            
            if i == (len(inputList) - 1):
                
                separator = endStringCharacter
                
            inputString = inputString + "{:.5e}".format(element) + separator
            i = i + 1
        
        return(inputString)
   
    
if __name__ == "__main__":
    
    # program runs this stuffs only if it's not called via 'import'
    
    prova = RSMCalculator("metamodel")
    dataValue = {"m_perc":[0.5, 0.5, 0.5], "m_GEO":[50, 50, 50], "T_GEO_in":[110, 100, 100], "T_DH_in": [60, 70, 80]}
    firstResult = prova.calculateValues(dataValue)
    print(firstResult["W_net"]["0"])
