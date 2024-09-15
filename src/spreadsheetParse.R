library(tidyverse)

# These are necessary parameters to identfiy in the spreadsheet
PRODUCT_NAME <- "product_name"
BRAND_NAME <- "brand_name"
PRICE_NAME <- "price"
DISCOUNT_NAME <- "discount"
columnConstants <- c(PRODUCT_NAME, BRAND_NAME, PRICE_NAME, DISCOUNT_NAME)

# Functions to set column names (if necessary)
setProductNameCol <- function(newProductName) PRODUCT_NAME = newProductName
setBrandNameCol <- function(newBrandName) BRAND_NAME = newBrandName
setPricenameCol <- function(newPriceName) PRICE_NAME = newPriceName
setDiscountCol <- function(newDiscountName) DISCOUNT_NAME = newDiscountName

# Function to validate the spreadsheet
validateSpreadSheet <- function(excelFilePath)
{
  return(true)
}

# Functions to parse the excel sheet
getRawExcelSpreadSheet <- function(excelFilePath)
{
  sheetDf <- readxl_example("datasets.xlsx")
  return(sheetDf)
}

spreadsheetFuncs <- c(getRawExcelSpreadSheet, validateSpreadSheet)

# Insert any functions to format spreadsheet further

package.skeleton(list = c(columnConstants, spreadsheetFuncs), name = "spreadsheetParser")