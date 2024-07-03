SaveExcelWithSuperscripts=function(dt, filename){
  wb <- openxlsx::createWorkbook() # create workbook
  openxlsx::addWorksheet(wb, sheetName = "data") # add sheet
  openxlsx::writeData(wb, sheet=1, x=dt, xy=c(1, 1)) # write data on workbook
  
  for(i in grep("\\_\\[([A-z0-9\\s\\-]*)\\]", wb$sharedStrings)){
    # if empty string in superscript notation, then just remove the superscript notation
    if(grepl("\\_\\[\\]", wb$sharedStrings[[i]])){
      wb$sharedStrings[[i]] <- gsub("\\_\\[\\]", "", wb$sharedStrings[[i]])
      next # skip to next iteration
    }
    
    # insert additioanl formating in shared string
    wb$sharedStrings[[i]] <- gsub("<si>", "<si><r>", gsub("</si>", "</r></si>", wb$sharedStrings[[i]]))
    
    # find the "_[...]" pattern, remove brackets and udnerline and enclose the text with superscript format
    wb$sharedStrings[[i]] <- gsub("\\_\\[([A-z0-9\\s\\-]*)\\]", "</t></r><r><rPr><vertAlign val=\"superscript\"/></rPr><t xml:space=\"preserve\">\\1</t></r><r><t xml:space=\"preserve\">", wb$sharedStrings[[i]])
  }
  openxlsx::saveWorkbook(wb, file=filename, overwrite = TRUE)
}