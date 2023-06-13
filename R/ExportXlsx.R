#---- ----
# 
# Scripts exporting tables to Excel
#
# Coder : Sébastien Metz
# Coding: Decembre 2022 - 


#---- Fonction fun.xlsx.addTitle pour ajouter des titres ----
fun.xlsx.addTitle <- 
  function(sheet, 
           rowIndex, 
           title, 
           titleStyle){
    rows <- 
      createRow(sheet,
                rowIndex=rowIndex)
    sheetTitle <- 
      createCell(rows, 
                 colIndex = 1)
    setCellValue(sheetTitle[[1,1]],
                 title)
    setCellStyle(sheetTitle[[1,1]],
                 titleStyle)
  }



#---- Fonction fun.xlsx.addTitle pour ajouter des titres ----
fun.xlsx.addContent <- 
  function(nom_feuille,
           titre,
           tab_donnees,
           source = "",
           fichier = wb,
           style_titre = StyleTitle,
           style_source = StyleSubTitle,
           style_col = StyleColNames,
           style_row = StyleRowNames,
           large_col = 22.5
           ){
    
    
    # Création feuille
    feuille <- 
      createSheet(
        fichier, 
        sheetName = nom_feuille)
    # Modifier première colonne
    setColumnWidth(feuille, 
                   colIndex = 1, 
                   colWidth = large_col)
    # Ajouter un titre
    fun.xlsx.addTitle(
      feuille, 
      rowIndex = 1, 
      title = titre,
      titleStyle = style_titre)
    # Ajouter un sous-titre
    fun.xlsx.addTitle(
      feuille, 
      rowIndex = 2, 
      title = source,
      titleStyle = style_source)
    # Ajouter une table
    addDataFrame(
      as.data.frame(tab_donnees),
      feuille, 
      row.names = FALSE,
      startRow = 4, 
      startColumn = 1, 
      colnamesStyle = style_col,
      rownamesStyle = style_row)
    
  }


