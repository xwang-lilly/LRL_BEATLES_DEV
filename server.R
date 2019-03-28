
# https://towardsdatascience.com/utilizing-quosures-to-create-ultra-flexible-filtering-controls-in-r-shiny-f3e5dc461399
# https://github.com/rstudio/gt
library(shiny)
library(shinyjs)
library(gt)
library(tidyverse)
library(glue)
library(rlang)
options(shiny.deprecation.messages=FALSE)


server<-function(input,output,session){

# loading data
  rawdata <- reactive({
    inFile1 <- input$file
    if (is.null(inFile1)) {
      return(NULL)
    }
    read.csv(inFile1$datapath, row.names = 1)
  })
 
# show data   
  output$gtTable1 <- DT::renderDataTable({
    req(input$file)
    dataA <- rawdata()
    dataA
  })
 
# create / modify gt table
  gttableA <- reactive({
    if(input$gtstub=="No stub"){
      table <- gt(rawdata())
        }
    if(input$gtstub=="Create a table stub"){
      table <- gt(rawdata())
      table <- table %>%
      gt(
        rowname_col = input$rownameCol,
        groupname_col = input$groupnameCol
         )
    }
    return(table)
  })

 # show created/modified gt table
  output$gttable1 <- gt::render_gt({
    req(input$file)
     gttableA()
    })

 # updated gt table
  gttable1 <- reactive({
    table <- gttableA()

          ## add header
    if(!is.na(input$title)){
      table1 <- table %>%
        tab_header(
          title=input$title,
          subtitle=input$subtitle
        )
    if(input$saveheader){
      table <- table1
         }
    }
    
        ## add conditional footnote
        if(input$footnote!="" & input$footnotecol!="" & input$footnoterow!=""){
         table3 <- table %>%
            tab_footnote(
              footnote=input$footnote,
              locations=cells_data(
                columns=vars(!!rlang::sym(input$footnotecol)),
                rows=eval_tidy(parse_expr(input$footnoterow))
               )
              )
            if(input$savefootnote){
              table <- table3
             }
           }

         ## add footnote to column label itself
         if(input$footnotecollab!="" & input$footnotecollabloc!=""){
           columnC <- unlist(strsplit(input$footnotecollabloc,","))
           table4 <- table
           for(i in 1:length(columnC)){
           table4 <- table4 %>%
              tab_footnote(
              footnote=input$footnotecollab,
              locations=cells_column_labels(
                columns=vars(!!rlang::sym(columnC[i]))
                )
               )
             }
                 if(input$glyphs=="letters"){
                   table4 <- table4 %>%
                      tab_options(
                      footnote.glyph=letters
                     )}
                 if(input$glyphs=="numbers"){
                   table4 <- table4 %>%
                     tab_options(
                     footnote.glyph=numbers
                     )}

                 if(input$savefootnotecollab){
                     table <- table4
                   }
          }
        
        ## add source note
        if(input$sourcenote!=""){
            table5 <- table %>%
            tab_source_note(
              source_note=input$sourcenote
                  )
           if(input$savesourcenote){
            table <- table5
           }
         }
    table
  })
    
 #  show updated gt table
  output$gttable2 <- gt::render_gt({
      req(input$file)
    gttable1()
    })

 
  
 ##### modify row from here: 
    output$gttable3 <- gt::render_gt({
    req(input$file)
    gttable1()
  })
  
    gttable4 <- reactive({
      table <- gttable1()
    
        ## reorder row groups
       if(input$groups!=""){
            groupsA <- unlist(strsplit(input$groups,","))
            table9 <- table %>%
            row_group_order(
                    groups=groupsA
                 )
            if(input$saveReorderGroup){
            table <- table9
             }
          }
      
         ## add summary row
    if(input$columnsForSummary!="" ){
            table10 <- table  %>%
             summary_rows(
               columns=vars(!!rlang::sym(input$columnsForSummary)),
               fns=eval_tidy(parse_expr(input$fns))
             )
            if(input$saveSummaryRow){
              table <- table10
            }
         }
      table
       })
    
    output$gttable4 <- gt::render_gt({
      req(input$file)
      gttable4()
    })
    

    
    
    
    ##### modify columns from here: 
    output$gttable5 <- gt::render_gt({
      req(input$file)
      gttable4()
    })
    
    gttable6 <- reactive({
      table <- gttable1()   
               ## set the alignment of the columns
          if(input$aligns!=""){
            table10 <- table %>%
              cols_align(
                align=input$aligns,
                columns = TRUE
              )
            if(input$saveAlign){
              table <- table10
             }
           }

               ## hide columns
          if(input$hidecolumns!=""){
             table2 <- table
             columnA <- unlist(strsplit(input$hidecolumns,","))
             for(i in 1:length(columnA)){
              col <- columnA[i]
              columnB <- vars(!!rlang::sym(col))
              table2 <- table2 %>%
                cols_hide(
                  columns=columnB
                )
              }
           if(input$savehidecols){
              table <- table2
             }
           }

          ## relabel columns
          if(input$relabelColumns!="list2()"){
            table12 <- table  %>%
            cols_label(
                  .list=eval_tidy(parse_expr(input$relabelColumns))
                )
            if(input$saveRelabelCols){
              table <- table12
              }
            }
  ###### more modification of column
  
    table
    })
    
    output$gttable6 <- gt::render_gt({
      req(input$file)
      gttable6()
    })
    
    
    
        
    
   
    
    
    ##### format cell body from here: 
    output$gttable7 <- gt::render_gt({
      req(input$file)
      gttable6()
    })
    
    gttable8 <- reactive({
      table <- gttable6()  
      ## format number
      if(input$numberformat!="" & input$pattern!=""){
        columnF<- unlist(strsplit(input$numberformat,","))
        table8<- table
        for(i in 1:length(columnF)){
          table8 <- table8 %>%
            fmt_number(
              columns = vars(!!rlang::sym(columnF[i])),
              scale_by = input$scaleby,
              pattern=input$pattern
            )
        }
        if(input$saveformatnumber){
          table <- table8
        }
      }
      
      ## format 'num' columns with scientific notation
      if(input$scinumformat!="" & input$scinumdecimal!=""){
        columnD <- unlist(strsplit(input$scinumformat,","))
        table6<- table
        for(i in 1:length(columnD)){
          table6 <- table6 %>%
            fmt_scientific(
              columns = vars(!!rlang::sym(columnD[i])),
              decimals = input$scinumdecimal
            )
        }
        if(input$saveformatnum){
          table <- table6
        }
      }
      
      ## format date columns in "Date"
      if(input$dateformatc!=""){
        columnE <- unlist(strsplit(input$dateformatc,","))
        table7 <- table
        for(i in 1:length(columnE)){
          table7 <- table7 %>%
            fmt_date(
              columns = vars(!!rlang::sym(columnE[i])),
              rows = eval_tidy(parse_expr(input$dateformatr)),
              date_style = input$datestyle
            )
        }
        if(input$saveformatdate){
          table <- table7
        }
      }
      
      ## format currency
      if(input$currencyformat!="" & input$currency!=""){
        columnE <- unlist(strsplit(input$currencyformat,","))
        table8 <- table
        for(i in 1:length(columnE)){
          table8 <- table8 %>%
            fmt_currency(
              columns = vars(!!rlang::sym(columnE[i])),
              currency = input$currency
            )
        }
        if(input$saveformatcurrency){
          table <- table8
        }
      }
      
      # color, size, stype ...
      if(input$colorColumnLabelBackg!="" & input$colorColumnLabelBackg!=""){
      table20 <- table %>%
        tab_options(
          footnote.font.size =input$footnoteSize,
          heading.background.color=input$colorHeaderBackg,
          table.background.color=input$colorTableBackg,
          column_labels.background.color=input$colorColumnLabelBackg,
          table.font.size=input$fontSize
          
        )
        table <- table20
      
      }
      
      
      
  ########## more format here
      
   table
    })
    
    output$gttable8 <- gt::render_gt({
      req(input$file)
      gttable8()
    }) 
      
      

  }
  
  
  
  
  
  
  
   
 



  
 