
library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)
library(shinyjqui)
library(shinyWidgets)


ui<-fluidPage(
  shinyjs::useShinyjs(),

  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
      }

    "))
  ),
  
  headerPanel("Display Table Generator"), 
  em("presentation ready table"),
  h3(""),
  
  tabsetPanel(
    tabPanel(strong('Data Input and Create a gt Table'), fluid=T,
      sidebarLayout(
         sidebarPanel(width = 3,
                fileInput("file", label = strong(h5("Choose your dataset from PC", bsButton("q1", label="", icon=icon("question"), style="info",size="extra-small"))),
                                  multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                bsPopover(id="q1",title="Input Data for Display Table",content="The data could be a tibble or a data frame ", placement = "right"),

                ## create a table stub
                prettyRadioButtons("gtstub", "Create A GT Table",choices=c("No stub","Create a table stub"), shape="curve", fill=TRUE, animation="pulse"),
                conditionalPanel(
                  condition = "input.gtstub == 'Create a table stub'",
                  textInput("rownameCol",h6("Select the column as row names"), "",width=300),
                  textInput("groupnameCol",h6("Select the column as group names"), "",width=300)
                     ),
                tags$hr(),

                ## add a header
                radioButtons("headerbutton", "Add A Table Header", c("hide","show"), inline=TRUE),
                conditionalPanel(
                 condition = "input.headerbutton == 'show'",
                   textInput("title",h6("Enter the title"), "", width = 300),
                   textInput("subtitle",h6("Enter the subtitle"),"",width =300),
                   actionButton("saveheader", "Save the header!")),
                
                ## conditional footnote
                radioButtons("footnotebutton", "Add A Conditional Footnote", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.footnotebutton == 'show'",
                    textInput("footnote",h6("Footnote"),"",width =300),
                    textInput("footnotecol",h6("Location (column)"),"",width =300),
                    textInput("footnoterow",h6("Location (row(s))"),"",width =300),
                    actionButton("savefootnote","Save Footnote !")),

                ## add footnote to column label
                radioButtons("footnotecollabbutton", "Add A Footnote to Column Label Itself", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.footnotecollabbutton == 'show'",
                    textInput("footnotecollab",h6("Footnote For column Label"),"",width =300),
                    textInput("footnotecollabloc",h6("Location (column(s))"),"",width =300),
                    radioButtons("glyphs",h6("Footnote Glyphs"), selected="letters", choices = c("letters"="letters","numbers"="NULL")),
                    actionButton("savefootnotecollab","Save Footnote For Column Label !")),
                
                
                ## Add a source note citation
                radioButtons("sourcenotebutton", "Add A Source Note Citation", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.sourcenotebutton == 'show'",
                    textInput("sourcenote",h6("Source Note"),"",width =300),
                    actionButton("savesourcenote","Save Source Note !"))
                ),
         mainPanel(
               h4("Raw data"),
               dataTableOutput(outputId="gtTable1", height="100%"),
               tags$hr(),
            #  h4("gt Table"),
            #  tableOutput("gttable1"),
               hr(),
               h4("Updatted gt Table"),
               gt_output("gttable2") 
                  )
                )
              ),
    
   tabPanel(strong('Modify Rows'), fluid=T,
        sidebarLayout(
            sidebarPanel(width = 3,
                ## re-order the row groups
                radioButtons("reorderGroupButton", "Re-order Row Groups", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.reorderGroupButton == 'show'",
                  textInput("groups",h6("Group order you want"), "", width = 300),
                  actionButton("saveReorderGroup", "Save the Reordering!")),
                
                ## add summary row
                radioButtons("addSummaryRowButton", "Add Summary Row", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.addSummaryRowButton == 'show'",
                  textInput("columnsForSummary",h6("Columns For Summary"), "", width = 300),
                  textInput("fns",h6("Function List"), "list()", width = 300),
                  actionButton("saveSummaryRow", "Save the Summary Row!"))
                ),
            mainPanel(
                 # h4("gt Table from last page"),
                 # gt_output("gttable3"),
                 # hr(),
                 h4("gt Table after row modification"),
                 gt_output("gttable4")

               )
              )
            ),
   
   tabPanel(strong('Modify Columns'), fluid=T,
      sidebarLayout(
          sidebarPanel(width = 3,
                ## set the alignment of the columns
                radioButtons("alignButton", "Set the Alignment of the Columns", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.alignButton == 'show'",
                  selectInput("aligns",h6("Alignment"), selected = "auto",choice=c("auto","left","right","center"), width = 300),
                  actionButton("saveAlign", "Save the Alignment!")),

                ## hide columns
                radioButtons("hidecolbutton", "Hide columns", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.hidecolbutton == 'show'",
                  textInput("hidecolumns",h6("Hide these columns"), "", width = 300),
                  actionButton("savehidecols", "Save the Hiding!")),

                ## re-label columns
                radioButtons("relabelColButton", "Re-Label columns", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.relabelColButton == 'show'",
                  textInput("relabelColumns",h6("Re_label these columns to"), "list2()", width = 300),
                  actionButton("saveRelabelCols", "Save the Relabeling!"))
                           

              ),
              mainPanel(
               # h4 ("gt Table from last page"),
               # gt_output("gttable5"),
               # hr(),
                h4("gt Table after column modification"),
                gt_output("gttable6")
                
              )
            )
          ),
   
   
   tabPanel(strong('Format Cell Body'), fluid=T,
      sidebarLayout(
          sidebarPanel(width = 3,
                ## format number
                radioButtons("formatnumberbutton", "Format the numbers", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.formatnumberbutton == 'show'",
                  textInput("numberformat",h6("Column(s) For Format"),"",width =300),
                  numericInput("scaleby",h6("Scale_by"),value=1/1E9,min=0,max=5, width =300),
                  textInput("pattern","Pattern", "{x}B"),
                  actionButton("saveformatnumber","Save Number Format!")),

                ## format numeric column with scientific notation
                radioButtons("formatnumbutton", "Format the 'num' column with scientific notation", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.formatnumbutton == 'show'",
                  textInput("scinumformat",h6("Column(s) For Format"),"",width =300),
                  numericInput("scinumdecimal",h6("Decimals"),value=3,min=1,max=5, width =300),
                  actionButton("saveformatnum","Save Scientific Format!")),

                ## format dates columns in 'date'
                radioButtons("formatdatebutton", "Format the dates column in 'Date'", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.formatdatebutton == 'show'",
                  textInput("dateformatc",h6("Column(s) For Format"),"",width =300),
                  textInput("dateformatr",h6("Conditional Rows For Format"),"",width =300),
                  numericInput("datestyle",h6("Date Style"),value=6,min=1,max=14,width=300),
                  actionButton("saveformatdate","Save Date Format!")),

                ## format currency
                radioButtons("formatcurrencybutton", "Format the Currency", c("hide","show"),inline=TRUE),
                conditionalPanel(
                  condition = "input.formatcurrencybutton == 'show'",
                  textInput("currencyformat",h6("Column(s) For Format"),"",width =300),
                  selectInput("currency",h6("Currency"),selected="USD", choices=c("AED","AFN","AUD","BRL","BWP",
                                                             "CAD","CNY","EUR","HKD","INR","JPY","SEK","USD","XAG"), width =300),
                  actionButton("saveformatcurrency","Save Currency Format!")),
                           
                 # numericInput("borderTopStyle","Table Border Top Style",value=1, min=1, max=6,width=300),
                  selectInput("colorTableBackg","Table Background Color",choices=c("white","lightcyan","blue","green","grey","purple"),width=300),
                  selectInput("colorHeaderBackg","Header Background Color",selected="white",choices=c("lightcyan","blue","green","grey","purple"),width=300),
                  selectInput("colorColumnLabelBackg","Column Label Background Color",selected="white",choices=c("white","blue","green","grey","purple"),width=300),
                  sliderInput("fontSize","Table Font Size",min=2, max=48,value=12, step=0.5,width=300),        
                  sliderInput("footnoteSize","Footnote Font Size",min=2, max=24,value=6, step=0.5,width=300)          
                                   
                           
              ),
              mainPanel(
                # h4 ("gt Table from last page"),
                # gt_output("gttable7"),
                # hr(),
                h4("gt Table after cell Formatting"),
                gt_output("gttable8")
                
              )
            )
          )
   
           
   
   
     )
    )
  
    

         
        

