#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libraries
library(shiny)

if(!require(ggplot2)){install.packages('ggplot2')} #plotting library
library(ggplot2)

if(!require(Hmisc)){install.packages('Hmisc')}#for standard correlations
library(Hmisc)

if(!require(MASS)){install.packages('MASS')}#for robust correlations
library(MASS)

###################################################################################################
# load data

load(url("https://github.com/rikunert/PLoS_API/raw/master/PLOSdata_2017-01-23_20k.RData"))
#loads data frame called PLOS_data into workspace
#columns in PLOS_data:
# 1 review_time
# 2 author_count
# 3 page_count
# 4 reference_count
# 5 subject_count
# 6 tweets
# 7 facebook
# 8 scopus_cites
# 9 reads
#10 saves
#11 id (doi)
#12 subject
#13 journal
#14 publication_date

########################################################
#prepare general look of plot (very clean)

theme_set(theme_bw(18)+#remove gray background, set font-size
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  legend.key = element_blank(),
                  legend.title = element_blank(),#remove all sorts of lines for a cleaner look
                  legend.position = 'top',#specify the legend to be on top
                  legend.direction = 'vertical'))#specify the legend to be arranged vertically

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #NON-INTERACTIVE###############
  titlePanel("Analysis of article level matrics in all Public Library of Science (PLoS)"),
  
  #INPUT 1#########################
  
  #article selection
  dateRangeInput(inputId = 'dateSelect', label = 'Publication date range:',
                 start = min(PLOS_data[,"publication_date"]),
                 end = max(PLOS_data[,"publication_date"])),#for choosing date range from which to select articles
  
  selectInput(inputId = 'journalSelect', label = 'Journal:',#for choosing journal from which to select articles
              c("all PLOS journals" = "all",
                "PLoS ONE" = "PLOS ONE",
                "PLoS Biology" = "PLOS Biology",
                "PLoS Computational Biology" = "PLOS Computational Biology",
                "PLoS Genetics" = "PLOS Genetics",
                "PLoS Medicine" = "PLOS Medicine",
                "PLoS Neglected Tropical Diseases", "PLOS Neglected Tropical Diseases",
                "PLoS Pathogens" = "PLOS Pathogens")),
  
  selectInput(inputId = 'subjectSelect', label = 'Subject:',#for choosing subject from which to select articles
              c("all subjects" = "all",
                "Biology and life sciences" = "Biology and life sciences",
                "Computer and information sciences" = "Computer and information sciences",
                "Earth sciences" = "Earth sciences",
                "Ecology and environmental sciences" = "Ecology and environmental sciences",
                "Engineering and technology" = "Engineering and technology",
                "Medicine and health sciences" = "Medicine and health sciences",
                "People and places" = "People and places",
                "Physical sciences" = "Physical sciences",
                "Research and analysis methods" = "Research and analysis methods",
                "Science policy" = "Science policy",
                "Social sciences" = "Social sciences")),
  
  #variable selection
  selectInput(inputId = 'xSelect', label = 'Variable on horizontal (x) axis',
              c("Citations (Scopus)" = "scopus_cites",
                "Views (PLOS & PMC)" = "reads",
                "Downloads (Mendeley & Citeulike)" = "saves",
                "Tweets" = "tweets",
                "Facebook engagements" = "facebook",
                "Number of authors" = "author_count",
                "Number of pages" = "page_count",
                "Number of References" = "reference_count",
                "Number of subject categories" = "subject_count",
                "Days between submission and publication" = "review time",
                "Publication date" = "publication_date"
              ),
              selected = "tweets"),#for choosing variable on x-axis
  
  selectInput(inputId = 'ySelect', label = 'Variable on vertical (y) axis',
              c("Citations (Scopus)" = "scopus_cites",
                "Views (PLOS & PMC)" = "reads",
                "Downloads (Mendeley & Citeulike)" = "saves",
                "Tweets" = "tweets",
                "Facebook engagements" = "facebook",
                "Number of authors" = "author_count",
                "Number of pages" = "page_count",
                "Number of References" = "reference_count",
                "Number of subject categories" = "subject_count",
                "Days between submission and publication" = "review_time",
                "Publication date" = "publication_date"
              )),#for choosing variable on y-axis
  
  #OUTPUT########################################
  plotOutput(outputId = 'scatterPlot'),
  
  textOutput(outputId = 'text'),
  
  #INPUT 2#########################
  
  #visualisation options
  checkboxGroupInput(inputId = 'fitSelect', label = 'Fit',
                     c("Correlation (Pearson: least squares regression)" = "pearson",
                       "Rank-order correlation (Spearman: least squares regression of ranks)" = "spearman",
                       "Robust regression (iterated reweighted least squares regression)" = "robust",
                       "Local fit (LOESS)" = "loess"),
                     selected = 'pearson'),#for adding fit to data cloud
  
  sliderInput(inputId = 'alphaSelect', label = 'Transparency of data points',
              value = 0.2, min = 0, max = 1)#for changing transparency of data points
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$scatterPlot = renderPlot({
    
    #select articles to plot
    
    #date restriction
    selection_date = PLOS_data[,"publication_date"] >= input$dateSelect[1] &
      PLOS_data[,"publication_date"] <= input$dateSelect[2]
    
    #journal restriction
    if (input$journalSelect == 'all') {
      selection_journal = rep(T, length(PLOS_data$journal)) 
    } else {#if a specific journal is chosen
      selection_journal = toupper(PLOS_data[,'journal']) == toupper(input$journalSelect)#convert to upper case to avoid mismatch due to case
    }
    
    #subject restriction
    if (input$subjectSelect == 'all') {
      selection_subject = rep(T, length(PLOS_data$subject)) 
    } else {#if a specific subject is chosen
      selection_subject = grepl(toupper(input$subjectSelect), toupper(PLOS_data[,'subject']), T, F)#convert to upper case to avoid mismatch due to case
    }
    
    selection = selection_date & selection_journal & selection_subject
    
    #select variables to plot
    if (input$xSelect == 'publication_date'){#a date on the x-axis
      x = PLOS_data[selection, input$xSelect]
    } else {#not a date on x-axis
      x = PLOS_data[selection, input$xSelect]  
    }
    
    y = PLOS_data[selection, input$ySelect]
    dat_corr = data.frame("x" = x,#x-axis
                          "y" = y)#y-axis
    
    #plot (including transparency options)
    scatterPlot = ggplot(dat_corr, aes(x = x, y = y)) +
      geom_point(size = 2, colour = "grey50", alpha = input$alphaSelect) +#add points
      labs(x = input$xSelect, y = input$ySelect)
    
    #add fit if required
    if (!length(input$fitSelect) == 0){
      
      #prepare fit text output if required
      if (max(grepl('pearson', input$fitSelect, T, F)) |
          max(grepl('spearman', input$fitSelect, T, F))){
        fitText = "Fit: "#
      }
      
      #Pearson correlation
      if (max(grepl('pearson', input$fitSelect, T, F))){
        
        #prepare text output
        Pea_r = rcorr(dat_corr$x, dat_corr$y, type = "pearson")
        fitText = sprintf("%s\nPearson r = %s", fitText, gsub("0\\.","\\.", sprintf('%1.2f', Pea_r$r[1,2])))
        
        #add fit line
        scatterPlot = scatterPlot +
          scale_color_grey() + #colour scale for lines
          stat_smooth(method = "lm", size = 1, se = FALSE,
                      aes(colour = "Correlation (Pearson)"),
                      lty = 1)
      }
      
      #Spearman correlation
      if (max(grepl('spearman', input$fitSelect, T, F))){
        
        #prepare text output
        Spe_r = rcorr(dat_corr$x, dat_corr$y, type = "spearman")
        fitText = sprintf("%s\nSpearman rho = %s", fitText, gsub("0\\.","\\.", sprintf('%1.2f', Spe_r$r[1,2])))
        
      }
      
      #add fit text output if required
      if (max(grepl('pearson', input$fitSelect, T, F)) |
          max(grepl('spearman', input$fitSelect, T, F))){
        
        XPos = YPos = Inf
        if (input$xSelect == 'publication_date') XPos = max(dat_corr$x)
        if (input$ySelect == 'publication_date') YPos = max(dat_corr$y)
        
        text_plotting = data.frame(x = XPos,
                                   y = YPos,
                                   t = fitText,
                                   hjust = 1, vjust = 1)
        
        scatterPlot = scatterPlot + 
          geom_text(data = text_plotting,
                    aes(x=x,y=y,hjust=hjust, vjust = vjust, label=t),
                    size = 5)
        
      }
      
      #Robust correlation
      if (max(grepl('robust', input$fitSelect, T, F))){
        
        #add fit line
        scatterPlot = scatterPlot +
          scale_color_grey() + #colour scale for lines
          stat_smooth(method = "rlm", size = 1, se = FALSE,
                      aes(colour = "Robust regression"),
                      lty = 1)
        
      }
      
      #LOESS fit
      if (max(grepl('loess', input$fitSelect, T, F))){
        
        #add fit line
        scatterPlot = scatterPlot +
          scale_color_grey() + #colour scale for lines
          stat_smooth(method = "loess", size = 1, se = FALSE,
                      aes(colour = "Local fit (LOESS)"),
                      lty = 1)
        
      }
    }
    scatterPlot
    
  })
  
  output$text = renderText({
    
    #select articles to plot
    
    #date restriction
    selection_date = PLOS_data[,"publication_date"] >= input$dateSelect[1] &
      PLOS_data[,"publication_date"] <= input$dateSelect[2]
    
    #journal restriction
    if (input$journalSelect == 'all') {
      selection_journal = rep(T, length(PLOS_data$journal)) 
    } else {#if a specific journal is chosen
      selection_journal = toupper(PLOS_data[,'journal']) == toupper(input$journalSelect)#convert to upper case to avoid mismatch due to case
    }
    
    #subject restriction
    if (input$subjectSelect == 'all') {
      selection_subject = rep(T, length(PLOS_data$subject)) 
    } else {#if a specific subject is chosen
      selection_subject = grepl(toupper(input$subjectSelect), toupper(PLOS_data[,'subject']), T, F)#convert to upper case to avoid mismatch due to case
    }
    
    selection = selection_date & selection_journal & selection_subject
    
    sprintf('Articles: %d (%1.1f per cent of total in data base)',
            sum(selection[!is.na(selection)]), round(sum(selection[!is.na(selection)])/length(selection) * 100))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

