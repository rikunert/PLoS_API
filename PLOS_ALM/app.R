# This is code for a Shiny web application. You can run the application by clicking
# the 'Run App' button in RStudio above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# (c) Richard Kunert (RiKunert@gmail.com) February 2017

#load libraries
library(shiny)

if(!require(colourpicker)){install.packages('colourpicker')} #colour choice button
library(colourpicker)

if(!require(ggplot2)){install.packages('ggplot2')} #main plotting library
library(ggplot2)

if(!require(ggExtra)){install.packages('ggExtra')} #plotting marginal histograms
library(ggExtra)

if(!require(Hmisc)){install.packages('Hmisc')}#for standard correlations
library(Hmisc)

if(!require(MASS)){install.packages('MASS')}#for robust correlations
library(MASS)

if(!require(rplos)){install.packages('rplos')}# PLOS API package
library(rplos)

###################################################################################################
# load data

load(url("https://github.com/rikunert/PLoS_API/raw/master/PLOSdata_2017-01-27_195k.RData"))
PLOS_data$page_count = as.integer(PLOS_data$page_count)#coerce into integer from factor (due to 'none' entries)
PLOS_data$reference_count[PLOS_data$reference_count <= 1] = NaN#remove articles with zero or just one reference from analysis (data entry errors)

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
                  plot.title = element_text(hjust = 0.5, face="bold"),
                  legend.key = element_blank(),
                  legend.title = element_blank(),#remove all sorts of lines for a cleaner look
                  legend.position = 'top',#specify the legend to be on top
                  legend.direction = 'vertical'))#specify the legend to be arranged vertically

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #NON-INTERACTIVE###############
  titlePanel("Analysis of article level metrics in Public Library of Science (PLoS)"),
  
  #INPUT 1#########################
  column(4,
         
         #variable selection
         wellPanel(tags$h3('Variable selection'),
                   selectInput(inputId = 'xSelect', label = 'Horizontal (x) axis',
                               c("Citations (Scopus)" = "scopus_cites",
                                 "Views (PLOS & PMC)" = "reads",
                                 "Downloads (Mendeley & Citeulike)" = "saves",
                                 "Tweets" = "tweets",#only collected since May 2012
                                 "Facebook engagements" = "facebook",
                                 "Number of authors" = "author_count",
                                 "Number of pages" = "page_count",
                                 "Number of References" = "reference_count",
                                 "Number of subject categories" = "subject_count",
                                 "Days between submission and publication" = "review_time",
                                 "Publication date" = "publication_date"
                               ),
                               selected = "publication_date"),#for choosing variable on x-axis
                   
                   selectInput(inputId = 'ySelect', label = 'Vertical (y) axis',
                               c("Citations (Scopus)" = "scopus_cites",
                                 "Views (PLOS & PMC)" = "reads",
                                 "Downloads (Mendeley & Citeulike)" = "saves",
                                 "Tweets" = "tweets",#only collected since May 2012
                                 "Facebook engagements" = "facebook",
                                 "Number of authors" = "author_count",
                                 "Number of pages" = "page_count",
                                 "Number of References" = "reference_count",
                                 "Number of subject categories" = "subject_count",
                                 "Days between submission and publication" = "review_time",
                                 "Publication date" = "publication_date"
                               ),
                               selected = 'review_time'),#for choosing variable on y-axis
                   
                   actionButton('variableSubmit', 'Submit variables')
         ),
         
         #article selection
         wellPanel(tags$h3('Article selection'),
                   
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
                                 "PLoS Neglected Tropical Diseases" = 'PLoS Neglected Tropical Diseases',
                                 "PLoS Pathogens" = "PLOS Pathogens",
                                 "all PLoS jounals except PLoS ONE" = "not PLoS ONE"),
                               selected = "PLOS Biology"),
                   
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
                   
                   actionButton('selectSubmit', 'Submit articles')
                   
         ),
         
         #article selected by double click in plot
         wellPanel(tags$h3('Chosen article'),
                   br(),
                   textOutput(outputId = 'articleSelect'),
                   actionButton(inputId = "linkButton", "Link")
         )
  ),
  
  #OUTPUT########################################
  column(8,
         plotOutput(outputId = 'scatterPlot',
                    
                    width = "100%",
                    
                    #add interactive (zoom and article link) functionality
                    click = "plot_click",
                    dblclick = "plot_dblclick",
                    brush = brushOpts(
                      id = "plot_brush",
                      resetOnNew = TRUE
                      
                    )),
         
         #add text under plot
         br(),#line break
         'The data are provided by ',a('PLOS', href="http://api.plos.org/api-display-policy/"),#this is a requirement by PLOS!
         '. The shinyApp was made by Richard Kunert (see code on ',a('github', href='https://github.com/rikunert/PLoS_API'),
         '). Licence: ',a('CC-BY-NC', href="https://creativecommons.org/licenses/by-nc/2.0/"),
         '. Version: 1.01. Data retrieved January 2017.',
         br(),
         tags$hr(),#horizontal line
         htmlOutput(outputId = 'text1'),
         tags$hr(),#horizontal line
         textOutput(outputId = 'text2'),
         br(),
         
         #INPUT 2#######################################
         #visualisation options
         
         wellPanel(tags$h3('Plotting options'),
                   
                   inputPanel(tags$h4('Summaries'),
                              checkboxGroupInput(inputId = 'fitSelect', label = 'Fits',
                                                 c("Correlation (Pearson: least squares regression)" = "pearson",
                                                   "Rank-order correlation (Spearman: least squares regression of ranks)" = "spearman",
                                                   "Robust regression (iterated reweighted least squares regression)" = "robust",
                                                   "Local fit (LOESS)" = "loess"),
                                                 selected = 'pearson'),#for adding fit to data cloud
                              radioButtons(inputId = 'marginSelect', label = 'Margins',
                                           c('None (enables interactivity)' = 'none',
                                             'Marginal histogram (disables interactivity)' = 'histogram',
                                             'Marginal density (disables interactivity)' = 'density'),
                                           selected = 'none')
                   ),
                   
                   inputPanel(tags$h4('Visuals'),
                              sliderInput(inputId = 'alphaSelect', label = 'Transparency of data points',
                                          value = 0.2, min = 0, max = 1),#for changing transparency of data points
                              
                              sliderInput(inputId = 'pointSelect', label = 'Size of data points',
                                          value = 2, min = 0, max = 10),#for changing size of data points
                              
                              sliderInput(inputId = 'lineSelect', label = 'Line thickness', step = 0.25,
                                          value = 1, min = 0, max = 5),#for changing line thickness
                              
                              colourInput(inputId = 'colourSelect', label = 'Colour of data points',
                                          showColour = 'background',#'both',#
                                          '#666666', palette = 'limited')#for changing colour of data points
                   ),
                   
                   inputPanel(tags$h4('Labels'),
                              textInput(inputId = 'xText', label = 'Label for horizontal (x) axis',
                                        value = '', placeholder = 'x-axis label'),
                              
                              textInput(inputId = 'yText', label = 'Label for vertical (y) axis',
                                        value = '', placeholder = 'y-axis label'),
                              
                              textInput(inputId = 'titleText', label = 'Title for plot',
                                        value = '', placeholder = 'plot title')
                   ),
                   
                   inputPanel(tags$h4('Size'),
                              sliderInput(inputId = 'titleSelect', label = 'Size of plot title',
                                          value = 24, min = 6, max = 40),#for changing title size,
                              
                              sliderInput(inputId = 'axisTitleSelect', label = 'Size of axis titles',
                                          value = 22, min = 6, max = 40),#for changing axis label size,
                              
                              sliderInput(inputId = 'axisLabelSelect', label = 'Size of axis labels',
                                          value = 20, min = 6, max = 40),#for changing axis label size,
                              
                              sliderInput(inputId = 'infoSelect', label = 'Size of info text in plot',
                                          value = 5, min = 1, max = 20),#for changing info text box text size,
                              
                              sliderInput(inputId = 'legendSelect', label = 'Size of legend text',
                                          value = 16, min = 6, max = 40),#for changing legend text size,
                              
                              radioButtons(inputId = 'plotSizeSelect', label = 'Dimensions of plot',
                                           choices = c('Automatic' = 'auto',
                                                       'Twitter (1024 x 512)' = 'twitter',
                                                       'Small (512 x 256)' = 'small'))
                   ),
                   
                   actionButton('visualSubmit', 'Submit changes')
                   
         )
         
  )
)

# Define server logic
server <- function(input, output) {
  
  #initialise variable for the interactive zooming xlim and ylim
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  #select articles to plot
  dat_corr = reactive({
    
    #date restriction
    selection_date = PLOS_data[,"publication_date"] >= input$dateSelect[1] &
      PLOS_data[,"publication_date"] <= input$dateSelect[2]
    
    #journal restriction
    if (input$journalSelect == 'all') {#if all journals
      selection_journal = rep(T, length(PLOS_data$journal)) 
    } else if(input$journalSelect == 'not PLoS ONE'){#if not PLoS ONE, otherwise all journals
      selection_journal = toupper(PLOS_data[,'journal']) != toupper('PLOS ONE')#convert to upper case to avoid mismatch due to case
    } else {#if a specific journal is chosen
      selection_journal = toupper(PLOS_data[,'journal']) == toupper(input$journalSelect)#convert to upper case to avoid mismatch due to case
    }
    
    #subject restriction
    if (input$subjectSelect == 'all') {
      selection_subject = rep(T, length(PLOS_data$subject)) 
    } else {#if a specific subject is chosen
      selection_subject = grepl(toupper(input$subjectSelect), toupper(PLOS_data[,'subject']), T, F)#convert to upper case to avoid mismatch due to case
    }
    
    #axes range restriction
    if (!is.null(ranges$x)) {
      selection_x = PLOS_data[,input$xSelect] >= ranges$x[1] & PLOS_data[, input$xSelect] <= ranges$x[2]
    } else {
      selection_x = rep(T, length(PLOS_data$id)) 
    }
    if (!is.null(ranges$y)) {
      selection_y = PLOS_data[,input$ySelect] >= ranges$y[1] & PLOS_data[, input$ySelect] <= ranges$y[2]
    } else {
      selection_y = rep(T, length(PLOS_data$id)) 
    }
    
    #combine restrictions for final selection of articles
    selection = selection_date & selection_journal & selection_subject & selection_x & selection_y
    
    #select variables to plot
    doi = PLOS_data[selection, 'id']
    x = PLOS_data[selection, input$xSelect]
    y = PLOS_data[selection, input$ySelect]
    dat_corr = data.frame("x" = x[!is.na(x) & !is.na(y)],#x-axis
                          "y" = y[!is.na(x) & !is.na(y)],#y-axis
                          'doi' = doi[!is.na(x) & !is.na(y)])#doi for interactivity
  })
  
  #control height and width of plot (implemented in renderPlot call below)
  heightSize = function(){
    if (input$plotSizeSelect == 'auto') {
      return('auto')
    } else if (input$plotSizeSelect == 'twitter') {
      return (512)
    } else if (input$plotSizeSelect == 'small') {
      return(512/2)
    }
  }
  
  widthSize = function(){
    if (input$plotSizeSelect == 'auto') {
      return('auto')
    } else if (input$plotSizeSelect == 'twitter') {
      return (1024)
    } else if (input$plotSizeSelect == 'small') {
      return(1024/2)
    }
  }
  
  #draw the plot
  output$scatterPlot = renderPlot({
    
    #Take dependency from submit buttons and zooming function
    input$variableSubmit == 1 | 
      input$visualSubmit == 1 | 
      input$selectSubmit == 1 |
      length(input$plot_dblclick) > 0
    
    #only execute the following code if one of submit buttons pressed or zooming function utilised (double click)
    isolate({ 
      
      #if no restriction on x- and y-axis, just use the minimum and maximum in data
      if (is.null(ranges$x)) ranges$x = c(min(dat_corr()$x[!is.na(dat_corr()$x)]), max(dat_corr()$x[!is.na(dat_corr()$x)]))
      if (is.null(ranges$y)) ranges$y = c(min(dat_corr()$y[!is.na(dat_corr()$y)]), max(dat_corr()$y[!is.na(dat_corr()$y)]))
      
      #put default column name on axes in case user did not specify axis labels
      if (input$xText == '') {xLabel = input$xSelect} else {xLabel = input$xText}
      if (input$yText == '') {yLabel = input$ySelect} else {yLabel = input$yText}
      
      #plot (including transparency options)
      scatterPlot = ggplot(dat_corr(), aes(x = x, y = y)) +
        geom_point(size = input$pointSelect, colour = input$colourSelect, alpha = input$alphaSelect) +#add points
        labs(x = xLabel, y = yLabel, title = input$titleText) +#add labels and title
        theme(axis.text = element_text(size = input$axisLabelSelect)) + #axis label size
        theme(axis.title = element_text(size = input$axisTitleSelect)) +#axis title size
        theme(plot.title = element_text(size = input$titleSelect))+#plot title size
        theme(legend.text = element_text(size = input$legendSelect))+#legend text size
        xlim(ranges$x) + ylim(ranges$y)#limits of axes
      
      
      #Add data information
      fitText = sprintf("Data provided by PLOS (%dk articles)", round(length(dat_corr()$x)/1000))#
      
      #add fit if required
      if (!length(input$fitSelect) == 0 && length(dat_corr()$x) > 5 && length(dat_corr()$y) > 5){#if adding fit information is desirable
        
        #Pearson correlation
        if (max(grepl('pearson', input$fitSelect, T, F))){
          
          #prepare text output
          Pea_r = rcorr(dat_corr()$x, dat_corr()$y, type = "pearson")
          
          #add result to info text in plot
          fitText = sprintf("%s\nPearson r = %s", fitText, gsub("0\\.","\\.", sprintf('%1.2f', Pea_r$r[1,2])))
          #fitText = sprintf('%s\nPearson r = %s, p = %1.4f', fitText, gsub("0\\.","\\.", sprintf('%1.2f', Pea_r$r[1,2])), Pea_r$P[1,2])#print more detailed result
          
          #add fit line
          scatterPlot = scatterPlot +
            scale_color_grey() + #colour scale for lines
            stat_smooth(method = "lm", size = input$lineSelect, se = FALSE,
                        aes(colour = "Correlation (Pearson)"),
                        lty = 1)
        }
        
        #Spearman correlation
        if (max(grepl('spearman', input$fitSelect, T, F))){
          
          #prepare text output
          Spe_r = rcorr(dat_corr()$x, dat_corr()$y, type = "spearman")
          
          #add result to info text in plot
          fitText = sprintf("%s\nSpearman rho = %s", fitText, gsub("0\\.","\\.", sprintf('%1.2f', Spe_r$r[1,2])))
          #fitText = sprintf('%s\nSpearman r = %s, p = %1.4f', fitText, gsub("0\\.","\\.", sprintf('%1.2f', Spe_r$r[1,2])), Spe_r$P[1,2])#print more detailed result
          
        }
        
        #Robust correlation
        if (max(grepl('robust', input$fitSelect, T, F))){
          
          #add fit line
          scatterPlot = scatterPlot +
            scale_color_grey() + #colour scale for lines
            stat_smooth(method = "rlm", size = input$lineSelect, se = FALSE,
                        aes(colour = "Robust regression"),
                        lty = 1)
          
        }
        
        #LOESS fit
        if (max(grepl('loess', input$fitSelect, T, F))){
          
          #add fit line
          scatterPlot = scatterPlot +
            scale_color_grey() + #colour scale for lines
            stat_smooth(method = "loess", size = input$lineSelect, se = FALSE,
                        aes(colour = "Local fit (LOESS)"),
                        lty = 1)
          
        }
      }
      
      #add fit text output
      XPos = YPos = Inf
      if (input$xSelect == 'publication_date') XPos = max(dat_corr()$x)
      if (input$ySelect == 'publication_date') YPos = max(dat_corr()$y)
      
      text_plotting = data.frame(x = XPos,
                                 y = YPos,
                                 t = fitText,
                                 hjust = 1, vjust = 1)
      
      scatterPlot = scatterPlot + 
        geom_text(data = text_plotting,
                  aes(x=x,y=y,hjust=hjust, vjust = vjust, label=t),
                  size = input$infoSelect)
      
      #add marginal distributions if input$marginSelect != 'none'
      if (input$marginSelect == 'histogram') {#add marginal histogram
        
        scatterPlot = ggMarginal(scatterPlot, 
                                 fill = input$colourSelect,
                                 type = input$marginSelect, margins = 'both',
                                 size = 3)#size refers to size of main plot (compared to marginals)
        
      } else if (input$marginSelect == 'density'){#add density plot
        
        scatterPlot = ggMarginal(scatterPlot, 
                                 colour = input$colourSelect, margins = 'both',
                                 xparams = list(size=input$lineSelect),#line thickness
                                 yparams = list(size=input$lineSelect),
                                 size = 3)#size refers to size of main plot (compared to marginals)
      }
      
      #print the actual plot
      scatterPlot
    }
    )
    
  },
  
  #implement plot dimensions
  width = widthSize,    
  height = heightSize
  
  )
  
  #The text under the horizontal line, telling the user what s/he can or cannot do
  output$text1 = renderUI({
    
    #Take dependency from submit button
    input$visualSubmit
    
    #only execute the following code if submit button pressed
    isolate({ 
      
      if (input$marginSelect != 'none'){#if plot not interactive
        HTML(sprintf('The plot is not interactive (marginal plots selected). Zooming and article selection disabled.'))
      } else {
        HTML(paste(
          sprintf('The plot is interactive in two ways.'),
          sprintf('1) Zooming. Hold and drag to create a rectangle. Double click the rectangle to zoom in. Double click without rectangle to remove zoom. '),
          sprintf('2) Link to article. Every data point you see is a link to the corresponding article. Single click article to choose it. It will appear in the left hand panel.'),
          sep = '<br/>'
        ))
      }
    })
  })
  
  #The lowest text, telling the user how many articles are in the analysis
  output$text2 = renderText({
    
    #Take dependency from submit buttons and zooming function
    input$selectSubmit | input$variableSubmit | length(input$plot_dblclick) > 0
    
    #only execute the following code if submit button pressed
    isolate( 
      
      sprintf('Articles: %d (%1.1f per cent of total in data base of %d articles)',
              length(dat_corr()$x), round(length(dat_corr()$x) / length(PLOS_data$id) * 100), length(PLOS_data$id))
    )
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush) && input$marginSelect == 'none') {#if there is a brush (highlighted area) on plot, if no marginal plots (which otherwise mess up this code)
      
      if (input$xSelect == 'publication_date') {
        ranges$x <- c(as.Date(brush$xmin, origin="1970-01-01"), as.Date(brush$xmax, origin="1970-01-01"))
      } else{
        ranges$x <- c(brush$xmin, brush$xmax)  
      }
      
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {#if no highlighted area and/or marginal plots displayed
      
      ranges$x = NULL
      ranges$y = NULL
      
    }
  })
  
  #reset axis limits to default when new variable for that axis chosen
  observeEvent(input$xSelect, ranges$x <- NULL)
  observeEvent(input$ySelect, ranges$y <- NULL)
  
  #reset axis limits to default when plot input changes
  observeEvent({
    input$marginSelect
    input$journalSelect
    input$dataSelect
    input$subjectSelect
  }, ignoreInit = T, {
    ranges$x <- NULL
    ranges$y <- NULL})
  
  #show article information
  output$articleSelect = renderText({
    
    if(input$marginSelect == 'none'){#interactivity only enabled without marginal plots
      
      #the article to link to
      selected_row = nearPoints(dat_corr(), input$plot_click,
                                threshold = input$pointSelect,#size of reactive area scaled with plotted point size
                                maxpoints = 1, #only return a single data point
                                addDist = F)#do not return distance to actual point
      
      if (length(selected_row$doi == 1)){ 
        
        selected_article = searchplos(q = selected_row$doi,#selected_row$doi,#search for potentially all articles
                                      fl='id, publication_date,
                                      author, journal,title',
                                      'doc_type:full',#only look for full articles
                                      start = 0,
                                      limit = 1)#how many articles to look for (50 is max for looking at the same time)
        
        sprintf('%s (%s). %s. %s. doi = %s',selected_article$data$author[1], substr(selected_article$data$publication_date[1], 1, 4),
                selected_article$data$title[1], selected_article$data$journal[1], selected_article$data$id[1])
        
      } else {sprintf('Click on data point to display corresponding article details here.')}
    } else {sprintf('Choosing article only enabled when no margin plots selected.')}
  })
  
  #open browser and go to article when person clicks on link button
  observeEvent(input$linkButton, {
    if(input$marginSelect == 'none'){
      
      #the article to link to
      selected_row = nearPoints(dat_corr(), input$plot_click,
                                threshold = input$pointSelect,#size of reactive area scaled with plotted point size
                                maxpoints = 1, #only return a single data point
                                addDist = F)#do not return distance to actual point
      
      #opening in browser if article doi present
      if (length(selected_row$doi == 1)) {browseURL(sprintf('https://doi.org/%s',selected_row$doi))}
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

