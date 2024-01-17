library(shiny)
source('BeeSelect.r')
source('BeeGraph.r')
source('modarith.r')

ui<-fluidPage(
             tags$head(HTML(
	           "<script>
      var _paq = _paq || [];
      _paq.push(['trackPageView']);
      _paq.push(['enableLinkTracking']);
      _paq.push(['enableHeartBeatTimer']);
      (function() {
        var u='<YOUR_MATOMO_SERVER_URL/matomo>';
        _paq.push(['setTrackerUrl', u+'matomo.php']);
        _paq.push(['setSiteId', '2']);
        var d=document,
            g=d.createElement('script'),
            s=d.getElementsByTagName('script')[0];
            g.type='text/javascript'; 
            g.async=true; g.defer=true;
            g.src=u+'matomo.js';
            s.parentNode.insertBefore(g,s);
      })();
    </script>"
        )),
tags$style(type = "text/css",
             "h1 {color: darkblue; font-weight: bold; font-size: 24px;}",
             "h4 {color: darkblue; font-weight: bold; font-size: 18px;}",
             "h2 {color: darkblue; font-weight: bold; font-size: 16px;}",
             "body {background-color: gainsboro;}",
             "label {font-size: 16px; color: darkblue;
                   font-weight: bold;}"
             ),          
             
tags$head(tags$style(HTML(
            "#text1 {font-size: 20px; color: red; font-weight: bold;}",
            "#text2 {font-size: 20px; color: red; font-weight: bold;}",
            "#text3 {font-size: 20px; color: red; font-weight: bold;}",
            "#table1 {font-size: 20px;color: blue; font-weight: bold;
                        background-color: white;}",
            "#table2 {font-size: 20px;color: blue; font-weight: bold;
                        background-color: white;}"))),
                        
   tags$h1('NYT Spelling Bee Solutions'),
   navlistPanel(
   tabPanel(tags$h4('Read.Me'),
     tags$h4("How This Works"),
     hr(),
     tags$h4("This app is based upon the New York Times online game 'Spelling Bee'.  In that game, a player (or just ordinary nice guy) is presented with a set of seven letters ('cleverly' arranged to resemble a cell in bee hive) and is asked to form words from these letters which (1) must contain the letter at the cell's center, (2) contain at least four letters, and (3) not be proper nouns or hyphenated words."
           ),
     hr(),
     tags$h4("If you have played this game at all, you will likely have discovered that some perfectly good words are excluded (e.g. 'atlatl', 'coon', 'luff') whilst other very suspect words are allowed (e.g. 'dunno','wanna').  Some of the exclusions, 'coon' for example, seem to have been rejected because of some kind of PC urge run amok ('coon' is a perfectly acceptable alternative for 'raccoon') whereas others, 'luff' say, seem to have been excluded owing the ignorance or general chowder-headedness (not allowed!) of the puzzle editor.  The acceptance or rejection of words is apparently to a fair degree a matter of the personal whim of this particular dimwit.  This idea that a good puzzle consists in having prospective solvers attempting to intuit some gimmick or secret thought in the mind of the puzzle creator seems especially dear to the heart of NYT puzzle Dimwit-in-Chief, Will Shortz.  In any case ..."),
     hr(),
     tags$h4("The list of possible acceptable words which I used to generate the suggested 'solution words' is a winnowed version of a list of English words which I grabbed at GitHub.  I've removed words of length less than four as well as capitalized or hyphenated words.  Bizarrely acceptable words ('wanna', for example) will not necessarily arise.  As I discover these strange forms ad hoc, I add them to the list of possible words.  Likewise genuine words which for the reasons outlined above are excluded I add to the 'verboten' list as I come across them.  If you wish to contribute to this process of curation, email me at 'rcgayle@protonmail.com'."),
     hr(),
     tags$h4("Finally, if you have R and R Studio installed, I can send you the shiny files so that you can run this locally on your machine which will spare you the necessity of having an internet connection.  just e-mail me.  Thanks for stopping by.")
           ),
   tabPanel(tags$h4('Enter Letters'),
     tags$h2("Click first on the required letter, then on the remaining six allowed letters. The required letter appears in magenta, the others in green.  Once all seven have been selected, click 'Save/Create Word Lists'. This version is much faster (10^4 times, at least) than an earlier version but it still might take a second or two for the list of suggested words to appear."), 
     plotOutput('plot', click='plot_click',height='350px',
                      width='100%'),
     hr(),
     fluidRow(
     column(5,
     actionButton(inputId='go1', label='Save/Create Word Lists',
                 style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;")),
     column(2,''),
     column(5,
     actionButton(inputId='go2', label='Reset All',
                 style="color: darkblue; font-size: 14px; font-weight: bold; 
                   background-color: white;"))          
             ),
     hr(),
     verbatimTextOutput('text1')
            ), 
   tabPanel(tags$h4('View Words'),
     tags$h4("These are the known words which contain the required letter and only the allowed letters, are of length at least four, and might be accepted by the NYT's resident numbskull."),
     hr(),
     verbatimTextOutput('text2'),
     hr(),
     tableOutput('table1')
        ),
   tabPanel(tags$h4('Disallowed Words'),
     tags$h4("These are the known words which contain the required letter and only the allowed letters, are of length at least four, and are NOT accepted by the NYT's resident numbskull."),
     hr(),
     verbatimTextOutput('text3'),
     hr(),
     tableOutput('table2')
        )
        )  
              )

        
server<-function(input, output){
      v<-reactiveValues(I=c(), Q=list(), GO=FALSE, L=c(), 
            GW=c(c('None', 'Found'),rep('',4)),
            BW=c(c('None', 'Found'),rep('',4)),
            W=readRDS('W.RData'))
            
      
      observeEvent(input$plot_click,{
         if (is.null(IFUN(input$plot_click))){}
         else
           {
             v$Q<-input$plot_click
             if (!is.element(IFUN(v$Q), v$I) & length(v$I)<7)
                 {v$I<-c(v$I, IFUN(v$Q))}
             else {}
            }
                                     })
                       
      observeEvent(input$go1,
            {v$L<-v$W[[v$I[1]]]
            v$NW<-as.character(read.csv('NW.csv')[,2])
            v$GW<-beeselectB(v$L, v$I)
            v$BW<-beeselect(v$NW, letters[v$I[2:7]], letters[v$I[1]])
            if (mod(length(v$GW),6)==0)
                      {}
            else {
                v$GW<-c(v$GW, rep('', 6-mod(length(v$GW),6)))
                 }
            if (mod(length(v$BW),6)==0)
                      {}
            else {
                v$BW<-c(v$BW, rep('', 6-mod(length(v$BW),6)))
                 }
            if (length(v$GW)==0)
                 {v$GW<-c(c('None', 'Found'),rep('',4))}
            else {}
            if (length(v$BW)==0)
                 {v$BW<-c(c('None', 'Found'),rep('',4))}
            else {}
            if (length(v$I)==7)
                {v$GO<-TRUE}
            else {}
             }
                   )
                   
      observeEvent(input$go2,
                 {v$GO<-FALSE
                 v$I<-c()
                 v$GW<-c(c('None', 'Found'),rep('',6))
                 v$BW<-c(c('None', 'Found'),rep('',6))}
                   )
                       
      output$plot<-renderPlot(
            if (length(v$I)==0)
                {SLG()}
            else {SDLG(v$I)}
                              )
                                
       output$text1<-renderText(
           if (v$GO==FALSE)
               {print('Select Letters')}
           else {
print(paste0('Required Letter: ', LETTERS[v$I[1]],'; Allowed Letters: ', LETTERS[v$I[2]],', ', LETTERS[v$I[3]],', ', LETTERS[v$I[4]],', ', LETTERS[v$I[5]],', ', LETTERS[v$I[6]],', ', LETTERS[v$I[7]]))
                 }
              )
              
       output$text2<-renderText(
              if (v$GW[1]=='None' & v$GW[2]=='Found')
                 {print(paste0('Number of words: ',0))}
              else 
                   {
                  print(paste0('Number of words: ',length(v$GW[v$GW!=''])))
                   }
              
                               )
       
       output$text3<-renderText(
              if (v$BW[1]=='None' & v$BW[2]=='Found')
                 {print(paste0('Number of words: ',0))}
              else if (v$BW[1]!='None' & length(v$BW)==6)
                   {
                  print(paste0('Number of words: ',length(v$BW[v$BW!=''])))
                   }
              else {
                  print(paste0('Number of words: ',length(v$BW)))
                   }
                               )
      
       output$table1<-renderTable(matrix(v$GW,ncol=6,byrow=T),colnames=FALSE)
       
       output$table2<-renderTable(matrix(v$BW,ncol=6,byrow=T),colnames=FALSE)
       

                                }
                                
shinyApp(ui=ui, server=server)



