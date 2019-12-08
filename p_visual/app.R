#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(Rplanktondata)
library(Rplanktonanalytic)
library(dplyr)
library(ggplot2)
library(datasets)
library(ggplot2)

GoM <- as_tibble(read.csv2("C:/Users/SZNUSER/Desktop/GOM_phyto.csv",dec="."))
GoM$date <- as.Date(GoM$date)
GoM <- ret_time(GoM)
GoM <- GoM %>% select(year,month,week,day,date,everything())
GoM$week <- factor(GoM$week)
GoN <- phytopkgon
GoN <- ret_time(GoN)
GoN <- GoN %>% select(location,station,year,month,week,day,date,everything())
GoN$week <- factor(GoN$week)
L4 <- phytopkWech
L4 <- ret_time(L4)
L4 <- L4 %>% select(location,station,year,month,week,day,date,everything())
L4$week <- factor(L4$week)
CB <- phytopkcar
CB <- ret_time(CB)
CB <- CB %>% select(location,station,year,month,week,day,date,everything())
CB$week <- factor(CB$week)

#runGist('5792778')
# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app


#helper function (convert vector to named list)
namel<-function (vec){
    tmp<-as.list(vec)
    names(tmp)<-as.character(unlist(vec))
    tmp
}

# shiny server side code for each call
server <- (function(input, output, session){
    #update variable and group based on dataset
    output$variable <- renderUI({ 
        obj<-switch(input$dataset,
                    "GoN" = GoN,
                    "L4" = L4,
                    "CB" = CB,
                    "GoM" = GoM)	 
        var.opts<-namel(colnames(obj))
        selectInput("variable","Variable:", var.opts) # uddate UI 				 
    }) 
    
    output$group <- renderUI({ 
        obj<-switch(input$dataset,
                    "GoN" = GoN,
                    "L4" = L4,
                    "CB" = CB,
                    "GoM" = GoM)	 
        var.opts<-namel(colnames(obj))
        selectInput("group","Groups:", var.opts) # uddate UI 				 
    }) 
    
    output$caption<-renderText({
        switch(input$plot.type,
               "boxplot" 	= 	"Boxplot",
               "histogram" =	"Histogram",
               "density" 	=	"Density plot",
               "bar" 		=	"Bar graph",
               "ts" 		=	"ts")
    })
    
    
    output$plot <- renderUI({
        plotOutput("p")
    })
    
    #plotting function using ggplot2
    output$p <- renderPlot({
        
        plot.obj<<-list() # not sure why input$X can not be used directly?
        plot.obj$data<<-get(input$dataset) 
        plot.obj$variable<<-with(plot.obj$data,get(input$variable)) 
        plot.obj$group<<-with(plot.obj$data,get(input$group)) 
        
        #dynamic plotting options
        plot.type<-switch(input$plot.type,
                          "boxplot" 	= 	geom_boxplot(),
                          "histogram"   =	geom_histogram(alpha=0.5,position="identity"),
                          "density" 	=	geom_density(alpha=.75),
                          "bar" 		=	geom_bar(position="dodge"),
                          "ts" 	       	=	geom_line()
                          
        )
        
        require(ggplot2)
        #plotting theme
        .theme<- theme(
            axis.line = element_line(colour = 'gray', size = .75), 
            panel.background = element_blank(),  
            plot.background = element_blank()
        )	 
        if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs 
            p<-ggplot(plot.obj$data, 
                      aes(
                          x 		= plot.obj$group, 
                          y 		= plot.obj$variable,
                          fill 	= as.factor(plot.obj$group)
                      )
            ) + plot.type
            
            if(input$show.points==TRUE)
            { 
                p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
            }
            
            if(input$log==TRUE)
            { 
                p<-ggplot(plot.obj$data, 
                          aes(
                              x 		= plot.obj$group, 
                              y 		= log(plot.obj$variable+1),
                              fill 	= as.factor(plot.obj$group)
                          )
                ) + plot.type
            }
            
        } else if (input$plot.type=="ts")	{		#control for 1D or 2D graphs 
            p<-ggplot(plot.obj$data, 
                      aes(
                          x 		= plot.obj$group, 
                          y 		= plot.obj$variable
                      )
            ) + plot.type+geom_point()
            
            if(input$show.points==TRUE)
            { 
                p<-p+ geom_point(color='black')
            }
            
            if(input$log==TRUE)
            { 
                p<-ggplot(plot.obj$data, 
                          aes(
                              x 		= plot.obj$group, 
                              y 		= log(plot.obj$variable+1)
                          )
                ) + plot.type+geom_point()
            }  
            
            
        } else {
            
            p<-ggplot(plot.obj$data, 
                      aes(
                          x 		= plot.obj$variable,
                          fill 	= as.factor(plot.obj$group),
                          group 	= as.factor(plot.obj$group),
                          #color 	= as.factor(plot.obj$group)
                      )
            ) + plot.type
        }
        
        p<-p+labs(
            fill 	= input$group,
            x 		= "",
            y 		= input$variable
        )  +
            .theme
        print(p)
    })	
})

# UI for app
ui <- (pageWithSidebar(
    # title
    headerPanel("Select Options"),
    
    #input
    sidebarPanel
    (
        selectInput("dataset","Data:", 
                    list(GoN = "GoN",L4 = "L4", CB = "CB", GoM = "GoM")
        ),
        uiOutput("variable"), 	# depends on dataset ( set by output$variable in  server.R)
        uiOutput("group"),  		# depends on dataset	( set by output$group in  server.R)
        selectInput("plot.type","Plot Type:", 
                    list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar",ts="ts")
        ),
        checkboxInput("show.points", "show points", TRUE),
        checkboxInput("log", "log", FALSE)
    ),	
    
    # output				
    mainPanel(
        h3(textOutput("caption")),
        #h3(htmlOutput("caption")),
        uiOutput("plot") # depends on input 
    )
))
shinyApp(ui = ui, server = server)
