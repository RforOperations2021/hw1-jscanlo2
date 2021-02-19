library(ggplot2)
library(shiny)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(RColorBrewer)
library(ggforce)
library(ggalluvial)
library(DT)
library(colourpicker)

#setwd("C:/Users/jeffr/Documents/GitHub/hw1-jscanlo2")
#voters <- read.csv(file = 'EAVS18.csv')
#voters_d <- read.csv(file = 'EAVS18.csv')
#policy <- read.csv(file = 'Policy18.csv')
#policy_d <- read.csv(file = 'Policy18.csv')
#juris <- read.csv(file = "Juris.csv")
policy <- read.csv("./Policy18.csv")
policy_d <- read.csv("./Policy18.csv")
voters <- read.csv("./Juris.csv")
voters_d <- read.csv("./Juris.csv")
juris <- read.csv("./EAVS18.csv")

juris <- as.data.frame(juris)
policy <- as.data.frame(policy)

voters <- voters[ !(voters$State_Full %in% c("Guam", "American Samoa", "Puerto Rico", "Virgin Islands")), ]
policy <- policy[ !(policy$State_Full %in% c("Guam", "American Samoa", "Puerto Rico", "Virgin Islands")), ]
juris <- juris[ !(juris$State %in% c("Guam", "American Samoa", "Puerto Rico", "Virgin Islands")), ]

juris$Category <- relevel(jurisdf$Category, "Inactive")

states <- voters %>%
            group_by(State_Full) %>%
            summarize(TotReg = sum(TotReg, na.rm=TRUE),
                      TotAct = sum(TotAct, na.rm=TRUE),
                      TotIn = sum(TotIn, na.rm=TRUE),
                      TotRegForms = sum(TotRegForms, na.rm=TRUE),
                      NewValRegForms = sum(NewValRegForms, na.RM=TRUE),
                      InvRejectRegForm = sum(InvRejectRegForms, na.rm=TRUE),
                      InvRegMail = sum(InvRegMail, na.rm=TRUE),
                      InvRegPerson = sum(InvRegPerson, na.rm=TRUE),
                      InvRegOnline = sum(InvRegOnline, na.rm=TRUE),
                      InvRegDMV = sum(InvRegDMV, na.rm=TRUE),
                      InvRegNVRA = sum(InvRegNVRA, na.rm=TRUE),
                      InvRegAgenciesDisab = sum(InvRegAgenciesDisab, na.rm=TRUE),
                      InvRegArmedForces = sum(InvRegArmedForces, na.rm=TRUE),
                      InvRegNonNVRA = sum(InvRegNonNVRA, na.rm=TRUE),
                      InvRegDrives = sum(InvRegDrives, na.rm=TRUE),
                      InvRegOther = sum(InvRegOther, na.rm=TRUE),
                      TotVotersRem = sum(TotVotersRem, na.rm=TRUE),
                      VotRemMove = sum(VotRemMove, na.rm=TRUE),
                      VotRemDeath = sum(VotRemDeath, na.rm=TRUE),
                      VotRemFelon = sum(VotRemFelon, na.rm=TRUE),
                      VotRemFail = sum(VotRemFail, na.rm=TRUE),
                      VotRemMent = sum(VotRemMent, na.rm=TRUE),
                      VotRemReq = sum(VotRemMent, na.rm=TRUE),
                      VotRemOther = sum(VotRemOther, na.rm=TRUE)
            )

states1 <- as.data.frame(states)
listO <- c(0, 1, -99)
listV <- c("No", "Yes", "No Data")

policy$gov_photoid_req[policy$gov_photoid_req == 0] <- "No"
policy$gov_photoid_req[policy$gov_photoid_req == 1] <- "Yes"
policy$gov_photoid_req[policy$gov_photoid_req == -99] <- "Missing Data"

policy$gov_nonphoto[policy$gov_nonphoto == 0] <- "No"
policy$gov_nonphoto[policy$gov_nonphoto == 1] <- "Yes"
policy$gov_nonphoto[policy$gov_nonphoto == -99] <- "Missing Data"

policy$nongov_id[policy$nongov_id == 0] <- "No"
policy$nongov_id[policy$nongov_id == 1] <- "Yes"
policy$nongov_id[policy$nongov_id == -99] <- "Missing Data"

policy$vouch[policy$vouch == 0] <- "No"
policy$vouch[policy$vouch == 1] <- "Yes"
policy$vouch[policy$vouch == -99] <- "Missing Data"

policy$affidavit[policy$affidavit == 0] <- "No"
policy$affidavit[policy$affidavit == 1] <- "Yes"
policy$affidavit[policy$affidavit == -99] <- "Missing Data"

policy$aff_other[policy$aff_other == 0] <- "No"
policy$aff_other[policy$aff_other == 1] <- "Yes"
policy$aff_other[policy$aff_other == -99] <- "Missing Data"

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),

    # Application title
    titlePanel("Voter Registrations, Purges, and Identification Laws"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            downloadButton("download1", "Download EAVS Data"),
            br(), br(),
            downloadButton("download2", "Download Policy Data"),
            br(), br(),
            selectizeInput("state",
                           "Select State:",
                           choices = unique(states1$State_Full)),
            br(), br(),
            sliderInput("range", "Filter Jurisdiction by Number of Registered Voters:",
                                       min = 0, max = 6000000,
                                       value = c(250000, 2000000),
                                       step = 10000)

        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Registrations",
                            plotOutput("alluvPlot"),
                         br(),
                         br(),
                            plotOutput("jurisPlot"),
                        br(),
                        br(),
                        uiOutput("source")),
                tabPanel("Invalid Registrations",
                         h4("Invalid Registrations"),
                         h5("Voter registrations can often be completed and processed through
                            many avenues, and registrations may be invalidated for a number of reasons.
                            This pie chart shows the number of invalid registrations processed through each of the given means."),
                            plotOutput("secPlot")),
                tabPanel("Voter Purges",
                         h4("Voter Purges"),
                         h5('Voter purges are regularly scheduled updates of voter registration lists which involve
                            removing registrations that are no longer deemed valid. The Brennan Center for Justice identifies that voter purging is
                            an "often-flawed" process that removes large swaths of otherwise eligible voters from the voter rolls "with little notice."'),
                         br(),
                                plotOutput("thirPlot"),
                         br(),
                         h4("Regarding Criminal Convictions:"),
                                uiOutput("felrule1"),
                                uiOutput("felrule2"),
                                uiOutput("felrule3"),
                         br(),
                         br(),
                         uiOutput("url")),
                tabPanel("Identification Laws",
                         selectInput("colfilter", "Select Column(s) to Colorize:",
                                     choices = c("Gov-Issued Photo ID", "Gov-Issued Nonphoto ID",
                                                 "Nongov-Issued ID", "Formal Vouch Sufficient", "Signed Affidavit Sufficient", "Affidavit + Follow-Up"),
                                     multiple = TRUE),
                         colourInput("color1", 'Select Color for "Yes":', value = "#EEA8A6"),
                         colourInput("color2", 'Select Color for "No":', value = "#B0ECAF"),
                         br(),
                         h3("Identification Requirements to Vote in Each State"),
                           DT::dataTableOutput("tab1")
            )
        )
    )))

# Comments here
server <- function(input, output, session) {
    
    ### Comments here
    output$download1 <- downloadHandler(
        filename = function() {
            paste("EAVS18.csv", sep="")
        },
        content = function(file) {
            write.csv(voters_d, file)
        })
    
    output$download2 <- downloadHandler(
        filename = function() {
            paste("VotingPolicy.csv", sep="")
        },
        content = function(file) {
            write.csv(policy_d, file)
        })

    
    ### Comment Here
    
    ###Comments here
    jurisdf1 <- reactive({
        juris <- juris %>%
            filter(State == input$state)%>%
            group_by(Jurisdiction) %>%
            summarize(Num_Voters = sum(N_Voters))%>%
            filter(Num_Voters >= min(input$range), Num_Voters <= max(input$range))
    })
    
    jurisdf2 <- reactive({
        juris %>%
            filter(State == input$state, Jurisdiction %in% jurisdf1()$Jurisdiction)
    })
    
    output$jurisPlot <- renderPlot({
        ggplot(jurisdf2(), aes(x=reorder(Jurisdiction, N_Voters), y=N_Voters, fill=Category))+
            geom_bar(stat='identity', position='stack')+
            labs(title=paste("Active and Inactive Voters in",input$state,"Jurisidictions"), y = "Number of Registered Voters",
                 fill = "Voter Type")+
            theme(legend.title=element_text(size=15),legend.text=element_text(size=12),
                  axis.title.y=element_text(size = 15), plot.title = element_text(size = 20, hjust = 0.5),
                  axis.text.x=element_text(size=12, angle = 90),
                  axis.title.x=element_blank(),
                  plot.background = element_rect(fill = "white"))
            
    })

    
    
    ### Comments Here
    
    state_reg <- reactive({
        req(input$state)
        subset(states1, State_Full == input$state, select=InvRegMail:InvRegOther)
    })
    
    variables <- reactive({colnames(state_reg())})
    
    values <- reactive({as.numeric(as.vector(state_reg()[1,]))})
    
    df <- reactive({
        df = data.frame(variables(),values())
        colnames(df) = c("Cause", "Count")
       df = df %>%
           mutate(end = 2 * pi * cumsum(values())/sum(values()),
               start = lag(end, default = 0),
               middle = 0.5 * (start + end),
               hjust = ifelse(middle > pi, 1, 0),
               vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
       levels(df$Cause) <- c("Disability Service","Armed Forces","DMV","Voter Drives","Mail","Non-NVRA","NVRA","Online","Other","In Person")
        df
        })
    
    output$alluvPlot <- renderPlot({
    
    policy <- policy %>%
        select(State_Full, AVR, Reg_online, Same_Day, Freq, Aff)
    
    policy <- policy[order(policy$Aff, policy$AVR, policy$Reg_online, policy$Same_Day),]
    
    policy1 <- policy
    
    levels(policy1$Aff) <- c(levels(policy1$Aff), as.character(policy1$State_Full[policy1$State_Full==input$state]))
    
    policy1$Aff[policy1$State_Full==input$state] = as.character(policy1$State_Full[policy1$State_Full==input$state])
    
    ggplot(policy1, aes(y=Freq, axis1 = AVR, axis2 = Reg_online, axis3 = Same_Day)) + 
        geom_alluvium(aes(fill=Aff), width=0.5, alpha=0.6) +
        scale_fill_manual(values=c("#479AFF", "#FF4747", "#5126B5", "#FFFF00"))+
        geom_stratum(width = 0.25, fill = "black", color = "white", alpha=0.8) +
        geom_label(stat = "stratum", aes(label = after_stat(stratum)), size = 2)+
        scale_x_discrete(labels = c("Automatic Registration?", "Online Registration?", "Same Day Registration?"),
                         limits = c("Automatic Voter Registration?", "Online Registration?", "Same Day Registration?"),
                         expand = c(.1, .1)) +labs(title="Voter Registration Options by State", y = "Individual States",
                                                   fill = "Historical Party Affiliation")+
        theme(legend.title=element_text(size=15),legend.text=element_text(size=12),
              axis.title.y=element_text(size = 15), plot.title = element_text(size = 20, hjust = 0.5),
              axis.text.y=element_blank(), axis.text.x=element_text(size=12),
              panel.background = element_rect(fill="white"),
              plot.background = element_rect(fill = "white"))
    })
    
    output$source <- renderUI({
        tagList("Data Source:", a("U.S. Election Assistance Commission", href="https://www.eac.gov/research-and-data/datasets-codebooks-and-surveys"))
    })

    ### Comments here
    output$secPlot <- renderPlot({
        ### Comments here
        ggplot(df()) + 
            geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                             start = start, end = end, fill=Cause)) +
            geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = Count,
                          hjust = hjust, vjust = vjust)) +
            coord_fixed() +
            scale_x_continuous(limits = c(-1.2, 1.2),  # Adjust so labels are not cut off
                               name = "", breaks = NULL, labels = NULL) +
            scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                               name = "", breaks = NULL, labels = NULL) +
            labs(title = "Number of Voter Registrations Invalidated from 2016-2018") +
            scale_fill_brewer("Source", palette = 'Set1')
            
        
    })
    
    ### Comments here
    state_drop <- reactive({
        req(input$state)
        subset(states1, State_Full == input$state, select=VotRemMove:VotRemOther)
    })
    
    variables2 <- reactive({colnames(state_drop())})
    values2 <- reactive({as.numeric(as.vector(state_drop()[1,]))})
 
    df2 <- reactive({
        df2 = data.frame(variables2(),values2())
        colnames(df2) = c("Reason", "Count")
        df2 = df2 %>%
            mutate(end2 = 2 * pi * cumsum(values2())/sum(values2()),
                   start2 = lag(end2, default = 0),
                   middle2 = 0.5 * (start2 + end2),
                   hjust2 = ifelse(middle2 > pi, 1, 0),
                   vjust2 = ifelse(middle2 < pi/2 | middle2 > 3 * pi/2, 0, 1))
        levels(df2$Reason) <- c("Death", "Failure to Respond", "Felony Conviction", "Declared Mentally Incompetent", "Moved", "Other", "Voter Request")
        df2
    })
    
    policy_st <- reactive({
                    policy %>%
                    filter(State_Full == input$state)
    })
    
    output$felrule1 <- renderUI({
        felony <- policy_st()[,43]
        
        if(policy_st()[1,43] == 1)
        {HTML(paste(input$state, "removes all individuals convicted of any felony from voter rolls."))}
        else if(policy_st()[1,43] == 2)
        {HTML(paste(input$state, "removes individuals convicted of certain felonies from voter rolls."))}
        else if(policy_st()[1,43] == 3)
        {HTML(paste(input$state, "removes individuals convicted of certain felonies and other crimes (such as election-related crimes) from voter rolls."))}
        else if(policy_st()[1,43] == 4)
        {HTML(paste(input$state, "removes individuals who are convicted and incarcerated from voter rolls."))}
        else if(policy_st()[1,43] == 5)
        {HTML(paste(input$state, "does not remove anyone from voter rolls because of criminal convictions."))}
        else {HTML(paste("Incomplete State Data"))}
    })   
    
    output$felrule2 <- renderUI({
        felony <- policy_st()[,44]
        
        if(policy_st()[1,44] == 1)
        {HTML(paste("These individuals lose the right to vote during the full extent of their incarceration.<br>"))}
        else if(policy_st()[1,44] == 2)
        {HTML(paste("These individuals lose the right to vote during the full extent of their incarceration and probation (or parole).<br>"))}
        else if(policy_st()[1,44] == 3)
        {HTML(paste("These individuals lose the right to vote during incarceration, probation or parole, plus additional time.<br>"))}
    }) 
        
    output$felrule3 <- renderUI({
        felony <- policy_st()[,45]
        
        if(policy_st()[1,45] == 1)
        {HTML(paste("These individuals are automatically eligible to register to vote after this time elapses.<br>"))}
        else if(policy_st()[1,45] == 2)
        {HTML(paste("These individuals have previous registrations automatically restored after this time elapses.<br>"))}
        else if(policy_st()[1,45] == 3)
        {HTML(paste("These individuals must present documentation of completing necessary requirements before they can register to vote again.<br>"))}
        else if(policy_st()[1,45] == 4)
        {HTML(paste("These individuals must proceed through a formal administrative process in order to restore their voting rights.<br>"))}
    }) 
    
    output$url <- renderUI({
        tagList("Source:", a("Brennan Center for Justice", href="https://www.brennancenter.org/issues/ensure-every-american-can-vote/vote-suppression/voter-purges"))
    })
        
    ### Comments Here
    output$thirPlot <- renderPlot({
        ### Comments Here
        ggplot(df2()) + 
            geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                             start = start2, end = end2, fill=Reason)) +
            geom_text(aes(x = 1.05 * sin(middle2), y = 1.05 * cos(middle2), label = Count,
                          hjust = hjust2, vjust = vjust2)) +
            coord_fixed() +
            scale_x_continuous(limits = c(-1.2, 1.2),  # Adjust so labels are not cut off
                               name = "", breaks = NULL, labels = NULL) +
            scale_y_continuous(limits = c(-1.2, 1.2),      # Adjust so labels are not cut off
                               name = "", breaks = NULL, labels = NULL) +
            labs(title = "Number of Voters Removed from Rolls from 2016-2018") +
            scale_fill_brewer('Reason for Removal', palette = 'Set1')
    })
    
    output$tab1 <- DT::renderDataTable({
        dtable <- policy %>%
            select(State_Full, gov_photoid_req,	gov_nonphoto, nongov_id, vouch, affidavit, aff_other)
        colnames(dtable) <- c("State", "Gov-Issued Photo ID", "Gov-Issued Nonphoto ID",
                              "Nongov-Issued ID", "Formal Vouch Sufficient", "Signed Affidavit Sufficient", "Affidavit + Follow-Up")
        dtable
        dtable <- datatable(dtable) %>%
            formatStyle(input$colfilter,
                        backgroundColor = styleEqual(c("No", "Yes"), c(input$color2, input$color1)))
        dtable
     
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
