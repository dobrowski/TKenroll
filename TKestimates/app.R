#

library(shiny)
library(tidyverse)
library(DT)

joint <- read_rds("joint.rds")

schools <- read_rds("schools.rds")

altjoint <- read_rds("altjoint.rds")

cspp_list <- read_rds("cspp-list.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Monterey County Estimates for future TK Enrollment LEAs"),
    
    # Sidebar to select LEA
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = h3("Select your LEA"),
                        choices = joint$NAME_LEA21,
                        selected = 1),
        ),
        
        # Main Bofy, in this case text output
mainPanel(
    p("This estimates the number of TK students that will be enrolled in your school for each of the following four years. It is based on prior TK enrollment, prior 1st grade enrollment and births in zip codes associated with your LEA. If you would like to see the specific calculations, please let me know."),
    htmlOutput("text"),
    p(),
    htmlOutput("sentence2"),
    p(),
    htmlOutput("zips"),
    p(""),
    h4("Future TK estimates"),
    DT::dataTableOutput("altTable"),
    p(""),
    h4("Local births"),
    p("The following graph shows the births in zip codes associated with the district over time."),
    plotOutput("births"),
    p(""),
    h4("Local State Preschool Programs"),
    p("The following table shows all California State Preschool Programs that are operating in your geographic area.  Please note this also includes head start sites but does not include all preschools nor family child care homes operating in your area."),
    DT::dataTableOutput("cspp"),
    p(""),
    # plotOutput("ratio"),
    # p(""),
    p(""),
    br(),
    h4("Sources:"),
    a("California Department of Education Downloadable Data File on Transitional Kindergarten Enrollment",href="     https://www.cde.ca.gov/ds/ad/filestkdata.asp"),
    br(),
    a("California Department of Education Downloadable Data File on Census Day Enrollment",href="     https://www.cde.ca.gov/ds/ad/filesenr.asp"),
    br(),
    a("National Center for Education Statistics Datafiles comparing LEAs to Zip Codes",href="     https://nces.ed.gov/programs/edge/Geographic/RelationshipFiles"),
    br(),
    a("California Health and Human Services Records of Births by Zip Code",href="     https://data.chhs.ca.gov/dataset/cdph_live-birth-by-zip-code"),
    br(),
    a("California Department of Finance County Population Projections",href="     https://www.dof.ca.gov/forecasting/demographics/projections/"),
    br(),
    img(src='logo.png', height="15%", width="15%", align = "right")
    
)
        )
    )

# Define server logic 
server <- function(input, output) {

    jointInput <- reactive({
        joint %>%
            filter(NAME_LEA21 == input$select) 
    })
    
    schoolsInput <- reactive({
        schools %>%
            filter(NAME_LEA21 == input$select) 
    })
    
    
    altjointInput <- reactive({
        altjoint %>%
            filter(NAME_LEA21 == input$select) 
    })
    
    
    csppInput <- reactive({
        cspp_list %>%
            filter(NAME_LEA21 == input$select) 
    })
    
    
    
    output$text <- renderText({
        altjoint2 <- altjointInput()
        
        paste0("<h4>For ",
               input$select,
               " there will be an estimated <b>",
               altjoint2 %>%
                   filter(Year == "2022-23") %>%
                   select(est.mean)  ,
               "</b> students in 2023.  They will require about ",
               altjoint2 %>%
                   filter(Year == "2022-23") %>%
                   select(est.teachers)  ,
               " teachers.</h4>")
    })
    
    
    
    output$zips <- renderText({
        schools2 <- schoolsInput()
        
        paste0("For ",
               input$select,
               " the following zip codes were considered to be associated: ",
               schools2 %>%
                   select(ZCTA5CE20),
               ".")
    })
    
    
    
    output$sentence2 <- renderText({
        altjoint2 <- altjointInput()
        
        paste0("The best estimate used is the average of three estimates.  One estimate is based on 2020-21 first grade cohorts and changes in county population projections (",
               altjoint2 %>%
                   filter(Year == "2022-23") %>%
                   select(est.1gr) ,
               ")a second estimate is based on the actual number of 2019-20 TK students and changes in county population projections (",
               altjoint2 %>%
                   filter(Year == "2022-23") %>%
                   select(est.TKactual) ,
                
               ") and a third estimate is based on the number of births in zip codes associated with the LEA (",
               altjoint2 %>%
                   filter(Year == "2022-23") %>%
                   select(est.zip.births) ,
               ").")
    })
    
    
    output$births <- renderPlot({
        joint2 <- jointInput()
        
        joint2[[12]][[1]]
        
    })
    
    
    output$ratio <- renderPlot({
        joint2 <- jointInput()
        
        joint2[[11]][[1]]
        
    })
    
    output$altTable <- DT::renderDataTable({
        altjoint2 <- altjointInput()
        
        ##rename column names to (inside renderDT)
        colnames(altjoint2) <- c("LEA", "Year", "Estimate from latest 1st Grade Enrollment", "Estimate from latest TK Enrollment", "Estimate from Zip code births", "Mean of estimates", "Estimated teachers needed")
        
        DT::datatable(altjoint2, options = list(paging = FALSE, 
                                           searching = FALSE)) %>%
            formatStyle('Mean of estimates',  color = 'black', backgroundColor = 'yellow', fontWeight = 'bold')
    })
    
    output$cspp <- DT::renderDataTable({
        cspp <- csppInput()
        
        ##rename column names to (inside renderDT)
    #    colnames(altjoint2) <- c("LEA", "Year", "Estimate from latest 1st Grade Enrollment", "Estimate from latest TK Enrollment", "Estimate from Zip code births", "Mean of estimates", "Estimated teachers needed")
        
        DT::datatable(cspp, options = list(paging = TRUE, 
                                         searching = TRUE,
                                           pageLength = 5)) # %>%
            # formatStyle('Mean of estimates',  color = 'black', backgroundColor = 'yellow', fontWeight = 'bold')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
