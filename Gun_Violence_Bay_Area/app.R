library(shiny)
library(plotly)

graphic_violence <- readRDS("graphic_violence.RDS")

violence_capita <- readRDS("graphic_violence_capita.RDS")

# Define UI for application 

ui <- navbarPage("Gun Violence Data in San Francisco and Oakland",
                 tabPanel("About",
                          h1("Background"),
                          p("The goal of this project is to find what caused gun violence to 
                          decrease in the Bay Area while it was increasing in many other US 
                          cities for the past decade. Other analysts have suggested that this 
                          decline is related to criminal justice reforms, tough gun laws, and 
                          investment in local communities. We will be comparing how different 
                          cities in the United States interact with these factors using data 
                          from individual cities and comparing the differences in laws, 
                          demographics, and history."),
                          
                          p("The plan for this project is to have different individuals work 
                          separately on analyzing gun violence in different cities across 
                          the United States. At the end of the individual research, we will 
                          all combine our findings together to compare what is and isn’t 
                          working in reducing gun violence rates across America."),
                          
                          p("For example, my area of focus is currently on San Francisco and the 
                          Bay Area rates. My friend, Erin Guetzloe, will be focusing on the 
                          gun violence rates in Boston. After we both finish our invidiual 
                          research and have our findings, we will meet together and look at 
                          our findings together in order to come up with potential solutions 
                          and failures."),
                          
                          p("This collaborative project grew from the request of ",
                            a("David Hogg", 
                              href = "https://en.wikipedia.org/wiki/David_Hogg_(activist)"),                          
                            "Co-Founder of March For Our Lives and gun control advocate, to 
                          work with students of ",
                            a("David Kane's",
                              href = "https://davidkane.info/"),
                            "Harvard Gov 1005 class. Hogg wanted to discover what specific factors 
                          had an effect in drastically decreasing the gun violence in the 
                          Bay Area and pinpoint specific examples of beneficial factors. 
                          One area of particular interest is in local initiatives that 
                          succeeded in the Bay Area. Any solid conclusions discovered at 
                          the end of this project will be used to try and lower gun violence 
                          in communities across the country."),
                          h1("The Data"),
                          p("The visualizations from this project are based off of data from 
                          fbi.gov and Census.gov. There will be additional data from local 
                          governments to analyze specific changings in gun violence after 
                          changes are implemented (community action, laws, etc.)."),
                          h1("About Me"),
                          p("My name is Yao Yu and I’m currently an undergraduate at Harvard 
                          studying Data Science."),
                          p("Reach me at ",
                            a("yaodongyu@college.harvard.edu",
                              href = "yaodongyu@college.harvard.edu",),
                            "or LinkedIn ",
                            a("here",
                              href = "https://www.linkedin.com/in/yaodong-yu"))),
                 tabPanel("Models",
                          plotlyOutput("violence_Plotly"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          plotlyOutput("violence_capita_Plotly"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          imageOutput("SF"),
                          imageOutput("OK")),
                 tabPanel("Methods")
)

# The code for this server was found here:
# https://stackoverflow.com/questions/35421923/how-to-create-and-display-an-animated-gif-in-shiny
server <- function(input, output, session) {
    output$violence_Plotly <- renderPlotly({
        violence <- plot_ly(
            data = graphic_violence,
            x = ~year, 
            y = ~deaths,
            color = ~cities,
            frame = ~frame,
            text = ~cities, 
            hoverinfo = "text",
            type = 'scatter',
            mode = 'lines'
        ) %>% 
            layout(
                width = 1000, 
                height = 500,
                title = 'Number of Violent Crimes in Cities Per Year',
                xaxis = list(
                    title = "Year",
                    zeroline = F
                ),
                yaxis = list(
                    title = "Violent Crimes",
                    zeroline = F
                ),
                annotations = list(x = 1, y = -0.12, text = "Source: fbi.gov", 
                                   showarrow = F, xref='paper', yref='paper', 
                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                   font=list(size=15, color="black"))
            )
    })
    output$violence_capita_Plotly <- renderPlotly({
        violence_capita <- plot_ly(
            data = violence_capita,
            x = ~year, 
            y = ~deaths,
            color = ~cities,
            frame = ~frame,
            text = ~cities, 
            hoverinfo = "text",
            type = 'scatter',
            mode = 'lines'
        ) %>% 
            layout(
                width = 1000, 
                height = 500,
                title = 'Number of Violent Crimes in Cities Per Year Per Capita',
                xaxis = list(
                    title = "Year",
                    zeroline = F
                ),
                yaxis = list(
                    title = "Violent Crimes Per Capita",
                    zeroline = F
                ),
                annotations = list(x = 1, y = -0.12, text = "Source: Census.gov, fbi.gov", 
                                   showarrow = F, xref='paper', yref='paper', 
                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                   font=list(size=15, color="black"))
            )
    })
    output$SF <- renderImage({
        # Return a list containing the filename
        list(src = "AAGun_SF.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)
    output$OK <- renderImage({
        # Return a list containing the filename
        list(src = "trace_OK.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)}

shinyApp(ui, server)
