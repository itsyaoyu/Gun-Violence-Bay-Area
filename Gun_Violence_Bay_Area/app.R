library(shiny)
library(plotly)
library(tidyverse)

graphic_violence <- readRDS("graphic_violence.RDS")

violence_capita <- readRDS("graphic_violence_capita.RDS")

total_data <- readRDS("imprisonment.RDS")

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
                          changes are implemented (community action, laws, etc.). More specific 
                          details about the sources can be found under the Methods tab."),
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
                          plotlyOutput("imprisonment_Plotly"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          fillRow(
                          imageOutput("SF"),
                          imageOutput("OK"))),
                 tabPanel("Methods",
                          h1("Modeling"),
                          p("For my graphics, I chose to include three types: one showing the
                            decrease in violent crimes in San Francisco and Oakland, California;
                            one showing the trend between gun violence and imprisonment in
                            California; and one showing all the datapoints of gun violence
                            in both cities."),
                          h2("Violent Crimes"),
                          p("The violent crime graphics used data from the ",
                            a("Data Commons Graph", 
                              href = "https://browser.datacommons.org/gni"),                          
                            ", a tool that combines data from various sources such as 
                            Wikipedia, the US Census, NOAA, FBI, and etc. In my graph,
                            I first look at violent crimes from 2011 to 2017. But, then I
                            realized that Chicago's large crime rate made it difficult to
                            see the other trends. So, I calculated the violent crime rates
                            per capita to make these trends more visible."),
                          h2("Violent Crimes and California Imprisonment"),
                          p("Reading other reports on the low gun violence rates in
                            California, there was a suggestion that this decrease related to
                            a decrease in prison sentencing. While it was difficult to find
                            sentencing rates specifically in San Francisco and Oakland, the
                            overall imprisonment rates in California were available through
                            the ",
                            a("California Sentencing Institute", 
                              href = "http://casi.cjcj.org/about.html#download"),
                            ". What I found was a slight negative trend from 2011 to 2016,
                            with the imprisonment rate following closely well to the Oakland
                            gun violence rate. One problem with the imprisonment data is that
                            most of San Francisco's imprisonment rates are not included because
                            the California Sentencing Institute was unable to get the data from
                            San Francisco."),
                          h2("Gun Violence Datapoints"),
                          p("The last type of graph shows the datapoints of each victim
                            to gun violence in San Francisco and Oakland. First, I got the maps
                            from ",
                            a("Google Map's API",
                            href = "https://cloud.google.com/maps-platform/"),
                            ". The datapoints from San Francisco was pulled from",
                            a("a crime dataset",
                              href = "https://data.sfgov.org/Public-Safety/Police-Department-Incident-Reports-Historical-2003/tmnf-yvry"),
                            "with over 2 million rows of crime in San Francisco from 2003 to 2018. 
                            I chose to pull out Aggravated Assault with a Gun data because there was
                            no category just for violent crime or gun violence. The Oakland gun violence
                            data came from",
                            a("The Trace",
                              href = "https://www.thetrace.org/violent-crime-data/"),
                            ", an organization dedicated to bring awareness to
                            gun violence. This dataset did have gun violence datapoints, which I
                            used in my visualization. I was able to gather the coordinates from street
                            addresses after using a modified script I found",
                            a("here",
                              href = "http://www.storybench.org/geocode-csv-addresses-r/"),
                            ", which once again used Google map's api. I chose not to use the 
                            San Francisco data from The Trace because it was extremely difficult to get 
                            the coordinates from the San Francisco dataset."))
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
    output$imprisonment_Plotly <- renderPlotly({
        
        # Cleaning names and using gather to reformat the data for plotly
        
        imprisonment_data <- total_data %>%
            gather(key = "variables", value = "numbers", Imprisonment:oakland_violent) %>%
            mutate(variables = ifelse(variables == "san_francisco_violent", "San Francisco", variables),
                   variables = ifelse(variables == "oakland_violent", "Oakland", variables))
        
        imprisonment_graphic <- plot_ly(
            data = imprisonment_data,
            x = ~year, 
            y = ~numbers,
            color = ~variables,
            text = ~numbers, 
            hoverinfo = "text",
            type = 'scatter',
            mode = 'line'
        ) %>% 
            layout(
                width = 1000, 
                height = 500,
                title = 'Violent Crimes vs. California Imprisonment',
                xaxis = list(
                    title = "Year",
                    zeroline = F
                ),
                yaxis = list(
                    title = "Violent Crimes and Imprisonment",
                    zeroline = F
                ),
                annotations = list(x = 1, y = -0.08, text = "Source: Census.gov, fbi.gov, California Sentencing Institute", 
                                   showarrow = F, xref='paper', yref='paper', 
                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                   font=list(size=12, color="black"))
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
        )}, deleteFile = FALSE)
    }

shinyApp(ui, server)
