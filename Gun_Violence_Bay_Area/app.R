library(shiny)
library(plotly)
library(tidyverse)
library(shinythemes)

# Reads in data

graphic_violence <- readRDS("graphic_violence.RDS")

violence_capita <- readRDS("graphic_violence_capita.RDS")

imprisonment_data <- readRDS("imprisonment.RDS")

laws <- readRDS("laws.RDS")

laws_analysis <- readRDS("laws_analysis.RDS")

fit_SF <- lm(san_francisco_violent ~ year, data = laws)
fit_OK <- lm(oakland_violent ~ year, data = laws)
fit_Law <- lm(lawtotal ~ year, data = laws)

# Define UI for application 

ui <- navbarPage(theme = shinytheme("united"),
                 "Gun Violence in San Francisco and Oakland",
                 tabPanel("About",
                          column(7,
                          h1("Background"),
                          p("The goal of this project is to find what caused gun violence to 
                          decrease in the Bay Area while it was increasing in many other US 
                          cities for the past decade. Other ",
                          a("analysts",
                            href = "https://www.theguardian.com/us-news/ng-interactive/2019/jun/03/gun-violence-bay-area-drop-30-percent-why-investigation"),
                          "have suggested that this 
                          decline is related to criminal justice reforms, tough gun laws, and 
                          investment in local communities. We will be comparing how different 
                          cities in the United States interact with these factors using data 
                          from individual cities and comparing the differences."),
                          
                          p("The plan for this project is to have different individuals work 
                          separately on analyzing gun violence in different cities across 
                          the United States. At the end of the individual research, we will 
                          combine our findings together to compare what is and isn’t 
                          working in reducing gun violence rates across America."),
                          
                          p("For example, my area of focus is currently on San Francisco and the 
                          Bay Area. My friend, ",
                          a("Erin Guetzloe",
                          href = "https://eringuetzloe.shinyapps.io/gun-violence-visualizations/"),
                          " will be focusing on 
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
                          p("The first few visualizations from this project are based off of data from 
                          fbi.gov and Census.gov. There will be additional data from local 
                          governments to analyze specific changings in gun violence after 
                          changes are implemented (community action, laws, etc.). More specific 
                          details about the sources will be mentioned in explinations and they 
                          can all be found under the Methods tab."),
                          p("You can find the code for this project on my ",
                            a("GitHub",
                              href = "https://github.com/itsyaoyu/Gun-Violence-Bay-Area",)),
                          h1("About Me"),
                          p("My name is Yao Yu and I’m currently an undergraduate at Harvard 
                          studying Government with a specialization in Data Science."),
                          p("You can reach me at ",
                            a("yaodongyu@college.harvard.edu",
                              href = "yaodongyu@college.harvard.edu",),
                            "or ",
                            a("LinkedIn",
                              href = "https://www.linkedin.com/in/yaodong-yu"))),
                          column(2,
                                 imageOutput("SF", height = "100%"),
                                 imageOutput("OK", height = "100%"))),
                 tabPanel("Violent Crime in Bay Area",
                          h1("Violent Crimes"),
                          fixedRow(
                              column(4,
                                     p("In this graph,
                            I started my research searching for statistics on the number of
                            gun violence crimes in different cities across America. This data
                            was much more difficult to find than I originally thought because
                            I learned that different police departments across the country all
                            code incident reports differently. Some, like San Francisco's PD,
                            had specific coding down to the level of homicides with different
                            types of weapons. Other cities, like Oakland, coded reports much more
                            vaguely with just homicides or gun violence. Instead, I resorted to
                            looking at violent crime data from the FBI because all cities were 
                            required to report with similar coding. I found the ",
                            a("Data Commons Graph", 
                            href = "https://browser.datacommons.org/gni"),                          
                            " tool useful for this because it could combine the FBI data from
                            many different cities into one for graphing. I chose the cities of
                            Baltimore, Chicago, and St. Louis to compare because they are also
                            other large cities in the US that are being affected by gun violence.
                            One problem with this data was that it does not go as far back in years
                            as I would have liked. However, 2011 to 2017 is still a good representation
                            to look at the decrease in gun violence in San Francisco and Oakland
                            that this project is focused on.")),
                              column(5, 
                                     plotlyOutput("violence_Plotly", height = "100%"))
                          ),
                          br(),
                          fixedRow(
                              column(4,
                                     p("After looking at the graph created from the gun
                                     violence data, I realized that Chicago's high crime
                                     rate made it difficult to see the trends of all the
                                     cities. To clarify the trends, I took the crime rates
                                     and used the per capita normalization function in the
                                     ",
                                     a("Data Commons Graph", 
                                     href = "https://browser.datacommons.org/gni"),                          
                                     " to show the number of violent crimes per capita.")),
                              column(5, 
                                     plotlyOutput("violence_capita_Plotly", height = "100%")))
                          ),
                 tabPanel("Imprisonment and Gun Control Laws",
                          h1("Violent Crimes and Imprisonment"),
                          fixedRow(
                              column(4,
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
                            San Francisco.")),
                              column(5, 
                                     plotlyOutput("imprisonment_Plotly", height = "100%"))
                          ),
                          br(),
                          h1("Regression"),
                          fixedRow(
                              column(4,
                                     p("The regression graphic shows the regression of three sets of data: the violent crime
                            data from San Francisco, the violent crime data from Oakland, and the number of gun
                            control laws in California. The violent crime data is the same that is shown in the
                            first grpahic and the data on gun control laws is from ",
                                       a("State Firearm Laws",
                                         href = "http://www.statefirearmlaws.org/resources"),
                                       ". The regressions show that over the years of 2011 to 2017, the
                            number of violent crimes in San Francisco has actually had a slight increase,
                            the number of violent crimes in Oakland had a slight decrease, and the number
                            of gun control laws in California steadily rose. However, the violent crime
                            rates of both San Francisco and Oakland both started decreasing from 2013
                            on and from 2012 to 2013, there was a slightly higher number of gun control
                            laws passed, from 95 to 99. This suggests that one of those 4 new pieces of
                            legislation might possibly have something to do with the decrease in gun
                            violence in San Francisco and Oakland while it increased in many other US
                            cities (as seen in the first two graphics).")),
                              column(5, 
                                     plotlyOutput("laws_Plotly", height = "100%")))
                          ),
                 tabPanel("Methods",
                          h1("Modeling"),
                          p("For my graphics, I chose to include four types: one showing the
                            decrease in violent crimes in San Francisco and Oakland, California;
                            one showing the trend between gun violence and imprisonment in
                            California; one showing all the datapoints of gun violence
                            in both cities; and one showing the Regression of violent crimes
                            and gun control laws."),
                          h1("Gun Violence Datapoints"),
                          p("The graphs on the front page show the datapoints of each victim
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
                            the coordinates from the San Francisco dataset."),
                          h1("Violent Crimes"),
                          p("The violent crime graphics used data from the ",
                            a("Data Commons Graph", 
                              href = "https://browser.datacommons.org/gni"),                          
                            ", a tool that combines data from various sources such as 
                            Wikipedia, the US Census, NOAA, FBI, and etc. In my graph,
                            I first look at violent crimes from 2011 to 2017. But, then I
                            realized that Chicago's large crime rate made it difficult to
                            see the other trends. So, I calculated the violent crime rates
                            per capita to make these trends more visible."),
                          h1("Violent Crimes and California Imprisonment"),
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
                          h1("Regression"),
                          p("The regression graphic shows the regression of three sets of data: the violent crime
                            data from San Francisco, the violent crime data from Oakland, and the number of gun
                            control laws in California. The violent crime data is the same that is shown in the
                            first grpahic and the data on gun control laws is from ",
                            a("State Firearm Laws",
                              href = "http://www.statefirearmlaws.org/resources"),
                            ". The regressions show that over the years of 2011 to 2017, the
                            number of violent crimes in San Francisco has actually had a slight increase,
                            the number of violent crimes in Oakland had a slight decrease, and the number
                            of gun control laws in California steadily rose. However, the violent crime
                            rates of both San Francisco and Oakland both started decreasing from 2013
                            on and from 2012 to 2013, there was a slightly higher number of gun control
                            laws passed, from 95 to 99. This suggests that one of those 4 new pieces of
                            legislation might possibly have something to do with the decrease in gun
                            violence in San Francisco and Oakland while it increased in many other US
                            cities (as seen in the first two graphics)."
                            )),
                 tabPanel("Analysis",
                          h1("Thought Process"),
                          p("At the start of the whole project, I wanted to start by finding data to
                            visualize and confirm that gun violence in San Francisco and Oakland. The
                            closest data I could find at first was violent crime data, which I showed
                            in my first two graphics under Violent Crimes. After that, I tried to find
                            more data specifically relating to gun violence. As I mentioned before, ",
                          a("The Trace",
                            href = "https://www.thetrace.org/violent-crime-data/"),
                          " had some very helpful datasets. However, these datasets were difficult to
                          work with and in many cases just included too many variables that were not useful
                          for my general graphic. I ended up using the Oakland data from The Trace and
                          a big crime dataset with over 2 million rows from the City of San Francisco to
                          plot datapoints with coordinates. I give a more in depth explanation about this 
                          process in my Methods tab under Gun Violence Datapoints. Confirming that there
                          was a decrease in gun violence in these two cities from 2013 to 2017, I went to
                          dig deeper into why. I started by looking at criminal justice reforms to see
                          if if there was any connection between that and the decrease in gun violence.
                          What I found was inconclusive, because there is very limited data on imprisonment
                          and most of the data I found did not include San Francisco. I still created a graph
                          showing the trend in violent crimes and imprisonment rates, but since San Francisco
                          data was missing, I decided not to pursue any further there. Instead, I moved onto
                          gun control legislation and that is where things got interesting. Looking at the
                          regression model I made showing violent crimes and gun control laws in California,
                          there appeared to a jump in four new gun control laws passed from 2012 to 2013 - 
                          exactly when violent crimes started decreasing in Oakland and San Francisco. Looking
                          deeper into these four new laws, one had to do with ballistic fingerprinting 
                          and microstamping, two had to do with gun storage and child access, and one had to
                          do with removing guns from people with a domestic violence-related restraining order.
                          Cross-referencing back to other states that have implemented these same laws (especially
                          looking at the states of the cities that showed an increase in violent crimes in my first
                          two graphs) I found that the first three laws were also shared in states with increasing
                          gun violence and the only one that only California had was removing guns from people with 
                          a domestic violence-related restraining order - identified as dvroremoval in the dataset."),
                          h1("dvroremoval"),
                          p("The first thing I did after realizing that this law was not as common in other states,
                            I cleaned the data to create a graphic that would show which states did have this same law.
                            The resulting graphic can be seen below. Massachusetts seemed to have adopted this piece of
                            law back in 1994, long before California in 2013, Minnesota in 2014, and New Jersey in 2017.
                            Looking at gun violence data in these other states, ",
                            a("Erin Guetzloe's",
                              href = "https://eringuetzloe.shinyapps.io/gun-violence-visualizations/"),
                            " study on Boston, Massachusetts 
                            saw an increase in gun violence. However, they adopted this law much earlier in the 90s, so it may
                            be an outdated law or other factors are causing an increase. In Minneapolis, Minnesota there
                            is limited data on gun violence and I suggest further research into gun violence there. For
                            New Jersey, the law is too new which leads to limited data. It would wise to keep track of
                            gun violence rates in cities like Newark, New Jersey in the next few years."),
                          h1("Conclusion"),
                          p("While my findings may be inconclusive, there is simply not enough data to determine if
                            dvroremoval laws were the leading cause of San Francisco's and Oakland's decrease in gun
                            violence from 2013 to 2017. Each of these four implementations of dvroremoval might also
                            vary between each other, leading some to be more effective than others. It does appear
                            that California's implementation is much more specific than others as seen in ",
                            a("this",
                              href = "https://lawcenter.giffords.org/gun-laws/policy-areas/who-can-have-a-gun/domestic-violence-firearms/"),
                            " article by Giffords Law Center to Prevent Gun Violence. That being said, I strongly
                              recommend keeping a tab on dvroremoval in the next few years and seeing its effects on
                              gun violence in states that have implemented it. For further studies, I also recommend
                              comparing how different states have implemented and are enforcing this piece of
                              legislation. Contacting someone or an organization with more law background would be
                              especially helpful in comparing these different implementations."),
                          plotlyOutput("laws_analysis_Plotly", height = "100%")
))

# The code for this server was found here:
# https://stackoverflow.com/questions/35421923/how-to-create-and-display-an-animated-gif-in-shiny
server <- function(input, output, session) {
    
    # Renders violence plot
    
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
            mode = 'lines',
            width = 950, 
            height = 500
        ) %>% 
            layout(
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
    
    # Renders violence per capita plot
    
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
            mode = 'lines',
            width = 950, 
            height = 500
        ) %>% 
            layout(
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
    
    # Renders imprisonment plot
    
    output$imprisonment_Plotly <- renderPlotly(
        imprisonment_graphic <- plot_ly(
            data = imprisonment_data,
            x = ~year, 
            y = ~numbers,
            color = ~variables,
            text = ~numbers, 
            hoverinfo = "text",
            type = 'scatter',
            mode = 'line',
            width = 950, 
            height = 500
        ) %>% 
            layout(
                title = 'Violent Crimes and California Imprisonment vs. Year',
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
    )
    
    # Renders laws dropdown plot
    
    output$laws_Plotly <- renderPlotly(
        laws_graphic <- plot_ly(data = laws, x = ~year, width = 850, height = 500) %>%
            add_markers(y = ~san_francisco_violent, name = "San Francisco") %>%
            add_lines(x = ~year, y = fitted(fit_SF)) %>%
            add_markers(y = ~oakland_violent, name = "Oakland", visible = F) %>%
            add_lines(x = ~year, y = fitted(fit_OK), visible = F) %>%
            add_markers(y = ~lawtotal, name = "Gun Control Laws", visible = F) %>%
            add_lines(x = ~year, y = fitted(fit_Law), visible = F) %>%
            layout(
                showlegend = FALSE,
                updatemenus = list(
                    list(
                        y = 0.6,
                        x = -0.1,
                        buttons = list(
                            list(method = "restyle",
                                 args = list("visible", list(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)),
                                 label = "San Francisco"),
                            list(method = "restyle",
                                 args = list("visible", list(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)),
                                 label = "Oakland"),
                            list(method = "restyle",
                                 args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)),
                                 label = "Gun Control Laws"))))) %>% 
            layout(
                title = 'Regression of Violent Crimes and Gun Control Laws',
                xaxis = list(
                    title = "Year",
                    zeroline = F
                ),
                yaxis = list(
                    title = "Violent Crimes and Gun Control Laws",
                    zeroline = F
                ),
                annotations = list(x = 1, y = -0.08, text = "Source: Census.gov, fbi.gov, State Firearm Laws", 
                                   showarrow = F, xref='paper', yref='paper', 
                                   xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                   font=list(size=12, color="black"))
            ))
    
    # Plots dvroremoval plot in conclusion
    
    output$laws_analysis_Plotly <- renderPlotly(
        ggplotly(laws_analysis %>%
                ggplot(aes(x = year, fill = state)) +
                geom_bar() +
                theme_classic() +
                theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.line.y = element_blank()) +
                labs(title = "States that have Adopted dvroremoval with Year",
                     x = "Year",
                     fill = "States"),
                width = 950, 
                height = 500
                
        )
    )
    
    # Plots gif of San Francisco crimes
    
    output$SF <- renderImage({
        # Return a list containing the filename
        list(src = "AAGun_SF.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)
    
    # Plots gif of Oakland crimes
    
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
