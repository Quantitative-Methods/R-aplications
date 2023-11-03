########################
#######  R-STAT  #######
########################

library(shiny)
library(bs4Dash)
library(plotly)
library(DT)
library(shinyWidgets)
library(data.table)     
library(readODS)      
library(ggplot2)        
library(ggdist)         
library(psych)         
library(tigerstats)     
library(lsr)           
library(Rmisc)         
library(nortest)        
library(emmeans)        
library(rstatix)        
library(afex)          
library(gmodels)       
library(car)            
library(GPArotation)    
library(yacca)          
library(MASS)           
library(candisc)        
library(dendextend)
library(irr)
library(ggdendro)
library(distributional)
library(fresh)
library(datamods)
library(waiter)

# theme------------------
boja   <- "#4472C4"
tekst  <- "#9D9D9D"
pravac <- "#C0504D"

colors <-c("#4472C4","#c0504e","#9dbb61","#7f66a0","#4bafc7", 
           "#f19f53","#2C4D75","#772C2A","#5F7530","#4D3B62",
           "#276A7C","#B65708","#729ACA","#CD7371","#AFC97A",
           "#9983B5","#6FBDD1","#F9AB6B","#3A679C","#9F3B38",
           "#7E9D40","#664F83","#358EA6","#F3740B","#95B3D7",
           "#D99694","#C3D69B","#B3A2C7","#93CDDD","#FAC090")

  mytheme <- create_theme(bs4dash_status(primary = "#007F5F", success = "#007F5F", info = "#272c30"))
  status <- "success"
  css <- "style1.css"
  logo <- "logo.png"
  home <- "www/home1.png"
  preloader <- "#007F5F"
  
# box------------
headerBorder <- F
solidHeader <- F
elevation <- 1

# table----------
style <- "bootstrap5"
class <- "compact"

# Shiny app------
ui <- bs4DashPage(
  preloader = list(html = tagList(spin_1(), "Loading ..."), color = preloader),
  fullscreen = TRUE,
  help = NULL,
  dark = FALSE,
  controlbar = bs4DashControlbar(
    h4("Setting Graph"), hr(),
      id = NULL,
      disable = FALSE,
      width = 400,
      collapsed = TRUE,
      overlay = TRUE,
      skin = "dark",
      pinned = NULL,
      sliderInput("opa",
                  "Opacity (0-1):",
                  min = 0,
                  max = 1,
                  value = 1,
                  step = 0.1
      ),
      sliderInput("width",
                  "Width  (0-1):",
                  min = 0,
                  max = 1,
                  value = 0.9,
                  step = 0.1
      ),
      plotlyOutput("graph")
      
    ),
  #footer = dashboardFooter(left = "R-STAT; Version: 2.0", right = "Authors: Dražan Dizdar & Darko Katović",  fixed = FALSE),
  
 # Header
  header = dashboardHeader(
    use_theme(mytheme),
    status = status,
    title = bs4DashBrand(
      color = status,
      title = "R - STAT",
      image = logo
    )
  ),
 
  # Sidebar
  sidebar = bs4DashSidebar(
    disable = FALSE,
    width = "320px",
    skin = "dark",
    status = status,
    elevation = 4,
    collapsed = F,
    minified = F,
    bs4SidebarMenu(
      bs4SidebarMenuItem("Home", tabName = "home", icon = icon("home")),
      bs4SidebarMenuItem("Choose Data File", tabName = "data", icon = icon("folder-open")),
      bs4SidebarMenuItem("View Data", tabName = "WD", icon = icon("table-cells")),
      bs4SidebarMenuItem("Data Transformation", tabName = "DT", icon = icon("recycle")),
      bs4SidebarMenuItem("Grouping Categorical Data", icon = icon("users"),
               bs4SidebarMenuSubItem("Chi-Square Goodness of Fit Test", tabName = "FT"),
               bs4SidebarMenuSubItem("Chi-Square Test of Independence", tabName = "CT"),
               bs4SidebarMenuSubItem("McNemar’s Test", tabName = "MCN")
               ),
      bs4SidebarMenuItem("Grouping Continuous Data", tabName = "GCD", icon = icon("chart-area")),
      bs4SidebarMenuItem("Basic Statistics", icon = icon("chart-pie"),
               bs4SidebarMenuSubItem("Descirptive Parameters", tabName = "DP"),
               bs4SidebarMenuSubItem("Testing for Normality", tabName = "TN"),
               bs4SidebarMenuSubItem("CI for Population Mean", tabName = "PM"),
               bs4SidebarMenuSubItem("Correlations Analysis", tabName = "COR")
      ),
      bs4SidebarMenuItem("Univariate Methods", icon = icon("share-nodes"),
               bs4SidebarMenuSubItem("Independent Samples T-Test", tabName = "ISTT"),
               bs4SidebarMenuSubItem("Dependent Samples T-Test", tabName = "DSTT"),
               bs4SidebarMenuSubItem("One-way ANOVA", tabName = "OWA"),
               bs4SidebarMenuSubItem("Repeated Measures ANOVA", tabName = "RMA")
      ),
      bs4SidebarMenuItem("Multivariate Methods", icon = icon("sitemap"), 
               bs4SidebarMenuSubItem("Regression Analysis (simple)", tabName = "SRA"),
               bs4SidebarMenuSubItem("Regression Analysis (multiple)", tabName = "RA"),
               bs4SidebarMenuSubItem("Factor Analysis", tabName = "FA"),
               bs4SidebarMenuSubItem("Canonical Analysis", tabName = "CA"),
               bs4SidebarMenuSubItem("Discriminant Analysis", tabName = "DA"),
               bs4SidebarMenuSubItem("Cluster Analysis", tabName = "CLA")
      ),
      bs4SidebarMenuItem("Reliability Analysis", tabName = "REA",icon = icon("scale-balanced")),
      bs4SidebarMenuItem("Probability Calculator", icon = icon("calculator"),
               bs4SidebarMenuSubItem("Normal Distribution", tabName = "PCN"),
               bs4SidebarMenuSubItem("T - Distribution", tabName = "PCT"),
               bs4SidebarMenuSubItem("F - Distribution", tabName = "PCF"),
               bs4SidebarMenuSubItem("Chi Square - Distribution", tabName = "PCH")
      ),
      bs4SidebarMenuItem("About", tabName = "about", icon = icon("circle-info")),
      bs4SidebarMenuItem("Help", tabName = "help", icon = icon("question"))
    )
  ),
  # Body
  body = bs4DashBody(includeCSS(css),
              tabItems(
                  # Home			
                  tabItem(tabName = "home",
                          fluidRow(
                                   box(title = "Home",
                                       icon = icon("home"),
                                       width = 12,
                                       status = status,
                                       headerBorder = headerBorder,
                                       solidHeader = solidHeader,
                                       elevation = elevation,
                                       collapsible = F,
                                       tags$img(src=home),
                                       br(),
                                       h4("Version: 2.0"),
                                       hr(),
                                       h6("R-STAT is a web application for statistical-graphic data analysis.
                                       R-STAT was developed using the R free software environment for statistical computing and graphics 
                                       and the R packages Shiny and bs4Dash for creating interactive web applications. 
                                       It is hosted on shinyapps.io, a self-service web-hosting platform for Shiny applications managed by RStudio."),
                                       hr(),
                                       h6("Data for statistical-graphical analysis should be prepared so that:"),
                                       h6("- first column of the data table contains unique identifiers of the entity"),
                                       h6("- other columns contain the entity data in variables"),
                                       h6("- modality labels of qualitative variables must not be numbers."),
                                       hr(),
                                       h6("Data files can be created in:"), 
                                       h6("- Comma Separated Values (.csv) format,"), 
                                       h6("- Microsoft Excel Worksheet (.xlsx) format,"),
                                       h6("- LibreOffice Open Document Spreadsheet (.ods) format,"),
                                       h6("- SPSS Statistics Data Document (.sav) format."),
                                       hr(),
                                       h5(a("Create a Data File in Only Office", href="https://personal.onlyoffice.com/"))
                                    )
                          )
                  ),
                  # Import Data
                  tabItem(tabName = "data",
                          fluidRow(
                            box(title = "Choose Data File",
                                width = 3,
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                icon = icon("folder-open"),
                                import_file_ui(
                                  id = "myid",
                                  preview_data = F,
                                  file_extensions = c(".csv", ".xls", ".xlsx", ".sav", ".ods")
                                  ), hr(),
                                h4("Select Entities"),
                                filter_data_ui(id = "filter")
                              ),
                              box(title = "Update & Select Variables",
                                  width = 9,
                                  status = status,
                                  headerBorder = headerBorder,
                                  solidHeader = solidHeader,
                                  elevation = elevation,
                                  collapsible = F,
                                  maximizable = T,
                                  icon = icon("circle-check"),
                                  update_variables_ui("vars"), hr(),
                                  h4("Edit Data"),
                                  edit_data_ui(id = "ed")
                                )
                          )
                  ),
                  # View Data			
                  tabItem(tabName = "WD",
                          fluidRow(
                            box(title = "View Data",
                                width = 12,
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                maximizable = T,
                                icon = icon("table-cells"),
                                dataTableOutput("data")
                            )
                        )
                  ),
                  # Data Transformation 
                  tabItem(tabName = "DT",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                icon = icon("list"),
                                awesomeCheckboxGroup(
                                  "NS", 
                                  "Normally Scaled:",
                                  choices = "", 
                                  selected ="",
                                  status = status
                                ),
                                awesomeCheckboxGroup(
                                  "OS", 
                                  "Inversely Scaled:",
                                  choices = "", 
                                  selected ="",
                                  status = status
                                )
                            ),
                            tabBox(title = "",
                                   width = 8,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   status = status,
                                   type = "tabs",
                                   maximizable = T,
                                   collapsible = F,
                                   
                                   tabPanel("Z-value",
                                   icon = icon("table"),
                                            dataTableOutput("zz")
                                   ),
                                   tabPanel("T-value",
                                            icon = icon("table"),
                                            dataTableOutput("tt")
                                   ),
                                   tabPanel("L-value",
                                            icon = icon("table"),
                                            dataTableOutput("ll")
                                   ),
                                   tabPanel("Z-profile",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("zpro")
                                   ),
                                   tabPanel("T-profile",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("tpro")
                                   ),
                                   tabPanel("L-profile",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("lpro")
                                   )
                            ),
                            box(title = "Entity",
                                width = 2,
                                icon = icon("user"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                awesomeRadio(
                                  inputId = "ENTITETI",
                                  label = "",
                                  choices = "", 
                                  selected ="",
                                  inline = F, 
                                  status = status
                                )
                            )
                        )
                  ),
                  # Chi-Square Goodness of Fit Test  
                  tabItem(tabName = "FT",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                
                                awesomeRadio(
                                  "FT", 
                                  label = "",
                                  choices = "", 
                                  selected ="",
                                  inline = F, 
                                  status = status
                                ),
                                awesomeRadio(
                                  "ori", 
                                  label = "Orientation:",
                                  inline = F, 
                                  status = status,
                                  choices = c(
                                    vertical = "x",
                                    horizontal = "y"
                                  ), 
                                  selected ="x")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   collapsible = F,
                                   status = status,
                                   type = "tabs",
                                   
                                   tabPanel("Frequency Tables", 
                                            icon = icon("table"),
                                            dataTableOutput("tf"),
                                            htmlOutput("chi"),
                                   ),
                                   tabPanel("Bar Chart", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("bar")
                                   ),
                                   tabPanel("Pie Chart", 
                                            icon = icon("chart-pie"),
                                            plotlyOutput("pie")
                                   )
                            )
                       )
                  ),
                  # 3. Chi-Square Test of Independence      
                  tabItem(tabName = "CT",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                
                                awesomeRadio("CT1", 
                                             label = "",
                                             choices = "", 
                                             selected ="",
                                             inline = F, 
                                             status = status
                                ),
                                awesomeRadio("CT2", 
                                             label = "",
                                             choices = "", 
                                             selected ="",
                                             inline = F, 
                                             status = status
                                ),
                                awesomeRadio(
                                  "ori2", 
                                  label = "Orientation:",
                                  inline = F, 
                                  status = status,
                                  choices = c(
                                    vertical = "x",
                                    horizontal = "y"
                                  ), 
                                  selected ="x")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   collapsible = F,
                                   status = status,
                                   type = "tabs",
                                   
                                   tabPanel("Contingency Tables",
                                            icon = icon("table"),
                                            htmlOutput("chi2"), br(),
                                            h5("Observed Frequency"), br(),
                                            dataTableOutput("ctf"), br(),
                                            h5("Observed Frequency (%)"), br(),
                                            dataTableOutput("ctfp"), br(),
                                            h5("Expected Frequency"), br(),
                                            dataTableOutput("ctfe")
                                            
                                   ),
                                   tabPanel("Stacked Bar Chart (f)",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("graf2") 
                                   ),
                                   tabPanel("Stacked Bar Chart (%)", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("graf3")
                                   )
                            )
                        )
                  ),
                  # 4.  McNemar’s Test      
                  tabItem(tabName = "MCN",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                
                                awesomeRadio("MCN1", 
                                             label = "",
                                             choices = "", 
                                             selected ="",
                                             inline = F, 
                                             status = status
                                ),
                                awesomeRadio("MCN2", 
                                             label = "",
                                             choices = "", 
                                             selected ="",
                                             inline = F, 
                                             status = status
                                ),
                                awesomeRadio(
                                  "ori3", 
                                  label = "Orientation:",
                                  inline = F, 
                                  status = status,
                                  choices = c(
                                    vertical = "x",
                                    horizontal = "y"
                                  ), 
                                  selected ="x")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   collapsible = F,
                                   status = status,
                                   type = "tabs",
                                   
                                   tabPanel("Contingency Tables",
                                            icon = icon("table"),
                                            dataTableOutput("ctf2"), br(),
                                            dataTableOutput("ctfp2"),
                                            htmlOutput("mcn")
                                   ),
                                   tabPanel("Stacked Bar Chart (f)",
                                            icon = icon("chart-simple"),
                                            plotlyOutput("graf4") 
                                   ),
                                   tabPanel("Stacked Bar Chart (%)", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("graf5")
                                   )
                            )
                        )
                  ),
                  # 4. Grouping Continuous Data      
                  tabItem(tabName = "GCD",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                awesomeRadio("GCD", 
                                             label = "",
                                             inline = F, 
                                             status = status,
                                             choices = "", 
                                             selected =""
                                ), hr(),
                                sliderInput("nb",
                                            "Number of Bins:",
                                            min = 3,
                                            max = 15,
                                            value = 7
                                ), hr(),
                                htmlOutput("br")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   status = status,
                                   type = "tabs",
                                   tabPanel("Frequency Distribution", 
                                            icon = icon("table"),
                                            dataTableOutput("ftab"),
                                   ),
                                   tabPanel("Histogram", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("hist")
                                   ),
                                   tabPanel("Frequency Polygons",
                                            icon = icon("chart-line"),
                                            plotlyOutput("poly")
                                   )
                             )
                          )
                    ),
                # 5. Descirptive Parameters       
                  tabItem(tabName = "DP",
                          fluidRow(
                            box(title = "Variables",
                                width = 2,
                                icon = icon("list"),
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                awesomeCheckboxGroup("DP", 
                                             label = "",
                                             inline = F, 
                                             status = status,
                                             choices = "", 
                                             selected =""),
                                awesomeRadio("DP2", 
                                             label = "",
                                             inline = F, 
                                             status = status,
                                             choices = "", 
                                             selected ="")
                            ),
                            tabBox(title = "",
                                   width = 7,
                                   headerBorder = headerBorder,
                                   solidHeader = solidHeader,
                                   elevation = elevation,
                                   status = status,
                                   collapsible = F,
                                   type = "tabs",
                                   tabPanel("Descirptive Parameters", 
                                            icon = icon("table"),
                                            dataTableOutput("des")
                                   ),
                                   tabPanel("Box and Whiskers plot", 
                                            icon = icon("chart-simple"),
                                            plotlyOutput("boxds")
                                   )
                            )
                        )
                  ),
                # 5. Testing for Normality       
                tabItem(tabName = "TN",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeRadio("TN", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected =""
                              ),
                              sliderInput("NB",
                                          "Number of Bins:",
                                          min = 1,
                                          max = 100,
                                          value = 7
                              )
                          ),
                          tabBox(title = "",
                                 width = 7,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Testing for Normality", 
                                          icon = icon("note-sticky"), 
                                          textOutput("ks"),
                                          textOutput("lks"),
                                          textOutput("ad"),
                                          textOutput("sf"),
                                          textOutput("sw")
                                 ),
                                 tabPanel("Histogram", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("histogram")
                                 ),
                                 tabPanel("Box and Whiskers plot", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("box")
                                 ),
                                 tabPanel("QQ plot", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("qq")
                                 )
                          )
                      )
                ),
                # 6. Confidence Intervals for the Population Mean        
                tabItem(tabName = "PM",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeRadio("PM", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected =""
                              ),
                              sliderInput("PMp",
                                          "p-value:",
                                          min = 0.01,
                                          max = 0.99,
                                          value = 0.05
                              ),
                              sliderInput("PMn",
                                          "Sample Size:",
                                          min = 3,
                                          max = 500,
                                          value = 100
                              )
                          ),
                          tabBox(title = "",
                                 width = 6,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("CI for the Population Mean", 
                                          icon = icon("note-sticky"),
                                          htmlOutput("ci")
                                 ),
                                 tabPanel("Standard Error and Sample Size", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("gsem")
                                 )
                          )
                        )
                ),
                # 8.  Correlations       
                tabItem(tabName = "COR",
                        fluidRow(
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("COR", 
                                                 "Select Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""
                              ),
                              awesomeRadio("CORM", "Method:",
                                           inline = F, 
                                           status = status,
                                           choices = c(
                                             Pearson = "pearson",
                                             Spearman = "spearman",
                                             Kendall = "kendall"
                                           ),
                                           selected = "pearson"
                              )
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 maximizable = T,
                                 tabPanel("Correlations",
                                          icon = icon("table"),
                                          p("Marked correlations are significant at p < 0.05"),
                                          dataTableOutput("r")
                                 ),
                                 tabPanel("Level of Significance", 
                                          icon = icon("table"),
                                          p("Marked p-value < 0.05"),
                                          dataTableOutput("p")
                                 ),
                                 tabPanel("Scatter Plot", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("scatter")
                                 )
                          ),
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              tabPanel("Scatter Plot",
                                       icon = icon("chart-simple"),
                                       awesomeRadio("XX", 
                                                    label = "",
                                                    inline = F, 
                                                    status = status,
                                                    choices = "", selected =""),
                                       awesomeRadio("YY", 
                                                    label = "",
                                                    inline = F, 
                                                    status = status,
                                                    choices = "", selected ="")
                              )
                          ) 
                      )
                ),
                # 9.  Independent Samples T-Test 
                tabItem(tabName = "ISTT",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("DVITT", 
                                                 "Dependent Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", selected =""
                              ),
                              awesomeRadio("IVITT", 
                                           label = "Independent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("ISTD", 
                                           label = "Dependent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              )
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("T-test (more variables)",
                                          icon = icon("table"),
                                          dataTableOutput("titt")
                                 ),
                                 tabPanel("Descirptive Parameters", 
                                          icon = icon("table"),
                                          dataTableOutput("dest"),
                                          hr(),
                                          h5("Box and Whiskers plot"),
                                          plotlyOutput("boxt")
                                 ),
                                 tabPanel("T-test (one variable)", 
                                          icon = icon("note-sticky"),
                                          htmlOutput("t"),
                                          hr(),
                                          h5("Mean Plot with 95% Confidence Interval"),
                                          plotlyOutput("err")
                                 )
                          )
                     )
                ),
                # 10.  Dependent (Paired) Samples T-Test   
                tabItem(tabName = "DSTT",
                        fluidRow(
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("DSTDV", 
                                                 "Dependent Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", selected =""
                              ),
                              awesomeRadio("DSTI", 
                                           label = "Independent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("DSTD", 
                                           label = "Dependent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              )
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("T-test (more variables)",
                                          icon = icon("table"),
                                          dataTableOutput("tdtt")
                                 ),
                                 tabPanel("Descirptive Parameters", 
                                          icon = icon("table"),
                                          dataTableOutput("dest2"),
                                          hr(),
                                          h5("Normality Test (Shapiro-Wilk)"),
                                          textOutput ("shapiro2"),
                                          hr(),
                                          h5("Box and Whiskers plot"),
                                          plotlyOutput("boxt2")

                                 ),
                                 tabPanel("T-test (one variable)", 
                                          icon = icon("note-sticky"),
                                          htmlOutput("t2"),
                                          hr(),
                                          h5 ("Mean Plot with 95% Confidence Interval"),
                                          plotlyOutput("err2")
                                 )

                          )
                      )
                ),
                # 11.  One Way ANOVA
                tabItem(tabName = "OWA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("ANOVADV", 
                                                 "Dependent Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", selected =""
                              ),
                              awesomeRadio("ANOVAI", 
                                           label = "Independent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("ANOVAD", 
                                           label = "Dependent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("ADJ", 
                                           label = "Method for adjusting p-values:",
                                           inline = F, 
                                           status = status,
                                           choices = c(
                                             none = "none",
                                             Tukey = "tukey",
                                             Scheffe = "scheffe",
                                             Bonferroni = "bonferroni",
                                             Holm  = "holm",
                                             Sidak ="sidak"),
                                           selected = "none"
                              )
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("ANOVA (more variable)",
                                          icon = icon("table"),
                                          dataTableOutput("arez")
                                 ),
                                 tabPanel("Descirptive Parameters", 
                                          icon = icon("table"),
                                          dataTableOutput("desa"),
                                          hr(),
                                          h5("Box and Whiskers plot"),
                                          plotlyOutput("bwp")
                                          
                                 ),
                                 tabPanel("ANOVA (one variable)", 
                                          icon = icon("note-sticky"),
                                          dataTableOutput("anovat"),
                                          hr(),
                                          htmlOutput("anova"),
                                          hr(),
                                          h5 ("Mean Plot with 95% Confidence Interval"),
                                          plotlyOutput("erra"),
                                          hr(),
                                          h5("Post Hoc Test for Multiple Comparisons"),
                                          dataTableOutput ("PH")
                                 )
                          )
                      )
                ),
                # 12. Repeated Measures ANOVA
                tabItem(tabName = "RMA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeRadio("ENT", 
                                           label = "Entities:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("RMAM", 
                                           label = "Measurements:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("RMADV", 
                                           label = "Dependent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", selected = ""
                              ),
                              awesomeRadio("PHr", 
                                           "Post-hoc pairwise comparisons:",
                                           inline = F, 
                                           status = status,
                                           choices = c(
                                             none = "none",
                                             Tukey = "tukey",
                                             Scheffe = "scheffe",
                                             Bonferroni = "bonferroni",
                                             Holm  = "holm",
                                             Sidak ="sidak"),
                                           selected = "none"
                              )
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Descirptive Parameters", 
                                          icon = icon("table"),
                                          dataTableOutput("desrma"), 
                                          hr(),
                                          h5("Box and Whiskers plot"),
                                          plotlyOutput("bwprma")
                                          
                                 ),
                                 tabPanel("ANOVA", 
                                          icon = icon("note-sticky"),
                                          dataTableOutput("aovrm"), 
                                          hr(),
                                          htmlOutput("rma2"),
                                          hr(),
                                          h5 ("Mean Plot with 95% Confidence Interval"),
                                          plotlyOutput("errrma"),
                                          hr(),
                                          h5("Post Hoc Test for Multiple Comparisons"),
                                          dataTableOutput ("pwc")
                                 )
                          )
                      )
                ),
                # 12.  Regression Analysis (simple)
                tabItem(tabName = "SRA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeRadio("X", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected =""),
                              awesomeRadio("Y", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected ="")
                              
                          ),
                          tabBox(title = "",
                                 width = 6,
                                 status = status,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 type = "tabs",
                                 tabPanel("Regression Results", 
                                          icon = icon("note-sticky"),
                                          htmlOutput("rsra"), hr(),
                                          plotlyOutput("gsra")
                                 ),
                                 tabPanel("Predict Dependent Var.", 
                                          icon = icon("note-sticky"),
                                          numericInput("b0", "Intercept (b0):", 0, width = '220px'),
                                          numericInput("xx", "Independent value:", 0, width = '220px'),
                                          numericInput("pp", "Confidence interval (%):", 95, width = '220px'),
                                          htmlOutput("yy")
                                 ),
                                 tabPanel("Regression Scores", 
                                          icon = icon("table"),
                                          dataTableOutput("rs")
                          )
                       )
                    )
                ),
                # 13.  Regression Analysis (multipe)
                tabItem(tabName = "RA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("PRED", 
                                                 label = "",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""),
                              awesomeRadio("KRIT", 
                                           label = "",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected ="")
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Reg. Results", 
                                          icon = icon("table"),
                                          htmlOutput("ro"), hr(),
                                          dataTableOutput("rez")
                                 ),
                                 tabPanel("Graph (P)", 
                                          icon = icon("chart-simple"),
                                          h6("Graph of Partial Coefficients of Determinations (P)"),
                                          plotlyOutput("gro")
                                 ),
                                 tabPanel("Graph (Beta, Part R, R)", 
                                          icon = icon("chart-simple"),
                                          h6("Graph of Standardized Regression Coeff.(BETA), Partial Correlations (PART-R) and Correlations (R)"),
                                          plotlyOutput("gcoef")
                                 ),
                                 tabPanel("Reg. Scores", 
                                          icon = icon("table"),
                                          dataTableOutput("dep")
                                 )
                          )
                      )
                ),
                
                # 14.  Factor Analysis
                tabItem(tabName = "FA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("MV", 
                                                 "Select Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""
                              ),
                              sliderInput("k",
                                          "Number Principal Components:",
                                          step = 1,
                                          min = 1,
                                          max = 1,
                                          value = 1
                              ),
                              awesomeRadio("method2", 
                                           inline = F, 
                                           status = status,
                                           "Rotated Principal Components:",
                                           choices = c(none = "none",
                                                       varimax = "varimax",
                                                       quartimax = "quartimax",
                                                       oblimin = "oblimin",
                                                       promax = "promax"
                                           ),
                                           selected = "none"
                              )
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Eigenvalue", 
                                          icon = icon("table"),
                                          dataTableOutput("Lamda"),
                                          htmlOutput("ssmc")
                                 ),
                                 tabPanel("Scree Plot", 
                                          icon = icon("chart-simple"),
                                          plotlyOutput("spgk")
                                 ),
                                 tabPanel("Pattern", 
                                          icon = icon("table"),
                                          dataTableOutput("A")
                                 ),
                                 tabPanel("Structure", 
                                          icon = icon("table"),
                                          dataTableOutput("FF")
                                 ),
                                 tabPanel("Correlation", 
                                          icon = icon("table"),
                                          dataTableOutput("M")
                                 ),
                                 tabPanel("Graph", 
                                          icon = icon("chart-simple"),
                                          h6("Graph Factor Loadings (Variables)"),
                                          plotlyOutput("GFF1"),
                                          h6("Graph Factor Loadings (Factors)"),
                                          plotlyOutput("GFF2")
                                 ),
                                 tabPanel("Scores", 
                                          icon = icon("table"),
                                          dataTableOutput("Scores")
                                 )
                          )
                      )
                ),
                # 15.  Canonical Analysis
                tabItem(tabName = "CA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("CA1", 
                                                 "Select Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""
                              ),
                              awesomeCheckboxGroup("CA2", 
                                                 "Select Variables:",
                                                 inline = F, 
                                                 status = status,
                                                 choices = "", 
                                                 selected =""
                              )
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("Canonical Factor",
                                          icon = icon("table"),
                                          dataTableOutput("RC")
                                 ),
                                 tabPanel("Structure",
                                          icon = icon("table"),
                                          h6("Factor Structure 1. Set"),
                                          dataTableOutput("F1"),
                                          hr(),
                                          h6("Factor Structure 2. Set"), 
                                          dataTableOutput("F2"),
                                 ),
                                 tabPanel("Graph", 
                                          icon = icon("chart-simple"),
                                          h6("Graph Factor Structure 1. Set"),
                                          plotlyOutput("GFFF1"),
                                          hr(),
                                          h6("Graph Factor Structure 2. Set"),
                                          plotlyOutput("GFFF2"),
                                 ),
                                 tabPanel("Scores 1. Set", 
                                          icon = icon("table"),
                                          dataTableOutput("CF1")
                                 ),
                                 tabPanel("Scores 2. Set", 
                                          icon = icon("table"),
                                          dataTableOutput("CF2")
                                 )
                           )
                      )
                ),
                # 16.  Discriminant Analysis
                tabItem(tabName = "DA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("DAD", 
                                                   "Select Variables:",
                                                   inline = F, 
                                                   status = status,
                                                    choices = "", 
                                                    selected =""
                              ),
                              awesomeRadio("DAI", 
                                           label = "Independent Variables:",
                                           inline = F, 
                                           status = status,
                                           choices = "", 
                                           selected =""
                              )
                          ),
                          tabBox(title = "",
                                 width = 8,
                                 headerBorder = headerBorder,
                                 solidHeader = solidHeader,
                                 elevation = elevation,
                                 collapsible = F,
                                 status = status,
                                 type = "tabs",
                                 tabPanel("DF", 
                                          icon = icon("table"),
                                          dataTableOutput("df")
                                 ),
                                 tabPanel("Structure", 
                                          icon = icon("table"),
                                          h6("Structure Discriminant Functions"),
                                          dataTableOutput("sdf")
                                 ),
                                 tabPanel("Centroids",
                                          icon = icon("table"),
                                          h6("Group Centroids"),
                                          dataTableOutput("cg")
                                 ),
                                 tabPanel("Classifications",
                                          icon = icon("table"),
                                          h6("Classifications Matrix (Frequency)"),
                                          dataTableOutput("clas1"),
                                          hr(),
                                          h6("Classifications Matrix (%)"),
                                          dataTableOutput("clas2"),
                                          hr(),
                                          "Rows: Actual classifications, Columns: Predicted classifications"
                                 ),
                                 tabPanel("Graph",
                                          icon = icon("chart-simple"),
                                          h6("Graph Structure Discriminant Functions"),
                                          plotlyOutput("gsdf")
                                 ),
                                 tabPanel("Scores", 
                                          icon = icon("table"),
                                          dataTableOutput("dfs")
                                 )
                          )
                      )
                ),
                # 17.  Cluster Analysis
                tabItem(tabName = "CLA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2,
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("CLV", 
                                                   "Select Variables:",
                                                   inline = F, 
                                                   status = status,
                                                   choices = "", 
                                                   selected =""
                              ),
                              awesomeRadio("dist", "Distances:",
                                           inline = F, 
                                           status = status,
                                           choices = c(euclidean = "euclidean",
                                                       maximum = "maximum",
                                                       manhattan = "manhattan",
                                                       canberra = "canberra",
                                                       minkowski = "minkowski"
                                           ),
                                           selected = "euclidean"
                              ),
                              awesomeRadio("clmethod", "Method:",
                                           inline = F, 
                                           status = status,
                                           choices = c(ward.D = "ward.D",
                                                       ward.D2 = "ward.D2",
                                                       single = "single",
                                                       complete = "complete",
                                                       average = "average",
                                                       mcquitty = "mcquitty",
                                                       median = "median",
                                                       centroid ="centroid"
                                           ),
                                           selected = "ward.D"
                              ),
                              sliderInput("nk",
                                          "Number Clusters:",
                                          step = 1,
                                          min = 1,
                                          max = 1,
                                          value = 1
                              )
                          ),
                          box(title = "Groups",
                              icon = icon("table"),
                              width = 2,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              dataTableOutput("clus")
                          ),
                          box(title = "Cluster dendrogram",
                              icon = icon("sitemap"),
                              width = 8,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              collapsible = F,
                              status = status,
                              plotlyOutput("dend")
                          )
                      )
                ),
                # 18.  Reliability Analysis
                tabItem(tabName = "REA",
                        fluidRow(
                          box(title = "Variables",
                              width = 2, 
                              icon = icon("list"),
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              awesomeCheckboxGroup("REA", 
                                                 "Select Variables:",
                                                 choices = "", 
                                                 selected =""
                              ),
                              hr(),
                              h5("Intraclass Correlation"),
                              awesomeRadio("model", "Model:",
                                           inline = F, 
                                           status = status,
                                           c("Oneway" = "one",
                                             "Twoway" = "two")),
                              awesomeRadio("type", "Type:",
                                           inline = F, 
                                           status = status,
                                           c("Consistency" = "con",
                                             "Agreement" = "agr")),
                              awesomeRadio("unit", "Unit:",
                                           inline = F, 
                                           status = status,
                                           c("Single" = "sin",
                                             "Average" = "ave"))
                          ),
                          box(title = "Reliability Analysis Results",
                              icon = icon("table"),
                              width = 5, 
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              h5("Reliability Coefficients"),
                              htmlOutput("alpha"),
                              hr(),
                              h5("Intraclass Correlation"),
                              htmlOutput("icc"),
                              hr(),
                              h5("Item Reliability Statistics"),
                              dataTableOutput("irs")
                          ),
                          box(title = "Condensed Data",
                              icon = icon("table"),
                              width = 3, 
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              dataTableOutput("cd")
                          )
                        )
                ),
                # 19.  Probability Calculator
                # Normal (Gauss) Distribution
                tabItem(tabName = "PCN",
                        fluidRow(
                          box(title = "Input Values",
                              icon = icon("pen"),
                              width = 2,
                              status = status,
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              numericInput("as1", "Mean", 180, width = '150px'),
                              verbatimTextOutput("as1"),
                              numericInput("sd1", "Standard Deviation", 10, width = '150px'),
                              verbatimTextOutput("sd1"),
                              numericInput("rez1", "Value (X)", 190, width = '150px'),
                              verbatimTextOutput("rez1"), hr(),
                              htmlOutput("px")
                          ),
                          box(title = "Normal (Gauss) Distribution",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("grafnd")
                          )
                        )
                ),
                tabItem(tabName = "PCT",
                        fluidRow(
                          box(title = "Input Values",
                              icon = icon("pen"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 2,
                              numericInput("dft", 
                                           "df", 
                                           value = 3, 
                                           min = 1,
                                           max = 500,
                                           step = 1,
                                           width = '150px'
                              ),
                              numericInput("pt", 
                                           "p-value", 
                                           value = 0.05, 
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           width = '150px'
                              ), br(),
                              htmlOutput("tp"), hr(),
                              numericInput("xt", 
                                           "t-value", 
                                           value = 2, 
                                           min = 0,
                                           max = 10,
                                           step = 0.01,
                                           width = '150px'
                              ), br(),
                              htmlOutput("pt")
                          ),
                          box(title = "T - Distribution - Upper tailed",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("graftd")
                          ),
                          box(title = "T - Distribution - Two tailed",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("graftd2")
                          )
                        )
                ),
                tabItem(tabName = "PCF",
                        fluidRow(
                          box(title = "Input Values",
                              icon = icon("pen"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 2,
                              numericInput("df1", 
                                           "df1", 
                                           value = 3, 
                                           min = 1,
                                           max = 100,
                                           step = 1,
                                           width = '150px'
                              ),
                              numericInput("df2", 
                                           "df2", 
                                           value = 100, 
                                           min = 1,
                                           max = 500,
                                           step = 1,
                                           width = '150px'
                              ),
                              numericInput("pf", 
                                           "p-value", 
                                           value = 0.05, 
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           width = '150px'
                              ), br(),			           
                              htmlOutput("fp"), hr(),
                              numericInput("xf", 
                                           "F-value", 
                                           value = 3, 
                                           min = 0,
                                           max = 10,
                                           step = 0.01,
                                           width = '150px'
                              ),br(),
                              htmlOutput("pf")
                          ),
                          box(title = "F-Distribution",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("graff")
                          )
                        )
                ),
                tabItem(tabName = "PCH",
                        fluidRow(
                          box(title = "Input Values",
                              icon = icon("pen"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 2,
                              numericInput("dfh", 
                                           "df", 
                                           value = 10, 
                                           min = 1,
                                           max = 30,
                                           step = 1,
                                           width = '150px'
                              ),
                              numericInput("ph", 
                                           "p-value", 
                                           value = 0.05, 
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           width = '150px'
                              ), br(),
                              htmlOutput("hp"), hr(),
                              numericInput("xh", 
                                           "Chi-value", 
                                           value = 10, 
                                           min = 0,
                                           max = 100,
                                           step = 0.1,
                                           width = '150px'
                              ), br(),
                              htmlOutput("ph")
                          ),
                          box(title = "Chi-Square Distribution",
                              icon = icon("chart-area"),
                              headerBorder = headerBorder,
                              solidHeader = solidHeader,
                              elevation = elevation,
                              collapsible = F,
                              status = status,
                              width = 5,
                              plotOutput("grafh")
                          )
                      )
                  ),
                  # 22. About   
                  tabItem(tabName = "about",
                          fluidRow(
                            box(title = "About",
                                icon = icon("circle-info"),
                                width = 12,
                                status = status,
                                headerBorder = headerBorder,
                                solidHeader = solidHeader,
                                elevation = elevation,
                                collapsible = F,
                                tags$img(src=home), 
                                br(),
                                h4("Version: 2.0"),
                                hr(),
                                h6("Built on: 21.10.2023."),
                                h6("Licence: MIT LicenseCopyright (c) 2021 Quantitative-Methods"),
                                h6("Permission is hereby granted, free of charge, to any person obtaining a copy
                    							of this software and associated documentation files (the 'Software'), to deal
                    							in the Software without restriction, including without limitation the rights
                    							to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
                    							copies of the Software, and to permit persons to whom the Software is 
                    							furnished to do so, subject to the following conditions:
                    							The above copyright notice and this permission notice shall be included in all
                    							copies or substantial portions of the Software."),
                                hr(),
                                h6("THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
                    							IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
                    							FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
                    							AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
                    							LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
                    							OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
                    							SOFTWARE."),
                                hr(),
                                h6("Authors: Dražan Dizdar & Darko Katović"),
                                hr(),
                                h5(a("Source", href="https://github.com/Quantitative-Methods/R-aplications/"))
                              )
                          )
                  ),
                  # 24. Help  
                  tabItem(tabName = "help",
                          fluidRow(
                            tabPanel("pdf", tags$iframe(style = "height: 900px; width: 1500px; scrolling=yes", 
                                                        src="RSTAT-help.pdf"
                                                        )
                            )
                        )
                  )
            )
      )
)

# Server #
server <- function(input, output) {
  
  
  # Import Data

  id <- import_file_server(
    id = "myid",
    btn_show_data = TRUE,
    show_data_in = "modal"
  )
  
  # Filter Data
  
  fd <- filter_data_server(
    id = "filter",
    data = reactive(id$data()),
    vars = reactive(names(id$data())),
    widget_char = "virtualSelect"
  )
  
  # Edit data
  
  ed <- edit_data_server(
    id = "ed",
    data_r = reactive(fd$filtered()),
    add = TRUE,
    update = TRUE,
    delete = TRUE,
    download_csv = TRUE,
    download_excel = TRUE,
    file_name_export = "data"
  )
  
  # Update Data
  
  data <- update_variables_server(
    id = "vars",
    data = reactive(data.frame(ed()[,-1], row.names=ed()[,1]))
  )
  
# Selekcija podataka
  observeEvent(data(),{
    
    m <- ncol(data() %>% select_if(is.numeric))
    k <- ncol(data() %>% select_if(is.character))    
    n <- nrow(data())
    
    if(m<3){
      updateSliderInput(inputId = "k", 
                        min = 1, 
                        max = m, 
                        step = 1,
                        value = 1
      )
    }
    else{
      my_data <- data() %>% select_if(is.numeric)
      R <- cor(my_data)
      ev <- eigen(R)
      ev <- ev$values
      zgk <- length(which(1 <= ev))
      updateSliderInput(inputId = "k", 
                        min = 1, 
                        max = m, 
                        step = 1,
                        value = zgk
      )
    }
    updateNumericInput(inputId = "b0", value = 0)
    updateSliderInput(inputId = "PMn", 
                      min = 3, 
                      max = 500, 
                      value = n
    )
    updateSliderInput(inputId = "nk", 
                      min = 2, 
                      max = 25, 
                      step = 1,
                      value = 2
    )
    
    if(k>0){
      updateAwesomeRadio(inputId = "FT",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "CT1",
                         label = "Select Variable 1:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "CT2",
                         label = "Select Variable 2:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)),
      )
      updateAwesomeRadio(inputId = "MCN1",
                         label = "Select Variable 1:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "MCN2",
                         label = "Select Variable 2:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)),
      )
      updateAwesomeRadio(inputId = "IVITT",
                         label = "Select Grouping Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)),
      )
      updateAwesomeRadio(inputId = "DSTI",
                         label = "Select Grouping Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "ANOVAI",
                         label = "Select Grouping Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "RMAM",
                         label = "Select Measurements Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)),
      )
      updateAwesomeRadio(inputId = "DAI",
                         label = "Select Grouping Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)),
      )
      updateAwesomeRadio(inputId = "GBAR1",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "GBAR2",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "GPIE1",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "GPIE2",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "GG",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "GG1",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "GG2",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
      updateAwesomeRadio(inputId = "GG3",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.character)), 
      )
    }
    else{}

    if(m>0){
      updateAwesomeCheckboxGroup(inputId = "NS",
                                 label = "Normally Scaled:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric)),
                                 selected = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeCheckboxGroup(inputId = "OS",
                                 label = "Inversely Scaled:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeRadio(inputId = "ENTITETI",
                         label = "Select Entity:",
                         status = status,
                         choices = row.names(data() %>% select_if(is.numeric))
      )
      updateAwesomeCheckboxGroup(inputId = "DT",
                               label = "Select Variables:",
                               status = status,
                               choices = names(data() %>% select_if(is.numeric)), 
                               selected = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeCheckboxGroup(inputId = "DP",
                               label = "Select Variables:",
                               status = status,
                               choices = names(data() %>% select_if(is.numeric)), 
                               selected = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeRadio(inputId = "DP2",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "GCD",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "TN",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "PM",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeCheckboxGroup(inputId = "COR",
                         label = "Select Variables:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
                         selected = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeRadio(inputId = "ISTD",
                         label = "Select Dependent Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "DSTD",
                         label = "Select Dependent Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric))[2:m], 
      )
      updateAwesomeRadio(inputId = "ANOVAD",
                         label = "Select Dependent Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "ENT",
                         label = "Select Entities Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "RMADV",
                         label = "Select Dependent Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric))[2:m], 
      )
      updateAwesomeCheckboxGroup(inputId = "DVITT",
                               label = "Select Dependent Variables:",
                               status = status,
                               choices = names(data() %>% select_if(is.numeric)), 
                               selected = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeCheckboxGroup(inputId = "DSTDV",
                               label = "Select Dependent Variables:",
                               status = status,
                               choices = names(data() %>% select_if(is.numeric))[2:m],
                               selected = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeCheckboxGroup(inputId = "ANOVADV",
                               label = "Select Dependent Variables:",
                               status = status,
                               choices = names(data() %>% select_if(is.numeric)), 
                               selected = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeRadio(inputId = "XX",
                         label = "Select X Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "YY",
                         label = "Select Y Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "X",
                         label = "Select Independent Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "Y",
                         label = "Select Dependent Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "GX",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "GX1",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "GX2",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "GX3",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "GY3",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "GX4",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      updateAwesomeRadio(inputId = "GY4",
                         label = "Select Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)), 
      )
      
      if(m>1){
        updateAwesomeCheckboxGroup(inputId = "PRED",
                                 label = "Select Independent Variables:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric)), 
                                 selected = names(data() %>% select_if(is.numeric))[1:m-1]
        )
        updateAwesomeCheckboxGroup(inputId = "REA",
                                 label = "Select Variables:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric)), 
                                 selected = names(data() %>% select_if(is.numeric))[1:2]
        )
      }
      else{
        updateAwesomeCheckboxGroup(inputId = "PRED",
                                 status = status,
                                 label = "Select Independent Variables:",
                                 choices = names(data() %>% select_if(is.numeric)), 
                                 selected = names(data() %>% select_if(is.numeric))[1:m]
        )
        updateAwesomeCheckboxGroup(inputId = "REA",
                                 label = "Select Variables:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric)), 
                                 selected = names(data() %>% select_if(is.numeric))[1]
        )
      }
      
      if(m>2){
        updateAwesomeCheckboxGroup(inputId = "CA1",
                                 label = "Select Variables 1. Set:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric)), 
                                 selected = names(data() %>% select_if(is.numeric))[1:(m-2)]
        )
        updateAwesomeCheckboxGroup(inputId = "CA2",
                                 label = "Select Variables 2. Set:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric)), 
                                 selected = names(data() %>% select_if(is.numeric))[(m-1):m]
        )
        updateAwesomeCheckboxGroup(inputId = "REA",
                                 label = "Select Variables:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric)), 
                                 selected = names(data() %>% select_if(is.numeric))[1:3]
        )
      }
      else{
        updateAwesomeCheckboxGroup(inputId = "CA1",
                                 label = "Select Variables 1. Set:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric)), 
                                 selected = names(data() %>% select_if(is.numeric))[1]
        )
        updateAwesomeCheckboxGroup(inputId = "CA2",
                                 label = "Select Variables 2. Set:",
                                 status = status,
                                 choices = names(data() %>% select_if(is.numeric)), 
                                 selected = names(data() %>% select_if(is.numeric))[m]
        )
      }
      updateAwesomeRadio(inputId = "KRIT",
                         label = "Select Dependent Variable:",
                         status = status,
                         choices = names(data() %>% select_if(is.numeric)),
                         selected = names(data() %>% select_if(is.numeric))[m]
      )
      updateAwesomeCheckboxGroup(inputId = "MV",
                               label = "Select Variables:",
                               status = status,
                               choices = names(data() %>% select_if(is.numeric)), 
                               selected = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeCheckboxGroup(inputId = "DAD",
                               label = "Select Dependent Variables:",
                               status = status,
                               choices = names(data() %>% select_if(is.numeric)),
                               selected = names(data() %>% select_if(is.numeric))
      )
      updateAwesomeCheckboxGroup(inputId = "CLV",
                               label = "Select Variables:",
                               status = status,
                               choices = names(data() %>% select_if(is.numeric)),
                               selected = names(data() %>% select_if(is.numeric))
      )
      
    }
    
    else{}
    
  })
  
  # Data

  output$data <- renderDataTable({
    my_data <- data()
    n <- nrow(my_data)
    numeric <- my_data %>% select_if(is.numeric) 
    character <- my_data %>% select_if(is.character) 
    numeric <- round(numeric, digits = 3)
    my_data <- cbind(character, numeric)
    my_data %>% 
        DT::datatable (rownames = TRUE,
                       style = style,
                       class = class,
                       extensions = c("Buttons"),
                       options = list(pageLength = n, 
                                      dom = 'Bt', 
                                      buttons = 'copy'
                       )
        )
  })
  
  
  # Data Transformation
  output$zz <- renderDataTable({
    my_data <- data()
    ns <- my_data[, input$NS]
    os <- my_data[, input$OS]
    n <- nrow(my_data)
    Zns <- scale(ns)
    Zos <- scale(os)*(-1)
    Z <- cbind(Zns, Zos)
    MEAN_Z = rowMeans(Z)
    Z <- round(cbind(Z, MEAN_Z), digits = 3)
    Z %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = n, 
                                    dom = 'Bt', 
                                    buttons = 'copy')) %>% 
      formatStyle('MEAN_Z', fontWeight = 'bold')
  })
  output$tt <- renderDataTable({
    my_data <- data()
    ns <- my_data[, input$NS]
    os <- my_data[, input$OS]
    n <- nrow(my_data)
    tns <- scale(ns)*10+50
    tos <- (scale(os)*(-1))*10+50
    tt <- cbind(tns, tos)
    MEAN_T = rowMeans(tt)
    tt <- round(cbind(tt, MEAN_T), digits = 3)
    tt %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = n, 
                                    dom = 'Bt', 
                                    buttons = 'copy')) %>% 
      formatStyle('MEAN_T', fontWeight = 'bold')
  })
  output$ll <- renderDataTable({
    my_data <- data()
    n <- nrow(my_data)
    ns <- my_data[, input$NS]
    os <- my_data[, input$OS]
    Lns <- scale(ns)*0.83333+3
    Los <- (scale(os)*(-1))*0.83333+3
    L <- cbind(Lns, Los)
    MEAN_L = rowMeans(L)
    L <- round(cbind(L, MEAN_L), digits = 3)
    L %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = n, 
                                    dom = 'Bt', 
                                    buttons = 'copy')) %>% 
      formatStyle('MEAN_L', fontWeight = 'bold')
  })

  output$zpro <- renderPlotly({
    my_data <- data()
    ns <- my_data[, input$NS]
    os <- my_data[, input$OS]
    Zns <- scale(ns)
    Zos <- scale(os)*(-1)
    Z <- cbind.data.frame(round(Zns, digits = 2), round(Zos, digits = 2))
    vars <- names(Z)
    Zt <- t((Z[input$ENTITETI,]))
    Ze <- cbind.data.frame(vars, Zt)
    colnames(Ze) <- c("Variables", "Z")
    
    zpro <- ggplot(data=Ze, aes(x=Variables, y=Z, fill=Variables, color=Variables)) +
      geom_bar(stat="identity", alpha = input$opa, width = input$width) +
      geom_abline(intercept = 0, slope = 0, color = boja) + 
      ylim(-3.5,3.5) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
      
    ggplotly(zpro) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Variables", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Z-score", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
    
  })
  output$tpro <- renderPlotly({
    my_data <- data()
    ns <- my_data[, input$NS]
    os <- my_data[, input$OS]
    Zns <- scale(ns)
    Zos <- scale(os)*(-1)
    Tns <- Zns*10+50
    Tos <- Zos*10+50
    Tv <- cbind.data.frame(round(Tns, digits = 2), round(Tos, digits = 2))
    vars <- names(Tv)
    Tt <- t((Tv[input$ENTITETI,]))
    Te <- cbind.data.frame(vars, Tt)
    colnames(Te) <- c("Variables", "Tv")
    
    tpro <- ggplot(data=Te, aes(x=Variables, y=Tv, fill=Variables, color=Variables)) +
      geom_bar(stat="identity", alpha = input$opa, width = input$width) +
      geom_abline(intercept = 50, slope = 0, color = boja) + 
      ylim(0, 90) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(tpro) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Variables", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Z-score", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  
  output$lpro <- renderPlotly({
    my_data <- data()
    ns <- my_data[, input$NS]
    os <- my_data[, input$OS]
    Zns <- scale(ns)
    Zos <- scale(os)*(-1)
    Tns <- Zns*0.83+3
    Tos <- Zos*0.83+3
    Tv <- cbind.data.frame(round(Tns, digits = 2), round(Tos, digits = 2))
    vars <- names(Tv)
    Tt <- t((Tv[input$ENTITETI,]))
    Te <- cbind.data.frame(vars, Tt)
    colnames(Te) <- c("Variables", "Tv")
    
    lpro <- ggplot(data=Te, aes(x=Variables, y=Tv, fill=Variables, color=Variables)) +
      geom_bar(stat="identity", alpha = input$opa, width = input$width) +
      geom_abline(intercept = 3, slope = 0, color = boja) + 
      ylim(0, 5.5) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(lpro) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Variables", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Z-score", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  
  # Chi-Square Goodness of Fit Test #
  output$tf <- renderDataTable({
    my_data <- data()
    var <- my_data[,input$FT]
    fo <- table(var)
    k <- length(fo)
    tf <- as.data.frame(fo)
    colnames(tf) <- c(input$FT, "F")
    tf$RF <- tf$F/sum(tf$F)*100
    tf$FO <- sum(tf$F)/k
    tf$RO <- tf$FO/sum(tf$FO)*100
    colnames(tf) <- c(input$FT, "Observed Frequency", "Observed Percent", "Expected Frequency", "Expected Percent")
    tf %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = 50, 
                                    dom = 'Bt', 
                                    buttons = 'copy')) %>% 
      formatRound(c('Observed Percent', "Expected Percent"), 2)
  })
  output$chi <- renderText({
    my_data <- data()
    var <- my_data[,input$FT]
    ft <- table(var)
    tf <- as.data.frame(ft)
    hi <- chisq.test(tf[2])
    chi <- round(hi$statistic[1], digits = 3)
    df <- round(hi$parameter[1], digits = 3)
    p <- round(hi$p.value [1], digits = 3)
    paste("Chi-square = ", chi,"; df = ", df, "; p =", p)
  })
  
  
  output$bar <- renderPlotly({
    my_data <- data()
    Group <- my_data[,input$FT]
    if(input$ori=="x") {
      
      bar <- ggplot(my_data, aes(x = Group, fill = Group, color = Group)) + 
        geom_bar (alpha = input$opa, width = input$width) +
        labs(fill = input$FT, color = input$FT) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)

      ggplotly(bar) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = input$FT, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text = "Frequency", font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
      )
    }
    else {      
    
      bar <- ggplot(my_data, aes(y = Group, fill = Group, color = Group)) + 
        geom_bar (alpha = input$opa, width = input$width) +
        labs(fill = input$FT, color = input$FT) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)
      
      ggplotly(bar) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = "Frequency", font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text =  input$FT, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
       )
     }	  
  })
  
  output$pie <- renderPlotly({
    my_data <- data()
    Group <- my_data[,input$FT]
    fo <- table(Group)
    tf <- as.data.frame(fo)
    colnames(tf) <- c("G", "F")
    x <- tf$G
    y <- tf$F
    
    pie <- plot_ly(labels = x, 
                   values = y, 
                   type = "pie", 
                   opacity = input$opa,
                   textposition = 'inside',
                   textinfo='label+percent',
                   insidetextorientation='auto',
                   insidetextfont = list(color = '#FFFFFF'),
                   showlegend = T,
                   marker = list(colors = colors,
                                 line = list(color = colors, width = 0.5)
                   )
    )
    pie %>% layout(legend=list(
      font=list(size = 14, color = tekst),
      title=list(font=list(size = 14, color = tekst)) 
    ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
  })
  
  # Chi-Square Test of Independence
  output$ctf <- renderDataTable({
    my_data <- data()
    var1 <- my_data[,input$CT1]
    var2 <-  my_data[,input$CT2]
    ctf <- CrossTable(var1, var2, chisq = T)
    ctf <- as.data.frame.matrix(ctf[["chisq"]][["observed"]])
    ctf$Total = rowSums(ctf[,])
    k <- length(ctf$Total)+1
    ctf <- rbind(ctf, colSums(ctf[,]))
    row.names(ctf)[k] <- "Total"
    ctf %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = 50, 
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$ctfp <- renderDataTable({
    my_data <- data()
    var1 <- my_data[,input$CT1]
    var2 <-  my_data[,input$CT2]
    ctfp <- CrossTable(var1, var2)
    ctfp <- round(as.data.frame.matrix(ctfp$prop.row)*100, digits = 2)
    ctfp$Total = round(rowSums(ctfp[,]), digits = 0)
    ctfp %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = 50, 
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$ctfe <- renderDataTable({
    my_data <- data()
    var1 <- my_data[,input$CT1]
    var2 <-  my_data[,input$CT2]
    ctfe <- CrossTable(var1, var2, chisq = T)
    ctfe <- round(as.data.frame.matrix(ctfe[["chisq"]][["expected"]]), digits = 2)
    ctfe$Total = round(rowSums(ctfe[,]), digits = 0)
    k <- length(ctfe$Total)+1
    ctfe <- rbind(ctfe, colSums(ctfe[,]))
    row.names(ctfe)[k] <- "Total"
    ctfe %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = 50, 
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$chi2 <- renderText({
    my_data <- data()
    var1 <- my_data[,input$CT1]
    var2 <-  my_data[,input$CT2]
    chit <- CrossTable(var1, var2, chisq = T)
    chi <- round(chit[["chisq"]][["statistic"]][["X-squared"]], digits = 3)
    df <- round(chit[["chisq"]][["parameter"]][["df"]], digits = 3)
    p <- round(chit[["chisq"]][["p.value"]], digits = 3)
    paste("Chi-square = ", chi,"; df = ", df, "; p =", p)
  })
  
  output$graf2 <- renderPlotly({
    my_data <- data()
    var1 <- my_data[,input$CT1]
    var2 <-  my_data[,input$CT2]
    if(input$ori2=="x") {
      
      graf2 <- ggplot(my_data, aes(x=var1, fill = var2, color = var2)) + 
        geom_bar (alpha = input$opa, width = input$width) +
        labs(x= input$CT1, y = "Frequency", fill = input$CT2, color = input$CT2) +
        theme(panel.background = element_rect(fill = "#ffffff")) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)


      ggplotly(graf2) %>% 
                      layout(legend=list(
                        font=list(size = 14, color = tekst),
                        title=list(font=list(size = 14, color = tekst)) 
                      ), 
                       plot_bgcolor = "rgba(0,0,0,0)",
                       paper_bgcolor = "rgba(0,0,0,0)",
                       xaxis = list( 
                         title = list(text = input$CT1, font=list(size=16, color=tekst)),
                         gridcolor = "rgba(0,0,0,0)",
                         tickfont = list(size=14, color=tekst)
                       ),
                       yaxis = list(
                         title = list(text = "Frequency", font=list(size=16, color=tekst)),
                         gridcolor = "rgba(0,0,0,0)",
                         tickfont = list(size=14, color=tekst)
                       )
            )
    }
    else {
      
      graf2 <- ggplot(my_data, aes(y=var1, fill = var2, color = var2)) + 
        geom_bar (alpha = input$opa, width = input$width) +
        labs(y= input$CT1, x = "Frequency", fill = input$CT2, color = input$CT2) +
        theme(panel.background = element_rect(fill = "#ffffff")) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)
      
      ggplotly(graf2) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = "Frequency", font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text =  input$CT1, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
      )
    }
  })
  output$graf3 <- renderPlotly({
    my_data <- data()
    var1 <- my_data[,input$CT1]
    var2 <-  my_data[,input$CT2]
    if(input$ori2=="x") {
      graf3 <- ggplot(my_data, aes(x=var1, fill = var2, color = var2)) + 
        geom_bar (alpha = input$opa, position="fill", width = input$width)+
        labs(x= input$CT1, y = "%", fill = input$CT2, color = input$CT2) +
        theme(panel.background = element_rect(fill = "#ffffff")) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)
      
      ggplotly(graf3) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = input$CT1, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text = "Frequency" , font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
      )
    }
    else {
      graf3 <- ggplot(my_data, aes(y=var1, fill = var2, color = var2)) + 
        geom_bar (alpha = input$opa, position="fill", width = input$width)+
        labs(y= input$CT1, x = "%", fill = input$CT2, color = input$CT2) +
        theme(panel.background = element_rect(fill = "#ffffff")) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)
      
       ggplotly(graf3) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = "Frequency", font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text =  input$CT1, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
      )
    }
  })
  
  # McNemar's test #
  output$ctf2 <- renderDataTable({
    my_data <- data()
    var1 <- my_data[,input$MCN1]
    var2 <-  my_data[,input$MCN2]
    ctf <- CrossTable(var1, var2)
    ctf <- as.data.frame.matrix(ctf$t)
    ctf$Total = rowSums(ctf[,])
    ctf %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = 50, 
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$ctfp2 <- renderDataTable({
    my_data <- data()
    var1 <- my_data[,input$MCN1]
    var2 <-  my_data[,input$MCN2]
    ctfp <- CrossTable(var1, var2)
    ctfp <- round(as.data.frame.matrix(ctfp$prop.row)*100, digits = 2)
    ctfp$Total = round(rowSums(ctfp[,]), digits = 0)
    ctfp %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = 50, 
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$mcn <- renderText({
    my_data <- data()
    var1 <- my_data[,input$MCN1]
    var2 <-  my_data[,input$MCN2]
    mcchit <- CrossTable(var1, var2, mcnemar = T)
    mcchi <- round(mcchit[["mcnemar"]][["statistic"]][["McNemar's chi-squared"]], digits = 3)
    mcdf <- round(mcchit[["mcnemar"]][["parameter"]][["df"]], digits = 3)
    mcp <- round(mcchit[["mcnemar"]][["p.value"]], digits = 3)
    mcchi.cor <- round(mcchit[["mcnemar.corr"]][["statistic"]][["McNemar's chi-squared"]], digits = 3)
    mcdf.cor <- round(mcchit[["mcnemar.corr"]][["parameter"]][["df"]], digits = 3)
    mcp.cor <- round(mcchit[["mcnemar.corr"]][["p.value"]], digits = 3)
    paste("McNemar's Chi-square = ", mcchi,"; df = ", mcdf, "; p =", mcp, "</br>", 
          "McNemar's Chi-square with continuity correction = ", mcchi.cor,"; df = ", mcdf.cor, "; p =", mcp.cor) 
  })
  output$graf4 <- renderPlotly({
    my_data <- data()
    var1 <- my_data[,input$MCN1]
    var2 <-  my_data[,input$MCN2]
    if(input$ori3=="x") {
      
      graf4 <- ggplot(my_data, aes(x=var1, fill = var2, color = var2)) + 
        geom_bar (alpha = input$opa, width = input$width) +
        labs(x= input$MCN1, y = "Frequency", fill = input$MCN2, color = input$MCN2) +
        theme(panel.background = element_rect(fill = "#ffffff")) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)
      
      ggplotly(graf4) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = input$MCN1, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text = "Frequency", font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
      )
    }
    else {
      
      graf4 <-  ggplot(my_data, aes(y=var1, fill = var2, color = var2)) + 
        geom_bar (alpha = input$opa, width = input$width) +
        labs(y= input$MCN1, x = "Frequency", fill = input$MCN2, color = input$MCN2) +
        theme(panel.background = element_rect(fill = "#ffffff")) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)
    
      ggplotly(graf4) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = "Frequency", font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text =  input$MCN1, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
      )
    }
  })
  output$graf5 <- renderPlotly({
    my_data <- data()
    var1 <- my_data[,input$MCN1]
    var2 <-  my_data[,input$MCN2]
    if(input$ori3=="x") {
      
      graf5 <-  ggplot(my_data, aes(x=var1, fill = var2, color = var2)) + 
        geom_bar (alpha = input$opa, position = "fill", width = input$width)+
        labs(x = input$MCN1, y = "%", fill = input$MCN2, color = input$MCN2) +
        theme(panel.background = element_rect(fill = "#ffffff")) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)
      
      ggplotly(graf5) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = input$MCN1, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text =  "Frequency", font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
      )
    }
    else {
     
      graf5 <- ggplot(my_data, aes(y=var1, fill = var2, color = var2)) + 
        geom_bar (alpha = input$opa, position = "fill", width = input$width)+
        labs(y = input$MCN1, y = "%", fill = input$MCN2, color = input$MCN2) +
        theme(panel.background = element_rect(fill = "#ffffff")) +
        theme_minimal() +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors)
      
      ggplotly(graf5) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = "Frequency", font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text =  input$MCN1, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
      )
    }
  })
  
  # Grouping Continuous Data
  output$br <- renderText({
    my_data <- data()
    n <- nrow(my_data)
    k <-  1 + 3.3 * log10(n)
    paste("Optimal Bins (Sturges'Rule) = ", round(k,digits = 0))
  })	

  output$ftab <- renderDataTable({
    my_data <- data()
    var <- my_data[,input$GCD]
    ir <- (max(var) - min(var))/(input$nb-1)
    ir2 <- ir/2
    breaks <- round(seq(min(var)-ir2, max(var)+ir2, by=ir), digits = 2)
    f.cut = cut(var, breaks, right=TRUE)
    d.freq = table(f.cut)
    df <- cbind.data.frame(d.freq)
    colnames(df) <- c("Bins", "F")
    df$RF <- round(df$F/sum(df$F)*100, digits = 2)
    df$CF <- cumsum(df$F)
    df$RCF  <- round(cumsum(df$RF), digits = 2)
    df <- cbind.data.frame(df)
    colnames(df) <- c("Bins", "Frequency", "Percent", "Cumulative","Cumulative Percent")
    df %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = 50, 
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$hist <- renderPlotly({
    my_data <- data()
    var <- my_data[,input$GCD]
    ir <- (max(var) - min(var))/(input$nb-1)
    ir2 <- ir/2
    breaks = seq(min(var)-ir2, max(var)+ir2, by=ir)
    f.cut = cut(var, breaks, right=TRUE)
    d.freq = table(f.cut)
    df <- cbind.data.frame(d.freq)
    colnames(df) <- c("Bins", "Freq")
    
    hist <- ggplot(df, aes(x=Bins, y=Freq)) + 
      geom_bar (stat="identity", alpha = input$opa, fill=boja, color="rgba(255, 255, 255, 0)", width=0.99) +
      labs(x= input$GCD, y = "Frequency") +
      theme_minimal()
    
    ggplotly(hist) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$GCD, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Frequency", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
    
  })
  output$poly <- renderPlotly({
    my_data <- data()
    var <- my_data[,input$GCD]
    ir <- (max(var) - min(var))/(input$nb-1)
    ir2 <- ir/2
    xmin <- min(var)-ir
    breaks = seq(min(var)-ir2, max(var)+ir2, by=ir)
    f.cut = cut(var, breaks, right=TRUE)
    d.freq = table(f.cut)
    tf <- as.data.frame(d.freq)
    colnames(tf) <- c(input$GCD, "F")
    tf$RF <- tf$F/sum(tf$F)*100 
    tf$CF <- cumsum(tf$F)
    tf$RCF  <- cumsum(tf$RF)
    rcf <- c(0, tf$RCF)
    ggr <- c(xmin, breaks[2:(input$nb+1)])
    rcf <- cbind(ggr, rcf)
    rcf <- round(as.data.frame(rcf), digits = 1)
    
    poly <- ggplot(rcf, aes(x=ggr, y = rcf)) + 
      geom_point(alpha = input$opa, color=boja, size=2)+
      geom_line(color=boja, size=1)+
      labs(x= input$GCD, y = "Frequency") +
      theme_minimal()
    
    ggplotly(poly) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$GCD, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Frequency", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
      )
  })
  
  # Descirptive Parameters #
  output$des <- renderDataTable({
    my_data <- data()
    vars <- my_data[, input$DP]
    n <- nrow(vars)                                  
    m <- ncol(vars)
    dp <- describe(vars, type=3)
    kv <- dp$sd/dp$mean*100
    dp <- cbind(dp,kv)
    dp <- dp[c(3, 5, 4, 14, 8, 9, 10, 11, 12)]
    colnames(dp) <- c("MEAN", "MEDIAN", "SD", "CV", "MIN", "MAX", "RANGE", "SKEW", "KURT")
    dp <- round(dp, digits = 2)
    dp %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = m, 
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$boxds <- renderPlotly({
    my_data <- data()
    X <-  my_data[, input$DP2]
    
    boxds <- plot_ly(x = X, 
                     type = "box",
                     name = input$DP2, 
                     boxpoints = "all", 
                     jitter = 0.3,
                     line = list(color = boja, width = 2),
                     fillcolor = list(color = boja),
                     marker = list(color = boja)
                    )
  
    boxds %>% layout(
                    plot_bgcolor = "rgba(0,0,0,0)",
                    paper_bgcolor = "rgba(0,0,0,0)",
                    xaxis = list(
                     gridcolor = "rgba(0,0,0,0)",
                      tickfont = list(size=14, color=tekst)
                    ),
                    yaxis = list(
                      gridcolor = "rgba(0,0,0,0)",
                      tickfont = list(size=14, color=tekst)
                    )
    )
  })

  # Testing for Normality  #
  output$ks <- renderText({ 
    my_data <- data()
    var <- my_data[,input$TN]
    ks <- ks.test(var, "pnorm", mean=mean(var), sd=sd(var))
    D <- round(ks[["statistic"]][["D"]], digits = 3)
    pks <- round(ks[["p.value"]], digits = 3)
    paste("Kolmogorov-Smirnov test:", "D = ", D,"; p = ", pks)
  }) 
  output$lks <- renderText({ 
    my_data <- data()
    var <- my_data[,input$TN]
    lt <- lillie.test(var)
    Dlks <- round(lt[["statistic"]][["D"]], digits = 3)
    plks <- round(lt[["p.value"]], digits = 3)
    paste("Lilliefors test:", "D = ", Dlks,"; p = ", plks)
  })  
  output$ad <- renderText({ 
    my_data <- data()
    var <- my_data[,input$TN]
    ad <- ad.test(var)
    A <- round(ad[["statistic"]][["A"]], digits = 3)
    pa <- round(ad[["p.value"]], digits = 3)
    paste("Anderson-Darling test:", "A = ", A,"; p = ", pa)
  })  
  output$sf <- renderText({ 
    my_data <- data()
    var <- my_data[,input$TN]
    sf <- sf.test(var)
    Wsf <- round(sf[["statistic"]][["W"]], digits = 3)
    psf <- round(sf[["p.value"]], digits = 3)
    paste("Shapiro-Francia test:", "W = ", Wsf,"; p = ", psf)
  }) 
  output$sw <- renderText({ 
    my_data <- data()
    var <- my_data[,input$TN]
    sw <- shapiro.test(var)
    W <- round(sw$statistic, digits = 3)
    psw <- round(sw$p.value, digits = 3)
    paste("Shapiro-Wilk test:", "W = ", W,"; p = ", psw)
  })
  
  output$histogram <- renderPlotly({
    my_data <- data()
    var <- my_data[,input$TN]
      ir <- (max(var) - min(var))/(input$nb-1)
      ir2 <- ir/2
      breaks = seq(min(var)-ir2, max(var)+ir2, by=ir)
      f.cut = cut(var, breaks, right=TRUE)
      d.freq = table(f.cut)
      df <- cbind.data.frame(d.freq)
      colnames(df) <- c("Bins", "Freq")
      
      hist <- ggplot(df, aes(x=Bins, y=Freq)) + 
        geom_bar (stat="identity", alpha = input$opa, fill=boja, color="rgba(255, 255, 255, 0)", width=0.99) +
        labs(x= input$GCD, y = "Frequency") +
        theme_minimal()
      
      ggplotly(hist) %>% 
        layout(legend=list(
          font=list(size = 14, color = tekst),
          title=list(font=list(size = 14, color = tekst)) 
        ), 
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = input$TN, font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        ),
        yaxis = list(
          title = list(text = "Frequency", font=list(size=16, color=tekst)),
          gridcolor = "rgba(0,0,0,0)",
          tickfont = list(size=14, color=tekst)
        )
      )
})
  output$box <- renderPlotly({
    my_data <- data()
    X <- my_data[,input$TN]
    
    box <- plot_ly(x = X, 
                   type = "box",
                   name = input$TN, 
                   boxpoints = "all", 
                   jitter = 0.3,
                   line = list(color = boja, width = 2),
                   fillcolor = list(color = boja),
                   marker = list(color = boja)
                  )
    
    box %>% layout( plot_bgcolor = "rgba(0,0,0,0)",
                     paper_bgcolor = "rgba(0,0,0,0)",
                     xaxis = list(
                       gridcolor = "rgba(0,0,0,0)",
                       tickfont = list(size=14, color=tekst)
                     ),
                     yaxis = list(
                       gridcolor = "rgba(0,0,0,0)",
                       tickfont = list(size=14, color=tekst)
                     )
    )
  })
  
  output$qq <- renderPlotly({
    my_data <- data()
    var <- my_data[,input$TN]
    
    qq <- ggplot(my_data, aes(sample = var)) + 
      stat_qq_line(color=pravac, size=1) + 
      stat_qq(alpha = input$opa, color=boja, size=1) + 
      labs(x = input$TN, y=input$TN) +
      theme_minimal()
    
    ggplotly(qq) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$TN, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = input$TN, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  
  # Confidence Intervals for the Population Mean #
  output$ci <- renderText({
    my_data <- data()
    x <- my_data[,input$PM]
    n <- nrow(my_data)
    as <- round(mean(x), digits = 2)
    sd <- round(sd(x), digits = 2)
    se <- round(sd/sqrt(input$PMn), digits = 2)
    t <- round(qt(input$PMp/2, input$PMn, lower.tail = F), digits = 2)
    x1 <- round(as-t*se, digits = 2)
    x2 <- round(as+t*se, digits = 2)
    paste("Sample Size =", n, "</br>",
          "Sample Mean =", as, "</br>",
          "Sample Standard Deviation =", sd, "</br>",
          "Standard Error of the Mean =", se, "</br>",
          "p =", input$PMp, "</br>",
          "t =", t, "</br>",
          "-----------------------------------------------------", "</br>",
          "<b>", x1, "~ Population Mean ~", x2
    )
  }) 
  output$gsem <- renderPlotly({
    my_data <- data()
    xx <- my_data[,input$PM]
    sd <- round(sd(xx), digits = 2)
    ss <- c(1:300)
    se <- round(sd/sqrt(ss), digits = 2)
    sem <-cbind.data.frame(ss,se)
    
    gsem <- ggplot(sem, aes(x=ss, y=se)) +
      geom_line(color=boja, size=1) +
      theme_minimal()
    
    ggplotly(gsem) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Sample Size", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Standard Error of the Mean", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  }) 
  
  # Correlations Analysis #
  output$r <- renderDataTable({
    my_data <- data()
    vars <- my_data[, input$COR]
    var_name_numeric <- names(vars)
    m <- ncol(vars)
    n <- nrow(vars)
    df <- n-2
    p <- 0.05
    critical.t <- qt(p/2, df, lower.tail = F)
    critical.r1 <- sqrt((critical.t^2)/(critical.t^2+df))
    critical.r2 <- -1*critical.r1
    R <- cor(vars, method = input$CORM)
    upper.tri(R)
    R[upper.tri(R)] <- NA
    R %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = m, 
                                    dom = 'Bt', 
                                    buttons = 'copy')) %>%	
    formatRound(colnames(R), digits=3) %>% 
    formatStyle(var_name_numeric, 
                color = styleInterval(
                  c(critical.r2, 0, 0, critical.r1), 
                  c('#C0504D', '#9D9D9D', '#9D9D9D', '#9D9D9D','#C0504D')
                  )
                )
  })
  output$p <- renderDataTable({
    my_data <- data()
    vars <- my_data[, input$COR]
    var_name_numeric <- names(vars)
    m <- ncol(vars)
    n <- nrow(vars)
    P <- corr.test(vars, method = input$CORM)$p 
    upper.tri(P)
    P[upper.tri(P)] <- NA
    P %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = m, 
                                    dom = 'Bt', 
                                    buttons = 'copy')) %>%	
     formatRound(colnames(P), digits=4)  %>%
      formatStyle(var_name_numeric, 
                  color = styleInterval(
                    c(0, 0.05), 
                    c('#C0504D', '#C0504D','#9D9D9D')
                    )
                  )
  })
  
  output$scatter <- renderPlotly({
    my_data <- data()
    X <- my_data[, input$XX]
    Y <- my_data[, input$YY]
    
    scatter <- plot_ly(data = my_data, 
                       x = X, 
                       y = Y,
                       opacity = input$opa,
                       marker = list(size = 7, color = boja)
                       )

    scatter %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$XX, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = input$YY, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  
  # T - test Independent samples #
  output$titt <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$DVITT]
    Factor <- my_data[, input$IVITT]
    VFDF <- cbind.data.frame(Factor, Variables)
    vn <- variable.names(Variables)
    m <- ncol(Variables)
    n <- nrow(Variables)
    j <- 0
    lt <- c(m)
    lmat <- matrix(data=1:2*m, nrow = m, ncol = 2)
    for (j in 1:m) {
      lt[j] <- list(leveneTest (Variables[,j] ~ Factor, center=mean))
      lmat[j,1] <- lt[[j]]$`F value`[1]
      lmat[j,2] <- lt[[j]]$`Pr(>F)`[1]
    }
    names(lt) <- vn
    ldf <- data.frame(lmat)
    row.names(ldf) <-vn
    ldf <- round(ldf, digits = 3)
    t_rez <- lapply(VFDF[,vn], function(Variables) t.test(Variables ~ Factor, var.equal = TRUE))
    mean <- data.frame(mean = sapply(t_rez, getElement, name = "estimate"))
    ser <- data.frame(ser = sapply(t_rez, getElement, name = "stderr"))
    tt <- data.frame(t = sapply(t_rez, getElement, name = "statistic"))
    tdf <- data.frame(df = sapply(t_rez, getElement, name = "parameter"))
    tp <- data.frame(p = sapply(t_rez, getElement, name = "p.value"))
    tconf.int <- data.frame(conf.int = sapply(t_rez, getElement, name = "conf.int"))
    tmean <- t(mean)
    ci <- t(tconf.int)
    rez <- cbind.data.frame(tmean, ser, abs(tt), tdf, tp, ci)
    d <- lapply(VFDF[,vn], function(Variables) cohensD(Variables ~ Factor))
    df <- ldply (d, data.frame)
    df <- round(df[[2]], digits = 3)
    tt_rez <- round(rez, digits = 3)
    titt <- cbind.data.frame(tt_rez, ldf, df)
    colnames(titt) <- c("MEAN1", "MEAN2", "SED", "T", "DF", "P", "CI-95%", "CI+95%", "Levene's F", "P", "Cohen's D")
    row.names(titt) <- c(vn)
    titt%>% 
      DT::datatable (rownames = TRUE, 
                     style = style,
                     class = class,
                     extensions = 'Buttons', 
                     options = list(pageLength = m, 
                                    dom = 'Bt',
                                    buttons = 'copy'))
  })
  output$dest <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$ISTD]
    Factor <- my_data[, input$IVITT]
    data <- cbind.data.frame(Variables, Factor)
    DT <- data.table(data)
    dest <- summarySE(my_data, measurevar = input$ISTD, groupvars=c(input$IVITT))
    CI1 <- dest[3] - dest$ci
    CI2 <- dest[3] + dest$ci
    dest <- cbind(dest, CI1, CI2)
    sw <- DT[,.(W = shapiro.test(Variables)$statistic, P.value = shapiro.test(Variables)$p.value), by = .(Factor)]
    dest <- cbind(dest,sw)
    dest <- dest[c(1, 2, 3, 4, 5, 7, 8, 10, 11)]
    colnames(dest) <- c(input$IVITT, "N", "MEAN", "SD", "SEM", "-CI95%", "+CI95%", "S-W", "P")
    dest%>% 
      DT::datatable (rownames = FALSE, 
                     style = 'bootstrap',
                     class = 'compact',
                     extensions = 'Buttons', 
                     options = list(pageLength = 100, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(c("MEAN", "SD", "SEM", "-CI95%", "+CI95%", "S-W", "P"), 3)
  })
  output$boxt <- renderPlotly({
    my_data <- data()
    Variables <- my_data[, input$ISTD]
    Groups    <- my_data[, input$IVITT]

    boxt <- plot_ly(my_data, 
                    x = Variables, 
                    y = Groups, 
                    color = Groups, 
                    type = "box")
    
    boxt %>% 
      layout(legend=list(title = list(text = input$IVITT, font=list(size=16, color=tekst)),
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$ISTD, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = input$IVITT, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })

  output$t <- renderText({
    my_data <- data()
    Variables <- my_data[, input$ISTD]
    Factor <- my_data[, input$IVITT]
    lt <- leveneTest (Variables ~ Factor, data = my_data, center = mean)
    Fl <- round(lt$`F value`[1], digits = 3)
    pl <- round(lt$`Pr(>F)`[1], digits = 3)
    tt <- t.test(Variables ~ Factor, data = my_data, var.equal = TRUE)
    tf <- t.test(Variables ~ Factor, data = my_data, var.equal = FALSE)
    ttt <- round(tt$statistic[1], digits = 3)
    dft <- round(tt$parameter[1], digits = 3)
    ptt <- round(tt$p.value[1], digits = 3)
    ttf <- round(tf$statistic[1], digits = 3)
    dff <- round(tf$parameter[1], digits = 3)
    ptf <- round(tf$p.value[1], digits = 3)
    d <- round(cohensD(Variables ~ Factor, method = "pooled"), digits = 3)
    mw <- wilcox.test(Variables~Factor, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F)
    if(pl>0.05){
      paste(
        h5("Levene's Test for Equality of Variances"),
        "Levene's F = ", Fl, "</br>",
        "Levene's p = ", pl, "</br>",
        "Levene's test is not significant (p > .05)", "</br>",
        h5("Student's T-Test"),
        "Student's t = ", ttt ,"</br>",
        "df =", dft, "</br>",
        "p = ", ptt, "</br>", 
        "Cohen's d =", d, "</br>",  
        h5("Mann Whitney U-Test"),
        "Mann Whitney U =",  round(mw$statistic, digits = 3), "</br>", 
        "Mann Whitney p =",  round(mw$p.value, digits = 3)
      )
    }
    else{
      paste(
        h5("Levene's Test for Equality of Variances"),
        "Levene's F = ", Fl, "</br>",
        "Levene's p = ", pl, "</br>",
        "Levene's test is significant (p < .05), suggesting a violation of the assumption of equal variances", "</br>",
        h5("Welch's t-test for Unequal Variances"),
        "Welch's t = ", ttf , "</br>",
        "df =", dff, "</br>",
        "p = ", ptf, "</br>",
        "Cohen's d =", d, "</br>", 
        h5("Mann Whitney U-Test"),
        "Mann Whitney U =",  round(mw$statistic, digits = 3), "</br>", 
        "Mann Whitney p =",  round(mw$p.value, digits = 3)
      )
    }
  })  
  output$err <- renderPlotly({
    my_data <- data()
    Variables <- my_data[, input$ISTD]
    Factor <- my_data[, input$IVITT]
    tgc <- summarySE(my_data, measurevar = input$ISTD, groupvars = input$IVITT,  conf.interval = 0.95)
    colnames(tgc) <- c("groups", "n", "mean", "sd","se", "ci")
    
    err <- ggplot(tgc, aes(x=groups, y=mean, group = "mean")) + 
      geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, colour=boja) +
      geom_line(colour=boja) +
      geom_point(size=2, colour=boja, shape=21, fill=boja) + 
      labs(x = input$IVITT, y = input$ISTD) +
      theme_minimal()
    
    ggplotly(err) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$IVITT, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = input$ISTD, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  
  # Dependent (Paired) Samples T-Test #
  output$tdtt <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$DSTDV]
    Factor    <- my_data[, input$DSTI]
    VFDF <- cbind.data.frame(Factor, Variables)
    vn <- variable.names(Variables)
    m <- ncol(Variables)
    n <- nrow(Variables)
    t_rez <- lapply(my_data[,vn], function(Variables) t.test(Variables ~ Factor,  paired = TRUE, var.equal = TRUE))
    d <- lapply(my_data[,vn], function(Variables) cohensD(Variables ~ Factor, method = "paired"))
    df <- ldply (d, data.frame)
    rmean <- data.frame(mean = sapply(t_rez, getElement, name = "estimate"))
    ser <- data.frame(ser = sapply(t_rez, getElement, name = "stderr"))
    tt <- data.frame(t = sapply(t_rez, getElement, name = "statistic"))
    tdf <- data.frame(df = sapply(t_rez, getElement, name = "parameter"))
    tp <- data.frame(p = sapply(t_rez, getElement, name = "p.value"))
    tconf.int <- data.frame(conf.int = sapply(t_rez, getElement, name = "conf.int"))
    ci <- t(tconf.int)
    tdtt <- cbind.data.frame(rmean, ser, abs(tt), tdf, tp, ci, df[2])
    colnames(tdtt) <- c("DIFF", "SED", "T", "DF", "P", "CI-95%", "CI+95%", "Cohen's D")
    row.names(tdtt) <- c(vn)
    tdtt <- round(tdtt, digits = 3)
    tdtt%>% 
      DT::datatable (rownames = TRUE, 
                     style = style,
                     class = class,
                     extensions = 'Buttons', 
                     options = list(pageLength = m, 
                                    dom = 'Bt',
                                    buttons = 'copy'))
  })

  output$dest2 <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$DSTD]
    Factor    <- my_data[, input$DSTI]
    dest2 = summarySE(my_data, measurevar= input$DSTD, groupvars=c(input$DSTI))
    CI1 <- dest2[3] - dest2$ci
    CI2 <- dest2[3] + dest2$ci
    dest2 <- cbind(dest2, CI1, CI2)
    dest2 <- dest2[c(1, 2, 3, 4, 5, 7, 8)]
    colnames(dest2) <- c(input$DSTI, "N", "MEAN", "SD", "SEM", "-CI95%", "+CI95%")
    dest2%>% 
      DT::datatable (rownames = FALSE, 
                     style = style,
                     class = class,
                     extensions = 'Buttons', 
                     options = list(pageLength = 100, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(c("MEAN", "SD", "SEM", "-CI95%", "+CI95%"), 3)
  })
  
  output$shapiro2 <- renderText({
    my_data <- data()
    n <- nrow(my_data)
    e <- n/2
    Variables <- my_data[, input$DSTD]
    Variables1 <- Variables[1:e]
    Variables2 <- Variables[(e+1):n]
    dif <- Variables1 - Variables2
    sw <- shapiro.test(dif)
    W <- round(sw$statistic, digits = 3)
    p <- round(sw$p.value, digits = 3)
    paste("W = ", W,"; p = ", p)
  })
  
  output$boxt2 <- renderPlotly({
    my_data <- data()
    Variables <- my_data[, input$DSTD]
    Groups    <- my_data[, input$DSTI]
    
    boxt2 <- plot_ly(my_data, 
                     x = Variables, 
                     y = Groups, 
                     color = Groups, 
                     type = "box"
                     )
    
    boxt2 %>% 
      layout(legend=list(title = list(text = input$DSTI, font=list(size=16, color=tekst)),
                         font=list(size = 14, color = tekst),
                         title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$DSTD, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = input$DSTI, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
    
  })
  output$t2 <- renderText({
    my_data <- data()
    Variables <- my_data[, input$DSTD]
    Factor    <- my_data[, input$DSTI]
    n <- nrow(my_data)
    e <- n/2
    tt <- t.test(Variables ~ Factor, data = my_data,  paired = TRUE, var.equal = TRUE)
    md <-  round(tt$estimate[[1]], digits = 3)
    se <- round(tt$stderr[1], digits = 3)
    r <- round(cor(Variables[1:e],Variables[e+1:e]), digits = 3)
    ttt <- round(tt$statistic[1], digits = 3)
    dft <- round(tt$parameter[1], digits = 3)
    ptt <- round(tt$p.value[1], digits = 3)
    d <- round(cohensD(Variables~Factor, method = "paired"), digits = 3)
    wt <- wilcox.test(Variables~Factor, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=T)
    paste(			 
      h5("Student's T-Test"),
      "Mean difference =", md , "</br>", 
      "St. error difference =", se , "</br>", 
      "Correlations =", r, "</br>", 
      "Student's t = ", ttt , "</br>", 
      "df =", dft, "</br>",
      "p = ", ptt, "</br>",
      "Cohen's d =", d, "</br>",
      h5("Wilcoxon signed-rank test"),
      "Wilcoxon W =",  round(wt$statistic, digits = 3), "</br>", 
      "Wilcoxon p =",  round(wt$p.value, digits = 3)
    )
  })  
  output$err2 <- renderPlotly({
    my_data <- data()
    Variables <- my_data[, input$DSTD]
    Factor    <- my_data[, input$DSTI]
    tgc <- summarySE(my_data, measurevar = input$DSTD, groupvars = input$DSTI,  conf.interval = 0.95)
    colnames(tgc) <- c("groups", "n", "mean", "sd","se", "ci")
    
    err2 <- ggplot(tgc, aes(x=groups, y=mean, group = "mean")) + 
      geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, colour=boja) +
      geom_line(colour=boja) +
      geom_point(size=2, colour=boja, shape=21, fill=boja) + 
      theme_minimal()
    
    ggplotly(err2) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$DSTI, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = input$DSTD, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  
  # One-Way ANOVA #
  output$arez <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$ANOVADV]
    Factor    <- my_data[, input$ANOVAI]
    vn <- variable.names(Variables)
    m <- ncol(Variables)
    j <- 0
    la <- c(m)
    lmat <- matrix(data=1:2*m, nrow = m, ncol = 2)
    for (j in 1:m) {
      la[j] <- list(leveneTest (Variables[,j] ~ Factor, center=mean))
      lmat[j,1] <- la[[j]]$`F value`[1]
      lmat[j,2] <- la[[j]]$`Pr(>F)`[1]
    }
    
    ladf <- data.frame(lmat)
    X <- as.matrix(Variables)
    Y <- as.matrix(Factor)
    a_rez <- lapply(my_data[,vn], function(X) oneway.test(X ~ Y, var.equal = TRUE))
    aF <- data.frame(F = sapply(a_rez, getElement, name = "statistic"))
    adf<- data.frame(df1 = sapply(a_rez, getElement, name = "parameter"))
    ap <- data.frame(p = sapply(a_rez, getElement, name = "p.value"))
    arez <- cbind.data.frame(round(aF, digits = 3), t(adf), round(ap, digits = 3), round(ladf, digits = 3))
    colnames(arez) <- c("Fisher's F", "DF1", "DF2", "Fisher's p","Levene's F", "Levene's p")
    row.names(arez) <- c(vn)
    arez%>% 
      DT::datatable (rownames = TRUE, 
                     style = 'bootstrap',
                     class = 'compact',
                     extensions = 'Buttons', 
                     options = list(pageLength = m, 
                                    dom = 'Bt',
                                    buttons = 'copy'))
  })
  
  output$desa <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$ANOVAD]
    Factor    <- my_data[, input$ANOVAI]
    data <- cbind.data.frame(Variables, Factor)
    DT <- data.table(data)
    desa <- summarySE(my_data,measurevar= input$ANOVAD, groupvars=c(input$ANOVAI))
    CI1 <- desa[3] - desa$ci
    CI2 <- desa[3] + desa$ci
    desa <- cbind(desa, CI1, CI2)
    sw <- DT[,.(W = shapiro.test(Variables)$statistic, P.value = shapiro.test(Variables)$p.value), by = .(Factor)]
    desa <- cbind(desa,sw)
    desa <- desa[c(1, 2, 3, 4, 5, 7, 8, 10, 11)]
    colnames(desa) <- c(input$ANOVAI, "N", "MEAN", "SD", "SEM", "-CI95%", "+CI95%", "S-W", "P")
    desa %>% 
      DT::datatable (rownames = FALSE, 
                     style = style,
                     class = class,
                     extensions = 'Buttons', 
                     options = list(pageLength = 100, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(c("MEAN", "SD", "SEM", "-CI95%", "+CI95%", "S-W", "P"), 3)
  })
  
  output$bwp <- renderPlotly({
    my_data <- data()
    Variables <- my_data[, input$ANOVAD]
    Groups    <- my_data[, input$ANOVAI]
    
    bwp <- plot_ly(my_data, 
                   x = Variables, 
                   y = Groups, 
                   color = Groups, 
                   type = "box")
    
    bwp %>% 
      layout(legend=list(title = list(text = input$ANOVAI, font=list(size=16, color=tekst)),
                         font=list(size = 14, color = tekst),
                         title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$ANOVAD, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = input$ANOVAI, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  
  output$anovat <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$ANOVAD]
    Factor    <- my_data[, input$ANOVAI]
    aov <- summary(aov(Variables ~ Factor, data = my_data))
    colnames(aov[[1]]) <- c("df", "Sum of Squares", "Mean Square", "F", "p")
    rownames(aov[[1]]) <- c(input$ANOVAI, "Residual")
    aov[[1]] %>% 
      DT::datatable (rownames = TRUE, 
                     style = style,
                     class = class,
                     extensions = 'Buttons', 
                     options = list(pageLength = 100, 
                                    dom = 'Bt',
                                    buttons = 'copy'))%>% 
      formatRound(c("Sum of Squares", "Mean Square", "F", "p"), 3)
    
  })
  output$anova <- renderText({
    my_data <- data()
    Variables <- my_data[, input$ANOVAD]
    Factor    <- my_data[, input$ANOVAI]
    aov <- summary(aov(Variables ~ Factor, data = my_data))
    lt <- leveneTest (Variables ~ Factor, data = my_data, center = mean)
    at <- oneway.test(Variables ~ Factor, data = my_data, var.equal = TRUE)
    af <- oneway.test(Variables ~ Factor, data = my_data, var.equal = FALSE)
    kw <- kruskal.test(Variables ~ Factor, data = my_data)
    if(lt$`Pr(>F)`[1]>0.05){
      paste(
        h5("Homogeneity of Variances Test (Levene's)"),
        "F = ", round(lt$`F value`[1], digits = 3), "</br>",
        "df1 =", lt$Df[1], "</br>",
        "df2 =", lt$Df[2], "</br>",
        "p = ", round(lt$`Pr(>F)`[1], digits = 3), "</br>",
        "Levene's test is not significant (p > .05)", "</br>",
        h5("One-Way ANOVA"), 
        "Fisher's F = ", round(at[["statistic"]], digits = 3),"</br>",
        "df1 =", round(at[["parameter"]][[1]], digits = 3), "</br>",
        "df2 =", round(at[["parameter"]][[2]], digits = 3), "</br>",
        "p = ", round(at[["p.value"]], digits = 3), "</br>",
        h5("Kruskal-Wallis Test "), 
        "H = ", round(kw[["statistic"]][["Kruskal-Wallis chi-squared"]], digits = 3), "</br>",
        "p = ", round(kw[["p.value"]], digits = 3)
      )
    }
    else{
      paste(
        h5("Homogeneity of Variances Test (Levene's)"),
        "F = ", round(lt$`F value`[1], digits = 3), "</br>",
        "df1 =", lt$Df[1], "</br>",
        "df2 =", lt$Df[2], "</br>",
        "p = ", round(lt$`Pr(>F)`[1], digits = 3), "</br>",
        "Levene's test is significant (p < .05), suggesting a violation of the assumption of equal variances", "</br>",
        h5("One-Way ANOVA"), 
        "Welch's F = ", round(af[["statistic"]], digits = 3), "</br>",
        "df1 =", round(af[["parameter"]][[1]], digits = 3), "</br>",
        "df2 =", round(af[["parameter"]][[2]], digits = 3), "</br>",
        "p = ", round(af[["p.value"]], digits = 3), "</br>",
        h5("Kruskal-Wallis H Test "), 
        "H = ", round(kw[["statistic"]][["Kruskal-Wallis chi-squared"]], digits = 3), "</br>",
        "p = ", round(kw[["p.value"]], digits = 3)
      ) 
    }
  })
  output$PH <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$ANOVAD]
    Factor    <- my_data[, input$ANOVAI]
    res.aov <- aov(Variables ~ Factor, data = my_data)
    phh <- emmeans(res.aov, "Factor")
    ph <- summary(pairs(phh), adjust = input$ADJ)
    colnames(ph) <- c("Groups", "Mean difference", "SE","df", "t", "p")
    ph %>% 
      DT::datatable (
                    style = style,
                    class = class,
                     extensions = 'Buttons', 
                     options = list(pageLength = 100, 
                                    dom = 'Bt',
                                    buttons = 'copy'))%>% 
      formatRound(c("Mean difference", "SE", "t", "p"), 3)
  })
  
  output$erra <- renderPlotly({
    my_data <- data()
    Variables <- my_data[, input$ANOVAD]
    Factor    <- my_data[, input$ANOVAI]
    tgc <- summarySE(my_data, measurevar = input$ANOVAD, groupvars= input$ANOVAI,  conf.interval = 0.95)
    colnames(tgc) <- c("groups", "n", "mean", "sd","se", "ci")
   
    erra <- ggplot(tgc, aes(x=groups, y=mean, group = "mean")) + 
      geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, colour=boja) +
      geom_line(colour=boja) +
      geom_point(size=2, colour=boja, shape=21, fill=boja) + 
      theme_minimal()
     
     ggplotly(erra) %>% 
       layout(legend=list(
         font=list(size = 14, color = tekst),
         title=list(font=list(size = 14, color = tekst)) 
       ), 
       plot_bgcolor = "rgba(0,0,0,0)",
       paper_bgcolor = "rgba(0,0,0,0)",
       xaxis = list( 
         title = list(text = input$ANOVAI, font=list(size=16, color=tekst)),
         gridcolor = "rgba(0,0,0,0)",
         tickfont = list(size=14, color=tekst)
       ),
       yaxis = list(
         title = list(text = input$ANOVAD, font=list(size=16, color=tekst)),
         gridcolor = "rgba(0,0,0,0)",
         tickfont = list(size=14, color=tekst)
       )
      )
  })
  
  # Repeated Measures ANOVA #
  output$desrma <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$RMADV]
    Factor    <- my_data[, input$RMAM]
    data <- cbind.data.frame(Variables, Factor)
    DT <- data.table(data)
    desrm <- summarySE(my_data,measurevar= input$RMADV, groupvars=c(input$RMAM))
    CI1 <- desrm[3] - desrm$ci
    CI2 <- desrm[3] + desrm$ci
    desrm <- cbind(desrm, CI1, CI2)
    sw <- DT[,.(W = shapiro.test(Variables)$statistic, P.value = shapiro.test(Variables)$p.value), by = .(Factor)]
    desrm <- cbind(desrm,sw)
    desrm <- desrm[c(1, 2, 3, 4, 5, 7, 8, 10, 11)]
    colnames(desrm) <- c(input$RMAM, "N", "MEAN", "SD", "SEM", "-CI95%", "+CI95%", "S-W", "P")
    desrm %>% 
      DT::datatable (rownames = FALSE, 
                     style = style,
                     class = class,
                     extensions = 'Buttons', 
                     options = list(pageLength = 100, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(c("MEAN", "SD", "SEM", "-CI95%", "+CI95%", "S-W", "P"), 3)
  })
  
  output$bwprma <- renderPlotly({
    my_data <- data()
    Variables <- my_data[, input$RMADV]
    Groups    <- my_data[, input$RMAM]
    
    bwp <- plot_ly(my_data, 
                   x = Variables, 
                   y = Groups, 
                   color = Groups, 
                   type = "box"
                   )
    
    bwp %>% 
      layout(legend=list(title = list(text = input$RMAM, font=list(size=16, color=tekst)),
                         font=list(size = 14, color = tekst),
                         title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$RMADV, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = input$RMAM, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
     )
  })
  
  output$aovrm <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$RMADV]
    Factor    <- factor(my_data[, input$RMAM])
    Entiteti <- factor(my_data[, input$ENT])
    df <- cbind.data.frame(Entiteti, Factor, Variables)
    model <- aov(Variables~Factor+Error(Entiteti), data = df)
    aovr <- summary(model)
    aovrm <- aovr[["Error: Within"]][[1]]
    colnames(aovrm) <- c("df", "Sum of Squares", "Mean Square", "F", "p")
    aovrm %>% 
      DT::datatable (rownames = TRUE, 
                     style = style,
                     class = class,
                     extensions = 'Buttons', 
                     options = list(pageLength = 100, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(c("Sum of Squares", "Mean Square", "F", "p"), 3)
  }) 
  output$rma2 <- renderText({
    my_data <- data()
    Variables <- my_data[, input$RMADV]
    Factor    <- my_data[, input$RMAM]
    Entiteti <- my_data[, input$ENT]
    res.aov<-anova_test(data=my_data, dv = input$RMADV, wid = input$ENT, within = input$RMAM)
    res.frid <- friedman.test(y=Variables, groups=Factor, blocks=Entiteti, data = my_data)
    paste(
      h5("Mauchly's Test for Sphericity"),
      "W = ", res.aov[["Mauchly's Test for Sphericity"]][["W"]], "</br>",
      "p =", res.aov[["Mauchly's Test for Sphericity"]][["p"]], "</br>",
      h5("Friedman test"),
      "Q = ", round(res.frid[["statistic"]][["Friedman chi-squared"]], digits = 3), "</br>",
      "df =",  res.frid[["parameter"]][["df"]], "</br>", 
      "p = ", round(res.frid[["p.value"]], digits = 3)
    )
  }) 
  output$pwc <- renderDataTable({
    my_data <- data()
    Variables <- my_data[, input$RMADV]
    Factor    <- my_data[, input$RMAM]
    Entiteti <- my_data[, input$ENT]
    df <- cbind.data.frame(Entiteti, Factor, Variables)
    anova <- aov_ez(data = df, dv = "Variables", id = "Entiteti", within = c("Factor"))
    pwc <- anova %>% emmeans(~Factor)
    pwc <- contrast(pwc, method = "pairwise", adjust = input$PHr)
    pwc <- summary(pwc)
    colnames(pwc) <- c("Groups", "Mean difference", "SE","df", "t", "p")
    pwc %>% 
      DT::datatable (
                    style = style,
                    class = class,
                    extensions = 'Buttons', 
                    options = list(pageLength = 100, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(c("Mean difference", "SE", "t", "p"), 3)
  })
  output$errrma <- renderPlotly({
    my_data <- data()
    Variables <- my_data[, input$RMADV]
    Factor    <- my_data[, input$RMAM]
    tgc <- summarySE(my_data, measurevar = input$RMADV, groupvars= input$RMAM,  conf.interval = 0.95)
    colnames(tgc) <- c("groups", "n", "mean", "sd","se", "ci")
   
     errrma <- ggplot(tgc, aes(x=groups, y=mean, group = "mean")) + 
       geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, colour=boja) +
       geom_line(colour=boja) +
       geom_point(size=2, colour=boja, shape=21, fill=boja) + 
       theme_minimal()
     
     ggplotly(errrma) %>% 
       layout(legend=list(
         font=list(size = 14, color = tekst),
         title=list(font=list(size = 14, color = tekst)) 
       ), 
       plot_bgcolor = "rgba(0,0,0,0)",
       paper_bgcolor = "rgba(0,0,0,0)",
       xaxis = list( 
         title = list(text = input$RMAM, font=list(size=16, color=tekst)),
         gridcolor = "rgba(0,0,0,0)",
         tickfont = list(size=14, color=tekst)
       ),
       yaxis = list(
         title = list(text = input$RMADV, font=list(size=16, color=tekst)),
         gridcolor = "rgba(0,0,0,0)",
         tickfont = list(size=14, color=tekst)
       )
    )
  })
  
  # Regression Analysis (simple)
  output$rsra <- renderText({
    my_data <- data()
    X <- my_data[, input$X]
    Y <- my_data[, input$Y]
    n <- nrow(my_data) 
    model <- lm(Y ~ X, data = my_data)
    b0 <- model[["coefficients"]][["(Intercept)"]]
    b1 <- model[["coefficients"]][["X"]]
    e <- model[["residuals"]]
    see <- sqrt(sum(e*e)/(n-2))
    r <- cor(X,Y)
    paste("Dependent Variable (y) = ", input$Y, br(),
          "Independent Variable (x) = ", input$X, br(),
          "Intercept (b0) = ", round(b0, digits = 3), br(),
          "Regression Coefficients (b1) = ", round(b1, digits = 3), br(),
          "Standard Error of Estimate (SEE) = ", round(see, digits = 3), br(), 
          "Correlation (r) = ", round(r, digits = 3)
          )
    
  })
  output$gsra <- renderPlotly({
    my_data <- data()
    X <- my_data[, input$X]
    Y <- my_data[, input$Y]
    
    gsra <- ggplot(my_data, aes(x=X, y=Y)) +
      geom_smooth(method = "lm", colour = pravac, size=1, se = FALSE) +
      geom_point(alpha = input$opa, size=1, colour = boja) +
      theme_minimal()
    
    ggplotly(gsra) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = input$X, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = input$Y, font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
    
  })
  output$rs <- renderDataTable({
    my_data <- data()
    X <- my_data[, input$X]
    Y <- my_data[, input$Y]
    n <- nrow(my_data) 
    model <- lm(Y ~ X, data = my_data)
    Yp <- model[["fitted.values"]]
    e <- model[["residuals"]]
    rs <- cbind(Y, Yp, e)
    colnames(rs) <- c("Observed Value","Predicted Value", "Residual Values")
    rs %>% 
      DT::datatable (
                      style = style,
                      class = class,
                      extensions = c('Buttons'),
                      options = list(pageLength = n, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(colnames(rs), digits=3) 
  })
  output$yy <- renderText({
    my_data <- data()
    X <- my_data[, input$X]
    Y <- my_data[, input$Y]
    n <- nrow(my_data) 
    model <- lm(Y ~ X, data = my_data)
    b0 <- model[["coefficients"]][["(Intercept)"]]
    b1 <- model[["coefficients"]][["X"]]
    e <- model[["residuals"]]
    see <- sqrt(sum(e*e)/(n-2))
    pp <- input$pp
    pp <- 100-pp
    pp <- pp/100
    pp <- pp/2
    zz <-abs(qnorm(pp, mean= 0, sd=1)) 
    yy <- input$b0+b1*input$xx
    dg <- yy-zz*see
    gg <- yy+zz*see
    paste("Predicted value = ", round(yy, digits = 3), br(),
          "Lower value = ", round(dg, digits = 3), br(),
          "Upper value =" , round(gg,digits = 3)
    )
  })
  
  # Regression Analysis (multiple)
  output$ro <- renderText({
    my_data <- data()
    PR <- my_data[, input$PRED]
    KR <- my_data[, input$KRIT]
    X <- as.matrix(PR)
    Y <- as.matrix(KR)
    m <- ncol(X)+1
    n <- nrow(X)
    B0 <- rep.int(1, n)
    Xi <- cbind(B0, X)
    XTXi <- t(Xi) %*% Xi
    XTY <- t(Xi) %*% Y
    b <- solve(XTXi) %*% XTY
    Yp <- Xi %*% b 
    e <- Y - Yp
    se <- round(sqrt(sum(e*e)/(n-m)), digits = 2)
    pss <- sum((Yp-mean(Yp))**2)
    rss <- sum((e-mean(e))**2)
    ro2 <- pss/(pss+rss)
    ro <- round(sqrt(ro2), digits = 2)
    dfp <- m-1
    dfe <- n-m
    Fval <- (pss/dfp)/(rss/dfe)
    pf <- pf(Fval, dfp, dfe, lower.tail = FALSE)
    dw <- durbinWatsonTest(lm(Y ~ X))
    paste("RO =", round(ro, digits = 3), 
          "; RO2 =", round(ro2, digits = 3), 
          "; SEE =", round(se, digits = 2), 
          "; F-value =", round(Fval, digits = 3), 
          "; p-value  =", round(pf, digits = 4), br(),
          "Autocorrelation =", round(dw$r, digits = 3),
          "; Durbin-Watson d =", round(dw$dw, digits = 3),
          "; p =", round(dw$p, digits = 4)
    )
  })
  output$rez <- renderDataTable({
    my_data <- data()
    PR <- my_data[, input$PRED]
    KR <- my_data[, input$KRIT]
    X <- as.matrix(PR)
    Y <- as.matrix(KR)
    m <- ncol(X)+1
    n <- nrow(X)
    XY <- cbind(X,Y)
    Z <- scale(X)
    K <- scale(Y)
    ZK <- round(cbind(Z,K), digits = 2)
    Rxy <- cor(XY)
    R <- cor(X)
    B0 <- rep.int(1, n)
    Xi <- cbind(B0,X)
    XTXi <- t(Xi) %*% Xi
    XTY <- t(Xi) %*% Y
    b <- solve(XTXi) %*% XTY
    r1 <- (t(Z) %*% K)/(n-1)
    r <- rbind(0, r1)
    beta1 <- solve(R) %*% r1
    beta <- rbind(0,beta1)
    S2 <- 1/diag(solve(R))
    s2 <- data.frame(S2)
    s2<- rbind(0,s2)
    S2xy <- 1/diag(solve(Rxy))
    Sxy <- sqrt(S2xy)
    Pr <- diag(Sxy) %*% solve(Rxy) %*% diag(Sxy)
    pr <- (Pr[,m]*-1)
    pr <- pr[1:m-1]
    pr <- data.frame(pr)
    pr <- rbind(0,pr)
    p <- beta*r
    Yp <- Xi %*% b 
    e <- Y - Yp
    se <- round(sqrt(sum(e*e)/(n-m)), digits = 2)
    wjj <- solve(XTXi)
    diagwjj <- diag(wjj)
    seb <- se*sqrt(diagwjj)
    tb <- abs(b/seb)
    df <- n-m
    pval <- c()
    for (a in 0:m) {pval[a] <- c(2*(pt(abs(tb[a,1]), df, lower.tail = FALSE)))}
    tb <- data.frame(tb)
    pval <- data.frame(pval)
    rez <- cbind(b, seb, beta, pr, r, p, s2, tb, pval)
    colnames(rez) <- c("B","SE(B)", "Beta", "Part_R", "R","P", "Tolerance", "t", "p")
    rez %>% 
      DT::datatable (
                      style = 'bootstrap',
                      class = 'compact',
                      extensions = 'Buttons', 
                      options = list(pageLength = m, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(colnames(rez), digits=3) 
  })
  output$gro <- renderPlotly({
    my_data <- data()
    PR <- my_data[, input$PRED]
    KR <- my_data[, input$KRIT]
    X <- as.matrix(PR)
    Y <- as.matrix(KR)
    XY <- cbind(X,Y)
    n <- nrow(X)
    Variables <- colnames(X)
    m <- ncol(XY)
    Z <- scale(X)
    K <- scale(Y)
    Rxy <- cor(XY)
    R <- cor(X)
    r1 <- (t(Z) %*% K)/(n-1)
    r <- rbind(0, r1)
    beta1 <- solve(R) %*% r1
    beta <- rbind(0,beta1)
    S2xy <- 1/diag(solve(Rxy))
    Sxy <- sqrt(S2xy)
    Pr <- diag(Sxy) %*% solve(Rxy) %*% diag(Sxy)
    pr <- (Pr[,m]*-1)
    pr <- pr[1:m-1]
    pr <- data.frame(pr)
    pr <- rbind(0,pr)
    p <- beta*r
    pgg <- round(p[2:m], digits = 2)
    pggdf <- cbind.data.frame(Variables,pgg)
    
    gro <- ggplot(pggdf, aes(x= Variables, y = pgg, fill = Variables, color=Variables)) +
      geom_bar(alpha = input$opa, stat = "identity", width = input$width) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(gro) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Variables", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "value", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
    
  })
  output$gcoef <- renderPlotly({
    my_data <- data()
    PR <- my_data[, input$PRED]
    KR <- my_data[, input$KRIT]
    X <- as.matrix(PR)
    Y <- as.matrix(KR)
    XY <- cbind(X,Y)
    n <- nrow(X)
    Variables <- colnames(X)
    m <- ncol(XY)
    Z <- scale(X)
    K <- scale(Y)
    Rxy <- cor(XY)
    R <- cor(X)
    r1 <- (t(Z) %*% K)/(n-1)
    r <- rbind(0, r1)
    beta1 <- solve(R) %*% r1
    beta <- rbind(0,beta1)
    S2xy <- 1/diag(solve(Rxy))
    Sxy <- sqrt(S2xy)
    Pr <- diag(Sxy) %*% solve(Rxy) %*% diag(Sxy)
    pr <- (Pr[,m]*-1)
    pr <- pr[1:m-1]
    pr <- data.frame(pr)
    pr <- rbind(0,pr)
    koef <- cbind(beta, pr, r)
    colnames(koef) <- c("Beta", "Part_R", "R")
    grez <- cbind.data.frame(Variables,koef$Beta[2:m], koef$Part_R[2:m], koef$R[2:m])
    ncoef <- c("BETA", "PART-R", "R")
    Coefficients <- rep(ncoef, each=m-1)
    var <- rep(Variables, times=3)
    val <- c()
    for (a in 2:4) {val <- c(val,grez[,a])}
    ggrez <- data_frame(Coefficients,var,val)
    
    gcoef <- ggplot(ggrez, aes(x=var, y = val, fill = Coefficients, color = Coefficients)) +
      geom_bar(alpha = input$opa, position="dodge",stat = "identity", width = input$width)+
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(gcoef) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Variables", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "value", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
      )
  })
  
  output$dep <- renderDataTable({
    my_data <- data()
    PR <- my_data[, input$PRED]
    KR <- my_data[, input$KRIT]
    X <- as.matrix(PR)
    Y <- as.matrix(KR)
    n <- nrow(X)
    B0 <- rep.int(1, n)
    Xi <- cbind(B0,X)
    XTXi <- t(Xi) %*% Xi
    XTY <- t(Xi) %*% Y
    b <- solve(XTXi) %*% XTY
    Yp <- Xi %*% b 
    e <- Y - Yp
    dep <- cbind(Y, Yp, e)
    colnames(dep) <- c("Observed Value","Predicted Value", "Residual Values")
    dep %>% 
      DT::datatable (
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(pageLength = n, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(colnames(dep), digits=3) 
  })
  
  # Factor Analysis #
  output$Lamda <- renderDataTable({
    my_data <- data()
    my_data <- my_data[, input$MV]
    mm <- ncol(my_data)
    fit <- principal(my_data, nfactors=input$k, rotate=input$method2)
    Eigenvalue <- fit$values
    Cum.Eign <- cumsum(Eigenvalue)
    Percentage <- Eigenvalue/sum(Eigenvalue)*100
    Cum.Per <- cumsum(Percentage)
    Lamda <- cbind(Eigenvalue, Cum.Eign, Percentage, Cum.Per)
    rownames(Lamda) <- 1:mm
    R <- cor(my_data)
    SMC <- 1-(1/diag(solve(R)))
    j <- rep(1,mm)
    SSMC<- SMC %*% j
    ssmc <- SSMC[1,1]
    Lamda %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = 'Buttons',
                     options = list(pageLength = mm,
                                    dom = 'Bt', 
                                    buttons = 'copy')) %>% 
      formatRound(colnames(Lamda), digits=3) %>% 
      formatStyle('Eigenvalue', fontWeight = styleInterval(1, c('normal', 'bold'))) %>% 
      formatStyle('Cum.Eign', fontWeight = styleInterval(ssmc, c('bold','normal')))
  })
  
  output$ssmc <- renderText({ 
    my_data <- data()
    my_data <- my_data[, input$MV]
    mm <- ncol(my_data)
    R <- cor(my_data)
    SMC <- 1-(1/diag(solve(R)))
    j <- rep(1,mm)
    SSMC<- SMC %*% j
    ssmc <- SSMC[1,1]
    ev <- eigen(R)
    ev <- ev$values
    cum.eign <- cumsum(ev)
    gk <- length(which(ev >= 1 ))
    pb <- length(which(cum.eign < ssmc))
    paste("Sum of Squares Multiple Correlation (SSMC) =", round(ssmc, digits = 2), br(),
          "Number of Common Principal Components:", br(),
          " - GK-Criterion (Guttman-Kiser) =", gk, br(),
          " - PB-Criterion (Stalec-Momirovic) =", pb
    )
  })
  output$spgk <- renderPlotly({
    my_data <- data()
    my_data <- my_data[, input$MV]
    ev <- eigen(cor(my_data))
    ev <- ev$values
    mm <- ncol(my_data)
    j <- c(1:mm)
    evdf <- cbind.data.frame(j, ev)
    
    spgk <- ggplot(data=evdf, aes(x=j, y=ev))+
      geom_point(color=boja, size=2)+
      geom_line(color=boja, size=1)+
      geom_abline(intercept = 1, slope = 0, color = pravac) + 
      scale_x_continuous(breaks=min(evdf$j):max(evdf$j)) +
      theme_minimal()
    
    ggplotly(spgk) %>% 
      layout(
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Principal Components", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Eigenvalue", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  output$A <- renderDataTable({
    my_data <- data()
    my_data <- my_data[, input$MV]
    mm <- ncol(my_data)
    fnames <- paste0("F", 1:mm)
    fit <- principal(my_data, nfactors=input$k, rotate=input$method2)
    Complexity <- fit$complexity
    A <- fit$loadings
    critical.a1 <- 0.6
    critical.a2 <- -0.6
    colnames(A) <- fnames[1:input$k]
    A <- cbind(A,Complexity)
    A %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(dom = 'Bt',
                                    pageLength = mm, 
                                    buttons = 'copy'
                                    )) %>%
      formatRound(colnames(A), digits=3) 
    #%>% formatStyle(fnames[1:input$k], color = styleInterval(c(critical.a2, 0, 0, critical.a1), c('red', 'black', 'black', 'black','red')))
  })
  output$FF <- renderDataTable({
    my_data <- data()
    my_data <- my_data[, input$MV]
    mm <- ncol(my_data)
    fnames <- paste0("F", 1:mm)
    fit <- principal(my_data, nfactors=input$k, rotate=input$method2)
    Communality <- fit$communality
    FF <- fit$Structure
    critical.f1 <- 0.6
    critical.f2 <- -0.6
    colnames(FF) <- fnames[1:input$k]
    FF <- cbind(FF, Communality)
    FF %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(dom = 'Bt', 
                                    pageLength = mm, 
                                    buttons = 'copy'
                                    )) %>%
      formatRound(colnames(FF), digits=3) 
    #%>% formatStyle(fnames[1:input$k], color = styleInterval(c(critical.f2, 0, 0, critical.f1), c('red', 'black', 'black', 'black','red')))
  })
  output$M <- renderDataTable({
    my_data <- data()
    my_data <- my_data[, input$MV]
    mm <- ncol(my_data)
    fnames <- paste0("F", 1:mm)
    fit <- principal(my_data, nfactors=input$k, rotate = input$method2)
    M <- cor(fit$scores)
    colnames(M) <- fnames[1:input$k]
    rownames(M) <- fnames[1:input$k]
    M %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(dom = 'Bt',
                                    pageLength = mm, 
                                    buttons = 'copy'
                                    )) %>%
      formatRound(colnames(M), digits=3)
  })
  output$GFF1 <- renderPlotly({
    my_data <- data()
    my_data <- my_data[, input$MV]
    mm <- ncol(my_data)
    fnames <- paste0("F", 1:mm)
    var_names <- names(my_data)
    Faktori <- fnames[1:input$k]
    fit <- principal(my_data, nfactors=input$k, rotate=input$method2)
    F1 <- fit$loadings*fit$loadings
    Factors <- rep(Faktori, each=mm)
    Var <- rep(var_names, times=input$k)
    Loa1 <- c()
    for (a in 1:input$k) {Loa1 <- c(Loa1,F1[,a])}
    FF1 <- data_frame(Factors,Var,Loa1)
    
    GFF1 <- ggplot(data=FF1, aes(y = Var, x = Loa1, fill = Factors, color = Factors)) +
      geom_bar(alpha = input$opa, position ="stack",stat="identity", width = input$width) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(GFF1) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Loadings", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Variables", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  output$GFF2 <- renderPlotly({
    my_data <- data()
    my_data <- my_data[, input$MV]
    var_names <- names(my_data)
    mm <- ncol(my_data)
    fnames <- paste0("F", 1:mm)
    Faktori <- fnames[1:input$k]
    fit <- principal(my_data, nfactors=input$k, rotate=input$method2)
    F1 <- fit$loadings*fit$loadings
    Fac <- rep(Faktori, each=mm)
    Variables <- rep(var_names, times=input$k)
    Loa1 <- c()
    for (a in 1:input$k) {Loa1 <- c(Loa1,F1[,a])}
    FF1 <- data_frame(Fac, Variables, Loa1)
    
    GFF2 <- ggplot(data=FF1, aes(y=Fac, x=Loa1, fill = Variables, color = Variables)) +
      geom_bar(alpha = input$opa, position ="stack",stat="identity", width = input$width) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(GFF2) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Loadings", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Factors", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  output$Scores <- renderDataTable({
    my_data <- data()
    my_data <- my_data[, input$MV]
    mm <- ncol(my_data)
    n <- nrow(my_data)
    fnames <- paste0("F", 1:mm)
    fit <- principal(my_data, nfactors=input$k, rotate=input$method2)
    FS <- fit$scores
    fnames <- paste0("F", 1:mm)
    colnames(FS) <- fnames[1:input$k]
    FS %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(dom = 'Bt', 
                                    pageLength = n, 
                                    buttons = 'copy'
                                    )) %>%
      formatRound(colnames(FS), digits=3)
  })
  
  # Canonical Correlation Analysis #
  output$RC <- renderDataTable({
    my_data <- data()
    B1 <- my_data[, input$CA1]
    B2 <- my_data[, input$CA2]
    mm <- ncol(my_data)
    m1 <- ncol(B1)
    m2 <- ncol(B2)
    fnames <- paste0("CF", 1:mm)
    if (m1<=m2) {kk=m1} else {kk=m2}
    if (m1>=m2) {rcca<- cca (B1, B2, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    else {rcca<- cca (B2, B1, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    p <- pchisq(rcca$chisq, rcca$df, lower.tail = FALSE)
    RC <- cbind.data.frame(fnames[1:kk], round(rcca$corr, digits = 3), round(rcca$chisq, digits = 3), rcca$df, round(p, digits = 3))
    Rc <- RC[c(1,2,3,4,5)]
    colnames(RC) <- c("CF", "Rc", "Chi-sq.","df", "p-level")
    RC %>% 
      DT::datatable (rownames = FALSE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(pageLength = kk,
                                    dom = 'Bt', 
                                    buttons = 'copy')) 
    #%>% formatStyle("p-level", color = styleInterval(c(0.05), c('red', 'black')))
  })
  output$F1 <- renderDataTable({
    my_data <- data()
    B1 <- my_data[, input$CA1]
    B2 <- my_data[, input$CA2]
    mm <- ncol(my_data)
    m1 <- ncol(B1)
    m2 <- ncol(B2)
    fnames <- paste0("CF", 1:mm)
    if (m1<=m2) {kk=m1} else {kk=m2}
    if (m1>=m2) {rcca<- cca (B1, B2, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    else {rcca<- cca (B2, B1, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    if (m1>=m2) {F1 <- round(rcca$xstructcorr, digits = 3)} else {F1 <- round(rcca$ystructcorr, digits = 3)} 
    colnames(F1) <- fnames[1:kk]
    F1 %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(pageLength = m1,
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$F2 <- renderDataTable({
    my_data <- data()
    B1 <- my_data[, input$CA1]
    B2 <- my_data[, input$CA2]
    mm <- ncol(my_data)
    m1 <- ncol(B1)
    m2 <- ncol(B2)
    fnames <- paste0("CF", 1:mm)
    if (m1<=m2) {kk=m1} else {kk=m2}
    if (m1>=m2) {rcca<- cca (B1, B2, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    else {rcca<- cca (B2, B1, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    if (m1>=m2) {F2 <- round(rcca$ystructcorr, digits = 3)} else {F2 <- round(rcca$xstructcorr, digits = 3)} 
    colnames(F2) <- fnames[1:kk]
    F2 %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(pageLength = m2,
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$CF1 <- renderDataTable({
    my_data <- data()
    B1 <- my_data[, input$CA1]
    B2 <- my_data[, input$CA2]
    n <- nrow(my_data)
    mm <- ncol(my_data)
    m1 <- ncol(B1)
    m2 <- ncol(B2)
    fnames <- paste0("CF", 1:mm)
    if (m1<=m2) {kk=m1} else {kk=m2}
    if (m1>=m2) {rcca<- cca (B1, B2, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    else {rcca<- cca (B2, B1, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    if (m1>=m2) {CF1 <- round(rcca$canvarx, digits = 3)} else {CF1 <- round(rcca$canvary, digits = 3)} 
    colnames(CF1) <- fnames[1:kk]
    CF1 %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(dom = 'Bt', 
                                    pageLength = n,
                                    buttons = 'copy')) 
  })
  output$CF2 <- renderDataTable({
    my_data <- data()
    B1 <- my_data[, input$CA1]
    B2 <- my_data[, input$CA2]
    n <- nrow(my_data)
    mm <- ncol(my_data)
    m1 <- ncol(B1)
    m2 <- ncol(B2)
    fnames <- paste0("CF", 1:mm)
    if (m1<=m2) {kk=m1} else {kk=m2}
    if (m1>=m2) {rcca<- cca (B1, B2, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    else {rcca<- cca (B2, B1, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    if (m1>=m2) {CF2 <- round(rcca$canvary, digits = 3)} else {CF2 <- round(rcca$canvarx, digits = 3)} 
    colnames(CF2) <- fnames[1:kk]
    CF2 %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(dom = 'Bt', 
                                    pageLength = n,
                                    buttons = 'copy')) 
  })
  output$GFFF1 <- renderPlotly({
    my_data <- data()
    B1 <- my_data[, input$CA1]
    B2 <- my_data[, input$CA2]
    n <- nrow(my_data)
    mm <- ncol(my_data)
    m1 <- ncol(B1)
    m2 <- ncol(B2)
    fnames <- paste0("CF", 1:mm)
    if (m1<=m2) {kk=m1} else {kk=m2}
    if (m1>=m2) {rcca<- cca (B1, B2, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)} 
    else {rcca<- cca (B2, B1, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    nvar1 <- colnames(B1)
    if (m1>=m2) {F1 <- round(rcca$xstructcorr, digits = 3)} else {F1 <- round(rcca$ystructcorr, digits = 3)} 
    Faktori <- fnames[1:kk]
    Fac1 <- rep(Faktori, each=m1)
    Variables <- rep(nvar1, times=kk)
    Loa1 <- c()
    for (a in 1:kk) {Loa1 <- c(Loa1,F1[,a])}
    CFF1 <- data_frame(Fac1,Variables,Loa1)
    
    GFFF1 <- ggplot(data=CFF1, aes(y = Fac1, x = Loa1, fill = Variables, color = Variables)) +
      geom_bar(alpha = input$opa, position = "stack", stat = "identity", width = input$width) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(GFFF1) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Loadings", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Canonical factors", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  output$GFFF2 <- renderPlotly({
    my_data <- data()
    B1 <- my_data[, input$CA1]
    B2 <- my_data[, input$CA2]
    n <- nrow(my_data)
    mm <- ncol(my_data)
    m1 <- ncol(B1)
    m2 <- ncol(B2)
    fnames <- paste0("CF", 1:mm)
    if (m1<=m2) {kk=m1} else {kk=m2}
    if (m1>=m2) {rcca<- cca (B1, B2, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)} 
    else {rcca<- cca (B2, B1, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)}
    nvar2 <- colnames(B2)
    if (m1>=m2) {F2 <- round(rcca$ystructcorr, digits = 3)} else {F2 <- round(rcca$xstructcorr, digits = 3)} 
    Faktori <- fnames[1:kk]
    Fac2 <- rep(Faktori, each=m2)
    Variables <- rep(nvar2, times=kk)
    Loa2 <- c()
    for (a in 1:kk) {Loa2 <- c(Loa2,F2[,a])}
    CFF2 <- data_frame(Fac2, Variables, Loa2)
    
    GFFF2 <- ggplot(data=CFF2, aes(y = Fac2, x = Loa2, fill = Variables, color = Variables)) +
      geom_bar(alpha = input$opa, position ="stack",stat="identity", width = input$width) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(GFFF2) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Loadings", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Canonical factors", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  
  # Discriminant Analysis #
  output$df <- renderDataTable({
    my_data <- data()
    Independent <- my_data[, input$DAI]
    Dependent    <- my_data[, input$DAD]
    X <- as.matrix(Dependent)
    Y <- as.matrix(Independent)
    var_disc <- lm(X ~ Y, data = my_data)
    cd_candisc <- candisc(var_disc, data = my_data)
    w <- Wilks(cd_candisc)
    k <- cd_candisc$ndim
    Eigenval <- round(cd_candisc$eigenvalues, digits = 3)
    Canonical_corr <- round(sqrt((cd_candisc$canrsq)), digits = 3)
    Wilks_lam <- round(w$`LR test stat`, digits = 3)
    F_aprox <- round(w$`approx F`, digits = 3)
    df1 <- w$numDF
    df2 <- w$denDF
    p_value <- round(w$`Pr(> F)`, digits = 3)
    df <- cbind(Eigenval, Canonical_corr, Wilks_lam, F_aprox, df1, df2, p_value)
    df <- data.frame(df)
    colnames(df) <- c("Eigenvalue", "Canonical R", "Wilks' Lambda","aprox. F", "df1", "df2", "p-level")
    df <- df[1:k,]
    df %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(pageLength = k,
                                    dom = 'Bt', 
                                    buttons = 'copy')) 
    #%>% formatStyle("p-level", color = styleInterval(c(0.05), c('red', 'black')))
  })
  output$sdf <- renderDataTable({
    my_data <- data()
    Independent <- my_data[, input$DAI]
    Dependent    <- my_data[, input$DAD]
    X <- as.matrix(Dependent)
    Y <- as.matrix(Independent)
    m <- ncol(Dependent)
    dfnames <- paste0("DF", 1:m)
    var_disc <- lm(X ~ Y, data = my_data)
    cd_candisc <- candisc(var_disc, data = my_data)
    k <- cd_candisc$ndim
    sdf <- round(cd_candisc$structure, digits = 3)
    sdf <- data.frame (sdf)
    colnames(sdf) <- dfnames[1:k]
    sdf %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(pageLength = m,
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$cg <- renderDataTable({
    my_data <- data()
    Independent <- my_data[, input$DAI]
    Dependent    <- my_data[, input$DAD]
    X <- as.matrix(Dependent)
    Y <- as.matrix(Independent)
    m <- ncol(Dependent)
    dfnames <- paste0("DF", 1:m)
    var_disc <- lm(X ~ Y, data = my_data)
    cd_candisc <- candisc(var_disc, data = my_data)
    f <- table(cd_candisc$factors)
    k <- cd_candisc$ndim
    cg <- round(cd_candisc$means, digits = 3)
    cg <- data.frame(cg)
    cg <- cbind.data.frame(f, cg)
    colnames(cg) <- c(input$DAI, "FREQ.", dfnames[1:k])
    cg %>% 
      DT::datatable (rownames = FALSE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(pageLength = k+1,
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })
  output$gsdf <- renderPlotly({
    my_data <- data()
    Independent <- my_data[, input$DAI]
    Dependent    <- my_data[, input$DAD]
    X <- as.matrix(Dependent)
    Y <- as.matrix(Independent)
    m <- ncol(Dependent)
    dfnames <- paste0("DF", 1:m)
    var_disc <- lm(X ~ Y, data = my_data)
    cd_candisc <- candisc(var_disc, data = my_data)
    k <- cd_candisc$ndim
    m <- ncol(Dependent)
    sdf <- round(cd_candisc$structure, digits = 3)
    sdf <- data.frame (sdf)
    colnames(sdf) <- dfnames[1:k]
    Varijable <- names(Dependent)
    Faktori <- dfnames[1:k]
    Fac <- rep(Faktori, each=m)
    Variables <- rep(Varijable, times=k)
    L <- c()
    for (a in 1:k) {L <- c(L,sdf[,a])}
    gsdf <- data_frame(Fac, Variables, L)
    
    gsdf <- ggplot(data=gsdf, aes(y = Fac, x = L, fill = Variables, color = Variables)) +
      geom_bar(alpha = input$opa, position = "stack", stat = "identity", width = input$width) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(gsdf) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Loadings", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Discriminant Function", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
  output$dfs <- renderDataTable({
    my_data <- data()
    Independent <- my_data[, input$DAI]
    Dependent    <- my_data[, input$DAD]
    X <- as.matrix(Dependent)
    Y <- as.matrix(Independent)
    m <- ncol(Dependent)
    dfnames <- paste0("DF", 1:m)
    n <- nrow(Dependent)
    var_disc <- lm(X ~ Y, data = my_data)
    cd_candisc <- candisc(var_disc, data = my_data)
    k <- cd_candisc$ndim
    df1 <- cd_candisc$scores[1]
    df2 <- cd_candisc$scores[1:k+1]
    df2 <- round(df2, digits = 3)
    dfs <- cbind(df1, df2)
    colnames(dfs) <- c(input$DAI, dfnames[1:k])
    dfs <- data.frame(dfs)
    dfs %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(dom = 'Bt',
                                    pageLength = n,
                                    buttons = 'copy')
                     ) 
  })
  output$clas1 <- renderDataTable({
    my_data <- data()
    groups <- my_data[, input$DAI]
    dependent <- my_data[, input$DAD]
    dependent <- as.matrix(dependent)
    var_disc <- lda(groups ~ dependent, my_data)
    predict <- predict(var_disc)
    predict <- predict$class
    ctf <- CrossTable(predict, groups)
    ctf <- as.data.frame.matrix(t(ctf$t))
    ctf$Total = rowSums(ctf[,])
    ctf %>%   
    DT::datatable (rownames = TRUE,
                   style = style,
                   class = class,
                   extensions = c('Buttons'),
                   options = list(pageLength = 100,
                                  dom = 'Bt', 
                                  buttons = 'copy'))
  })
  
  output$clas2 <- renderDataTable({
    my_data <- data()
    groups <- my_data[, input$DAI]
    dependent <- my_data[, input$DAD]
    dependent <- as.matrix(dependent)
    var_disc <- lda(groups ~ dependent, my_data)
    predict <- predict(var_disc)
    predict <- predict$class
    ctfp <- CrossTable(predict, groups)
    ctfp <- round(ctfp$prop.col*100, digits = 2)
    ctfp <- as.data.frame.matrix(t(ctfp))
    ctfp$Total = rowSums(ctfp[,])
    ctfp %>%
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(pageLength = 100,
                                    dom = 'Bt', 
                                    buttons = 'copy'))
  })

  #Cluster Analysis
  output$dend <- renderPlotly({
    my_data <- data()
    my_data <- my_data[, input$CLV]
    z_data <- scale(my_data)
    
    dend <- my_data %>% scale %>% 
      dist(method = input$dist) %>%
      hclust (method = input$clmethod) %>%
      as.dendrogram %>% 
      set("branches_k_color", k=input$nk) %>% 
      set("branches_lwd", .5) %>%
      set("labels_cex", .1) %>% 
      set("labels_colors", k=input$nk) 
    ggd <- as.ggdend(dend)
    
    dend <- ggplot(ggd, horiz = FALSE) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    ggplotly(dend) %>% 
      layout(
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        xaxis = list( 
          title = list(text = "", font=list(color="rgba(0,0,0,0)")),
          gridcolor = "rgba(0,0,0,0)"       
          ),
        yaxis = list(
          title = list(text = "", font=list(color="rgba(0,0,0,0)")),
          gridcolor = "rgba(0,0,0,0)"
          )
      )
  })
  
  output$clus <- renderDataTable({
    my_data <- data()
    my_data <- my_data[, input$CLV]
    my_data <- scale(my_data)
    n <- nrow(my_data)
    d <- dist(my_data, method = input$dist)
    hc <- hclust(d, method = input$clmethod)
    Groups <- cutree(hc, k=input$nk)
    Groups <- data.frame(Groups)
    Groups %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(dom = 'Bt', 
                                    pageLength = n,
                                    buttons = 'copy'))
  })
  
  # Reliability Analysis 
  output$alpha <- renderText({
    my_data <- data()
    my_data <- my_data[, input$REA]
    m <- ncol(my_data)
    X <- as.matrix(my_data)                    
    i <- rep(c(1), times=m)                     
    Xs <- X %*% i                               
    ds <- describe(my_data, type=2)
    svar <- sum(ds[,4]*ds[,4]) 
    varXs <- var(Xs)
    Z <- scale(X)
    Zs <- Z %*% i 
    varZs <- var(Zs)
    k <- principal(X, nfactors=1, scores=TRUE)   
    lamda1 <- k$values[1]     
    R <- cor(X)
    k <- (m*m-m)/2                              
    AIC<- c((sum(R)-m)/2)/k
    Cronbach <- m/(m-1)*(1-svar/varXs)
    SB <- m/(m-1)*(1-m/varZs) 
    KC <- m/(m-1)*(1-1/lamda1)
    paste("Cronbach's alpha =", round(Cronbach, digits = 3), br(),
          "Spearman-Brown alpha =", round(SB, digits = 3), br(),
          "Kaiser-Caffrey alpha =", round(KC, digits = 3), br(),
          "Average interitem correlation =", round(AIC, digits = 3))
  })
  output$icc <- renderText({
    my_data <- data()
    my_data <- my_data[, input$REA]
    icc <- icc(my_data, 
               model = input$model,
               type  = input$type,
               unit  = input$unit, 
               r0 = 0, 
               conf.level = 0.95)
    paste(
      "Intraclass Corr. Coeff. (ICC) = ", round(icc$value, digits = 3), 
      "; F-value =", round(icc$Fvalue, digits = 3),
      "; p - value = ", round(icc$p.value, digits = 4), "</br>",
      "95% Conf. Interval for ICC =", round(icc$lbound,digits = 3),  " ~ ", round(icc$ubound, digits = 3)
    )
  })
  
  output$irs <- renderDataTable({
    my_data <- data()
    my_data <- my_data[, input$REA]
    m <- ncol(my_data)
    alp <- alpha(my_data)
    is1 <- alp$item.stats
    is2 <- alp$alpha.drop
    is <- cbind.data.frame(is1, is2)
    irs <- is[c(6, 7, 5, 8)]
    colnames(irs) <- c("Mean", "St.dev", "Item-total correlation", "Alpha if deleted")
    irs %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(pageLength = m,
                                    dom = 'Bt', 
                                    buttons = 'copy')) %>% 
      formatRound(colnames(irs), digits=3)
  })
  output$cd <- renderDataTable({
    my_data <- data()
    my_data <- my_data[, input$REA]
    n <- nrow(my_data)
    m <- ncol(my_data)
    X <- as.matrix(my_data)
    i <- rep(c(1), times=m) 
    Xs <- X %*% i 
    Xas <- Xs/m                                 
    colnames(Xas) <- c("X-mean")
    Z <- scale(X)
    Zs <- Z %*% i 
    Zas <- Zs/m                                 
    colnames(Zas) <- c("Z-mean")
    k <- principal(X, nfactors=1, scores=TRUE)   
    K1 <- k$scores
    cd <- cbind.data.frame(Xas, Zas, K1)
    cd %>% 
      DT::datatable (rownames = TRUE,
                     style = style,
                     class = class,
                     extensions = c('Buttons'),
                     options = list(dom = 'Bt', 
                                    pageLength = n,
                                    buttons = 'copy')) %>% formatRound(colnames(cd), digits=3)
  })
  
# Probability Calculator #
  # Normal Distribution #
  output$px <- renderText({
    x <- seq(-4,4, 0.01)*input$sd1 + input$as1
    px <- dnorm(x, input$as1, input$sd1)
    pi <- pnorm(input$rez1, mean=input$as1, sd=input$sd1, lower.tail = F)
    peri <- round(pi*100, digits = 1)
    paste("Worse than x  =", (100-peri), "%", br(),
          "Better than x =", peri, " %")
  })
  output$grafnd <- renderPlot({
    df <- data.frame (mean = c(input$as1), sd=c(input$sd1))
    
    ggplot (df, aes(xdist = dist_normal(mean, sd))) +
      stat_slab(aes(fill_ramp = after_stat(stat(x > input$rez1))), fill=boja) +
      labs(x = "x value", y = "p(x)") +
      scale_x_continuous(limits=c(input$as1-5*input$sd1, input$as1+5*input$sd1), 
                         breaks=seq(input$as1-5*input$sd1, input$as1+5*input$sd1, input$sd1)) +
      theme(legend.position="none",
            plot.background = element_rect(fill = "#FFFFFF"), 
            panel.background = element_rect(fill = "#FFFFFF")
      )
  })
  
  # T - Distribution #
  output$tp <- renderText({
    tp2 <- qt(p=input$pt/2, df = input$dft, lower.tail = F)
    tp <- qt(p=input$pt, df = input$dft, lower.tail = F)
    paste("Upper tailed t-value = ", round(tp, digits = 3), br(),
          "Two tailed t-value = ", round(tp2, digits = 3)
          
    )
  })
  output$pt <- renderText({
    pt <- pt(input$xt, df = input$dft, lower.tail = F)
    paste("Upper tailed p-value = ", round(pt, digits = 3), br(),
          "Two tailed p-value = ", round(pt, digits = 3)*2
    )
  })
  
  output$graftd <- renderPlot({
    df <- data.frame (df = c(input$dft))
    tp <- qt(p=input$pt, df = input$dft, lower.tail = F)
    ggplot (df, aes(xdist = dist_student_t(df))) +
      stat_slab(aes(fill_ramp = after_stat(stat(x > tp))), fill=boja) +
      scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
      labs(x = "t value", y = "p(t)") +
      theme(legend.position="none",
            plot.background = element_rect(fill = "#FFFFFF"), 
            panel.background = element_rect(fill = "#FFFFFF")
      )
  })
  
  output$graftd2 <- renderPlot({
    df <- data.frame (df = c(input$dft))
    tp <- qt(p=input$pt/2, df = input$dft, lower.tail = F)
    ggplot (df, aes(xdist = dist_student_t(df))) +
      stat_slab(aes(fill_ramp = after_stat(stat(abs(x) > tp))), fill=boja) +
      labs(x = "t value", y = "p(t)") +
      scale_x_continuous(limits=c(-5, 5), breaks=seq(-5, 5, 1)) +
      theme(legend.position="none",
            plot.background = element_rect(fill = "#FFFFFF"), 
            panel.background = element_rect(fill = "#FFFFFF")
      )
  })
 
  # F - Distribution #
  output$fp <- renderText({
    fp <- qf(p=input$pf, df1 = input$df1, df2 = input$df2, lower.tail = F)
    paste("F - value = ", round(fp, digits = 3))
  })
  output$pf <- renderText({
    pf <- pf(input$xf, df1 = input$df1, df2 = input$df2, lower.tail = F)
    paste("p - value = ", round(pf, digits = 3))
  })
  
  output$graff <- renderPlot({
    df <- data.frame (df1 = c(input$df1), df2=c(input$df2))
    fp <- qf(p=input$pf, df1 = input$df1, df2 = input$df2, lower.tail = F)
    ggplot (df, aes(xdist = dist_f(df1, df2))) +
      stat_slab(aes(fill_ramp = after_stat(stat(x > fp))), fill=boja) +
      labs(x = "F value", y = "p(F)") +
      scale_x_continuous(limits=c(0, 6), breaks=seq(0, 6, 1)) +
      theme(legend.position="none",
            plot.background = element_rect(fill = "#FFFFFF"), 
            panel.background = element_rect(fill = "#FFFFFF")
      )
  })
  
  # Chi-Square Distribution#
  output$hp <- renderText({
    hp <- qchisq(p=input$ph, df = input$dfh, lower.tail = F)
    paste("Chi-Square - value = ", round(hp, digits = 3))
  })
  output$ph <- renderText({
    ph <- pchisq(input$xh, df = input$dfh, lower.tail = F)
    paste("p - value = ", round(ph, digits = 3))
  })
  
  output$grafh <- renderPlot({
    df <- data.frame (df = c(input$dfh))
    hp <- qchisq(p=input$ph, df = input$dfh, lower.tail = F)
   
     ggplot (df, aes(xdist = dist_chisq(df))) +
      stat_slab(aes(fill_ramp = after_stat(stat(x > hp))), fill=boja) +
      labs(x = "Chi-Square value", y = "p(Chi-Square value)") +
      scale_x_continuous(limits=c(0, 60), breaks=seq(0, 60, 10)) +
      theme(legend.position="none",
            plot.background = element_rect(fill = "#FFFFFF"), 
            panel.background = element_rect(fill = "#FFFFFF")
      )
  })
  
  # Setting Graph
  output$graph <- renderPlotly({
    df <- data.frame(Group=c("G1", "G2", "G3"),Frequency=c(5, 10, 8))
   
    graph <-ggplot(data=df, aes(x=Group, y=Frequency, color=Group, fill=Group)) +
      geom_bar(alpha = input$opa, stat="identity", width = input$width) +
      theme_minimal() 
    
    ggplotly(graph) %>% 
      layout(legend=list(
        font=list(size = 14, color = tekst),
        title=list(font=list(size = 14, color = tekst)) 
      ), 
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      xaxis = list( 
        title = list(text = "Group", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      ),
      yaxis = list(
        title = list(text = "Frequency", font=list(size=16, color=tekst)),
        gridcolor = "rgba(0,0,0,0)",
        tickfont = list(size=14, color=tekst)
      )
    )
  })
}
shinyApp(ui, server)
