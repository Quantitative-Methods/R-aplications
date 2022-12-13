########################
# Quantitative Methods #
########################

library(shinydashboard)
library(shinyWidgets)
library(DT)             
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
library(radarchart)


ui <- dashboardPage(
		dashboardHeader(title = "Quantitative Methods", titleWidth = 260, disable = FALSE),
			dashboardSidebar(disable = FALSE,   width = 260,
				sidebarMenu(
				  menuItem("Home", tabName = "home", icon = icon("home")),
					menuItem("Choose Data File", tabName = "data", icon = icon("file")),
					menuItem("Data Transformation", tabName = "DT", icon = icon("retweet")),
					menuItem("Grouping Categorical Data", icon = icon("stats", lib = "glyphicon"),
						menuSubItem("Frequency Tables", tabName = "FT"),
						menuSubItem("Contingency Tables", tabName = "CT")
					),
					menuItem("Grouping Continuous Data", tabName = "GCD", icon = icon("stats", lib = "glyphicon")),
					menuItem("Basic Statistics", icon = icon("stats", lib = "glyphicon"),
						menuSubItem("Descirptive Parameters", tabName = "DP"),
						menuSubItem("Testing for Normality", tabName = "TN"),
						menuSubItem("CI for Population Mean", tabName = "PM"),
						menuSubItem("Correlations Analysis", tabName = "COR")
					),
					menuItem("Univariate Methods", icon = icon("stats", lib = "glyphicon"),
						menuSubItem("Independent Samples T-Test", tabName = "ISTT"),
						menuSubItem("Dependent Samples T-Test", tabName = "DSTT"),
						menuSubItem("One-way ANOVA", tabName = "OWA"),
						menuSubItem("Repeated Measures ANOVA", tabName = "RMA")
					),
					menuItem("Multivariate Methods", icon = icon("stats", lib = "glyphicon"),
					  menuSubItem("Regression Analysis (simple)", tabName = "SRA"),
						menuSubItem("Regression Analysis (multiple)", tabName = "RA"),
						menuSubItem("Factor Analysis", tabName = "FA"),
						menuSubItem("Canonical Analysis", tabName = "CA"),
						menuSubItem("Discriminant Analysis", tabName = "DA"),
						menuSubItem("Cluster Analysis", tabName = "CLA")
					),
					menuItem("Reliability Analysis", tabName = "REA", icon = icon("stats", lib = "glyphicon")),
					menuItem("Probability Calculator", icon = icon("stats", lib = "glyphicon"),
					         menuSubItem("Normal Distribution", tabName = "PCN"),
					         menuSubItem("T - Distribution", tabName = "PCT"),
					         menuSubItem("F - Distribution", tabName = "PCF"),
					         menuSubItem("Chi Square - Distribution", tabName = "PCH")
          ),
					menuItem("Graph", tabName = "GRAPH", icon = icon("stats", lib = "glyphicon"),
					         menuSubItem("Bar Chart", tabName = "GBAR"),
					         menuSubItem("Histogram", tabName = "GHIST"),
					         menuSubItem("Density", tabName = "GDENS"),
					         menuSubItem("Box Plot", tabName = "GBOX"),
					         menuSubItem("Scatter Plot", tabName = "GSCATT"),
					         menuSubItem("Bland-Altman Plot", tabName = "GBA")
					),
					menuItem("Settings Graph", tabName = "setting", icon = icon("cog", lib = "glyphicon")),
					menuItem("About", tabName = "about", icon = icon("info-sign", lib = "glyphicon")),
					menuItem("Help", tabName = "help", icon = icon("question-sign", lib = "glyphicon"))
				)
			),
		dashboardBody(includeCSS("style.css"),
      tabItems(
        # 0 Home			
        tabItem(tabName = "home",
          fluidRow(tabPanel("home", tags$img(style = "width: 87%", src="home.png"))
                  )
          ),
        # 0 Data			
        tabItem(tabName = "data",
                fluidRow(
                  box(title = "Choose Data File",
                      width = 2,
                      status = "primary",
                      radioButtons("format",
                                   "Data Type",
                                   choices = c(
                                     .csv = "csv",
                                     .ods = "ods"
                                   ),
                                   selected = "csv"
                      ),
                      fileInput("data", "",
                                buttonLabel = "Browse..."),
                      htmlOutput("nm")
                  ),
                  box(title = "Data",
                      width = 10,
                      status = "primary",
                      dataTableOutput("data",
                      )
                  )
                )
        ),
        # 1 Data Transformation     
        tabItem(tabName = "DT",
                fluidRow(
                  box(title = "Variables",
                      width = 2,
                      status = "primary",
                      checkboxGroupInput(
                        "NS", 
                        "Normally Scaled:",
                        choices = "", selected =""
                      ),
                      checkboxGroupInput(
                        "OS", 
                        "Inversely Scaled:",
                        choices = "", selected =""
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("Z - value"), 
                                  dataTableOutput("zz")
                         ),
                         tabPanel(h4("T - value"),
                                  dataTableOutput("tt")
                         ),
                         tabPanel(h4("L - value (1-5)"),
                                  dataTableOutput("ll")
                         ),
                         tabPanel(h4("Z - profile"),
                                  plotOutput("zpro")
                         ),
                         tabPanel(h4("T - profile"),
                                  plotOutput("tpro")
                         ),
                         tabPanel(h4("L - profile"),
                                  plotOutput("lpro")
                         )

                  ),
                  box(title = "Entity",
                      width = 2, 
                      status = "primary",
                      radioButtons("ENTITETI", 
                                   label = "",
                                   choices = "", 
                                   selected =""
                      )
                  )
              )
        ),
        # 2. Frequency Tables      
        tabItem(tabName = "FT",
                fluidRow(
                  box(title = "Variables",
                      width = 2,
                      status = "primary",
                      radioButtons(
                        "FT", 
                        label = "",
                        choices = "", 
                        selected =""
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("Frequency Tables"), br(),
                                  dataTableOutput("tf"),
                                  htmlOutput("chi")
                         ),
                         tabPanel(h4("Bar Chart"), br(),
                                  plotOutput("bar")
                         ),
                         tabPanel(h4("Stacked Barchart"), br(),
                                  plotOutput("pie")
                         )
                    )
              )
        ),
        # 3. Contingency Tables       
        tabItem(tabName = "CT",
                fluidRow(
                  box(title = "Variables",
                      width = 2,
                      status = "primary",
                      radioButtons("CT1", 
                                   label = "",
                                   choices = "", 
                                   selected =""
                      ),
                      radioButtons("CT2", 
                                   label = "",
                                   choices = "", 
                                   selected =""
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("Contingency Tables"), br(),
                                  dataTableOutput("ctf"), br(),
                                  dataTableOutput("ctfp"),
                                  htmlOutput("chi2")
                         ),
                         tabPanel(h4("Bar Chart (f)"), br(),
                                  plotOutput("graf2") 
                         ),
                         tabPanel(h4("Bar Chart (%)"), br(),
                                  plotOutput("graf3")
                         )
                  )
              )
        ),
        # 4. Grouping Continuous Data      
        tabItem(tabName = "GCD",
                fluidRow(
                  box(title = "Variables",
                      width = 2,
                      status = "primary",
                      radioButtons("GCD", 
                                   label = "",
                                   choices = "", selected =""
                      ),
                      sliderInput("nb",
                                  "Number of Bins:",
                                  min = 3,
                                  max = 15,
                                  value = 7
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("Frequency Distribution"), 
                                  dataTableOutput("ftab")
                         ),
                         tabPanel(h4("Histogram"), 
                                  plotOutput("hist")
                         ),
                         tabPanel(h4("Frequency Polygons"), 
                                  plotOutput("poly")
                         )
                  )
              )
        ),
        # 4. Descirptive Parameters       
        tabItem(tabName = "DP",
                fluidRow(
                  box(title = "Variables",
                      width = 2,
                      status = "primary",
                      checkboxGroupInput("DP", 
                                         label = "",
                                         choices = "", selected =""),
                      radioButtons("DP2", 
                                   label = "",
                                   choices = "", selected ="")
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("Descirptive Parameters"), 
                                  dataTableOutput("des")
                         ),
                         tabPanel(h4("Box and Whiskers plot"), 
                                  plotOutput("boxds")
                         ),
                         tabPanel(h4("Density plot"), 
                                  plotOutput("dens1")
                         )
                  )
              )
        ),
        # 5. Testing for Normality       
        tabItem(tabName = "TN",
                fluidRow(
                  box(title = "Variables",
                      width = 2, 
                      status = "primary",
                      radioButtons("TN", 
                                   label = "",
                                   choices = "", selected =""
                      ),
                      sliderInput("NB",
                                  "Number of Bins:",
                                  min = 1,
                                  max = 100,
                                  value = 7
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("Testing for Normality"), 
                                  textOutput("ks"),
                                  textOutput("lks"),
                                  textOutput("ad"),
                                  textOutput("sf"),
                                  textOutput("sw")
                         ),
                         tabPanel(h4("Histogram"), 
                                  plotOutput("histogram")
                         ),
                         tabPanel(h4("Box and Whiskers plot"), 
                                  plotOutput("box")
                         ),
                         tabPanel(h4("QQ Plot"), 
                                  plotOutput("qq")
                         )
                  )
              )
        ),
        # 6. Confidence Intervals for the Population Mean        
        tabItem(tabName = "PM",
                fluidRow(
                  box(title = "Variables",
                      width = 2, 
                      status = "primary",
                      radioButtons("PM", 
                                   label = "",
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
                                  max = 300,
                                  value = 100
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("CI for the Population Mean"), 
                                  htmlOutput("ci")
                         ),
                         tabPanel(h4("Standard error and sample size"), 
                                  plotOutput("gsem")
                         )
                  )
                )
        ),
        # 8.  Correlations       
        tabItem(tabName = "COR",
                fluidRow(
                  box(title = "Variables",
                      width = 2,
                      status = "primary",
                      checkboxGroupInput("COR", 
                                         "Select Variables:",
                                         choices = "", 
                                         selected =""
                      ),
                      radioButtons("CORM", "Method:",
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
                         tabPanel(h4("Correlations"),
                                  p("Marked correlations are significant at p < 0.05"),
                                  dataTableOutput("r")
                         ),
                         tabPanel(h4("Level of Significance"), 
                                  p("Marked p-value < 0.05"),
                                  dataTableOutput("p")
                         ),
                         tabPanel(h4("Scatter Plot"), 
                                  plotOutput("scatter")
                         )
                  ),
                  box(title = "Variables",
                      width = 2,
                      status = "primary",
                      tabPanel(h4("Scatter Plot"),
                               radioButtons("XX", 
                                            label = "",
                                            choices = "", selected =""),
                               radioButtons("YY", 
                                            label = "",
                                            choices = "", selected ="")
                      ),
                  ) 
              )
        ),
        # 9.  Independent Samples T-Test 
        tabItem(tabName = "ISTT",
                fluidRow(
                  box(title = "Variables",
                      width = 2, 
                      status = "primary",
                      checkboxGroupInput("DVITT", 
                                         "Dependent Variables:",
                                         choices = "", selected =""
                      ),
                      radioButtons("IVITT", 
                                   label = "Independent Variables:",
                                   choices = "", selected = ""
                      ),
                      radioButtons("ISTD", 
                                   label = "Dependent Variables:",
                                   choices = "", selected = ""
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("T-test (more variables)"),
                                  dataTableOutput("titt")
                         ),
                         tabPanel(h4("Descirptive Parameters"), 
                                  dataTableOutput("dest"),
                                  hr(),
                                  h4("Box and Whiskers plot"),
                                  plotOutput("boxt"),
                                  hr(),
                                  h4("Density plot"), 
                                  plotOutput("dens2")
                         ),
                         tabPanel(h4("T-test (one variable)"), 
                                  htmlOutput("t"),
                                  hr(),
                                  h4 ("Mean Plot with 95% Confidence Interval"),
                                  plotOutput("err")
                         )
                  )
              )
        ),
        # 10.  Dependent (Paired) Samples T-Test   
        tabItem(tabName = "DSTT",
                fluidRow(
                  box(title = "Variables",
                      width = 2,
                      status = "primary",
                      checkboxGroupInput("DSTDV", 
                                         "Dependent Variables:",
                                         choices = "", selected =""
                      ),
                      radioButtons("DSTI", 
                                   label = "Independent Variables:",
                                   choices = "", selected = ""
                      ),
                      radioButtons("DSTD", 
                                   label = "Dependent Variables:",
                                   choices = "", selected = ""
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("T-test (more var.)"),
                                  dataTableOutput("tdtt")
                         ),
                         tabPanel(h4("Descirptive Parameters"), 
                                  dataTableOutput("dest2"),
                                  hr(),
                                  h4("Normality Test (Shapiro-Wilk)"),
                                  textOutput ("shapiro2"),
                                  hr(),
                                  h4("Box and Whiskers plot"),
                                  plotOutput("boxt2"),
                                  hr(),
                                  h4("Density plot"),
                                  plotOutput("dens3")
                         ),
                         tabPanel(h4("T-test (one var.)"), 
                                  htmlOutput("t2"),
                                  hr(),
                                  h4 ("Mean Plot with 95% Confidence Interval"),
                                  plotOutput("err2")
                         )
                  )
              )
        ),
        # 11.  One Way ANOVA
        tabItem(tabName = "OWA",
                fluidRow(
                  box(title = "Variables",
                      width = 2, 
                      status = "primary",
                      checkboxGroupInput("ANOVADV", 
                                         "Dependent Variables:",
                                         choices = "", selected =""
                      ),
                      radioButtons("ANOVAI", 
                                   label = "Independent Variables:",
                                   choices = "", selected = ""
                      ),
                      radioButtons("ANOVAD", 
                                   label = "Dependent Variables:",
                                   choices = "", selected = ""
                      ),
                      radioButtons("ADJ", 
                                   label = "Method for adjusting p-values:",
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
                         width = 6,
                         tabPanel(h4("ANOVA (more var.)"),
                                  dataTableOutput("arez")
                         ),
                         tabPanel(h4("Descirptive Parameters"), 
                                  dataTableOutput("desa"),
                                  hr(),
                                  h4("Box and Whiskers plot"),
                                  plotOutput("bwp"),
                                  hr(),
                                  h4("Density plot"),
                                  plotOutput("dens4"),

                         ),
                         tabPanel(h4("ANOVA (one var.)"), 
                                  dataTableOutput("anovat"),
                                  hr(),
                                  htmlOutput("anova"),
                                  hr(),
                                  h4 ("Mean Plot with 95% Confidence Interval"),
                                  plotOutput("erra"),
                                  hr(),
                                  h4("Post Hoc Test for Multiple Comparisons"),
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
                      status = "primary",
                      radioButtons("ENT", 
                                   label = "Entities:",
                                   choices = "", selected = ""
                      ),
                      radioButtons("RMAM", 
                                   label = "Measurements:",
                                   choices = "", selected = ""
                      ),
                      radioButtons("RMADV", 
                                   label = "Dependent Variables:",
                                   choices = "", selected = ""
                      ),
                      radioButtons("PHr", "Post-hoc pairwise comparisons:",
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
                         width = 6,
                         tabPanel(h4("Descirptive Parameters"), 
                                  dataTableOutput("desrma"), 
                                  hr(),
                                  h4("Box and Whiskers plot"),
                                  plotOutput("bwprma"),
                                  h4("Density plot"),
                                  plotOutput("dens5")

                         ),
                         tabPanel(h4("ANOVA"), 
                                  dataTableOutput("aovrm"), 
                                  hr(),
                                  htmlOutput("rma2"),
                                  hr(),
                                  h4 ("Mean Plot with 95% Confidence Interval"),
                                  plotOutput("errrma"),
                                  hr(),
                                  h4("Post Hoc Test for Multiple Comparisons"),
                                  dataTableOutput ("pwc")
                         )
                  )
              )
        ),
        
        # 13.  Regression Analysis (simple)
        tabItem(tabName = "SRA",
                fluidRow(
                  box(title = "Variables",
                      width = 2, #height = 946,
                      status = "primary",
                      radioButtons("X", 
                                   label = "",
                                   choices = "", selected =""),
                      radioButtons("Y", 
                                   label = "",
                                   choices = "", selected ="")
        
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("Regression Results"), 
                                  htmlOutput("rsra"), hr(),
                                  numericInput("xx", "Independent value:", 0, width = '220px'),
                                  numericInput("pp", "Confidence interval (%):", 95, width = '220px'),
                                  verbatimTextOutput("xx"), 
                                  htmlOutput("yy"), hr(),
                                  plotOutput("gsra")
                         ),
                         tabPanel(h4("Regression Scores"), 
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
                      status = "primary",
                      checkboxGroupInput("PRED", 
                                         label = "",
                                         choices = "", selected =""),
                      radioButtons("KRIT", 
                                   label = "",
                                   choices = "", selected ="")
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("Reg. Results"), 
                                  htmlOutput("ro"), hr(),
                                  dataTableOutput("rez")
                         ),
                         tabPanel(h4("Graph (P)"), 
                                  h4("Graph of Partial Coefficients of Determinations (P)"),
                                  plotOutput("gro")
                         ),
                         tabPanel(h4("Graph (Beta, Part R, R)"), 
                                  h4("Graph of Standardized Regression Coefficients, Partial Correlations and Correlations"),
                                  plotOutput("gcoef")
                         ),
                         tabPanel(h4("Reg. Scores"), 
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
                      status = "primary",
                      checkboxGroupInput("MV", 
                                         "Select Variables:",
                                         choices = "", selected =""
                      ),
                      sliderInput("k",
                                  "Number Principal Components:",
                                  step = 1,
                                  min = 1,
                                  max = 1,
                                  value = 1
                      ),
                      radioButtons("method2", "Rotated Principal Components:",
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
                         tabPanel(h4("Eigenvalue"), 
                                  dataTableOutput("Lamda"),
                                  htmlOutput("ssmc"),hr(),
                                  h4("Scree Plot"),
                                  plotOutput("spgk")
                         ),
                         tabPanel(h4("Pattern"), 
                                  dataTableOutput("A"),
                         ),
                         tabPanel(h4("Structure"), 
                                  dataTableOutput("FF"),
                         ),
                         tabPanel(h4("Correlation"), 
                                  dataTableOutput("M"),
                         ),
                         tabPanel(h4("Graph"), 
                                  h4("Graph Factor Loadings (Variables)"),
                                  plotOutput("GFF1"),
                                  h4("Graph Factor Loadings (Factors)"),
                                  plotOutput("GFF2")
                         ),
                         tabPanel(h4("Scores"), 
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
                      status = "primary",
                      checkboxGroupInput("CA1", 
                                         "Select Variables:",
                                         choices = "", 
                                         selected =""
                      ),
                      checkboxGroupInput("CA2", 
                                         "Select Variables:",
                                         choices = "", 
                                         selected =""
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("Canonical Factor"),
                                  dataTableOutput("RC")
                         ),
                         tabPanel(h4("Structure"),
                                  h4("Factor Structure 1. Set"),
                                  dataTableOutput("F1"),
                                  hr(),
                                  h4("Factor Structure 2. Set"), 
                                  dataTableOutput("F2"),
                         ),
                         tabPanel(h4("Graph"), 
                                  h4("Graph Factor Structure 1. Set"),
                                  plotOutput("GFFF1"),
                                  hr(),
                                  h4("Graph Factor Structure 2. Set"),
                                  plotOutput("GFFF2"),
                         ),
                         tabPanel(h4("Scores 1. Set"), 
                                  dataTableOutput("CF1")
                         ),
                         tabPanel(h4("Scores 2. Set"), 
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
                      status = "primary",
                      checkboxGroupInput("DAD", "Select Variables:",
                                         choices = "", 
                                         selected =""
                      ),
                      radioButtons("DAI", 
                                   label = "Independent Variables:",
                                   choices = "", 
                                   selected =""
                      )
                  ),
                  tabBox(title = "",
                         width = 6,
                         tabPanel(h4("DF"), 
                                  dataTableOutput("df")
                         ),
                         tabPanel(h4("Structure"), 
                                  h4("Structure Discriminant Functions"),
                                  dataTableOutput("sdf")
                         ),
                         tabPanel(h4("Centroids"),
                                  h4("Group Centroids"),
                                  dataTableOutput("cg")
                         ),
                         tabPanel(h4("Classifications"),
                                  h4("Classifications Matrix (Frequency)"),
                                  dataTableOutput("clas1"),
                                  hr(),
                                  h4("Classifications Matrix (%)"),
                                  dataTableOutput("clas2"),
                                  hr(),
                                  "Rows: Actual classifications, Columns: Predicted classifications"
                         ),
                         tabPanel(h4("Graph"),
                                  h4("Graph Structure Discriminant Functions"),
                                  plotOutput("gsdf")
                         ),
                         tabPanel(h4("Scores"), 
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
                      status = "primary",
                      checkboxGroupInput("CLV", 
                                         "Select Variables:",
                                         choices = "", selected =""
                      ),
                      radioButtons("dist", "Distances:",
                                   choices = c(euclidean = "euclidean",
                                               maximum = "maximum",
                                               manhattan = "manhattan",
                                               canberra = "canberra",
                                               minkowski = "minkowski"
                                   ),
                                   selected = "euclidean"
                      ),
                      radioButtons("clmethod", "Method:",
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
                      ),
                  ),
                  box(title = "Groups",
                      width = 2,
                      status = "primary",
                      dataTableOutput("clus")
                  ),
                  box(title = "Cluster dendrogram",
                      width = 8,
                      status = "primary",
                      plotOutput("dend")
                  )
                )
        ),
        # 18.  Reliability Analysis
        tabItem(tabName = "REA",
                fluidRow(
                  box(title = "Variables",
                      width = 2, 
                      status = "primary",
                      checkboxGroupInput("REA", 
                                         "Select Variables:",
                                         choices = "", 
                                         selected =""
                      ),
                  ),box(title = "Reliability Analysis Results",
                        width = 5, 
                        status = "primary",
                        htmlOutput("alpha"),
                        hr(),
                        h4("Item Reliability Statistics"),
                        dataTableOutput("irs")
                  ),
                  box(title = "Condensed Data",
                      width = 3, 
                      status = "primary",
                      dataTableOutput("cd")
                  )
            )
        ),
    # 19.  Probability Calculator
        # Normal (Gauss) Distribution
        tabItem(tabName = "PCN",
                fluidRow(
                  box(title = "Input Values",
                      status = "primary",
                      width = 2,
                      numericInput("as1", "Mean", 180, width = '150px'),
                      verbatimTextOutput("as1"),
                      numericInput("sd1", "Standard Deviation", 10, width = '150px'),
                      verbatimTextOutput("sd1"),
                      numericInput("rez1", "Value (X)", 190, width = '150px'),
                      verbatimTextOutput("rez1"), hr(),
                      htmlOutput("px")
                  ),
                  box(title = "Normal (Gauss) Distribution",
                      status = "primary",
                      width = 5,
                      plotOutput("grafnd"),
                  )
                )
        ),
        tabItem(tabName = "PCT",
                fluidRow(
                  box(title = "Input Values",
                      status = "primary",
                      width = 2,
                      numericInput("dft", 
                                   "df", 
                                   value = 2, 
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
                      htmlOutput("pt"), 
                  ),
                  box(title = "T - Distribution - Two tailed",
                      status = "primary",
                      width = 5,
                      plotOutput("graftd"),
                  ),
                  box(title = "T - Distribution - Upper tailed",
                      status = "primary",
                      width = 5,
                      plotOutput("graftd2"),
                  )
                )
        ),
        tabItem(tabName = "PCF",
                fluidRow(
                  box(title = "Input Values",
                      status = "primary",
                      width = 2,
                      numericInput("df1", 
                                   "df1", 
                                   value = 3, 
                                   min = 1,
                                   max = 10,
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
                      htmlOutput("pf"), 
                  ),
                  box(title = "F-Distribution",
                      status = "primary",
                      width = 5,
                      plotOutput("graff"),
                  )
                )
        ),
        tabItem(tabName = "PCH",
                fluidRow(
                  box(title = "Input Values",
                      status = "primary",
                      width = 2,
                      numericInput("dfh", 
                                   "df", 
                                   value = 10, 
                                   min = 1,
                                   max = 100,
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
                      htmlOutput("ph"), 
                  ),
                  box(title = "Chi-Square Distribution",
                      status = "primary",
                      width = 5,
                      plotOutput("grafh"),
                  )
              )
        ),
    
# 20. Graph 
    # Bar Chart     
    tabItem(tabName = "GBAR",
            fluidRow(
              box(title = "Variables",
                  width = 2, 
                  status = "primary",
                  radioButtons("GBAR1", 
                               label = "",
                               choices = "", 
                               selected =""
                  ),
                  radioButtons("GBAR2", 
                               label = "",
                               choices = "", 
                               selected =""
                  )
              ),
              tabBox(title = "",
                     width = 6,
                     tabPanel(h4("Bar Chart"), br(),
                              plotOutput("gbar1")
                     ),
                     tabPanel(h4("Bar Chart by groups"), br(),
                              plotOutput("gbar2")
                     ),
                     
              )
            )
    ),
    # Histogram  
    tabItem(tabName = "GHIST",
            fluidRow(
              box(title = "Variables",
                  width = 2,
                  status = "primary",
                  radioButtons("GX", 
                               label = "",
                               choices = "", 
                               selected =""
                  ),
                  radioButtons("GG", 
                               label = "",
                               choices = "", 
                               selected =""
                  ),
                  sliderInput("gnb",
                              "Number of Bins:",
                              min = 3,
                              max = 15,
                              value = 7
                  )
              ),
              tabBox(title = "",
                     width = 6,
                     tabPanel(h4("Histogram"), 
                              plotOutput("ghist1")
                     ),
                     tabPanel(h4("Histogram by groups"), 
                              plotOutput("ghist2")
                     ),
                     tabPanel(h4("Histogram by facets"), 
                              plotOutput("ghist3")
                     )
              )
            )
    ),
    #  Density   
    tabItem(tabName = "GDENS",
            fluidRow(
              box(title = "Variables",
                  width = 2,
                  status = "primary",
                  radioButtons("GX1", 
                               label = "",
                               choices = "", 
                               selected =""
                  ),
                  radioButtons("GG1", 
                               label = "",
                               choices = "", 
                               selected =""
                  )
              ),
              tabBox(title = "",
                     width = 6,
                     tabPanel(h4("Density"), 
                              plotOutput("gdens1")
                     ),
                     tabPanel(h4("Density by groups"), 
                              plotOutput("gdens2")
                     ),
                     tabPanel(h4("Density by facets"), 
                              plotOutput("gdens3")
                     )
               )
          )
    ),
    # Box $ Whisker   
    tabItem(tabName = "GBOX",
            fluidRow(
              box(title = "Variables",
                  width = 2,
                  status = "primary",
                  radioButtons("GX2", 
                               label = "",
                               choices = "", 
                               selected =""
                  ),
                  radioButtons("GG2", 
                               label = "",
                               choices = "", 
                               selected =""
                  )
              ),
              tabBox(title = "",
                     width = 6,
                     tabPanel(h4("Box & Whisker"), 
                              plotOutput("gbox1")
                     ),
                     tabPanel(h4("Box & Whisker by groups"), 
                              plotOutput("gbox2")
                     ),
                     
              )
          )
    ),
    # Scatter   
    tabItem(tabName = "GSCATT",
            fluidRow(
              box(title = "Variables",
                  width = 2,
                  status = "primary",
                  radioButtons("GX3", 
                               label = "",
                               choices = "", 
                               selected =""
                  ),
                  radioButtons("GY3", 
                               label = "",
                               choices = "", 
                               selected =""
                  ),
                  radioButtons("GG3", 
                               label = "",
                               choices = "", 
                               selected =""
                  ),
                  radioButtons("gmethod", 
                               label = "Method",
                               choices = c("lm", "loess"), 
                               selected ="lm"
                  )
              ),
              tabBox(title = "",
                     width = 6,
                     tabPanel(h4("Scatter"), 
                              plotOutput("gscatt1")
                     ),
                     tabPanel(h4("Scatter by groups"), 
                              plotOutput("gscatt2")
                     ),
                     tabPanel(h4("Scatter by facets"), 
                              plotOutput("gscatt3")
                     )
              )
          )
    ),
    # Bland-Altman    
    tabItem(tabName = "GBA",
            fluidRow(
              box(title = "Variables",
                  width = 2,
                  status = "primary",
                  radioButtons("GX4", 
                               label = "",
                               choices = "", 
                               selected =""
                  ),
                  radioButtons("GY4", 
                               label = "",
                               choices = "", 
                               selected =""
                  )
              ),
              tabBox(title = "",
                     width = 6,
                     tabPanel(h4("Bland-Altman Plot"), 
                              plotOutput("gba"),
                              htmlOutput("gbaci")
                              
                     )
              )
          )
     ),
# 21. Setting Graph 
tabItem(tabName = "setting",
        fluidRow(
          box(title = "Setting Graph",
              status = "primary",
              width = 4,
              sliderInput("opa",
                          "Opacity (0-1):",
                          min = 0,
                          max = 1,
                          value = 0.5,
                          step = 0.1
              ),
              sliderInput("lum",
                          "Luminance (0-100):",
                          min = 0,
                          max = 100,
                          value = 50,
                          step = 5
              ),
              sliderInput("sat",
                          "Saturation  (0-100):",
                          min = 0,
                          max = 100,
                          value = 50,
                          step = 5
              ),
              plotOutput("graph")
          )
      )
),
    # 22. About   
        tabItem(tabName = "about",
                fluidRow(
                  box(title = "Quantitative Methods",
                      status = "primary",
                      width = 5,
                      p("Version: 1.4"),
                      p("Built on: 5.12.2022."),
                      p("Source: https://github.com/Quantitative-Methods/R-aplications"),
                      p("Licence: MIT LicenseCopyright (c) 2021 Quantitative-Methods"),
                      p("Permission is hereby granted, free of charge, to any person obtaining a copy
  							of this software and associated documentation files (the 'Software'), to deal
  							in the Software without restriction, including without limitation the rights
  							to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  							copies of the Software, and to permit persons to whom the Software is 
  							furnished to do so, subject to the following conditions:
  							The above copyright notice and this permission notice shall be included in all
  							copies or substantial portions of the Software."),
                      p("THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  							IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  							FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  							AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  							LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  							OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  							SOFTWARE."),
                      p("Authors: Dražan Dizdar & Darko Katović"),
                ) 
            )
         ),
   
# 23. Help  
      tabItem(tabName = "help",
            fluidRow(
                tabPanel("pdf", tags$iframe(style = "height: 900px; width: 100%; scrolling=yes", 
                        src="help.pdf")
                        ),

           )
        )
      )
	 )
)
	
# Server #
server <- function(input, output) {

  selectedData <- reactive({
      req(input$data)
      tryCatch(
        if(input$format=="csv"){
        {
          read.csv(input$data$datapath,
                   header = TRUE,
                   sep = ";",
                   dec = ",",
                   stringsAsFactors = TRUE,
                   row.names = 1)
        }
          }
        else{
          read_ods(input$data$datapath, 
            sheet = 1,
            col_names = TRUE,
            row_names = TRUE,
            strings_as_factors = TRUE)
        }
     )
  })
  
observeEvent(selectedData(),{
		k <- ncol(selectedData() %>% select_if(is.factor))
		if(k>0){
			updateRadioButtons(inputId = "FT",
							   label = "Select Variable:",
							   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "CT1",
							   label = "Select Variable 1:",
							   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "CT2",
							   label = "Select Variable 2:",
							   choices = names(selectedData() %>% select_if(is.factor)),
			)
			updateRadioButtons(inputId = "IVITT",
			           label = "Select Grouping Variable:",
			           choices = names(selectedData() %>% select_if(is.factor)),
			)
			updateRadioButtons(inputId = "DSTI",
							   label = "Select Grouping Variable:",
							   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "ANOVAI",
							   label = "Select Grouping Variable:",
							   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "RMAM",
							   label = "Select Measurements Variable:",
							   choices = names(selectedData() %>% select_if(is.factor)),
			)
			updateRadioButtons(inputId = "DAI",
			                   label = "Select Grouping Variable:",
			                   choices = names(selectedData() %>% select_if(is.factor)),
			)
			updateRadioButtons(inputId = "GBAR1",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "GBAR2",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "GPIE1",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "GPIE2",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "GG",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "GG1",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "GG2",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			updateRadioButtons(inputId = "GG3",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.factor)), 
			)
			
		}
		else{}
		m <- ncol(selectedData() %>% select_if(is.numeric))
		if(m>0){
		  updateCheckboxGroupInput(inputId = "NS",
		                           label = "Normally Scaled:",
		                           choices = names(selectedData() %>% select_if(is.numeric)),
		                           selected = names(selectedData() %>% select_if(is.numeric))
		  )
		  updateCheckboxGroupInput(inputId = "OS",
		                           label = "Inversely Scaled:",
		                           choices = names(selectedData() %>% select_if(is.numeric))
		  )
		  updateRadioButtons(inputId = "ENTITETI",
		                     label = "Select Entity:",
		                     choices = row.names(selectedData() %>% select_if(is.numeric))
		  )
			updateCheckboxGroupInput(inputId = "DT",
									label = "Select Variables:",
									choices = names(selectedData() %>% select_if(is.numeric)), 
									selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateCheckboxGroupInput(inputId = "DP",
									label = "Select Variables:",
									choices = names(selectedData() %>% select_if(is.numeric)), 
									selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateRadioButtons(inputId = "DP2",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "GCD",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "TN",
							    label = "Select Variable:",
							    choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "PM",
							    label = "Select Variable:",
							    choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateCheckboxGroupInput(inputId = "COR",
									label = "Select Variables:",
									choices = names(selectedData() %>% select_if(is.numeric)), 
									selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateRadioButtons(inputId = "ISTD",
							    label = "Select Dependent Variable:",
							    choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "DSTD",
							    label = "Select Dependent Variable:",
							    choices = names(selectedData() %>% select_if(is.numeric))[2:m], 
			)
			updateRadioButtons(inputId = "ANOVAD",
							    label = "Select Dependent Variable:",
							    choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "ENT",
			                   label = "Select Entities Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "RMADV",
			                   label = "Select Dependent Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric))[2:m], 
			)
			updateCheckboxGroupInput(inputId = "DVITT",
			            label = "Select Dependent Variables:",
			            choices = names(selectedData() %>% select_if(is.numeric)), 
			            selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateCheckboxGroupInput(inputId = "DSTDV",
			            label = "Select Dependent Variables:",
			            choices = names(selectedData() %>% select_if(is.numeric))[2:m],
			            selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateCheckboxGroupInput(inputId = "ANOVADV",
			            label = "Select Dependent Variables:",
			            choices = names(selectedData() %>% select_if(is.numeric)), 
			            selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateRadioButtons(inputId = "XX",
			                   label = "Select X Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "YY",
			                   label = "Select Y Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "X",
			                   label = "Select Independent Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "Y",
			                   label = "Select Dependent Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "GX",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "GX1",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "GX2",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "GX3",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "GY3",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "GX4",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "GY4",
			                   label = "Select Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			
		if(m>1){
			updateCheckboxGroupInput(inputId = "PRED",
									label = "Select Independent Variables:",
									choices = names(selectedData() %>% select_if(is.numeric)), 
									selected = names(selectedData() %>% select_if(is.numeric))[1:m-1]
			)
		  updateCheckboxGroupInput(inputId = "REA",
		              label = "Select Variables:",
		              choices = names(selectedData() %>% select_if(is.numeric)), 
		              selected = names(selectedData() %>% select_if(is.numeric))[1:2]
		  )
		}
		else{
			updateCheckboxGroupInput(inputId = "PRED",
			            label = "Select Independent Variables:",
			            choices = names(selectedData() %>% select_if(is.numeric)), 
			            selected = names(selectedData() %>% select_if(is.numeric))[1:m]
			)
		  updateCheckboxGroupInput(inputId = "REA",
		              label = "Select Variables:",
		              choices = names(selectedData() %>% select_if(is.numeric)), 
		              selected = names(selectedData() %>% select_if(is.numeric))[1]
		  )
		}

		if(m>2){
			updateCheckboxGroupInput(inputId = "CA1",
									 label = "Select Variables 1. Set:",
									 choices = names(selectedData() %>% select_if(is.numeric)), 
									 selected = names(selectedData() %>% select_if(is.numeric))[1:(m-2)]
			)
			updateCheckboxGroupInput(inputId = "CA2",
									 label = "Select Variables 2. Set:",
									 choices = names(selectedData() %>% select_if(is.numeric)), 
									 selected = names(selectedData() %>% select_if(is.numeric))[(m-1):m]
			)
			updateCheckboxGroupInput(inputId = "REA",
                   label = "Select Variables:",
                   choices = names(selectedData() %>% select_if(is.numeric)), 
                   selected = names(selectedData() %>% select_if(is.numeric))[1:3]
			)
		}
		else{
			  updateCheckboxGroupInput(inputId = "CA1",
                   label = "Select Variables 1. Set:",
                   choices = names(selectedData() %>% select_if(is.numeric)), 
                   selected = names(selectedData() %>% select_if(is.numeric))[1]
			  )
			  updateCheckboxGroupInput(inputId = "CA2",
                   label = "Select Variables 2. Set:",
                   choices = names(selectedData() %>% select_if(is.numeric)), 
                   selected = names(selectedData() %>% select_if(is.numeric))[m]
			  )
		}
			updateRadioButtons(inputId = "KRIT",
			                   label = "Select Dependent Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)),
			                   selected = names(selectedData() %>% select_if(is.numeric))[m]
			)
			updateCheckboxGroupInput(inputId = "MV",
			                         label = "Select Variables:",
			                         choices = names(selectedData() %>% select_if(is.numeric)), 
			                         selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateCheckboxGroupInput(inputId = "DAD",
									 label = "Select Dependent Variables:",
									 choices = names(selectedData() %>% select_if(is.numeric)),
									 selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateCheckboxGroupInput(inputId = "CLV",
                   label = "Select Variables:",
                   choices = names(selectedData() %>% select_if(is.numeric)),
                   selected = names(selectedData() %>% select_if(is.numeric))
			)
			observe({
				my_data <- selectedData() %>% select_if(is.numeric)
				mm <- ncol(my_data)
				n <- nrow(my_data)
				R <- cor(my_data)
				ev <- eigen(R)
				ev <- ev$values
				zgk <- length(which(1 <= ev))
			
				updateSliderInput(inputId = "k", 
								min = 1, 
								max = mm, 
								step = 1,
								value = zgk
								)
				updateSliderInput(inputId = "PMn", 
								min = 3, 
								max = 300, 
								value = n
								)
				updateSliderInput(inputId = "nk", 
				                  min = 2, 
				                  max = n-1, 
				                  step = 1,
				                  value = 2
				)
			})
		}
		else{}
    })

	# Data #
	output$nm <- renderText({
		my_data <- selectedData()
		n <- nrow(my_data)
		m <- ncol(my_data)
		paste("Number of cases (n) = ", n, "</br>",
		"Number of variables (m) = ", m)
	})
	output$data <- renderDataTable({
		my_data <- selectedData()
		n <- nrow(my_data)
		my_data %>% 
		  DT::datatable (rownames = TRUE,
			style = 'bootstrap', 
			class = 'compact',
			extensions = c('Buttons','ColReorder', 'Scroller'),
			options = list(dom = 'Bt', 
			               buttons = 'copy', 
			               colReorder = TRUE, 
			               scrollX = TRUE, 
			               scrollY = 630,
			               scroller = TRUE)) 
	})
	# Data Transformation #
	output$zz <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','ColReorder', 'Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                colReorder = TRUE, 
		                                scrollX = TRUE, 
		                                scrollY = 630,
		                                scroller = TRUE)) %>% 
		  formatStyle('MEAN_Z', backgroundColor = '#e6a4a3', fontWeight = 'bold')
	})
	output$tt <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','ColReorder', 'Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                colReorder = TRUE, 
		                                scrollX = TRUE, 
		                                scrollY = 630,
		                                scroller = TRUE)) %>% 
		  formatStyle('MEAN_T', backgroundColor = '#e6a4a3', fontWeight = 'bold')
	})
	output$ll <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','ColReorder', 'Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                colReorder = TRUE, 
		                                scrollX = TRUE, 
		                                scrollY = 630,
		                                scroller = TRUE)) %>% 
		  formatStyle('MEAN_L', backgroundColor = '#e6a4a3', fontWeight = 'bold')
	})
	output$zpro <- renderPlot({
	  my_data <- selectedData()
	  ns <- my_data[, input$NS]
	  os <- my_data[, input$OS]
	  Zns <- scale(ns)
	  Zos <- scale(os)*(-1)
	  Z <- cbind.data.frame(round(Zns, digits = 2), round(Zos, digits = 2))
	  vars <- names(Z)
	  Zt <- t((Z[input$ENTITETI,]))
	  Ze <- cbind.data.frame(vars, Zt)
	  colnames(Ze) <- c("Variables", "Z")
	  ggplot(data=Ze, aes(x=Variables, y=Z, fill=Variables, color=Variables)) +
	    geom_bar(stat="identity", alpha = input$opa) +
	    ylim(-3,3) +
	    geom_text(aes(label=Z), y=3.2, colour = "#000000")+
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	    
	    
	})
	output$tpro <- renderPlot({
	  my_data <- selectedData()
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
	  ggplot(data=Te, aes(x=Variables, y=Tv, fill=Variables, color=Variables)) +
	    geom_bar(stat="identity", alpha = input$opa) +
	    geom_abline(intercept = 50, slope = 0, color = "#FB746C") + 
	    ylim(0, 85) +
	    geom_text(aes(label=Tv), colour = "#000000", vjust=-0.3) +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	})
	  output$lpro <- renderPlot({
	    my_data <- selectedData()
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
	    ggplot(data=Te, aes(x=Variables, y=Tv, fill=Variables, color=Variables)) +
	      geom_bar(stat="identity", alpha = input$opa) +
	      geom_abline(intercept = 3, slope = 0, color = "#FB746C") + 
	      ylim(0, 6) +
	      geom_text(aes(label=Tv), colour = "#000000", vjust=-0.3) +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	
	# Frequency Tables" #
	output$tf <- renderDataTable({
		my_data <- selectedData()
		var <- my_data[,input$FT]
		ft <- table(var)
		tf <- as.data.frame(ft)
		colnames(tf) <- c(input$FT, "F")
		tf$RF <- tf$F/sum(tf$F)*100 
		tf$CF <- cumsum(tf$F)
		tf$RCF  <- cumsum(tf$RF)
		colnames(tf) <- c(input$FT, "Frequency", "Percent", "Cumulative","Cumulative Percent")
		tf %>% 
		  DT::datatable (rownames = TRUE,
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = 'Buttons',
		                 options = list(pageLength = 50, 
		                                dom = 'Bt', 
		                                buttons = 'copy')) %>% 
		  formatRound(c('Percent', 'Cumulative Percent'), 2)
	})
	output$chi <- renderText({
		my_data <- selectedData()
		var <- my_data[,input$FT]
		ft <- table(var)
		tf <- as.data.frame(ft)
		hi <- chisq.test(tf[2])
		chi <- round(hi$statistic[1], digits = 3)
		df <- round(hi$parameter[1], digits = 3)
		p <- round(hi$p.value [1], digits = 3)
		paste("Chi-square = ", chi,"; df = ", df, "; p =", p)
	})
	
	output$bar <- renderPlot({
	  my_data <- selectedData()
	  Group <- my_data[,input$FT]
	  ggplot(my_data, aes(x=Group, fill=Group, color = Group)) + 
	    geom_bar (alpha = input$opa) +
	    labs(x= input$FT, y = "Frequency") +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	  })
	output$pie <- renderPlot({
	  my_data <- selectedData()
	  Group <- my_data[,input$FT]
	  ggplot(data=my_data, aes(x='', fill=Group, color = Group)) +
	    geom_bar(alpha = input$opa) +
	    labs(x= input$FT, y = "Frequency") +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	}) 
	
	# Contingency Tables #
	output$ctf <- renderDataTable({
		my_data <- selectedData()
		var1 <- my_data[,input$CT1]
		var2 <-  my_data[,input$CT2]
		ctf <- CrossTable(var1, var2)
		ctf <- as.data.frame.matrix(ctf$t)
		ctf$Total = rowSums(ctf[,])
		ctf %>% 
		  DT::datatable (rownames = TRUE,
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = 'Buttons',
		                 options = list(pageLength = 50, 
		                                dom = 'Bt', 
		                                buttons = 'copy'))
	})
	output$ctfp <- renderDataTable({
		my_data <- selectedData()
		var1 <- my_data[,input$CT1]
		var2 <-  my_data[,input$CT2]
		ctfp <- CrossTable(var1, var2)
		ctfp <- round(as.data.frame.matrix(ctfp$prop.row)*100, digits = 2)
		ctfp$Total = round(rowSums(ctfp[,]), digits = 0)
		ctfp %>% 
		  DT::datatable (rownames = TRUE,
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = 'Buttons',
		                 options = list(pageLength = 50, 
		                                dom = 'Bt', 
		                                buttons = 'copy'))
	})
	output$chi2 <- renderText({
		my_data <- selectedData()
		var1 <- my_data[,input$CT1]
		var2 <-  my_data[,input$CT2]
		chit <- CrossTable(var1, var2, chisq = T)
		chi <- round(chit[["chisq"]][["statistic"]][["X-squared"]], digits = 3)
		df <- round(chit[["chisq"]][["parameter"]][["df"]], digits = 3)
		p <- round(chit[["chisq"]][["p.value"]], digits = 3)
		paste("Chi-square = ", chi,"; df = ", df, "; p =", p)
	})
	output$graf2 <- renderPlot({
		my_data <- selectedData()
		var1 <- my_data[,input$CT1]
		Group <-  my_data[,input$CT2]
		ggplot(my_data, aes(x=var1, fill = Group, color =Group)) + 
  		geom_bar (alpha = input$opa)+
  		labs(x= input$CT1, y = "Frequency") +
		  theme(panel.background = element_rect(fill = "#ffffff")) +
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$graf3 <- renderPlot({
		my_data <- selectedData()
		var1 <- my_data[,input$CT1]
		Group <-  my_data[,input$CT2]
		ggplot(my_data, aes(x=var1, fill = Group, color = Group)) + 
  		geom_bar (alpha = input$opa, position="fill")+
  		labs(x = input$CT1, y = "Frequency") +
		  theme(panel.background = element_rect(fill = "#ffffff")) +
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	
# Grouping Continuous Data
	output$ftab <- renderDataTable({
	  my_data <- selectedData()
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
	                   style = 'bootstrap', 
	                   class = 'compact',
	                   extensions = 'Buttons',
	                   options = list(pageLength = 50, 
	                                  dom = 'Bt', 
	                                  buttons = 'copy'))
	})
	output$hist <- renderPlot({
	  my_data <- selectedData()
	  var <- my_data[,input$GCD]
	  ir <- (max(var) - min(var))/(input$nb-1)
	  ir2 <- ir/2
	  breaks = seq(min(var)-ir2, max(var)+ir2, by=ir)
	  f.cut = cut(var, breaks, right=TRUE)
	  d.freq = table(f.cut)
	  df <- cbind.data.frame(d.freq)
	  colnames(df) <- c("Bins", "Freq")
	  ggplot(df, aes(x=Bins, y=Freq)) + 
	    geom_bar (alpha = input$opa, stat="identity", fill="#D04C44", color="#323232", width=1) +
	    labs(x= input$GCD, y = "Frequency") +
	    theme_minimal()

	})
	output$poly <- renderPlot({
	  my_data <- selectedData()
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
	  ggplot(rcf, aes(x=ggr, y = rcf)) + 
	    geom_point(alpha = input$opa, color="#D04C44", size=4)+
	    geom_line(color="#323232", size=1)+
	    labs(x= input$GCD, y = "Frequency") +
	    theme_minimal()
	})
	# Descirptive Parameters #
	output$des <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = 'Buttons',
		                 options = list(pageLength = m, 
		                                dom = 'Bt', 
		                                buttons = 'copy'))
})
	output$boxds <- renderPlot({
	  my_data <- selectedData()
	  X <-  my_data[, input$DP2]
	  ggplot(my_data, aes(x=X)) +
	    geom_boxplot(alpha = input$opa, fill="#D04C44", color="#323232", outlier.colour="blue", outlier.shape=8,  outlier.size=2) +
	    labs(x = input$DP2, y = "p(x)") +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$dens1 <- renderPlot({
	  my_data <- selectedData()
	  X <- my_data[, input$DP2]
	  ggplot(my_data, aes(x=X)) + 
	    geom_density(fill="#D04C44", color="#323232", alpha = input$opa, adjust = 1.2) +
	    geom_vline(aes(xintercept=mean(X)), colour="blue",  linetype="dashed", size=.3) +
	    labs(x= input$DP2, y = "Frequency") +
	    theme_minimal()
	})
	# Testing for Normality  #
	output$ks <- renderText({ 
		my_data <- selectedData()
		var <- my_data[,input$TN]
		ks <- ks.test(var, "pnorm", mean=mean(var), sd=sd(var))
		D <- round(ks[["statistic"]][["D"]], digits = 3)
		pks <- round(ks[["p.value"]], digits = 3)
		paste("Kolmogorov-Smirnov test:", "D = ", D,"; p = ", pks)
	}) 
	output$lks <- renderText({ 
		my_data <- selectedData()
		var <- my_data[,input$TN]
		lt <- lillie.test(var)
		Dlks <- round(lt[["statistic"]][["D"]], digits = 3)
		plks <- round(lt[["p.value"]], digits = 3)
		paste("Lilliefors test:", "D = ", Dlks,"; p = ", plks)
	})  
	output$ad <- renderText({ 
		my_data <- selectedData()
		var <- my_data[,input$TN]
		ad <- ad.test(var)
		A <- round(ad[["statistic"]][["A"]], digits = 3)
		pa <- round(ad[["p.value"]], digits = 3)
		paste("Anderson-Darling test:", "A = ", A,"; p = ", pa)
	})  
	output$sf <- renderText({ 
		my_data <- selectedData()
		var <- my_data[,input$TN]
		sf <- sf.test(var)
		Wsf <- round(sf[["statistic"]][["W"]], digits = 3)
		psf <- round(sf[["p.value"]], digits = 3)
		paste("Shapiro-Francia test:", "W = ", Wsf,"; p = ", psf)
	}) 
	output$sw <- renderText({ 
		my_data <- selectedData()
		var <- my_data[,input$TN]
		sw <- shapiro.test(var)
		W <- round(sw$statistic, digits = 3)
		psw <- round(sw$p.value, digits = 3)
		paste("Shapiro-Wilk test:", "W = ", W,"; p = ", psw)
	})
	output$histogram <- renderPlot({
		my_data <- selectedData()
		var <- my_data[,input$TN]
		ggplot(my_data, aes(x=var)) + 
		geom_histogram(bins=input$NB,  alpha = input$opa, fill="#D04C44", color="#323232") +
		labs(x= input$TN, y = "Frequency") +
		  theme_minimal()
	})
	output$box <- renderPlot({
	  my_data <- selectedData()
	  X <- my_data[,input$TN]
	    ggplot(my_data, aes(x=X)) +
	      geom_boxplot(alpha = input$opa, fill="#D04C44", color="#323232", outlier.colour="blue", outlier.shape=8,  outlier.size=2) +
	      labs(x = input$TN, y = "p(x)") +
	      theme_minimal()
	  })
	output$qq <- renderPlot({
		my_data <- selectedData()
		var <- my_data[,input$TN]
		ggplot(my_data, aes(sample = var)) + 
		stat_qq_line(color="#323232", size=1) + 
		stat_qq(alpha = input$opa, color="#D04C44", size=3) + 
		labs(x = input$TN, y=input$TN) +
		  theme_minimal()
	})
	
	# Confidence Intervals for the Population Mean #
	output$ci <- renderText({
		my_data <- selectedData()
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
	output$gsem <- renderPlot({
		my_data <- selectedData()
		xx <- my_data[,input$PM]
		sd <- round(sd(xx), digits = 2)
		ss <- c(1:300)
		se <- round(sd/sqrt(ss), digits = 2)
		sem <-cbind.data.frame(ss,se)
		ggplot(sem, aes(x=ss, y=se)) +
		geom_line(color="#323232", size=1) +
		geom_point(color="#D04C44", size=2) +
		labs(x = "Sample Size", y="Standard Error of the Mean") +
		  theme_minimal()
	}) 

	# Correlations Analysis #
	output$r <- renderDataTable({
	  my_data <- selectedData()
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
	                   style = 'bootstrap', 
	                   class = 'compact',
	                   extensions = c('Buttons','Scroller'),
	                   options = list(dom = 'Bt', 
	                                  buttons = 'copy', 
	                                  scrollX = TRUE, 
	                                  scrollY = 600,
	                                  scroller = TRUE)) %>%	
	    formatRound(colnames(R), digits=3) %>% 
	    formatStyle(var_name_numeric, color = styleInterval(c(critical.r2, 0, 0, critical.r1), c('red', 'black', 'black', 'black','red')))
	})
	output$p <- renderDataTable({
	  my_data <- selectedData()
	  vars <- my_data[, input$COR]
	  var_name_numeric <- names(vars)
	  m <- ncol(vars)
	  n <- nrow(vars)
	  P <- corr.test(vars, method = input$CORM)$p 
	  upper.tri(P)
	  P[upper.tri(P)] <- NA
	  P %>% 
	    DT::datatable (rownames = TRUE,
	                   style = 'bootstrap', 
	                   class = 'compact',
	                   extensions = c('Buttons','Scroller'),
	                   options = list(dom = 'Bt', 
	                                  buttons = 'copy', 
	                                  scrollX = TRUE, 
	                                  scrollY = 600,
	                                  scroller = TRUE)) %>%	
	    formatRound(colnames(P), digits=4) %>%	
	    formatStyle(var_name_numeric, color = styleInterval(c(0, 0.05), c('red', 'red','black')))
	})
	
	output$scatter <- renderPlot({
	  my_data <- selectedData()
	  X <- my_data[, input$XX]
	  Y <- my_data[, input$YY]
	  ggplot(my_data, aes(x=X, y=Y)) +
	    geom_point(alpha = input$opa, size=2, colour = "#D04C44") +
	    labs(x= input$XX, y = input$YY) +
	    theme_minimal()
	})
	
	# T - test Independent samples #
	output$titt <- renderDataTable({
	  my_data <- selectedData()
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
                     style = 'bootstrap', 
	                   class = 'compact',
                     extensions = 'Buttons', 
                     options = list(pageLength = m, 
                                    dom = 'Bt',
                                    buttons = 'copy'))
	})
		output$dest <- renderDataTable({
		my_data <- selectedData()
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
	output$boxt <- renderPlot({
	  my_data <- selectedData()
	  Variables <- my_data[, input$ISTD]
	  Groups    <- my_data[, input$IVITT]
	  ggplot(my_data, aes(x = Variables, y = Groups, fill=Groups)) +
	    geom_boxplot(width = .1, alpha = input$opa, outlier.colour="blue", outlier.shape=8,  outlier.size=2, color = "#323232") +
	    labs(x = input$ISTD, y = input$IVITT) +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	  
	})
	output$dens2 <- renderPlot({
	  my_data <- selectedData()
	  Variables <- my_data[, input$ISTD]
	  Groups    <- my_data[, input$IVITT]
	  df <- data.frame(Groups, Variables)
	  mu <- ddply(df, "Groups", summarise, grp.mean=mean(Variables))
	  ggplot(my_data, aes(x=Variables, fill = Groups, colour=Groups)) + 
	    geom_density(alpha = input$opa, adjust = 1.2, trim = FALSE) +
	    geom_vline(data=mu, aes(xintercept=grp.mean, color= Groups), linetype="dashed", size=.3) +
	    labs(x= input$ISTD, y = "Frequency") +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$t <- renderText({
		my_data <- selectedData()
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
			  h4("Levene's Test for Equality of Variances"),
  			"Levene's F = ", Fl, "</br>",
  			"Levene's p = ", pl, "</br>",
  			"Levene's test is not significant (p > .05)", "</br>",
			  h4("Student's T-Test"),
  			"Student's t = ", ttt ,"</br>",
  			"df =", dft, "</br>",
  			"p = ", ptt, "</br>", 
  			"Cohen's d =", d, "</br>",  
			  h4("Mann Whitney U-Test"),
  		  "Mann Whitney U =",  round(mw$statistic, digits = 3), "</br>", 
  			"Mann Whitney p =",  round(mw$p.value, digits = 3)
  			)
		}
		else{
			paste(
			  h4("Levene's Test for Equality of Variances"),
  			"Levene's F = ", Fl, "</br>",
  			"Levene's p = ", pl, "</br>",
  			"Levene's test is significant (p < .05), suggesting a violation of the assumption of equal variances", "</br>",
			  h4("Welch's t-test for Unequal Variances"),
  			"Welch's t = ", ttf , "</br>",
  			"df =", dff, "</br>",
  			"p = ", ptf, "</br>",
  			"Cohen's d =", d, "</br>", 
			  h4("Mann Whitney U-Test"),
  			"Mann Whitney U =",  round(mw$statistic, digits = 3), "</br>", 
  			"Mann Whitney p =",  round(mw$p.value, digits = 3)
  			)
		}
	})  
	output$err <- renderPlot({
		my_data <- selectedData()
		Variables <- my_data[, input$ISTD]
		Factor <- my_data[, input$IVITT]
		tgc <- summarySE(my_data, measurevar = input$ISTD, groupvars = input$IVITT,  conf.interval = 0.95)
		colnames(tgc) <- c("groups", "n", "mean", "sd","se", "ci")
		ggplot(tgc, aes(x=groups, y=mean, group = "mean")) + 
		  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, colour="#323232") +
		  geom_line(colour="#323232") +
		  geom_point(size=4, colour="#D04C44", shape=21, fill="#D04C44") + 
		  labs(x = input$IVITT, y = input$ISTD) +
		  theme_minimal()
	})
	
	# Dependent (Paired) Samples T-Test #
	output$tdtt <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = 'Buttons', 
		                 options = list(pageLength = m, 
		                                dom = 'Bt',
		                                buttons = 'copy'))
	})
	output$shapiro2 <- renderText({
		my_data <- selectedData()
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
	output$dest2 <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap',
		                 class = 'compact',
		                 extensions = 'Buttons', 
		                 options = list(pageLength = 100, 
		                                dom = 'Bt',
		                                buttons = 'copy')) %>% 
		  formatRound(c("MEAN", "SD", "SEM", "-CI95%", "+CI95%"), 3)
	})
	output$boxt2 <- renderPlot({
		my_data <- selectedData()
		Variables <- my_data[, input$DSTD]
		Groups    <- my_data[, input$DSTI]
	  ggplot(my_data, aes(x = Variables, y = Groups, fill=Groups)) +
	    geom_boxplot(width = .1, alpha = input$opa, outlier.colour="blue", outlier.shape=8,  outlier.size=2, color = "#323232") +
	    labs(x = input$DSTD, y = input$DSTI) +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$dens3 <- renderPlot({
	  my_data <- selectedData()
	  Variables <- my_data[, input$DSTD]
	  Groups    <- my_data[, input$DSTI]
	  df <- data.frame(Groups, Variables)
	  mu <- ddply(df, "Groups", summarise, grp.mean=mean(Variables))
	  ggplot(my_data, aes(x=Variables, fill = Groups, colour=Groups)) + 
	    geom_density(alpha = input$opa, adjust = 1.2, trim = FALSE) +
	    geom_vline(data=mu, aes(xintercept=grp.mean, color= Groups), linetype="dashed", size=.3) +
	    labs(x= input$DSTD, y = "Frequency") +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$t2 <- renderText({
		my_data <- selectedData()
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
		  h4("Student's T-Test"),
  		"Mean difference =", md , "</br>", 
  		"St. error difference =", se , "</br>", 
  		"Correlations =", r, "</br>", 
  		"Student's t = ", ttt , "</br>", 
  		"df =", dft, "</br>",
  		"p = ", ptt, "</br>",
  		"Cohen's d =", d, "</br>",
		  h4("Wilcoxon signed-rank test"),
  		"Wilcoxon W =",  round(wt$statistic, digits = 3), "</br>", 
  		"Wilcoxon p =",  round(wt$p.value, digits = 3)
  		)
	})  
	output$err2 <- renderPlot({
		my_data <- selectedData()
		Variables <- my_data[, input$DSTD]
		Factor    <- my_data[, input$DSTI]
		tgc <- summarySE(my_data, measurevar = input$DSTD, groupvars = input$DSTI,  conf.interval = 0.95)
		colnames(tgc) <- c("groups", "n", "mean", "sd","se", "ci")
		ggplot(tgc, aes(x=groups, y=mean, group = "mean")) + 
		  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, colour="#323232") +
		  geom_line(colour="#323232") +
		  geom_point(size=4, colour="#D04C44", shape=21, fill="#D04C44") + 
		  labs(x = input$DSTI, y = input$DSTD) +
		  theme_minimal()
	})
	
	# One-Way ANOVA #
	output$arez <- renderDataTable({
	  my_data <- selectedData()
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
		my_data <- selectedData()
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
		                 style = 'bootstrap',
		                 class = 'compact',
		                 extensions = 'Buttons', 
		                 options = list(pageLength = 100, 
		                                dom = 'Bt',
		                                buttons = 'copy')) %>% 
			formatRound(c("MEAN", "SD", "SEM", "-CI95%", "+CI95%", "S-W", "P"), 3)
	})
	output$bwp <- renderPlot({
		my_data <- selectedData()
		Variables <- my_data[, input$ANOVAD]
		Groups    <- my_data[, input$ANOVAI]
		ggplot(my_data, aes(x = Variables, y = Groups, fill=Groups)) +
		  geom_boxplot(width = .1, alpha = input$opa, outlier.colour="blue", outlier.shape=8,  outlier.size=2, color = "#323232") +
		  labs(x= input$ANOVAD, y = input$ANOVAI) +
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$dens4 <- renderPlot({
	  my_data <- selectedData()
	  Variables <- my_data[, input$ANOVAD]
	  Groups    <- my_data[, input$ANOVAI]
	  df <- data.frame(Groups, Variables)
	  mu <- ddply(df, "Groups", summarise, grp.mean=mean(Variables))
	  ggplot(my_data, aes(x=Variables, fill = Groups, colour=Groups)) + 
	    geom_density(alpha = input$opa, adjust = 1.2, trim = FALSE) +
	    geom_vline(data=mu, aes(xintercept=grp.mean, color= Groups), linetype="dashed", size=.3) +
	    labs(x= input$ANOVAD, y = "Frequency") +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$anovat <- renderDataTable({
	  my_data <- selectedData()
	  Variables <- my_data[, input$ANOVAD]
	  Factor    <- my_data[, input$ANOVAI]
	  aov <- summary(aov(Variables ~ Factor, data = my_data))
	  colnames(aov[[1]]) <- c("df", "Sum of Squares", "Mean Square", "F", "p")
	  aov[[1]] %>% 
	    DT::datatable (rownames = TRUE, 
	                   style = 'bootstrap',
	                   class = 'compact',
	                   extensions = 'Buttons', 
	                   options = list(pageLength = 100, 
	                                  dom = 'Bt',
	                                  buttons = 'copy'))%>% 
	    formatRound(c("Sum of Squares", "Mean Square", "F", "p"), 3)
	  
	})
	output$anova <- renderText({
		my_data <- selectedData()
		Variables <- my_data[, input$ANOVAD]
		Factor    <- my_data[, input$ANOVAI]
		aov <- summary(aov(Variables ~ Factor, data = my_data))
		lt <- leveneTest (Variables ~ Factor, data = my_data, center = mean)
		at <- oneway.test(Variables ~ Factor, data = my_data, var.equal = TRUE)
		af <- oneway.test(Variables ~ Factor, data = my_data, var.equal = FALSE)
		kw <- kruskal.test(Variables ~ Factor, data = my_data)
		if(lt$`Pr(>F)`[1]>0.05){
			paste(
			  h4("Homogeneity of Variances Test (Levene's)"),
				"F = ", round(lt$`F value`[1], digits = 3), "</br>",
				"df1 =", lt$Df[1], "</br>",
				"df2 =", lt$Df[2], "</br>",
				"p = ", round(lt$`Pr(>F)`[1], digits = 3), "</br>",
				"Levene's test is not significant (p > .05)", "</br>",
				h4("One-Way ANOVA"), 
				"Fisher's F = ", round(at[["statistic"]], digits = 3),"</br>",
				"df1 =", round(at[["parameter"]][[1]], digits = 3), "</br>",
				"df2 =", round(at[["parameter"]][[2]], digits = 3), "</br>",
				"p = ", round(at[["p.value"]], digits = 3), "</br>",
				h4("Kruskal-Wallis Test "), 
				"H = ", round(kw[["statistic"]][["Kruskal-Wallis chi-squared"]], digits = 3), "</br>",
				"p = ", round(kw[["p.value"]], digits = 3)
			)
		}
		else{
			paste(
			  h4("Homogeneity of Variances Test (Levene's)"),
			  "F = ", round(lt$`F value`[1], digits = 3), "</br>",
			  "df1 =", lt$Df[1], "</br>",
			  "df2 =", lt$Df[2], "</br>",
			  "p = ", round(lt$`Pr(>F)`[1], digits = 3), "</br>",
				"Levene's test is significant (p < .05), suggesting a violation of the assumption of equal variances", "</br>",
				h4("One-Way ANOVA"), 
				"Welch's F = ", round(af[["statistic"]], digits = 3), "</br>",
				"df1 =", round(af[["parameter"]][[1]], digits = 3), "</br>",
				"df2 =", round(af[["parameter"]][[2]], digits = 3), "</br>",
				"p = ", round(af[["p.value"]], digits = 3), "</br>",
				h4("Kruskal-Wallis H Test "), 
				"H = ", round(kw[["statistic"]][["Kruskal-Wallis chi-squared"]], digits = 3), "</br>",
				"p = ", round(kw[["p.value"]], digits = 3)
			) 
		}
	})
	output$PH <- renderDataTable({
		my_data <- selectedData()
		Variables <- my_data[, input$ANOVAD]
		Factor    <- my_data[, input$ANOVAI]
		res.aov <- aov(Variables ~ Factor, data = my_data)
		phh <- emmeans(res.aov, "Factor")
		ph <- summary(pairs(phh), adjust = input$ADJ)
		colnames(ph) <- c("Groups", "Mean difference", "SE","df", "t", "p")
		ph %>% 
		  DT::datatable (style = 'bootstrap',
		                 class = 'compact',
		                 extensions = 'Buttons', 
		                 options = list(pageLength = 100, 
		                                dom = 'Bt',
		                                buttons = 'copy'))%>% 
	  formatRound(c("Mean difference", "SE", "t", "p"), 3)
		})
	
	output$erra <- renderPlot({
		my_data <- selectedData()
		Variables <- my_data[, input$ANOVAD]
		Factor    <- my_data[, input$ANOVAI]
		tgc <- summarySE(my_data, measurevar = input$ANOVAD, groupvars= input$ANOVAI,  conf.interval = 0.95)
		colnames(tgc) <- c("groups", "n", "mean", "sd","se", "ci")
		ggplot(tgc, aes(x=groups, y=mean, group = "mean")) + 
		geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, colour="#323232") +
		  geom_line(colour="#323232") +
		  geom_point(size=4, colour="#D04C44", shape=21, fill="#D04C44") + 
		  labs(x = input$ANOVAI, y = input$ANOVAD) +
		  theme_minimal()
	})

	# Repeated Measures ANOVA #
	output$desrma <- renderDataTable({
	  my_data <- selectedData()
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
	                   style = 'bootstrap',
	                   class = 'compact',
	                   extensions = 'Buttons', 
	                   options = list(pageLength = 100, 
	                                  dom = 'Bt',
	                                  buttons = 'copy')) %>% 
	    formatRound(c("MEAN", "SD", "SEM", "-CI95%", "+CI95%", "S-W", "P"), 3)
	})
	
	output$bwprma <- renderPlot({
	  my_data <- selectedData()
	  Variables <- my_data[, input$RMADV]
	  Groups    <- my_data[, input$RMAM]
	  ggplot(my_data, aes(x = Variables, y = Groups, fill=Groups)) +
	    geom_boxplot(width = .1, alpha = input$opa, outlier.colour="blue", outlier.shape=8,  outlier.size=2, color = "#323232") +
	    labs(x = input$RMADV,  y = input$RMAM) +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	})

	
	output$dens5 <- renderPlot({
	  my_data <- selectedData()
	  Variables <- my_data[, input$RMADV]
	  Groups    <- my_data[, input$RMAM]
	  df <- data.frame(Groups, Variables)
	  mu <- ddply(df, "Groups", summarise, grp.mean=mean(Variables))
	  ggplot(my_data, aes(x=Variables, fill = Groups, colour=Groups)) + 
	    geom_density(alpha = input$opa, adjust = 1.2, trim = FALSE) +
	    geom_vline(data=mu, aes(xintercept=grp.mean, colour= Groups), linetype="dashed", size=.3) +
	    labs(x= input$RMADV, y = "Frequency") +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	  
	})
	output$aovrm <- renderDataTable({
	    my_data <- selectedData()
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
	                     style = 'bootstrap',
	                     class = 'compact',
	                     extensions = 'Buttons', 
	                     options = list(pageLength = 100, 
	                                    dom = 'Bt',
	                                    buttons = 'copy')) %>% 
	      formatRound(c("Sum of Squares", "Mean Square", "F", "p"), 3)
	  }) 
  output$rma2 <- renderText({
    my_data <- selectedData()
    Variables <- my_data[, input$RMADV]
    Factor    <- my_data[, input$RMAM]
    Entiteti <- my_data[, input$ENT]
    res.aov<-anova_test(data=my_data, dv = input$RMADV, wid = input$ENT, within = input$RMAM)
    res.frid <- friedman.test(y=Variables, groups=Factor, blocks=Entiteti, data = my_data)
  	paste(
  	  h4("Mauchly's Test for Sphericity"),
  	  "W = ", res.aov[["Mauchly's Test for Sphericity"]][["W"]], "</br>",
  	  "p =", res.aov[["Mauchly's Test for Sphericity"]][["p"]], "</br>",
  	  h4("Friedman test"),
  	  "Q = ", round(res.frid[["statistic"]][["Friedman chi-squared"]], digits = 3), "</br>",
  	  "df =",  res.frid[["parameter"]][["df"]], "</br>", 
  	  "p = ", round(res.frid[["p.value"]], digits = 3)
  	)
  }) 
  output$pwc <- renderDataTable({
    my_data <- selectedData()
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
      DT::datatable (style = 'bootstrap',
                     class = 'compact',
                     extensions = 'Buttons', 
                     options = list(pageLength = 100, 
                                    dom = 'Bt',
                                    buttons = 'copy')) %>% 
      formatRound(c("Mean difference", "SE", "t", "p"), 3)
})
	output$errrma <- renderPlot({
	  my_data <- selectedData()
	  Variables <- my_data[, input$RMADV]
	  Factor    <- my_data[, input$RMAM]
	  tgc <- summarySE(my_data, measurevar = input$RMADV, groupvars= input$RMAM,  conf.interval = 0.95)
	  colnames(tgc) <- c("groups", "n", "mean", "sd","se", "ci")
	  ggplot(tgc, aes(x=groups, y=mean, group = "mean")) + 
	    geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, colour="#323232") +
	    geom_line(colour="#323232") +
	    geom_point(size=4, colour="#D04C44", shape=21, fill="#D04C44") + 
	    labs(x = input$RMAM, y = input$RMADV) +
	    theme_minimal()
	})
	
	# Regression Analysis (simple)
	output$rsra <- renderText({
	  my_data <- selectedData()
	  X <- my_data[, input$X]
	  Y <- my_data[, input$Y]
	  n <- nrow(my_data) 
	  model <- lm(Y ~ X, data = my_data)
	  b0 <- model[["coefficients"]][["(Intercept)"]]
	  b1 <- model[["coefficients"]][["X"]]
	  e <- model[["residuals"]]
	  see <- sqrt(sum(e*e)/(n-2))
	  r <- cor(X,Y)
	  paste(h4("b0 = ", round(b0, digits = 3),
	        ";  b1 = ", round(b1, digits = 3), 
	        ";  see = ", round(see, digits = 3), 
	        ";  r = ", round(r, digits = 3))
	  )
 })
	output$gsra <- renderPlot({
	  my_data <- selectedData()
	  X <- my_data[, input$X]
	  Y <- my_data[, input$Y]
	  ggplot(my_data, aes(x=X, y=Y)) +
	    geom_point(alpha = input$opa, size=2, colour = "#D04C44") +
	    geom_smooth(method = "lm", colour = "#323232", size=1, se = FALSE) +
	    labs(x= input$X, y = input$Y) +
	    theme_minimal()
	})
	output$rs <- renderDataTable({
	  my_data <- selectedData()
	  X <- my_data[, input$X]
	  Y <- my_data[, input$Y]
	  n <- nrow(my_data) 
	  model <- lm(Y ~ X, data = my_data)
	  Yp <- model[["fitted.values"]]
	  e <- model[["residuals"]]
	  rs <- cbind(Y, Yp, e)
	  colnames(rs) <- c("Observed Value","Predicted Value", "Residual Values")
	  rs %>% 
	    DT::datatable (style = 'bootstrap',
	                   class = 'compact',
	                   extensions = c('Buttons','Scroller'),
	                   options = list(dom = 'Bt', 
	                                  buttons = 'copy', 
	                                  scrollX = TRUE, 
	                                  scrollY = 630,
	                                  scroller = TRUE)) %>% 
	    formatRound(colnames(rs), digits=3) 
	})
	output$yy <- renderText({
	  my_data <- selectedData()
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
	  yy <- b0+b1*input$xx
	  dg <- yy-zz*see
	  gg <- yy+zz*see
	  paste("Predicted value = ", round(yy, digits = 3), br(),
	        "Lower value = ", round(dg, digits = 3), br(),
	        "Upper value =" , round(gg,digits = 3)
	        )
	})
	
	# Regression Analysis (multiple)
	output$ro <- renderText({
		my_data <- selectedData()
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
		my_data <- selectedData()
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
		  DT::datatable (style = 'bootstrap',
		                 class = 'compact',
		                 extensions = 'Buttons', 
		                 options = list(pageLength = m, 
		                                dom = 'Bt',
		                                buttons = 'copy')) %>% 
			formatRound(colnames(rez), digits=3) %>% 
			formatStyle('p', color = styleInterval(c(0.05), c('red', 'black')))
	})
	output$gro <- renderPlot({
		my_data <- selectedData()
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
		ggplot(pggdf, aes(x= Variables, y = pgg, fill = Variables, color=Variables)) +
		geom_bar(alpha = input$opa, stat = "identity") +
		labs(x ="Variables", y ="value") +
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$gcoef <- renderPlot({
		my_data <- selectedData()
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
		ggplot(ggrez, aes(x=var, y = val, fill = Coefficients, color = Coefficients)) +
		geom_bar(alpha = input$opa, position="dodge",stat = "identity")+
		labs(x ="Variables", y ="value")+
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$dep <- renderDataTable({
		my_data <- selectedData()
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
		  DT::datatable (style = 'bootstrap',
		                 class = 'compact',
		                 rownames = TRUE,
		                 extensions = c('Buttons','Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                scrollX = TRUE, 
		                                scrollY = 630,
		                                scroller = TRUE)) %>% 
			formatRound(colnames(dep), digits=3) 
	})
	
	# Factor Analysis #
	output$Lamda <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = 'Buttons',
		                 options = list(pageLength = mm,
		                                dom = 'Bt', 
		                                buttons = 'copy')) %>% 
			formatRound(colnames(Lamda), digits=3) %>% 
			formatStyle('Eigenvalue', fontWeight = styleInterval(1, c('normal', 'bold'))) %>% 
			formatStyle('Cum.Eign', fontWeight = styleInterval(ssmc, c('bold','normal')))
	})
	
	output$ssmc <- renderText({ 
		my_data <- selectedData()
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
	output$spgk <- renderPlot({
		my_data <- selectedData()
		my_data <- my_data[, input$MV]
		ev <- eigen(cor(my_data))
		ev <- ev$values
		mm <- ncol(my_data)
		j <- c(1:mm)
		evdf <- cbind.data.frame(j, ev)
		ggplot(data=evdf, aes(x=j, y=ev))+
	  geom_point(color="#D04C44", size=4)+
	  geom_line(color="#323232", size=1)+
		geom_abline(intercept = 1, slope = 0, color = "blue") + 
		labs(x ="Principal Components", y ="Eigenvalue", title= "") +
		scale_x_continuous(breaks=min(evdf$j):max(evdf$j)) +
		  theme_minimal()
	})
	output$A <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                scrollX = TRUE, 
		                                scrollY = 650,
		                                scroller = TRUE)) %>%
			formatRound(colnames(A), digits=3) %>% 
		  formatStyle(fnames[1:input$k], color = styleInterval(c(critical.a2, 0, 0, critical.a1), c('red', 'black', 'black', 'black','red')))
	})
	output$FF <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                scrollX = TRUE, 
		                                scrollY = 650,
		                                scroller = TRUE)) %>%
			formatRound(colnames(FF), digits=3) %>% 
		  formatStyle(fnames[1:input$k], color = styleInterval(c(critical.f2, 0, 0, critical.f1), c('red', 'black', 'black', 'black','red')))
	})
	output$M <- renderDataTable({
		my_data <- selectedData()
		my_data <- my_data[, input$MV]
		mm <- ncol(my_data)
		fnames <- paste0("F", 1:mm)
		fit <- principal(my_data, nfactors=input$k, rotate = input$method2)
		M <- cor(fit$scores)
		colnames(M) <- fnames[1:input$k]
		rownames(M) <- fnames[1:input$k]
		M %>% 
		  DT::datatable (rownames = TRUE,
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                scrollX = TRUE, 
		                                scrollY = 650,
		                                scroller = TRUE)) %>%
			formatRound(colnames(M), digits=3)
	})
	output$GFF1 <- renderPlot({
		my_data <- selectedData()
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
		ggplot(data=FF1, aes(y = Var, x = Loa1, fill = Factors, color = Factors)) +
		geom_bar(alpha = input$opa, position ="stack",stat="identity") +
		labs(x ="Loadings", y ="Variables") +
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$GFF2 <- renderPlot({
		my_data <- selectedData()
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
		ggplot(data=FF1, aes(y=Fac, x=Loa1, fill = Variables, color = Variables)) +
		geom_bar(alpha = input$opa, position ="stack",stat="identity") +
		labs(x ="Loadings", y ="Factors") +
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$Scores <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                scrollX = TRUE, 
		                                scrollY = 650,
		                                scroller = TRUE)) %>%
			formatRound(colnames(FS), digits=3)
	})
	
	# Canonical Correlation Analysis #
	output$RC <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons'),
		                 options = list(pageLength = kk,
		                                dom = 'Bt', 
		                                buttons = 'copy')) %>% 
			formatStyle("p-level", color = styleInterval(c(0.05), c('red', 'black')))
	})
	output$F1 <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons'),
		                 options = list(pageLength = m1,
		                                dom = 'Bt', 
		                                buttons = 'copy'))
	})
	output$F2 <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons'),
		                 options = list(pageLength = m2,
		                                dom = 'Bt', 
		                                buttons = 'copy'))
	})
	output$CF1 <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                scrollX = TRUE, 
		                                scrollY = 650,
		                                scroller = TRUE)) 
	})
	output$CF2 <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                scrollX = TRUE, 
		                                scrollY = 650,
		                                scroller = TRUE)) 
	})
	output$GFFF1 <- renderPlot({
		my_data <- selectedData()
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
		ggplot(data=CFF1, aes(x = Fac1, y = Loa1, fill = Variables, color = Variables)) +
		geom_bar(alpha = input$opa, position = "stack", stat = "identity") +
		labs(x ="Canonical factors", y ="Loadings") +
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$GFFF2 <- renderPlot({
		my_data <- selectedData()
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
		ggplot(data=CFF2, aes(x = Fac2, y = Loa2, fill = Variables, color = Variables)) +
		geom_bar(alpha = input$opa, position ="stack",stat="identity") +
		labs(x ="Canonical factors", y ="Loadings")+
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	
	# Discriminant Analysis #
	output$df <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons'),
		                 options = list(pageLength = k,
		                                dom = 'Bt', 
		                                buttons = 'copy')) %>% 
		  formatStyle("p-level", color = styleInterval(c(0.05), c('red', 'black')))
	})
	output$sdf <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons'),
		                 options = list(pageLength = m,
		                                dom = 'Bt', 
		                                buttons = 'copy'))
	})
	output$cg <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons'),
		                 options = list(pageLength = k+1,
		                                dom = 'Bt', 
		                                buttons = 'copy'))
	})
	output$gsdf <- renderPlot({
		my_data <- selectedData()
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
		ggplot(data=gsdf, aes(x = Fac, y = L, fill = Variables, color = Variables)) +
		geom_bar(alpha = input$opa, position = "stack", stat = "identity") +
		labs(x = "Discriminant Function", y = "Loadings") +
		  theme_minimal() +
		  scale_fill_hue(c=input$sat, l=input$lum) +
		  scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$dfs <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                scrollX = TRUE, 
		                                scrollY = 630,
		                                scroller = TRUE)) 
	})
	output$clas1 <- renderDataTable({
		my_data <- selectedData()
		Independent <- my_data[, input$DAI]
		Dependent    <- my_data[, input$DAD]
		X <- as.matrix(Dependent)
		Y <- as.matrix(Independent)
		var_disc <- lda(Independent ~ X, my_data)
		predict <- predict(var_disc, Independent)
		predict <- predict$class
		ctf <- CrossTable(predict, Independent)
		ctf <- as.data.frame.matrix(ctf$t)
		ctf$Total = colSums(ctf[,])
		ctf %>% 
		  DT::datatable (rownames = TRUE,
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons'),
		                 options = list(pageLength = 100,
		                                dom = 'Bt', 
		                                buttons = 'copy'))
	})
	output$clas2 <- renderDataTable({
		my_data <- selectedData()
		Independent <- my_data[, input$DAI]
		Dependent    <- my_data[, input$DAD]
		X <- as.matrix(Dependent)
		Y <- as.matrix(Independent)
		var_disc <- lda(Independent ~ X, my_data)
		predict <- predict(var_disc, Independent)
		predict <- predict$class
		ctfp <- CrossTable(predict, Independent)
		ctfp <- round(as.data.frame.matrix(ctfp$prop.col)*100, digits = 2)
		ctfp$Total = colSums(ctfp[,])
		ctfp %>%
		  DT::datatable (rownames = TRUE,
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons'),
		                 options = list(pageLength = 100,
		                                dom = 'Bt', 
		                                buttons = 'copy'))
	})
	
	#Cluster Analysis
	output$dend <- renderPlot({
	  my_data <- selectedData()
	  my_data <- my_data[, input$CLV]
	  z_data <- scale(my_data)

	  dend <- my_data %>% scale %>% 
	    dist(method = input$dist) %>%
	    hclust (method = input$clmethod) %>%
	    as.dendrogram %>% 
	    set("branches_k_color", k=input$nk) %>% 
	    set("branches_lwd", .7) %>%
	    set("labels_cex", .8) %>% 
	    set("labels_colors", k=input$nk) 
	  ggd <- as.ggdend(dend)
	  ggplot(ggd, horiz = FALSE) +
	    theme_minimal() +
	    scale_fill_hue(c=input$sat, l=input$lum) +
	    scale_colour_hue(c=input$sat, l=input$lum)
	})
	output$clus <- renderDataTable({
	  my_data <- selectedData()
	  my_data <- my_data[, input$CLV]
	  my_data <- scale(my_data)
	  n <- nrow(my_data)
	  d <- dist(my_data, method = input$dist)
	  hc <- hclust(d, method = input$clmethod)
	  Groups <- cutree(hc, k=input$nk)
	  Groups <- data.frame(Groups)
	  Groups %>% 
	    DT::datatable (rownames = TRUE,
	                   style = 'bootstrap', 
	                   class = 'compact',
	                   extensions = c('Buttons','Scroller'),
	                   options = list(dom = 'Bt', 
	                                  buttons = 'copy', 
	                                  scrollX = TRUE, 
	                                  scrollY = 690,
	                                  scroller = TRUE))
	})
	
	# Reliability Analysis #
	output$alpha <- renderText({
	  my_data <- selectedData()
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
	
	output$irs <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons'),
		                 options = list(pageLength = m,
		                                dom = 'Bt', 
		                                buttons = 'copy')) %>% 
			formatRound(colnames(irs), digits=3)
	})
	output$cd <- renderDataTable({
		my_data <- selectedData()
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
		                 style = 'bootstrap', 
		                 class = 'compact',
		                 extensions = c('Buttons','Scroller'),
		                 options = list(dom = 'Bt', 
		                                buttons = 'copy', 
		                                scrollX = TRUE, 
		                                scrollY = 640,
		                                scroller = TRUE)) %>% 
			formatRound(colnames(cd), digits=3)
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
	  xv <- seq(-4,4, 0.01)*input$sd1 + input$as1
	  pxv <- dnorm(xv, input$as1, input$sd1)
	  dxi <- dnorm(input$rez1, mean=input$as1, sd=input$sd1)
	  pxi <- pnorm(input$rez1, mean=input$as1, sd=input$sd1, lower.tail = F)
	  dfnd <- cbind.data.frame(xv,pxv)
	    fn <- function(xv) {
	      y <- dnorm(xv, mean = input$as1, sd = input$sd1)
	      y[xv < input$rez1 | xv > (max(xv))] <- NA
	      return(y)
	    }
	    
	  ggplot(data=dfnd, aes(x=xv, y=pxv)) +
	    geom_line(color="#323232", size=1)+
	    geom_vline(xintercept = input$as1, linetype = "dashed", alpha = 0.4) +
	    stat_function(fun=fn, geom="area", fill="#D04C44", alpha=0.5) +
	    xlab("x value") +
	    ylab("p(x)") +
	    theme_minimal()
	})
	
	# T - Distribution #
	output$tp <- renderText({
	  tp2 <- qt(p=input$pt/2, df = input$dft, lower.tail = F)
	  tp <- qt(p=input$pt, df = input$dft, lower.tail = F)
	  paste("Two tailed t-value = ", round(tp2, digits = 3), br(),
	        "Upper tailed t-value = ", round(tp, digits = 3)
	        )
	})
	output$pt <- renderText({
	  pt <- pt(input$xt, df = input$dft, lower.tail = F)
	  paste("Two tailed p-value = ", round(pt, digits = 3)*2, br(),
	        "Upper tailed p-value = ", round(pt, digits = 3)
	        )
	})

	output$graftd <- renderPlot({
	  xt <- seq(-5,5, 0.01)
	  pxt <- dt(xt, input$dft)
	  dftd <- cbind.data.frame(xt,pxt)
	  tpl <- qt(p=input$pt/2, df = input$dft, lower.tail = T)
	  tpd <- qt(p=input$pt/2, df = input$dft, lower.tail = F)
	  ftl <- function(xt) {
	    ytl <- dt(xt, input$dft)
	    ytl[xt > tpl] <- NA
	    return(ytl)
	  }
	  ftd <- function(xt) {
	    ytd <- dt(xt, input$dft)
	    ytd[xt < tpd] <- NA
	    return(ytd)
	  }
	  ggplot(data=dftd, aes(x=xt, y=pxt)) +
	    geom_line(color="#323232", size=1)+
	    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
	    stat_function(fun=ftl, geom="area", fill="#D04C44", alpha=0.5) +
	    stat_function(fun=ftd, geom="area", fill="#D04C44", alpha=0.5) +
	    xlab("t value") +
	    ylab("p(t)") +
	    geom_text(x = tpl, y= -0.005, label =  paste0(round(tpl, digits = 2)), color="#ffffff") +
	    geom_text(x = tpd, y= -0.005, label =  paste0(round(tpd, digits = 2)), color="#ffffff") +
	    theme_minimal()
	})
	  output$graftd2 <- renderPlot({
	    xt2 <- seq(-5,5, 0.01)
	    pxt2 <- dt(xt2, input$dft)
	    dftd2 <- cbind.data.frame(xt2,pxt2)
	    tp2 <- qt(p=input$pt, df = input$dft, lower.tail = F)
	    ft <- function(xt2) {
	      yt <- dt(xt2, input$dft)
	      yt[xt2 < tp2] <- NA
	      return(yt)
	    }
	    ggplot(data=dftd2, aes(x=xt2, y=pxt2)) +
	      geom_line(color="#323232", size=1)+
	      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
	      stat_function(fun=ft, geom="area", fill="#D04C44", alpha=0.5) +
	      xlab("t value") +
	      ylab("p(t)") +
	      geom_text(x = tp2, y= -0.005, label =  paste0(round(tp2, digits = 2)), color="#ffffff") +
	      theme_minimal()
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
	    xf <- seq(0, 6, 0.01)
	    pxf <- df(xf, input$df1, input$df2)
	    dff <- cbind.data.frame(xf,pxf)
	    fp <- qf(p=input$pf, df1 = input$df1, df2 = input$df2, lower.tail = F)
	    ff <- function(xf) {
	      yf <- df(xf, input$df1, input$df2)
	      yf[xf < fp] <- NA
	      return(yf)
	    }
	    ggplot(data=dff, aes(x=xf, y=pxf)) +
	      geom_line(color="#323232", size=1) +
	      stat_function(fun=ff, geom="area", fill="#D04C44", alpha=0.5) +
	      xlab("F value") +
	      ylab("p(F)") +
	      geom_text(x = fp, y= -0.01, label =  paste0(round(fp, digits = 2)), color="#ffffff") +
	      theme_minimal()
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
	    xh <- seq(0, 100, 0.1)
	    pxh <- dchisq(xh, input$dfh)
	    dfh <- cbind.data.frame(xh,pxh)
	    hp <- qchisq(p=input$ph, df = input$dfh, lower.tail = F)
	    fh <- function(xh) {
	      yh <- dchisq(xh, input$dfh)
	      yh[xh < hp] <- NA
	      return(yh)
	    }
	    ggplot(data=dfh, aes(x=xh, y=pxh)) +
	      geom_line(color="#323232", size=1) +
	      stat_function(fun=fh, geom="area", fill="#D04C44", alpha=0.5) +
	      xlab("Chi-Square value") +
	      ylab("p(Chi-Square)") +
	      geom_text(x = hp+1, y= -0.001, label =  paste0(round(hp, digits = 2)), color="#ffffff") +
	      theme_minimal()
	  })
	  
# Graph #
	  # Bar #
	  output$gbar1 <- renderPlot({
	    my_data <- selectedData()
	    Group <- my_data[,input$GBAR1]
	    ggplot(my_data, aes(x=Group, fill=Group, color=Group)) + 
	      geom_bar (alpha = input$opa) +
	      labs(x= input$GBAR1, y = "Frequency") +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	  output$gbar2 <- renderPlot({
	    my_data <- selectedData()
	    var1 <- my_data[,input$GBAR1]
	    Group <- my_data[,input$GBAR2]
	    ggplot(my_data, aes(x=var1, fill=Group, color = Group)) + 
	      geom_bar (alpha = input$opa) +
	      labs(x= input$GBAR1, y = "Frequency") +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	  # Histogram #
	  output$ghist1 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[,input$GX]
	    ggplot(my_data, aes(x=X)) + 
	      geom_histogram(bins=input$gnb, alpha = input$opa, fill="#D04C44", color="#323232") +
	      labs(x= input$GX, y = "Frequency") +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	  # Histogram by group #
	  output$ghist2 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[, input$GX]
	    Group <- my_data[, input$GG]
	    ggplot(my_data, aes(x=X, fill = Group)) + 
	      geom_histogram(bins=input$gnb, alpha = input$opa,  color = "#323232") +
	      labs(x= input$GX, y = "Frequency") +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	  # Histogram by facets #
	  output$ghist3 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[, input$GX]
	    Group <- my_data[, input$GG]
	    ggplot(my_data, aes(x=X, fill = Group)) + 
	      geom_histogram(bins=input$gnb, alpha = input$opa, color = "#323232") +
	      labs(x= input$GX, y = "Frequency") +
	      facet_wrap(Group) +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	  # Density #
	  output$gdens1 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[, input$GX1]
	    ggplot(my_data, aes(x=X)) + 
	      geom_density(fill="#D04C44", color="#323232", alpha = input$opa, adjust = 1.2) +
	      geom_vline(aes(xintercept=mean(X)), colour="blue",  linetype="dashed", size=.1) +
	      labs(x= input$GX1, y = "Frequency") +
	      theme_minimal()
	  })
	  output$gdens2 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[, input$GX1]
	    Group <- my_data[, input$GG1]
	    df <- data.frame(Group, X)
	    mu <- ddply(df, "Group", summarise, grp.mean=mean(X))
	    ggplot(my_data, aes(x=X, fill = Group, colour=Group)) + 
	      geom_density(alpha = input$opa, adjust = 1.2) +
	      geom_vline(data=mu, aes(xintercept=grp.mean, color= Group), linetype="dashed", size=.1) +
	      labs(x= input$GX1, y = "Frequency") +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	  output$gdens3 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[, input$GX1]
	    Group <- my_data[, input$GG1]
	    ggplot(my_data, aes(x=X, fill = Group, colour=Group)) + 
	      geom_density(alpha = input$opa, adjust = 1.2) +
	      labs(x= input$GX1, y = "Frequency") +
	      facet_wrap(Group) +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	  # Box $ Whisker
	  output$gbox1 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[,input$GX2]
	    ggplot(my_data, aes(x=X)) +
	      geom_boxplot(alpha = input$opa, fill="#D04C44", color="#323232", outlier.colour="blue", outlier.shape=8,  outlier.size=2) +
	      labs(x = input$GX2, y = "p(x)") +
	      theme_minimal()
	  })
	  output$gbox2 <- renderPlot({
	    my_data <- selectedData()
	    Variables <- my_data[, input$GX2]
	    Groups    <- my_data[, input$GG2]
	    ggplot(my_data, aes(x = Variables, y = Groups, fill=Groups)) +
	      geom_boxplot(width = .1, alpha = input$opa, outlier.colour="blue", outlier.shape=8,  outlier.size=2, color="#323232") +
	      labs(x = input$GX2, y = input$GG) +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	  
	  # Scatter
	  output$gscatt1 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[, input$GX3]
	    Y <- my_data[, input$GY3]
	    ggplot(my_data, aes(x=X, y=Y)) +
	      geom_point(alpha = input$opa, size=2, colour = "#D04C44") +
	      geom_smooth(method = input$gmethod, se=F, colour = "#323232", size=1, alpha = .1) +
	      labs(x= input$GX3, y = input$GY3) +
	      theme_minimal()
	  })
	  output$gscatt2 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[, input$GX3]
	    Y <- my_data[, input$GY3]
	    Group <- my_data[, input$GG3]
	    ggplot(my_data, aes(x=X, y=Y, color=Group)) +
	      geom_point(alpha = input$opa, size=2) +
	      geom_smooth(method = input$gmethod, se=F, size=1, alpha = .1) +
	      labs(x= input$GX3, y = input$GY3) +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
	  output$gscatt3 <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[, input$GX3]
	    Y <- my_data[, input$GY3]
	    Group    <- my_data[, input$GG3]
	    ggplot(my_data, aes(x=X, y=Y, color=Group)) +
	      geom_point(alpha = input$opa, size=2) +
	      geom_smooth(method = input$gmethod, se=F, colour = "#323232", size=1, alpha = .1) +
	      labs(x= input$GX3, y = input$GY3) +
	      facet_wrap(Group) +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	  })
# Bland-Altman  
	  output$gba <- renderPlot({
	    my_data <- selectedData()
	    X <- my_data[, input$GX4]
	    Y <- my_data[, input$GY4]
	    df <- cbind.data.frame(X, Y)
	    df$avg <- rowMeans(df)
	    df$diff <- df$X - df$Y
	    mean_diff <- mean(df$diff)
	    lower <- mean_diff - 1.96*sd(df$diff)
	    upper <- mean_diff + 1.96*sd(df$diff)
	    
	    ggplot(df, aes(x = avg, y = diff)) +
	      geom_point(alpha = input$opa, size=2, colour = "#D04C44") +
	      geom_hline(yintercept = mean_diff, color = "black") +
	      geom_hline(yintercept = lower, color = "blue", linetype="dashed") +
	      geom_hline(yintercept = upper, color = "blue", linetype="dashed") +
	      ylab("Difference Between Measurements") +
	      xlab("Average Measurement") +
	      theme_minimal()
	  })
	  output$gbaci <- renderText({
	    my_data <- selectedData()
	    X <- my_data[, input$GX4]
	    Y <- my_data[, input$GY4]
	    df <- cbind.data.frame(X, Y)
	    df$avg <- rowMeans(df)
	    df$diff <- df$X - df$Y
	    mean_diff <- mean(df$diff)
	    lower <- mean_diff - 1.96*sd(df$diff)
	    upper <- mean_diff + 1.96*sd(df$diff)
	    paste("Average Difference:", round(mean_diff, digits = 3), "</br>",
	          "Lower 95% confidence interval limits = ", round(lower, digits = 3), "</br>",
	          "Upper 95% confidence interval limits = ", round(upper, digits = 3))
	    
	  })
	 # Setting Graph
	  output$graph <- renderPlot({
	    df <- data.frame(Group=c("G1", "G2", "G3"),Frequency=c(5, 10, 8))
	    ggplot(data=df, aes(x=Group, y=Frequency, color=Group, fill=Group)) +
	      geom_bar(alpha = input$opa, stat="identity") +
	      theme_minimal() +
	      scale_fill_hue(c=input$sat, l=input$lum) +
	      scale_colour_hue(c=input$sat, l=input$lum)
	 
	  })
}
shinyApp(ui, server)