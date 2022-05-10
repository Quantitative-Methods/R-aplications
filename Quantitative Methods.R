########################
# Quantitative Methods #
########################

library(shinydashboard)
library(xlsx)
library(DT)
library(psych)
library(gmodels)
library(nortest)
library(tigerstats)
library(car)
library(Rmisc)
library(gplots)
library(lsr)
library(data.table)
library(GPArotation)
library(yacca)
library(candisc)
library(MASS)
library(corrplot)
library(corrr)
library(pwr)
library(dendextend)
library(rstatix)
library(emmeans)
library(ggdist)
library(afex)
library(ggthemes)
library(ggplot2)

ui <- dashboardPage(
		dashboardHeader(title = "Quantitative Methods", titleWidth = 260, disable = FALSE),
			dashboardSidebar(disable = FALSE,   width = 260,
				sidebarMenu(
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
					menuItem("Power Analysis", icon = icon("stats", lib = "glyphicon"),
						menuSubItem("Power Analysis T-test", tabName = "PAT"),
						menuSubItem("Power Analysis ANOVA", tabName = "PAA"),
						menuSubItem("Power Analysis Coreelation", tabName = "PAC")
					),
					menuItem("Probability Calculator", icon = icon("stats", lib = "glyphicon"),
					         menuSubItem("Normal Distribution", tabName = "PCN"),
					         menuSubItem("T - Distribution", tabName = "PCT"),
					         menuSubItem("F - Distribution", tabName = "PCF"),
					         menuSubItem("Chi Square - Distribution", tabName = "PCH")
          ),
					menuItem("About", tabName = "about", icon = icon("info-sign", lib = "glyphicon"))
				)
			),
		dashboardBody(includeCSS("style.css"),
      tabItems(   
        # 0 Data			
        tabItem(tabName = "data",
                fluidRow(
                  box(title = "Napomena!",
                      status = "primary",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      collapsed = TRUE,
                      width = 12, 
                      p("Podaci za statisticko-graficku obradu trebaju biti pripremljeni na sljedeci nacin:"),
                      p("- prvi stupac tablice s podacima sadrzi nazive (oznake) entiteta"),
                      p("- ostali stupci tablice s podacima sadrze podatke entiteta u varijablama"),
                      p("- oznake (kodovi) modaliteta kvalitativnih varijabli ne smiju biti brojevi"),
                      p("- za podatke kreirane u CSV (Comma Separated Values) 
                        formatu  kao separator izmedu podataka koristiti tocka-zarez (;), a za decimalne brojeve zarez (,).")
                  ),
                  box(title = "Choose Data File",
                      width = 2,
                      status = "primary",
                      radioButtons("format",
                                   "Data Type",
                                   choices = c(
                                     .csv = "csv",
                                     .xlsx = "xls"
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
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12,
							p("Opcija Data Transformation omogucava transformaciju originalnih podatka (podatka dobivenih mjerenjem) u:"),
							p("- Z - vrijednosti z = (x-as)/sd"),
							p("- T - vrijednosti T = 50 + z * 10"),
							p("- L - vrijednosti (skala skolskih ocjena) L =  3 + z * 0,83."),
						), 
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
							),
						),
						tabBox(title = "",
							width = 10,
							tabPanel(h4("Z - value"), 
							dataTableOutput("zz")
							),
							tabPanel(h4("T - value"),
							dataTableOutput("tt")
							),
							tabPanel(h4("L - value (1-5)"),
							dataTableOutput("ll")
							)
						)
					)
				),
				# 2. Frequency Tables      
				tabItem(tabName = "FT",
					fluidRow(
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12, 
							p("Grupiranje podataka je statisticki postupak razvrstavanja entiteta s 
							istim oblikom obiljezja u odreden broj disjunktnih podskupova. 
							Rezultat grupiranja su frekvencije, 
							odnosno broj entiteta u odredenoj grupi (klasi, kategoriji, razredu). 
							Odabirom opcije Frequency Tables prikazuje se tablica i 
							grafikon s rezultatima jednodimenzionalnog grupiranja 
							odabrane varijable te rezultati hi-kvadrat testa (Chi-square test) 
							za utvrdivanje statisticke znacajnosti razlika izmedu opazenih i 
							teoretskih frekvencija dobivenih uniformnom distribucijom."),
							p("Tablica za svaku grupu entiteta prikazuje:"),
							p("- Frequency - apsolutne frekvencije"),
							p("- Percent - relativne postotne frekvencije"),
							p("- Cumulative - kumulativne frekvencije"),
							p("- Cumulative Percent - relativne postotne kumulativne frekvencije.")
						),  
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							radioButtons(
								"FT", 
								label = "",
								choices = "", 
								selected =""
							),
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
						       tabPanel(h4("Pie Chart"), br(),
						                plotOutput("pie")
						       )
						    )
					  )
				),
				# 3. Contingency Tables       
				tabItem(tabName = "CT",
					fluidRow(
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12,
							p("Odabirom opcije Contingency Tables prikazuje se 
							tablica i grafikon s rezultatima dvodimenzionalnog 
							grupiranja odabranih varijabli te rezultati hi-kvadrat testa 
							(Chi-square test) za utvrdivanje statisticke znacajnosti 
							razlika izmedu opazenih i teoretskih frekvencija dviju kvalitativnih varijabli."),
						),
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
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
						       tabPanel(h4("Bar Chart"), br(),
						                plotOutput("graf2") , br(),
						                plotOutput("graf3")
						            )
						       )
					     )
				  ),
				# 4. Grouping Continuous Data      
				tabItem(tabName = "GCD",
				        fluidRow(
				          box(title = "Napomena!",
				              status = "primary",
				              solidHeader = TRUE,
				              collapsible = TRUE,
				              collapsed = TRUE,
				              width = 12, 
				              p("Odabirom opcije Grouping Continuous Data dobije se tablica koja za svaki razred prikazuje:"),
				              p("- Frequency - apsolutne frekvencije"),
				              p("- Percent - relativne postotne frekvencije"),
				              p("- Cumulative - kumulativne frekvencije"),
				              p("- Cumulative Percent - relativne postotne kumulativne frekvencije.")
				          ),
				          box(title = "Variables",
				              width = 2, #height = 946,
				              status = "primary",
				              #solidHeader = TRUE,
				              #collapsible = TRUE,
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
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12,
							p("Deskriptivni pokazatelji koriste se za opis varijabli, 
							a dijele se na mjere centralne tendencije ili sredisnje mjere, 
							mjere varijabilnosti ili disperzije te mjere oblika distribucije."),
							p(" - MEAN - aritmeticka sredina"),
							p(" - MEDIAN - medijan"),
							p(" - RANGE  - totalni raspon"),
							p(" - SD - Standardna devijacija"),
							p(" - CV - Koeficijent varijabilnosti"),
							p(" - SKEW - Skewness je koeficijent asimetrije distribucije"),
							p(" - KURT  - Kurtosis je stupanj spljostenosti odnosno izduzenosti distribucije.")
						),
						box(title = "Variables",
							width = 2,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							checkboxGroupInput("DP", 
          								label = "",
          								choices = "", selected =""),
							 radioButtons("DP2", 
					                 label = "",
					                 choices = "", selected =""),
						),
						tabBox(title = "",
						       width = 6,
						       tabPanel(h4("Descirptive Parameters"), 
						                dataTableOutput("des")
						       ),
						       tabPanel(h4("Box and Whiskers plot"), 
						                plotOutput("boxds")
						       )
						   )
					 )
			 ),
				# 5. Testing for Normality       
				tabItem(tabName = "TN",
					fluidRow(
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12,
							p("Normalitet distribucija varijabli tj. slicnost 
							empirijskih distribucija s normalnom ili Gaussovom 
							distribucijom je uvjet za koristenje mnogih statistickih metoda. 
							Velicina odstupanja empirijske distribucije od normalne ili Gaussove 
							distribucije moze se testirati statistickim postupcima kao sto su:"),
							p(" - Kolmogorov-Smirnov test"),
							p(" - Lilliefors test "),
							p(" - Anderson-Darling test"),
							p(" - Shapiro-Francia test "),
							p(" - Shapiro-Wilksov W test."),
						),
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
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
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12,
							p("Za odabranu varijablu (Select Variable) aplikacija racuna aritmeticku sredinu 
							(Sample Mean) i standardnu devijaciju (Sample Standard Deviation) uzorka te 
							zavisno o velicini uzorka racuna standardnu pogresku aritmeticke sredine (Standard Error od Mean) 
							na temelju koje racuna interval u kojem se nalazi aritmeticka sredina populacije (Population Mean) 
							zaviso o pogresci statistickog zakljucka (p-value)."),
						),
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
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
							),
						),
						tabBox(title = "",
						       width = 6,
						       tabPanel(h4("CI for the Population Mean"), 
						                htmlOutput("ci"),
						                plotOutput("asp")
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
						box(title = "Napomena!",
							 status = "primary",
							 solidHeader = TRUE,
							 collapsible = TRUE,
							 collapsed = TRUE,
							 width = 12, 
							 p("Pretpostavke:"),
							 p(" - Varijable cine podaci dobiveni pomocu intervalne ili omjerne mjerne ljestvice 
							   (npr. tjelesna visina, tjelesna tezina, skok u dalj s mjesta, taping rukom i sl.)."),
							 p(" - Podaci su dobiveni na slucajno odabranim i relativno velikim uzorcima entiteta (n>30)."),
							 p(" - Podaci u varijablama ne bi trebali imati znacajne outliere, odnosno ekstremno visoke ili niske rezultate."),
							 p(" - Varijable su u linearnom odnosu."),
							 p(" - Varijable imaju normalnu ili Gaussovu distribuciju."),
							 p("Ukoliko podaci ispunjavaju navedene pretpostavke za izracunavanje povezanosti izmedu dviju varijabli 
							   koristi se Pearsonov koeficijent korelacije. Ako su varijable mjerene na ordinalnoj, intervalnoj ili omjeranoj mjernoj ljestvici, 
							   koje nisu normalno distribuirane te se ne nalaze u linearnom, vec monotonom odnosu tada se za utvrdivanje povezanosti izmedu dviju varijabli 
							   koristi Spearmanov ili Kendallov koeficijent korelacije."),
						),
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
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
							),
							radioButtons("cmet", "Method:",
								choices = c(
											color = "color",
											circle = "circle",
											square = "square",
											ellipse = "ellipse"
											),
								selected = "color"
							),
							radioButtons("insig", "Insig:",
								choices = c(
											pvalue = "p-value",
											pch = "pch",
											blank = "blank"
											),
								selected = "p-value"
							),
							sliderInput("pv",
										"p-value:",
										step = 0.01,
										min = 0,
										max = 1,
										value = 0.05
							),
							sliderInput("ccl",
										"Correlation Coefficient Level:",
										min = 0,
										max = 1,
										value = 0.35
							),
						 ),
						tabBox(title = "",
						       width = 8,
						       tabPanel(h4("Correlations"),
						                p("Marked correlations are significant at p < 0.05"),
						                dataTableOutput("r")
						       ),
						       tabPanel(h4("Level of Significance"), 
						                plotOutput("cplot")
						       ),
						       tabPanel(h4("Correlations Network"), 
						                plotOutput("corPlot")
						       )
						   )
					  )
				),
				# 9.  Independent Samples T-Test 
				tabItem(tabName = "ISTT",
					fluidRow(
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12, 
							p("Pretpostavke:"),
							p(" - Nezavisnu varijablu cine podaci prikupljeni na dihotomnoj (s dva modaliteta) nominalnoj 
							ili ordinalnoj mjernoj ljestvici  (npr. muskarci-zene, uspjesni-neuspjesni, 
							pobjednici-porazeni, eksperimentalna grupa-kontrolna grupa i sl.)."),
							p(" - Zavisnu varijablu cine podaci prikupljeni na intervalnoj ili omjernoj mjernoj skali 
							(npr. tjelesna visina, tjelesna tezina, skok u dalj s mjesta, taping rukom i sl.).."),
							p(" - Podaci su prikupljeni na slucajno odabranim i relativno velikim uzorcima entiteta (n>30)."),
							p(" - Podaci u zavisnoj varijabli imaju normalnu ili Gaussovu distribuciju. 
							Ukoliko ova pretpostavka nije ispunjena onda se umjesto t-testa za nezavisne uzorke 
							moze koristiti neparametrijski Mann-Whitney U test koji ne zahtjeva normalnost 
							distribucije podataka u zavisnoj varijabli."),
							p(" - Podaci u zavisnoj varijabli dviju grupa entiteta imaju homogene varijance. 
							Ukoliko ova pretpostavka nije ispunjena onda se racuna korigirana Welchova t-vrijednost."),
							p("- Podaci u zavisnoj varijabli ne bi trebali imati znacajne outliere, odnosno ekstremno visoke ili niske rezultate."),
							p("U tablici Descirptive Parameters & Normality Test nalaze se sljedeci rezultati:"),
							p("- MEAN - aritmeticka sredina"),
							p("- SD - standardna devijacija"),
							p("- SEM - standardna pogreska aritmeticke sredine"),
							p("- -CI95% - donja granica intervala u kojem se s vjerojatnoscu 95% nalazi aritmeticka sredina populacije"),
							p("- +CI95% - goranja granica intervala u kojem se s vjerojatnoscu 95% nalazi aritmeticka sredina populacije"),
							p("- S-W - W vrijednost Shapiro-Wilkovog testa za procjenu normaliteta distribucije"),
							p("- P - pogreska s kojom tvrdimo da se empirijska distribucije statisticki znacajno razlikuju od normalne ili Gaussove. "),
							p("Ako je P < 0,05 zakljucujemo da empirijska distribucija statisticki znacajno odstupa od normalne ili Gaussove distribucije uz pogresku P 
							te da bi umjesto t - testa za nezavisne uzorke bilo primjerenije koristiti Mann-Whitney U test 
							koji ne zahtjeva normalnost distribucije podataka u zavisnoj varijabli. Pored toga, aplikacija omogucava prikazivanje podataka putem histograma frekvencija (Histogram) i 
							kutijastog dijagrama (Box and Whiskers Plot) za svaku grupa entiteta."),
							p("U prozoru Independent Samples T-Test prikazuju se sljedeci rezultati za svaku odabranu zavisnu varijablu:"),
							p("- Levene's F - Levenova F vrijednost temeljem koje se utvrduje homogenost varijanci, 
							odnosno da li su varijance analiziranih grupa statisticki znacajno razlicite."),
							p("- Leven's p - pogreska s kojom tvrdimo da su varijance analiziranih grupa statisticki znacajno razlicite. Ako je p < 0,05 ona zakljucujemo da varijance analiziranih grupa nisu homogene, odnosno da se statsticki znacajno razlikuju uz pogresku manju od 5%. 
							U tom slucaju se umjesto Studentove t - vrijednosti, racuna Welchova t - vrijednost."),
							p("- Student's t -Studentova  t- vrijednost koja pokazuje koliko je puta razlika izmedu aritmetickih sredina 
							veca od standardne pogreske razlika."),
							p("- df - broj stupnjava slobode (n - 2, gdje n broj entiteta  prve i druge grupe)"),
							p("-	p - pogreska s kojom tvrdimo da je razlika izmedu aritmetickih sredina prve i druge grupe statisticki znacajna."),
							p("- Cohen's d - Cohenova mjera velicine ucinka (effect size)."),
							p("- Mean Plot with 95% Confidence Interval - graficki prikaz vrijednosti aritmetickih sredina te donje i gornje granice intervala u kome se nalaze aritmeticke sredine populacija 
							analiziranih grupa entiteta uz pogresku do 5%.")
						),
						box(title = "Variables",
						    width = 2, #height = 946,
						    status = "primary",
						    #solidHeader = TRUE,
						    #collapsible = TRUE,
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
						    ),
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
						                plotOutput("boxt")
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
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12, 
							p("Pretpostavke:"),
							p(" - Nezavisnu varijablu cine upareni (zavisni) podaci (redosljed entiteta prvog i drugog mjerenja mora biti isti) 
							na nominalnoj ljestvici  (npr. prvo mjerenje - drugo mjerenje, prije tretmana - poslje tretmana i sl.)."),
							p(" - Zavisnu varijablu cine podaci prikupljeni na intervalnoj ili omjernoj mjernoj skali 
							(npr. tjelesna visina, tjelesna tezina, skok u dalj s mjesta, taping rukom i sl.).."),
							p(" - Podaci su prikupljeni na slucajno odabranim i relativno velikim uzorcima entiteta (n>30)."),
							p(" - Podaci u varijabli razlika prvog i drugog mjerenja u zavisnoj varijabli imaju normalnu ili Gaussovu distribuciju. 
							Ukoliko ova pretpostavka nije ispunjena onda se umjesto t-testa za zavisne uzorke moze koristiti 
							neparametrijski Wilcoxonov test ekvivalentnih parova (Wilcoxon Matched Pairs Test) 
							koji ne zahtjeva normalnost distribucije podataka u zavisnoj varijabli. "),
							p("- Podaci u zavisnoj varijabli ne bi trebali imati znacajne outliere, odnosno ekstremno visoke ili niske rezultate."),
							p("U tablici Descirptive Parameters & Normality Test nalaze se sljedeci rezultati:"),
							p("- MEAN - aritmeticka sredina"),
							p("- SD - standardna devijacija"),
							p("- SEM - standardna pogreska aritmeticke sredine"),
							p("- -CI95% - donja granica intervala u kojem se s vjerojatnoscu 95% nalazi aritmeticka sredina populacije"),
							p("- +CI95% - goranja granica intervala u kojem se s vjerojatnoscu 95% nalazi aritmeticka sredina populacije"),
							p("te rezultati Shapiro-Wilkovog W test kojim se utvrduje da li se varijabla razlika prvog i drugom mjerenja 
							statisticki znacajno razlikuje od normalne ili Gaussove distribucije:"),
							p("- W vrijednost Shapiro-Wilkovog testa za procjenu normaliteta distribucije"),
							p("- p - pogreska s kojom tvrdimo da se varijabla razlika prvog i drugog mjerenja statisticki znacajno razlikuju od normalne ili Gaussove."),
							p("Ako je p < 0,05 zakljucujemo da varijabla razlika prvog i drugog mjerenja statisticki znacajno odstupa od normalne ili Gaussove distribucije
							uz pogresku p te da bi umjesto t - testa za zavisne uzorke bilo primjerenije koristiti 
							Wilcoxonov test ekvivalentnih parova (Wilcoxon Matched Pairs Test). Pored toga, aplikacija omogucava prikazivanje podataka putem histograma frekvencija (Histogram) i 
							kutijastog dijagrama (Box and Whiskers Plot) za prvo i drugo mjerenje."),
							p("U prozoru Dependent Samples T-Test prikazuju se sljedeci rezultati za svaku odabranu zavisnu varijablu:"),
							p("- Mean difference - razlika izmedu aritmetickih sredina prvog i drugog mjerenja."),
							p("- St. error difference - standardna pogreska razlika prvog i drugom mjerenja."),
							p("- Correlations - korelacija izmedu prvog i drugog mjerenja."),
							p("- Student's t -Studentova  t- vrijednost koja pokazuje koliko je puta razlika izmedu aritmetickih sredina veca od standardne pogreske razlika."),
							p("-	df - broj stupnjava slobode (n - 1, gdje n broj entiteta)."),
							p("- p - pogreska s kojom tvrdimo da je razlika izmedu aritmetickih sredina prvog i drugom mjerenja statisticki znacajna."),
							p("- Cohen's d - Cohenova mjera velicine ucinka (effect size)."),
							p("- Mean Plot with 95% Confidence Interval - graficki prikaz vrijednosti aritmetickih sredina te donje i gornje granice intervala 
							u kome se nalaze aritmeticke sredine populacija prvog i drugom mjerenja uz pogresku do 5%.")
						),
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
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
							),
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
						                plotOutput("boxt2")
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
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12, 
							p("Pretpostavke:"),
							p(" - Nezavisnu varijablu cine podaci prikupljeni na nominalnoj ili ordinalnoj mjernoj ljestvici 
							s dva ili vise modaliteta (npr. muskarci-zene, bek-krilo-centar i sl.)."),
							p(" - Zavisnu varijablu cine podaci prikupljeni na intervalnoj ili omjernoj mjernoj skali 
							   (npr. tjelesna visina, tjelesna tezina, skok u dalj s mjesta, taping rukom i sl.)."),
							p(" - Podaci su prikupljeni na slucajno odabranim i relativno velikim uzorcima entiteta (n>30)."),
							p(" - Podaci u zavisnoj varijabli imaju normalnu ili Gaussovu distribuciju. Ukoliko ova pretpostavka nije ispunjena onda se umjesto 
							One - Way ANOVA moze koristiti neparametrijski Kruskal-Wallis test koji ne zahtjeva normalnost distribucije podataka u zavisnoj varijabli."),
							p(" - Podaci u zavisnoj varijabli dviju grupa entiteta imaju homogene varijance. 
							Ukoliko ova pretpostavka nije ispunjena onda se racuna korigirana Welchova F-vrijednost."),
							p(" - Podaci u zavisnoj varijabli ne bi trebali imati znacajne outliere, odnosno ekstremno visoke ili niske rezultate."),
							p("U tablici Descirptive Parameters & Normality Test nalaze se sljedeci rezultati:"),
							p("- MEAN - aritmeticka sredina"),
							p("- SD - standardna devijacija"),
							p("- SEM - standardna pogreska aritmeticke sredine"),
							p("- -CI95% - donja granica intervala u kojem se s vjerojatnoscu 95% nalazi aritmeticka sredina populacije"),
							p("- +CI95% - goranja granica intervala u kojem se s vjerojatnoscu 95% nalazi aritmeticka sredina populacije"),
							p(" - S-W - W vrijednost Shapiro-Wilkovog testa za procjenu normaliteta distribucije"),
							p(" - P - pogreska s kojom tvrdimo da se empirijska distribucija statisticki znacajno razlikuju od normalne ili Gaussove."),
							p("Ako je P < 0,05 zakljucujemo da empirijska distribucija statisticki znacajno odstupa od normalne ili Gaussove distribucije uz pogresku P te da bi umjesto 
							One - Way ANOVA bilo primjerenije koristiti Kruskal - Wallisov test. 
							Pored toga, aplikacija omogucava prikazivanje podataka putem histograma frekvencija (Histogram) i 
							kutijastog dijagrama (Box and Whiskers Plot) za analizirane grupe."),
							p("U prozoru One-Way ANOVA prikazuju se sljedeci rezultati za svaku odabranu zavisnu varijablu:"),
							p(" - Levene's F - Levenova F vrijednost temeljem koje se utvrduje homogenost varijanci, odnosno da li su varijance analiziranih grupa statisticki znacajno razlicite."),
							p(" - Leven's p - pogreska s kojom tvrdimo da su varijance analiziranih grupa statisticki znacajno razlicite. Ako je p < 0,05 ona zakljucujemo da varijance analiziranih 
							grupa nisu homogene, odnosno da se statsticki znacajno razlikuju uz pogresku manju od 5%. 
							U tom slucaju se umjesto Fisherove F - vrijednosti, racuna Welchova F - vrijednost."),
							p(" - Fisher's F - vrijednost koja pokazuje omjer varijance izmedu i unutar grupa."),
							p(" - df1 i df2  - broj stupnjava slobode (df1 = k - 1, df2 = n - k, gdje n broj entiteta, k - broj grupa)."),
							p(" - p - pogreska s kojom tvrdimo da je razlika izmedu aritmetickih sredina analiziranih grupe statisticki znacajna."),
							p(" - Post Hoc Test for Multiple Comparisons (Tukey) - tablica rezultata Tukeyovog post hoc testa za visestruko usporedivanje."),
							p(" - Mean Plot with 95% Confidence Interval - graficki prikaz vrijednosti aritmetickih sredina te donje i gornje granice intervala
							u kome se nalaze aritmeticke sredine populacija analiziranih grupa uz pogresku do 5%.")
						),
						box(title = "Variables",
							width = 2, 
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
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
						                h4 ("Mean Plot with 95% Confidence Interval"),
						                plotOutput("erra")
						       ),
						       tabPanel(h4("ANOVA (one var.)"), 
						                dataTableOutput("anovat"),
						                hr(),
						                htmlOutput("anova"),
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
				          box(title = "Napomena!",
				              status = "primary",
				              solidHeader = TRUE,
				              collapsible = TRUE,
				              collapsed = TRUE,
				              width = 12, 
				              p("Pretpostavke:"),
				              ),
				          box(title = "Variables",
				              width = 2, #height = 946,
				              status = "primary",
				              #solidHeader = TRUE,
				              #collapsible = TRUE,
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
				                          hr(),
				                          h4 ("Mean Plot with 95% Confidence Interval"),
				                          plotOutput("errrma")
				                 ),
				                 tabPanel(h4("ANOVA"), 
				                          dataTableOutput("aovrm"), 
				                          hr(),
				                          htmlOutput("rma2"),
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
				          box(title = "Napomena!",
				              status = "primary",
				              solidHeader = TRUE,
				              collapsible = TRUE,
				              collapsed = TRUE,
				              width = 12, 
				              p("Pretpostavke:"),
				              p(" - Varijable cine podaci dobiveni pomocu intervalne ili omjerne mjerne ljestvice 
							(npr. tjelesna visina, tjelesna tezina, skok u dalj s mjesta, taping rukom i sl.)."),
				              p(" - Podaci u varijablama ne bi trebali imati znacajne outliere, odnosno ekstremno visoke ili niske rezultate."),
				              p(" - Varijable su u linearnom odnosu."),
				              p(" - Varijable imaju normalnu ili Gaussovu distribuciju."),
				              p(" - Rezidualne vrijednosti su medusobno nezavisne (autokorelacija prvog reda nije statisticki znacajna) te 
							su nazvisne u odnosno na zavisnu i nezavisne varijabe. Ova pretpostavka se provjerava pomocu Durbin-Watsonovog testa."),
				              p(" -	Varijanca rezidualnih vrijednosti konstantna je za sve vrijednosti u zavisnoj varijabli (homoscedasticnost)."),
				              p(" - Rezidualne vrijednosti imaju normalnu ili Gaussovu distribuciju."),
				          ),
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
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12, 
							p("Pretpostavke:"),
							p(" - Varijable cine podaci dobiveni pomocu intervalne ili omjerne mjerne ljestvice 
							(npr. tjelesna visina, tjelesna tezina, skok u dalj s mjesta, taping rukom i sl.)."),
							p(" - Podaci su dobiveni na slucajno odabranim i relativno velikim uzorcima entiteta (n>30)."),
							p(" - Podaci u varijablama ne bi trebali imati znacajne outliere, odnosno ekstremno visoke ili niske rezultate."),
							p(" - Varijable su u linearnom odnosu."),
							p(" - Varijable imaju normalnu ili Gaussovu distribuciju."),
							p(" - Rezidualne vrijednosti su medusobno nezavisne (autokorelacija prvog reda nije statisticki znacajna) te 
							su nazvisne u odnosno na zavisnu i nezavisne varijabe. Ova pretpostavka se provjerava pomocu Durbin-Watsonovog testa."),
							p(" -	Varijanca rezidualnih vrijednosti konstantna je za sve vrijednosti u zavisnoj varijabli (homoscedasticnost)."),
							p(" - Rezidualne vrijednosti imaju normalnu ili Gaussovu distribuciju."),
							p("U prozoru Regression Results prikazuju se sljedeci pokazatelji za odabranu zavisnu i nezavisne varijable:"),
							p(" - RO - koeficijent multiple korelacije"),
							p(" - RO2 - koeficijent determinacije multiple korelacije"),
							p(" - SEE - standardna pogreska prognoze"),
							p(" - F-value - F-vrijednost kojom se izracunava pri testiranju statisticke znacajnosti multiple korelacije"),
							p(" - p-value - pogreska kojom tvrdimo da je multipla korelacija statisticki znacajna"),
							p("te tablica u kojoj su prikazani sljedeci pokazatelji:"),
							p(" - B - regresijski koeficijenti"),
							p(" - SE(B) - standardne pogreske regresijskih koeficijenata"),
							p(" - Beta - standardizirani regresijski koeficijenti"),
							p(" - Part R - koeficijenti parcijalne korelacije"),
							p(" - R - koeficijenti korelacije"),
							p(" - P - parcijalni koeficijenti determinacije (relativni udio svake prediktorske 
							varijable u objasnjenom varijabilitetu kriterijske varijable)"),
							p(" - Tolerance - neobjasnjeni dio varijance svake prediktorske varijable u odnosu na ostale (kolicina nezavisnih informacija)"),
							p(" - t-value - t-vrijednosti koje se izracunavaju pri testiranju statisticke znacajnosti regresijskih koeficijenata"),
							p(" - p-value - pogreske s kojima tvrdimo da su regresijski koeficijenti statisticki znacajni."),
							p("Pored toga, prikazani su i grafikoni parcijalnih koeficijenata determinacije (Graph of Partial Coefficients of Determinations (P)) i 
							standardiziranih regresijskih koeficijenata, parcijalnih korelacija i 
							korelacija (Standardized Regression Coefficients, Partial Correlations and Correlations)."),
							p("U prozoru Observed, Predicted & Residual Values prikazuje se tablica s izmjerenim, 
							prognoziranim i rezidualnim vrijednostima zavisne varijable.")
						),
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							checkboxGroupInput("PRED", 
										label = "",
										choices = "", selected =""),
							radioButtons("KRIT", 
										label = "",
										choices = "", selected ="")
						),
						tabBox(title = "",
						       width = 6,
						       tabPanel(h4("Regression Results"), 
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
						       tabPanel(h4("Regression Scores"), 
						          dataTableOutput("dep")
						       )
						    )
					   )
				),
				# 14.  Factor Analysis
				tabItem(tabName = "FA",
					fluidRow(
						box(title = "Napomena!",
						status = "primary",
						solidHeader = TRUE,
						collapsible = TRUE,
						collapsed = TRUE,
						width = 12, 
						p("Pretpostavke:"),
						p(" - Varijable cine podaci dobiveni pomocu intervalne ili 
						omjerne mjerne ljestvice (npr. tjelesna visina, tjelesna tezina, skok u dalj s mjesta, taping rukom i sl.)."),
						p(" - Podaci su dobiveni na slucajno odabranim i relativno velikim uzorcima entiteta (5 od 10 puta vise entiteta od broja varijabli)."),
						p(" - Podaci u varijablama ne bi trebali imati znacajne outliere, odnosno ekstremno visoke ili niske rezultate."),
						p(" - Varijable su u linearnom odnosu."),
						p(" - Varijable imaju normalnu ili Gaussovu distribuciju."),
						p("U kartica Eigenvalue prozora Factor Results prikazuju se sljedece rezultati za odabrane manifestne varijable:"),
						p(" - Eigenvalue - svojstvene vrijednosti, odnosno varijance glavnih komponenata"),
						p(" - Cum. Eign. - kumulativne svojstvene vrijednosti"),
						p(" - Percentage - relativne postotne svojstvene vrijednosti"),
						p(" - Cum. Per. - kumulativne relativne postotne svojstvene vrijednosti."),
						p(" - Sum of Squares Multiple Correlation (SSMC) - suma kvadrata multiplih korelacija svake manifestne varijable u odnosu na preostale."),
						p(" - Number of Common Principal Components - broj znacajnih glavnih komonenata koji se dobije primjenom:"),
						p("   - GK-Criterion (Guttman-Kiser) - Guttman-Kaiserovog kriterija prema kojem su znacajne sve glavne komponente 
						cija je varijanca odnosno svojstvena vrijednost veca ili jednaka 1."),
						p("   - PB-Criterion (Stalec-Momirovic) - PB - kriterija prama kojem je broj znacajnih glavnih
						jednak broju svojstvenih vrijednosti poredanih po velicini ciji zbroj ne prelazi SSMC (sumu kvadrata multiplih korelacija)."),
						p("   -	Scree Plot -  Na scree plotu se subjektivnom procjenom odredi tocka nakon koje se svojstvene vrijednosti smanjuju 
						u skladu s blagim linearnim trendom. Znacajnima se smatraju sve prethodne glavne komponente."),
						p("U kartica Factor Loadings prozora Factor Results prikazuju se sljedece rezultati za odabrane manifestne varijable:"),
						p(" - Pattern Matrix - matrica sklopa, odnosno paralelnih projekcija manifestnih varijabli na faktore te Hoffmanov index kompleksiteta (Complexity)."),
						p(" - Structure Matrix - matrica strukture, odnosno ortogonalnih projekcija (korelacija) manifestnih varijabli na faktore te komunaliteti (Communality)."),
						p(" - Factor Correlation Matrix - matrica korelacija izmedu faktora."),
						p("U prozoru Factor Scores nalazi se tablica s rezultatima entiteta u faktorima.")
						),
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
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
									obimin = "oblimin",
									promax = "promax"
								),
								selected = "none"
							),
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
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12, 
							p("Pretpostavke:"),
							p(" - Varijable cine podaci dobiveni pomocu intervalne ili omjerne mjerne ljestvice (npr. tjelesna visina, tjelesna tezina, skok u dalj s mjesta, taping rukom i sl.)."),
							p(" - Podaci su dobiveni na slucajno odabranim i relativno velikim uzorcima entiteta (5 od 10 puta vise entiteta od broja varijabli)."),
							p(" - Podaci u varijablama ne bi trebali imati znacajne outliere, odnosno ekstremno visoke ili niske rezultate."),
							p(" - Varijable su u linearnom odnosu."),
							p(" - Varijable imaju normalnu ili Gaussovu distribuciju."),
							p("U prozoru Canonical Analysis Results prikazuju se sljedece rezultati za odabrane varijable prvog i drugog skupa:"),
							p(" - Rc - koeficijenti kanonickih korelacija."),
							p(" - Chi-sq. - vrijednosti Bartlettovog ???2 - testa za testiranje statisticke znacajnosti kanonickih korelacija."),
							p(" - df - stupnjevi slobode"),
							p(" - p - pogreske s kojima tvrdimo da su koeficijenti kanonicke korelacije statisticki znacajni. "),
							p(" - Factor Structure-First Set - tablica faktorske strukture prvog skupa kanonickih faktora, odnosno korelacija manifestnih varijabli prvog skupa s kanonickim faktorima prvog skupa."),
							p(" - Factor Structure-Second Set - tablica faktorske strukture drugog skupa kanonickih faktora, odnosno korelacija manifestnih varijabli drugog skupa s kanonickim faktorima drugog skupa."),
							p("U karticama Canonical Scores - 1. Set i Canonical Scores - 2. Set nalaze se tablice s rezultatima entiteta u kanonickim faktorima prvog i drugug skupa.")
						),
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							checkboxGroupInput("CA1", 
							   "Select Variables:",
							   choices = "", 
							   selected =""
							),
							checkboxGroupInput("CA2", 
								"Select Variables:",
								choices = "", 
								selected =""
							),
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
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12, 
							p("Pretpostavke:"),
							p(" - Nezavisnu varijablu cine podaci prikupljeni na nominalnoj ili ordinalnoj mjernoj ljestvici  s dva ili vise modaliteta (npr. muskarci-zene, bek-krilo-centar i sl.)."),
							p(" - Zavisnu varijablu cine podaci prikupljeni na intervalnoj ili omjernoj mjernoj skali (npr. tjelesna visina, tjelesna tezina, skok u dalj s mjesta, taping rukom i sl.)."),
							p(" - Podaci su dobiveni na slucajno odabranim i relativno velikim uzorcima entiteta podjednake velicine (3 od 5 puta vise entiteta u svakoj grupi od broja varijabli)."),
							p(" - Podaci u varijablama ne bi trebali imati znacajne outliere, odnosno ekstremno visoke ili niske rezultate"),
							p(" - Varijable su u linearnom odnosu."),
							p(" - Varijable imaju normalnu ili Gaussovu distribuciju."),
							p("U prozoru Discriminant Analysis Results prikazuju se sljedece rezultati za odabrane zavisne i nezavisnu varijablu:"),
							p(" - Eigenvalue - svojstvene vrijednosti, odnosno varijance diskriminacijskih funkcija."),
							p(" - Canonical R - koeficijenti kanonicke korelacije (diskriminacije), odnosno korelacija diskirminacijskih funkcija s nezavisnom (selektorskom) varijablom."),
							p(" - Wilks' Lambda - Wilksove lambde (krecu u intervalu od 0 do 1, a sto im je vrijednost manja to je veca vjerojatnost da je razlika izmedu analiziranih grupa statisticki znacajna."),
							p(" - aprox. F - aproksimativna F - vrijednost temeljem koje se utvrduje statisticka znacajnost diskriminacijskih funkcija. "),
							p(" - df1 i df2 - broj stupnjeva slobode."),
							p(" - p-level - pogreska s kojima tvrdimo da pojedina diskriminacijska funkcija staisticki znacajno razlikuje analizirane grupe."),
							p(" - Structure Discriminant Functions - tablica strukture diskriminacijski funkcija, odnosno korelacija zavisnih varijabli i diskriminacijskih funkcija."),
							p(" - Group Centroids - tablica vrijednosti centroida, osnosno vektora aritmetickih sredina analiziranih grupa u diskriminacijskim funcijama."),
							p(" - Clasifications Matrix - tablice koje pokazuju broj i postotak ispravno i neispravno klasificiranih entiteta u odredenu grupu na temelju diskriminacijskih funkcija."),
							p("U prozoru Discriminant Functions Scores nalazi se tablica s rezultatima entiteta u diskriminacijskim funkcijama.")
						),
						box(title = "Variables",
							width = 2,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							checkboxGroupInput("DAD", "Select Variables:",
							   choices = "", 
							   selected =""
							),
							radioButtons("DAI", 
								label = "Independent Variables:",
								choices = "", 
								selected =""
							),
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
				          box(title = "Napomena!",
				              status = "primary",
				              solidHeader = TRUE,
				              collapsible = TRUE,
				              collapsed = TRUE,
				              width = 12, 
				              p("Pretpostavke:")
				          ),
				          box(title = "Variables",
				              width = 2, #height = 946,
				              status = "primary",
				              #solidHeader = TRUE,
				              #collapsible = TRUE,
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
				              #solidHeader = TRUE,
				              #collapsible = TRUE,
				              dataTableOutput("clus")
				          ),
				          box(title = "Cluster dendrogram",
				              width = 8,
				              status = "primary",
				              #solidHeader = TRUE,
				              #collapsible = TRUE,
				              plotOutput("dend")
				          )
				      )
				),
				# 18.  Reliability Analysis
				tabItem(tabName = "REA",
					fluidRow(
						box(title = "Napomena!",
							status = "primary",
							solidHeader = TRUE,
							collapsible = TRUE,
							collapsed = TRUE,
							width = 12, 
							p("U prozoru Reliability Analysis Results prikazuju se sljedece rezultati:"),
							p(" - Cronbach's alpha - Cronbachov koeficijent pouzdanosti"),
							p(" - Spearman-Brown alpha - Sperman-Brownov koeficijent pouzdanosti "),
							p(" - Kaiser-Caffrey alpha - Kaiser-Caffreyev koeficijent pouzdanosti"),
							p(" - Average interitem correlation - prosjecna korelacija izmedu cestica testa"),
							p("U tablici Item Reliability Statistics nalaze se sljedeci rezultati:"),
							p(" - Mean - aritmeticke sredine cestica testa"),
							p(" - St.dev. - standardne devijacije cestica testa"),
							p(" - Item-total correlation - korilacije cestice s jednostavnom linearnom kombinacijom preostalih"),
							p(" - Alpha if deleted - Cronbachov koeficijent pouzdanosti izracunat bez pripadajuce cestice."),
							p("U prozoru Condensed Data nalaze se kondenzirani rezultati entiteta u testu izracunati kao:"),
							p(" - X-mean - aritmeticka sredina originalnih rezultata"),
							p(" - Z-mean - aritmeticka sredina rezultata transformiranih u z-vrijednosti "),
							p(" - PC1 - prva glavna komponenta.")
						),
						box(title = "Variables",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							checkboxGroupInput("REA", 
							   "Select Variables:",
							   choices = "", 
							   selected =""
							),
						),
						box(title = "Reliability Analysis Results",
							width = 5, 
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							htmlOutput("alpha"),
							hr(),
							h5("Item Reliability Statistics"),
							dataTableOutput("irs")
						),
						box(title = "Condensed Data",
							width = 3, 
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							dataTableOutput("cd")
						)
					)
				),
				# 19.  Power Analysis t - test
				tabItem(tabName = "PAT",
					fluidRow(
						box(title = "Input",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							radioButtons("type", "Type t-test:",
								choices = c(two.sample = "two.sample",
										   paired = "paired",
										   one.sample = "one.sample"
								),
								selected = "two.sample"
							),
							radioButtons("alternative", "Alternative:",
								choices = c(
								two.sided = "two.sided",
								greater = "greater"
								),
								selected = "two.sided"
							),
							sliderInput("sig.level","Sig.level:",
								step = 0.01,
								min = 0,
								max = 1,
								value = 0.05
							),
							sliderInput("power","Power:",
								step = 0.1,
								min = 0,
								max = 1,
								value = 0.8
							),
							sliderInput("d","Cohen's d:",
								step = 0.1,
								min = 0,
								max = 1,
								value = 0.2
							),
							p("Conventional effect size:"),
							p(" - small 0.20"),
							p(" - medium 0.50"),
							p(" - large 0.80")
						),
						box(title = "Power  Analysis  Results",
							width = 5, 
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							plotOutput("pwrt"),
						)
					)
				),
				# 20.  Power Analysis ANOVA
				tabItem(tabName = "PAA",
					fluidRow(
						box(title = "Input",
							width = 2, #height = 946,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							sliderInput("kk","Number of groups:",
								step = 1,
								min = 2,
								max = 10,
								value = 2
							),
							sliderInput("sig.level2","Sig.level:",
								step = 0.01,
								min = 0,
								max = 1,
								value = 0.05
							),
							sliderInput("power2","Power:",
								step = 0.1,
								min = 0,
								max = 1,
								value = 0.8
							),
							sliderInput("f","Effect Size:",
								step = 0.1,
								min = 0,
								max = 1,
								value = 0.1
							),
							p("Conventional effect size:"),
							p(" - small 0.10"),
							p(" - medium 0.25"),
							p(" - large 0.40")
						),
						box(title = "Power  Analysis  Results",
							width = 5, 
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							plotOutput("pwra"),
						)
					)
				),
				# 21.  Power Analysis r
				tabItem(tabName = "PAC",
					fluidRow(
						box(title = "Input",
							width = 2,
							status = "primary",
							#solidHeader = TRUE,
							#collapsible = TRUE,
							sliderInput("sig.level3","Sig.level:",
								step = 0.01,
								min = 0,
								max = 1,
								value = 0.05
							),
							sliderInput("power3","Power:",
								step = 0.01,
								min = 0,
								max = 1,
								value = 0.8
							),
							sliderInput("r","Effect Size:",
								step = 0.1,
								min = 0,
								max = 1,
								value = 0.1
							),
							p("Conventional effect size:"),
							p(" - small 0.10"),
							p(" - medium 0.30"),
							p(" - large 0.50")
						),
						box(title = "Power  Analysis  Results",
						  width = 5, 
						  status = "primary",
						  #solidHeader = TRUE,
						  #collapsible = TRUE,
						  plotOutput("pwrr"),
						)
					)
				),
				# Probability Calculator
				# Normal (Gauss) Distribution
				tabItem(tabName = "PCN",
				    fluidRow(
				        box(title = "Napomena!",
				            status = "primary",
				            solidHeader = TRUE,
				            collapsible = TRUE,
				            collapsed = TRUE,
				            width = 12,
				            p("Kalkulator vjerojatnosti omogucava racunanje postotka boljih i losijih rezultata od rezultata (X) u populaciji entiteta 
							cije obiljezje (varijabla) ima normalnu ili Gaussovu distribuciju s odredenom aritmetickm sredinom (Mean) 
							i standardnom devijacijom (Standard Deviation).")
				          ),
				        box(title = "Input Values",
				            status = "primary",
				            #solidHeader = TRUE,
				            #collapsible = TRUE,
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
				            #solidHeader = TRUE,
				            #collapsible = TRUE,
				            width = 5,
				            plotOutput("grafnd"),
				        )
				    )
				),
				tabItem(tabName = "PCT",
				    fluidRow(
				        box(title = "Napomena!",
				            status = "primary",
				            solidHeader = TRUE,
				            collapsible = TRUE,
				            collapsed = TRUE,
				            width = 12,
				            p("Kalkulator vjerojatnosti za t-distribuciju omogucava racunanje t-vrijednosti za zadanu p-vrijednost (p-value) i broj stupnjeva slobode (df).")
				        ),
				        box(title = "Input Values",
				            status = "primary",
				            #solidHeader = TRUE,
				            #collapsible = TRUE,
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
				            #solidHeader = TRUE,
				            #collapsible = TRUE,
				            width = 5,
				            plotOutput("graftd"),
				        ),
				        box(title = "T - Distribution - Upper tailed",
				            status = "primary",
				            #solidHeader = TRUE,
				            #collapsible = TRUE,
				            width = 5,
				            plotOutput("graftd2"),
				        )
				    )
				),
				tabItem(tabName = "PCF",
				    fluidRow(
				        box(title = "Napomena!",
				            status = "primary",
				            solidHeader = TRUE,
				            collapsible = TRUE,
				            collapsed = TRUE,
				            width = 12,
				            p("Kalkulator vjerojatnosti za F-distribuciju omogucava racunanje F-vrijednosti za zadanu p-vrijednost (p-value) i broj stupnjeva slobode (df1 i df2).")
				          ),
				        box(title = "Input Values",
				            status = "primary",
				            #solidHeader = TRUE,
				            #collapsible = TRUE,
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
				            #solidHeader = TRUE,
				            #collapsible = TRUE,
				            width = 5,
				            plotOutput("graff"),
				        )
				    )
				),
				tabItem(tabName = "PCH",
				    fluidRow(
				        box(title = "Napomena!",
				            status = "primary",
				            solidHeader = TRUE,
				            collapsible = TRUE,
				            collapsed = TRUE,
				            width = 12,
				            p("Kalkulator vjerojatnosti za hi-kvadrat distribuciju omogucava racunanje hi-kvadrat vrijednosti za zadanu p-vrijednost (p-value) i broj stupnjeva slobode (df).")
				        ),
				        box(title = "Input Values",
				            status = "primary",
				            #solidHeader = TRUE,
				            #collapsible = TRUE,
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
				            #solidHeader = TRUE,
				            #collapsible = TRUE,
				            width = 5,
				            plotOutput("grafh"),
				        )
				    )
			   ),
				# 22. About   
				tabItem(tabName = "about",
				    fluidRow(
  						box(title = "About",
  							status = "primary",
  							#solidHeader = TRUE,
  							#collapsible = TRUE,
  							#collapsed = FALSE,
  							width = 5,
  							p(h4("Quantitative Methods")),
  							hr(),
  							p("Version: 1.2"),
  							p("Built on: 1.5.2022."),
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
  					) 
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
                   #sep = input$sep,
                   #dec = input$dec,
                   sep = ";",
                   dec = ",",
                   stringsAsFactors = TRUE,
                   row.names = 1)
        }
          }
        else{
          data.frame(read.xlsx(input$data$datapath, 1, stringsAsFactors = TRUE), row.names=1)
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
							    choices = names(selectedData() %>% select_if(is.numeric)), 
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
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateCheckboxGroupInput(inputId = "DVITT",
			            label = "Select Dependent Variables:",
			            choices = names(selectedData() %>% select_if(is.numeric)), 
			            selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateCheckboxGroupInput(inputId = "DSTDV",
			            label = "Select Dependent Variables:",
			            choices = names(selectedData() %>% select_if(is.numeric)), 
			            selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateCheckboxGroupInput(inputId = "ANOVADV",
			            label = "Select Dependent Variables:",
			            choices = names(selectedData() %>% select_if(is.numeric)), 
			            selected = names(selectedData() %>% select_if(is.numeric))
			)
			updateRadioButtons(inputId = "X",
			                   label = "Select Independent Variable:",
			                   choices = names(selectedData() %>% select_if(is.numeric)), 
			)
			updateRadioButtons(inputId = "Y",
			                   label = "Select Dependent Variable:",
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
		  formatStyle('MEAN_Z', backgroundColor = '#deeaee', fontWeight = 'bold')
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
		  formatStyle('MEAN_T', backgroundColor = '#deeaee', fontWeight = 'bold')
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
		  formatStyle('MEAN_L', backgroundColor = '#deeaee', fontWeight = 'bold')
	})
	# Grouping Categorical Data #
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
	  var <- my_data[,input$FT]
	  ggplot(my_data, aes(x=var, fill=var)) + 
	    geom_bar (alpha=.7) +
	    labs(x= input$FT, y = "Frequency") +
	    theme_minimal() + scale_fill_ptol()
	})
	output$pie <- renderPlot({
	  my_data <- selectedData()
	  var <- my_data[,input$FT]
	  ggplot(data=my_data, aes(x='', fill=var)) +
	    geom_bar(alpha=.7) +
	    coord_polar("y") +
	    labs(x= input$FT, y = "Frequency", fill=input$FT) +
	    theme_minimal() + scale_fill_ptol()
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
		var2 <-  my_data[,input$CT2]
		ggplot(my_data, aes(x=var1, fill = var2)) + 
  		geom_bar (alpha=0.7)+
  		labs(x= input$CT1, fill = input$CT2, y = "Frequency") +
		  theme_minimal() + scale_fill_ptol()
	})
	output$graf3 <- renderPlot({
		my_data <- selectedData()
		var1 <- my_data[,input$CT1]
		var2 <-  my_data[,input$CT2]
		ggplot(my_data, aes(x=var1, fill = var2)) + 
  		geom_bar (alpha=0.7, position="fill")+
  		labs(x = input$CT1, fill = input$CT2, y = "Frequency") +
		  theme_minimal() + scale_fill_ptol()
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
	    geom_bar (alpha=0.7, stat="identity", fill="#4474AC", color="#FFFFFF", width=1)+
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
	    geom_point(color="#4474AC", size=4)+
	    geom_line(color="#4474AC", size=1)+
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
	  vars <- my_data[, input$DP2]
	  ggplot(my_data, aes(x = vars)) +
	    geom_boxplot(width = .1, alpha = .7, fill="#4474AC") +
	    labs(x = input$DP2) +
	    stat_slab(fill="#4474AC", alpha = .7, adjust = 1.5, justification = -.1, .width = 0,  trim = FALSE) +
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
		geom_histogram(bins=input$NB, alpha = .7, color="#FFFFFF", fill="#4474AC") +
		labs(x= input$TN, y = "Frequency") +
		theme_minimal()
	})
	output$box <- renderPlot({
	  my_data <- selectedData()
	  var <- my_data[,input$TN]
	  ggplot(my_data, aes(x = var)) +
	    geom_boxplot(width = .1, alpha = .7, fill="#4474AC") +
	    labs(x = input$TN, y = "Frequency") +
	    stat_slab(fill="#4474AC", alpha = .7, adjust = 1.5, justification = -.1, .width = 0,  trim = FALSE) +
	    theme_minimal() + scale_fill_ptol()
	})
	output$qq <- renderPlot({
		my_data <- selectedData()
		var <- my_data[,input$TN]
		ggplot(my_data, aes(sample = var)) + 
		stat_qq_line(color="#053742", size=1) + 
		stat_qq(color="#4474AC", size=3) + 
		labs(x = input$TN, y=input$TN) +
		theme_minimal() + scale_fill_ptol()
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
	output$asp <- renderPlot({
		my_data <- selectedData()
		xx <- my_data[,input$PM]
		as <- round(mean(xx), digits = 2)
		sd <- round(sd(xx), digits = 2)
		se <- round(sd/sqrt(input$PMn), digits = 2)
		t <- qt(input$PMp/2, input$PMn, lower.tail = F)
		x1 <- round(as-t*se, digits = 2)
		x2 <- round(as+t*se, digits = 2)
		x <- seq(as-3*sd, as+3*sd, length=2000)
		y <- dnorm(x, mean=as, sd=sd)
		plot(x, y, type = "l", lwd = 2, axes = FALSE, xlab = input$PM, ylab = "")
		axis_bounds <- seq(-5*sd+as, 5*sd+as, by = sd)
		axis(side = 1, at = axis_bounds, pos = 0)
		a <- seq(x1,x2,length=2000)
		b <- dnorm(x, mean=as, sd=sd)
		polygon(c(x1, a, x2), c(0, b, 0), col="#4474AC")
	}) 
	output$gsem <- renderPlot({
		my_data <- selectedData()
		xx <- my_data[,input$PM]
		sd <- round(sd(xx), digits = 2)
		ss <- c(1:300)
		se <- round(sd/sqrt(ss), digits = 2)
		sem <-cbind.data.frame(ss,se)
		ggplot(sem, aes(x=ss, y=se)) +
		geom_line(color="#000000", size=1) +
		geom_point(color="#4474AC", size=2) +
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
		RP <- corr.test(vars, method = input$CORM)
		R <- RP$r
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
		  formatRound(colnames(R), digits=2) %>% 
		  formatStyle(var_name_numeric, color = styleInterval(c(critical.r2, 0, 0, critical.r1), c('red', 'black', 'black', 'black','red')))
	})
	output$cplot <- renderPlot({
		my_data <- selectedData()
		vars <- my_data[, input$COR]
		RP <- corr.test(vars, method = input$CORM)
		R <- RP$r
		testR <-  cor.mtest(vars, conf.level = 0.95)
		corrplot(R, p.mat = testR$p, sig.level = input$pv, insig = input$insig, method = input$cmet, diag = T)
	})
	output$corPlot <- renderPlot({
		my_data <- selectedData()
		vars <- my_data[, input$COR]
		vars %>% correlate() %>% network_plot(min_cor=input$ccl)
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
	    geom_boxplot(width = .1) +
	    labs(x = input$IVITT, y = input$ISTD, fill = input$IVITT) +
	    stat_slab(alpha = .7, adjust = 1.2, justification = -.1, .width = 0,  trim = FALSE) +
	    theme_minimal() + scale_fill_ptol()
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
		plotmeans(Variables ~ Factor, 
		data = my_data, 
		col = "#053742",
		barcol ="#4474AC", 
		barwidth = 3,
		connect = TRUE, 
		n.label = FALSE, 
		xlab = input$IVITT, 
		ylab = input$ISTD,
		)
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
	    geom_boxplot(width = .1) +
	    labs(x = input$DSTI, y = input$DSTD, fill = input$DSTI) +
	    stat_slab(alpha = .7, adjust = 1.2, justification = -.1, .width = 0,  trim = FALSE) +
	    theme_minimal() + scale_fill_ptol()
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
		plotmeans(Variables~Factor, 
			data=my_data, 
			col = "#053742",
			barcol ="#4474AC", 
			barwidth = 3,
			connect = TRUE, 
			n.label = FALSE, 
			xlab = input$DSTD,
			ylab = input$DSTI
		)
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
		Factor    <- my_data[, input$ANOVAI]
		ggplot(my_data, aes(x = Variables, y = Factor, fill=Factor)) +
		  geom_boxplot(width = .1) +
		  labs(x = input$ANOVAI, y = input$ANOVAD, fill = input$ANOVAI) +
		  stat_slab(alpha = .7, adjust = 1.2, justification = -.1, .width = 0,  trim = FALSE) +
		  theme_minimal() + scale_fill_ptol()
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
		plotmeans(Variables ~ Factor, 
			data = my_data, 
			col = "#053742",
			barcol ="#4474AC", 
			barwidth = 3,
			connect = TRUE, 
			n.label = FALSE, 
			xlab = input$ANOVAI, 
			ylab = input$ANOVAD,
		)
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
	  Factor    <- my_data[, input$RMAM]
	  ggplot(my_data, aes(x = Variables, y = Factor, fill=Factor)) +
	    geom_boxplot(width = .1) +
	    labs(x = input$ANOVAI, y = input$ANOVAD, fill = input$ANOVAI) +
	    stat_slab(alpha = .7, adjust = 1.2, justification = -.1, .width = 0,  trim = FALSE) +
	    theme_minimal() + scale_fill_ptol()
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
	  plotmeans(Variables ~ Factor, 
	            data = my_data, 
	            col = "#053742",
	            barcol ="#4474AC", 
	            barwidth = 3,
	            connect = TRUE, 
	            n.label = FALSE, 
	            xlab = input$RMAM, 
	            ylab = input$RMADV
	  )
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
	    geom_point(size=2, colour = "#4474AC") +
	    geom_smooth(method = "lm", colour = "#D1495B", size=1, se = FALSE, alpha = .1) +
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
		ggplot(pggdf, aes(x= Variables, y = pgg, fill = Variables)) +
		geom_bar(alpha = .7, stat = "identity", color = "white") +
		labs(x ="Variables", y ="value", fill ="", title= "") +
		theme_minimal() + scale_fill_ptol()
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
		coef <- rep(ncoef, each=m-1)
		var <- rep(Variables, times=3)
		val <- c()
		for (a in 2:4) {val <- c(val,grez[,a])}
		ggrez <- data_frame(coef,var,val)
		ggplot(ggrez, aes(x=var, y = val, fill= coef)) +
		geom_bar(alpha = .7, position="dodge",stat = "identity")+
		labs(x ="Variables", y ="value", fill ="", title= "")+
		theme_minimal() + scale_fill_ptol()
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
		geom_point(color="#4474AC", size=4)+
		geom_line(color="#4474AC", size=1)+
		geom_abline(intercept = 1, slope = 0) +
		labs(x ="Principal Components", y ="Eigenvalue", title= "") +
		scale_x_continuous(breaks=min(evdf$j):max(evdf$j)) +
		theme_minimal() + scale_fill_ptol()
	})
	output$A <- renderDataTable({
		my_data <- selectedData()
		my_data <- my_data[, input$MV]
		mm <- ncol(my_data)
		fnames <- paste0("F", 1:mm)
		fit <- principal(my_data, nfactors=input$k, rotate=input$method2)
		Complexity <- fit$complexity
		A <- fit$loadings
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
			formatRound(colnames(A), digits=3)
	})
	output$FF <- renderDataTable({
		my_data <- selectedData()
		my_data <- my_data[, input$MV]
		mm <- ncol(my_data)
		fnames <- paste0("F", 1:mm)
		fit <- principal(my_data, nfactors=input$k, rotate=input$method2)
		Communality <- fit$communality
		FF <- fit$Structure
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
			formatRound(colnames(FF), digits=3)
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
		Fac <- rep(Faktori, each=mm)
		Var <- rep(var_names, times=input$k)
		Loa1 <- c()
		for (a in 1:input$k) {Loa1 <- c(Loa1,F1[,a])}
		FF1 <- data_frame(Fac,Var,Loa1)
		ggplot(data=FF1, aes(y=Var,x=Loa1,fill=Fac)) +
		geom_bar(alpha = .7, position ="stack",stat="identity") +
		labs(x ="Variables", y ="Loadings", fill ="Factors",title= "") +
		theme_minimal() + scale_fill_ptol()
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
		Var <- rep(var_names, times=input$k)
		Loa1 <- c()
		for (a in 1:input$k) {Loa1 <- c(Loa1,F1[,a])}
		FF1 <- data_frame(Fac,Var,Loa1)
		ggplot(data=FF1, aes(y=Fac,x=Loa1,fill=Var)) +
		geom_bar(alpha = .7, position ="stack",stat="identity") +
		labs(x ="Factors",y ="Loadings",fill ="Variables",title= "") +
		theme_minimal() + scale_fill_ptol()
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
		Var1 <- rep(nvar1, times=kk)
		Loa1 <- c()
		for (a in 1:kk) {Loa1 <- c(Loa1,F1[,a])}
		CFF1 <- data_frame(Fac1,Var1,Loa1)
		ggplot(data=CFF1, aes(x=Fac1,y=Loa1,fill=Var1)) +
		geom_bar(alpha = .7, position ="stack",stat="identity") +
		labs(x ="Canonical factors",y ="Loadings",fill ="Variables",title= "") +
		theme_minimal() + scale_fill_ptol()
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
		Var2 <- rep(nvar2, times=kk)
		Loa2 <- c()
		for (a in 1:kk) {Loa2 <- c(Loa2,F2[,a])}
		CFF2 <- data_frame(Fac2,Var2,Loa2)
		ggplot(data=CFF2, aes(x=Fac2,y=Loa2,fill=Var2)) +
		geom_bar(alpha = .7, position ="stack",stat="identity") +
		labs(x ="Canonical factors",y ="Loadings",fill ="Variables",title= "")+
		theme_minimal() + scale_fill_ptol()
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
		                                buttons = 'copy'))
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
		Var <- rep(Varijable, times=k)
		L <- c()
		for (a in 1:k) {L <- c(L,sdf[,a])}
		gsdf <- data_frame(Fac,Var,L)
		ggplot(data=gsdf, aes(x=Fac, y=L, fill=Var)) +
		geom_bar(alpha = .7, position ="stack",stat="identity") +
		labs(x ="Discriminant Function", y ="Loadings", fill ="Variables", title= "")+
		theme_minimal() + scale_fill_ptol()
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
	  dend <- my_data %>% scale %>% 
                  	    dist(method = input$dist) %>%
                  	    hclust (method = input$clmethod) %>%
                  	    as.dendrogram
	  dend %>% set("branches_k_color", k=input$nk) %>% 
	           set("branches_lwd", 2) %>% plot
	  dend %>% rect.dendrogram(k=input$nk, border = 8, lty = 3, lwd = 1)
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
		                                scrollY = 670,
		                                scroller = TRUE)) %>% 
			formatRound(colnames(cd), digits=3)
	})
	# Power Analysis t-test #
	output$pwrt <- renderPlot({
		pwrt <- pwr.t.test(d = input$d,
			sig.level = input$sig.level,
			type = input$type,
			alternative = input$alternative,
			power = input$power
		)
		plot(pwrt) + theme_minimal()
	})
	# Power Analysis ANOVA #
	output$pwra <- renderPlot({
		pwra <- pwr.anova.test(k = input$kk,
			f = input$f,
			sig.level = input$sig.level2,
			power = input$power2
		)
		plot(pwra) + theme_minimal()
	})
	# Power Analysis r #
		output$pwrr <- renderPlot({
		pwrr <- pwr.r.test(r = input$r,
			sig.level = input$sig.level3,
			power = input$power3
		)
		plot(pwrr) + theme_minimal()
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
	    geom_line(color="#4474AC", size=1.5)+
	    geom_vline(xintercept = input$as1, linetype = "dashed", alpha = 0.4) +
	    stat_function(fun=fn, geom="area", fill="#4474AC", alpha=0.5) +
	    xlab("x value") +
	    ylab("p(x)") +
	    theme_minimal() + scale_fill_ptol()
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
	    geom_line(color="#4474AC", size=1.5)+
	    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
	    stat_function(fun=ftl, geom="area", fill="#4474AC", alpha=0.5) +
	    stat_function(fun=ftd, geom="area", fill="#4474AC", alpha=0.5) +
	    xlab("t value") +
	    ylab("p(t)") +
	    geom_text(x = tpl, y= -0.005, label =  paste0(round(tpl, digits = 2))) +
	    geom_text(x = tpd, y= -0.005, label =  paste0(round(tpd, digits = 2))) +
	    theme_minimal() + scale_fill_ptol()
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
	      geom_line(color="#4474AC", size=1.5)+
	      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
	      stat_function(fun=ft, geom="area", fill="#4474AC", alpha=0.5) +
	      xlab("t value") +
	      ylab("p(t)") +
	      geom_text(x = tp2, y= -0.005, label =  paste0(round(tp2, digits = 2))) +
	      theme_minimal() + scale_fill_ptol()
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
	      geom_line(color="#4474AC", size=1.5) +
	      stat_function(fun=ff, geom="area", fill="#4474AC", alpha=0.5) +
	      xlab("F value") +
	      ylab("p(F)") +
	      geom_text(x = fp, y= -0.01, label =  paste0(round(fp, digits = 2))) +
	      theme_minimal() + scale_fill_ptol()
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
	      geom_line(color="#4474AC", size=1.5) +
	      stat_function(fun=fh, geom="area", fill="#4474AC", alpha=0.5) +
	      xlab("Chi-Square value") +
	      ylab("p(Chi-Square)") +
	      geom_text(x = hp+1, y= -0.001, label =  paste0(round(hp, digits = 2))) +
	      theme_minimal() + scale_fill_ptol()
	  })
}
shinyApp(ui, server)