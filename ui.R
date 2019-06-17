library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  #includeCSS("style 4.css"),
  
  # Application title
  titlePanel(strong("Manufacturing Cost Estimation"),windowTitle = "Manufacturing Cost Estimation"),

  
  tabsetPanel(
  #navbarPage(
    tabPanel("Load Data",fluidPage(
      titlePanel(h4("Upload File")),
      sidebarLayout(
        sidebarPanel(width=2,
          fileInput('file1', 'Choose CSV File',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv')),
          tags$hr(),
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',
                       c(Comma=',',
                         Semicolon=';',
                         Tab='\t'),
                       ','),
          radioButtons('quote', 'Quote',
                       c(None='',
                         'Double Quote'='"',
                         'Single Quote'="'"),
                       '"'),
          textInput('var_type','Enter Variable Type'),
          helpText(
            p("Enter the type of variable for each attribute with a comma between values"),
            p("1: Continuous | Interval Scaled"),
            p("2: Continuous | Ratio Scaled"),
            p("3: Categorical | Nominal"),
            p("4: Categorical | Ordinal"),
            p("5: Categorical | Symmetric Binary"),
            p("6: Categorical | Asymmetric Binary"),
            p("0: Dependent Variable")
          )
        ),
        mainPanel(
          verticalLayout(textOutput("title_upload"),br(),tableOutput('contents'))
        )
      )
    )),
    tabPanel("CLU",fluidPage(
      titlePanel(h4("Clustering Cost Estimation")),
      sidebarLayout(
        sidebarPanel(width=2,splitLayout(
          numericInput('k_min',"Min k",2),
          numericInput('k_max','Max k',30)),
          numericInput('red_dot','Red Dot',0),hr(),
          numericInput('k_best','Best k',0),
          radioButtons('mce1_degree', 'Regression Degree',
                       c('Linear'=1,
                         'Quadratic'=2,'Other'=3),
                       1),
          numericInput('mce1_poly_degree',"Polynomial Degree (If Other)",3)),
        mainPanel(
          verticalLayout(textOutput("title1_mce1"),plotOutput("distPlot"),br(),textOutput("title2_mce1"),plotOutput("actual_predicted1"),br(),textOutput("title3_mce1"),br(),tableOutput("cluster_result"))
        )
      )
    )),
    tabPanel("SPL",fluidPage(
      titlePanel(h4("Spline Cost Estimation")),
      sidebarLayout(
        sidebarPanel(width=2,
          splitLayout(
          numericInput('degree_min',"Degree Min",0),
          numericInput('degree_max','Degree Max',10)),
          splitLayout(
          numericInput('segment_min',"Segments Min",1),
          numericInput('segment_max','Segments Max',10)),
          
          radioButtons('complexity', 'Complexity',
                       c('Degree and Knots'="degree-knots",
                         'Degree Only'="degree",
                         'Knots Only'="knots"),
                       "degree-knots"),
          textInput('knots_vector','Knots Vector (If Degree Only)'),
          textInput('degree_vector','Degree Vector (If Knots Only)'),
          radioButtons('knots', 'Knot Placement',
                       c('Auto'="auto",
                         'Quantiles'="quantiles",
                         'Uniform'="uniform"),
                       "auto"),
          radioButtons('basis', 'Spline Basis',
                       c('Auto'="auto",
                         'Tensor'="tensor",
                         'Additive'="additive"),
                       "tensor"),
          radioButtons('cv', 'Optimization',
                       c('NOMAD'="nomad",
                         'Exhaustive'="exhaustive"),
                       "nomad"),
          radioButtons('cv.func', 'Cross-Validation',
                       c('Least Squares'="cv.ls",
                         'Generalized CV'="cv.gcv",
                         'AIC'="cv.aic"),
                       "cv.ls")
          ),
        mainPanel(
          verticalLayout(textOutput("title1_mce2"),plotOutput("actual_predicted2"),br(),textOutput("title2_mce2"),br(),tableOutput("spline_result"))
        )
      )
    )),
    tabPanel("REG",fluidPage(
      titlePanel(h4("Single Regression Model")),
      sidebarLayout(
        sidebarPanel(width=2,
          radioButtons('mce3_degree', 'Regression Degree',
                       c('Linear'=1,
                         'Quadratic'=2,
                         'Other'=3),
                       1),
          numericInput('mce3_poly_degree',"Polynomial Degree (If Other)",3)),
        mainPanel(
          verticalLayout(textOutput("title1_mce3"),plotOutput("actual_predicted3"),br(),textOutput("title2_mce3"),br(),tableOutput("nocluster_result"))
        )
      )
    ))
  )
  )
)
