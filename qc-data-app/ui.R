#ui.R

library(shiny)
library(shinycssloaders)

shinyUI(fluidPage(
	titlePanel("Media Filler QC"),
	sidebarLayout(
		sidebarPanel(
			numericInput("fill", label = h3("Fill Volume (uL)"), value = 200),
			numericInput("err", label = h3("Error Tolerance (uL)"), value = 5),
			textInput("mdlN", label = h3("MDL Number")),
			p("Database connection isn't super fast. Once MDL is entered, hang tight for a minutes..."),
			textOutput("plateWarning")
		),
		mainPanel(
			tabsetPanel(id = 'tabs',
				tabPanel("summary",     #blank cols are just for formatting
					fluidRow(column(2),column(10,h3(withSpinner(textOutput("pltSummary"))))), 
					fluidRow(column(2),column(10,h3(withSpinner(textOutput("colSummary"))))),
					fluidRow(column(2),column(10,h3(withSpinner(textOutput("rowSummary")))))
				),
				tabPanel("plates",
					fluidRow(column(2,uiOutput("pltSet")),column(10,withSpinner(plotOutput("plotPlate")))),
					fluidRow(column(12,dataTableOutput("plate_table")))
				),
				tabPanel("columns",
					fluidRow(column(2,uiOutput("colSet")),column(10,withSpinner(plotOutput("plotCol")))),
					fluidRow(column(12,dataTableOutput("col_table")))
				),
				tabPanel("rows",
					fluidRow(column(2,uiOutput("rowSet")),column(10,withSpinner(plotOutput("plotRow")))),
					fluidRow(column(12,dataTableOutput("row_table")))
				)
			)
		)
	)

))
