#ui.R

library(shiny)

shinyUI(fluidPage(
	titlePanel("Media Filler QC"),
	sidebarLayout(
		sidebarPanel(
			uiOutput("mdlSet"),
			numericInput("fill", label = h3("Fill Volume (uL)"), value = 235),
			numericInput("err", label = h3("Error Tolerance (uL)"), value = 5),
			fileInput("dataFile", label = h3("Data File Input")),
			textOutput("plateWarning"),
			textOutput("fileInfo")
		),
		mainPanel(
			tabsetPanel(id = 'tabs',
				tabPanel("plates",
					fluidRow(column(2,uiOutput("pltSet")),column(10,plotOutput("plotPlate"))),
					fluidRow(column(12,dataTableOutput("plate_table")))
				),
				tabPanel("columns",
					fluidRow(column(2,uiOutput("colSet")),column(10,plotOutput("plotCol"))),
					fluidRow(column(12,dataTableOutput("col_table")))
				),
				tabPanel("rows",
					fluidRow(column(2,uiOutput("rowSet")),column(10,plotOutput("plotRow"))),
					fluidRow(column(12,dataTableOutput("row_table")))
				)
			)
		)
	)

))
