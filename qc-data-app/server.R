#server.R

library(shiny)
library(plyr)
library(ggplot2)
library(RODBC)

#Helper Functions and Values
default_fill_vol <- 200.0
default_vol_err <- 5.0
trz_slope <- 163.2255   #ESTIMATE! change this
trz_intc <- -6.2383    #ESTIMATE! change this
trz_conc <- 0.04   #g/L
max_cv <- 0.8  #flag if over this value.

#TODO: change MDL header to assay_MDL


CV <- function(data) {
	(sd(data, na.rm=TRUE)/mean(data, na.rm=TRUE))*100
}
std_err <- function(data) {
	sd(data, na.rm=TRUE) / sqrt(length(data[!is.na(data)]))
}
outlN <- function(vals,target,err) {
	length(vals[vals > target+err | vals < target-err])
}
outlPercent <- function(vals,target,err) {
    total <- length(vals)
    outl <- length(vals[vals > target+err | vals < target-err])
    (outl / total)*100
}
trz_to_vol <- function(assay_val) {
	slope <- trz_slope
	intc <- trz_intc
	(slope*assay_val) + intc
}



#Shiny Server Interaction
shinyServer(function(input, output) {

	fill_vol <- reactive({ input$fill })
	vol_err <- reactive({ input$err })


	#INPUT DATA
	#data input from user-input MDL number. Assay data must be available on Argus.
	df <- reactive({
		mdl_id <- paste('MDL-', toString(input$mdlN), sep="")
		# SQL Query
		dbhandle <- odbcDriverConnect('driver={SQL Server};
			server=sqlwarehouse1.amyris.local;
			database=dataout;
			uid=warehouse_user;
			pwd=warehouse_user')
		dataInput <- sqlQuery(dbhandle, paste("SELECT assay_MDL,assay_plate_label,raw_assay_value,row,col
									FROM dataout.furnace.hts_all_well_data
									WHERE assay_MDL = ", mdl_id, sep=""))
		if((is.null(dataInput))||(NROW(dataInput) == 0)) {return(NULL)}
		dataInput$volume_uL <- trz_to_vol(dataInput[["raw_assay_value"]])
		return(dataInput)
	)


	# DATA TABLES
	plt_df <- reactive({
		if (is.null(df())) {return(NULL)} 
		ddply(df(), c("assay_MDL","assay_plate_label"), here(summarize),
			mean_uL = mean(volume_uL), 
			cv = CV(volume_uL),
			std_err = std_err(volume_uL),
			n_outliers = outlN(volume_uL,fill_vol(),vol_err())
		) 
	})
	output$plate_table <- renderDataTable({ plt_df() })

	col_df <- reactive({
		if (is.null(df())) {return(NULL)}
		        ddply(df(),c("assay_MDL","row"), here(summarize),
			mean_uL = mean(volume_uL), 
			cv = CV(volume_uL),
			std_err = std_err(volume_uL),
			n_outliers = outlN(volume_uL,fill_vol(),vol_err())
		) 
	})
	output$col_table <- renderDataTable({ col_df() })
	
	row_df <- reactive({
		if (is.null(df())) {return(NULL)} 
		ddply(df(), c("assay_MDL","row"), here(summarize),
			mean_uL = mean(volume_uL), 
			cv = CV(volume_uL),
			std_err = std_err(volume_uL),
			n_outliers = outlN(volume_uL,fill_vol(),vol_err())
		) 
	})
	output$row_table <- renderDataTable({ row_df() })	 

	#DATA CV FLAGS
	output$pltSummary <- renderText({
		if (is.null(df())) {return(NULL)}
		plt_data <- plt_df()
		hi_cvs <- plt_data[plt_data$cv > max_cv]
		if (length(hi_cvs) > 0) {
			return(paste("By Plate:","WARNING - one or more CVs is >",max_cv))
		}
		return(paste("By Plate:", "CVs are all below",max_cv))
	})

	output$colSummary <- renderText({
		if (is.null(df())) {return(NULL)}
		col_data <- col_df()
		hi_cvs <- col_data[col_data$cv > max_cv]
		if (length(hi_cvs) > 0) {
			return(paste("By Column:","WARNING - one or more CVs is >",max_cv))
		}
		return(paste("By Column:", "CVs are all below",max_cv))
	})

	output$rowSummary <- renderText({
		if (is.null(df())) {return(NULL)}
		row_data <- row_df()
		hi_cvs <- row_data[row_data$cv > max_cv]
		if (length(hi_cvs) > 0) {
			return(paste("By Row:","WARNING - one or more CVs is >",max_cv))
		}
		return(paste("By Row:", "CVs are all below",max_cv))
	})





	# PLATE PLOT
	output$pltSet <- renderUI({
		if (is.null(df())) {return(NULL)}
		pltList <- sort(unique(df()$assay_plate_label))
		if (is.null(pltList) | length(pltList) > 15) { pltList <- c() }
		checkboxGroupInput("plates", label = h4("Choose Plate(s) to Display"),choices = pltList,selected = pltList)
	})
	plt_colors <- reactive({
		colors <- c("#FD026C","#4682B8","#A5D22D","#F5CC0A","#FE8C01","#6B9494","#B97C46","#84ACD0","#C2E173","#F9DD5B","#FF53A7","#FEBA55")	
		names(colors) <- sort(unique(df()$assay_plate_label))	
		return(colors)
	})
	selected_plts <- reactive({
		input$plates
	})
	output$plotPlate <- renderPlot({
		if (is.null(df())) {return(NULL)}
		data <- df()[ which(df()[, "assay_plate_label"] %in% selected_plts()), ]
		data$assay_plate_label <- as.factor(data$assay_plate_label)
		sp <- qplot(assay_plate_label,volume_uL,color=assay_plate_label,data=data)
		color_set <- plt_colors()[selected_plts()]
		sp+scale_color_manual(values=color_set)+labs(title=paste("PLATES: MDL-",input$mdlN,sep=""))+
>>>>>>> a1178b22e230637eaa16751264223ae58bfaf0d1
		geom_hline(yintercept=fill_vol(),color='black')+
		geom_hline(yintercept=fill_vol()-vol_err(),color='blue',linetype='dotdash')+
		geom_hline(yintercept=fill_vol()+vol_err(),color='blue',linetype='dotdash')+
		geom_boxplot(alpha = 0.45, fill = color_set)+
		stat_boxplot(geom = 'errorbar', width = 0.6)
	})



	# COLUMN PLOT
	output$colSet <- renderUI({
		if (is.null(df())) {return(NULL)}
		colList <- sort(unique(df()$col))
		if (is.null(colList)) { colList <- c() }
		checkboxGroupInput("cols", label = h4("Choose Column(s) to Display"),choices = colList, selected = colList)
	})
	selected_cols <- reactive({
		input$cols
	})
	col_colors <- reactive({
		colors <- c("#FD026C","#4682B8","#A5D22D","#F5CC0A","#FE8C01","#6B9494","#B97C46","#84ACD0","#C2E173","#F9DD5B","#FF53A7","#FEBA55")	
		names(colors) <- sort(unique(df()$col))	
		return(colors)
	})

	output$plotCol <- renderPlot({
		if (is.null(df())) {return(NULL)}
		data <- df()[ which(df()[, "col"] %in% selected_cols()), ]
		data$col <- as.factor(data$col)
		sp <- qplot(col,volume_uL,color=col,data=data)
		color_set <- col_colors()[as.integer(selected_cols())]
<<<<<<< HEAD
		sp+scale_color_manual(values=color_set)+labs(title=paste("COLUMNS:"))+
=======
		sp+scale_color_manual(values=color_set)+labs(title=paste("COLUMNS: MDL-",input$mdlN,sep=""))+
		geom_hline(yintercept=fill_vol(),color='black')+
		geom_hline(yintercept=fill_vol()-vol_err(),color='blue',linetype='dotdash')+
		geom_hline(yintercept=fill_vol()+vol_err(),color='blue',linetype='dotdash')+
		geom_boxplot(alpha = 0.6, fill = color_set)+
		stat_boxplot(geom = 'errorbar', width = 0.6)
	})



	# ROW PLOT
	rows <- c('A','B','C','D','E','F','G','H')
	names(rows) <- c('1','2','3','4','5','6','7','8')
	output$rowSet <- renderUI({
		if (is.null(df())) {return(NULL)}
		row_letters <- chartr("12345678","ABCDEFGH",df()[["row"]]) #convert to alpha to display
		rowList <- sort(unique(row_letters))
		if (is.null(rowList)) { rowList <- c() }
		checkboxGroupInput("rows", label = h4("Choose Row(s) to Display"),choices = rowList, selected = rowList)
	})
	selected_rows <- reactive({
		chartr("ABCDEFGH","12345678",input$rows)  #convert back to numerals
	})
	row_colors <- reactive({
		colors <- c("#FD026C","#4682B8","#A5D22D","#F5CC0A","#FE8C01","#6B9494","#B97C46","#84ACD0","#C2E173","#F9DD5B","#FF53A7","#FEBA55")	
		names(colors) <- c('1','2','3','4','5','6','7','8') #alt: sort(unique(df()$row))	
		return(colors)
	})
	output$plotRow <- renderPlot({
		if (is.null(df())) {return(NULL)}
		data <- df()[ which(df()[, "row"] %in% selected_rows()), ]
		data$row <- as.factor(data$row)
		sp <- qplot(row,volume_uL,color=row,data=data)
		color_set <- row_colors()[as.integer(selected_rows())]
		sp+scale_color_manual(values=color_set)+labs(title=paste("ROWS: MDL-",input$mdlN,sep=""))+
		geom_hline(yintercept=fill_vol(),color='black')+
		geom_hline(yintercept=fill_vol()-vol_err(),color='blue',linetype='dotdash')+
		geom_hline(yintercept=fill_vol()+vol_err(),color='blue',linetype='dotdash')+
		geom_boxplot(alpha = 0.6, fill = color_set)+
		stat_boxplot(geom = 'errorbar', width = 0.6)
	})
	

	output$plateWarning <- renderText({
		nPlates <- length(unique(df()$assay_plate_label))
		if (nPlates < 12) {return(NULL)}
		paste(c("Warning: cannot plot by plate if > 12 plates. Input data has",nPlates))
	})



})
