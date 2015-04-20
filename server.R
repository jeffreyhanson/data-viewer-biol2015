shinyServer(function(input,output,session) { 
	#### initalize
	# set defaults
	manager=MANAGER$new()
	output$plot_type=renderText({'histogram'})
	output$panel_mode=renderText({'welcome'})
	output$model_TXT=renderText({''})
	output$main_PLOT=renderPlot({emptyPlot()})
	output$diagnostic_PLOT=renderPlot({emptyPlot()})
	output$data_DF=renderDataFrame({emptyDataFrame()})
	
	# set initial states
	session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=TRUE))
	session$sendCustomMessage("setWidgetProperty",list(id="load_data_BTN",prop="disabled", status=TRUE))
	
	#### reactive handlers
	## load data panels
	# project name observer
	observe({
		if (is.empty(input$project_name_CHR)) {
			closeAlert(session,'loadingAlert')
			return()
		}
		isolate({
			# set field
			manager$setProjectName(.project_name_CHR)
			# try loading master data
			if (!manager$loadProjectDataFromFile()) {
				createAlert(
					session,'alert','loadingAlert', title='Error', append=FALSE, style='danger',
					content='Error loading master data for project.\n\nPlease check that you have entered the correct project name. \n\nIf you have still receive this message, please ask your tutor for help.'
				)
				session$sendCustomMessage("setWidgetProperty",list(id="week_number_CHR",prop="disabled", status=TRUE))
				session$sendCustomMessage("setWidgetProperty",list(id="group_color_CHR",prop="disabled", status=TRUE))
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=FALSE))
				updateSelectInput(session, "week_number_CHR", choices=c(""))
				updateSelectInput(session, "group_color_CHR", choices=c(""))
				updateSelectInput(session, "group_names_VCHR", choices=c(""))
			}
		})
	})
		
	
	# group subset checkbox observer
	observe({
		if (is.empty(input$group_subset_BOOL)) {
			closeAlert(session,'loadingAlert')
			return()
		}
		isolate({
		
				session$sendCustomMessage("setWidgetProperty",list(id="week_number_CHR",prop="disabled", status=!input$group_subset_BOOL))
				session$sendCustomMessage("setWidgetProperty",list(id="group_color_CHR",prop="disabled", status=!input$group_subset_BOOL))
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=FALSE))
				if (!input$group_subset_BOOL) {
					updateSelectInput(session, "week_number_CHR", choices=c(""))
					updateSelectInput(session, "group_color_CHR", choices=c(""))
					updateSelectInput(session, "group_names_VCHR", choices=c(""))
				} else {
					updateSelectInput(session, "week_number_CHR", choices=week_numbers_VCHR)
					updateSelectInput(session, "group_color_CHR", choices=group_colors_VCHR)
					updateSelectInput(session, "group_names_VCHR", choices=c(""))

				}
		})
	})

	
	# group names update widget observer
	observe({
		if(is.empty(input$week_number_CHR) & is.empty(input$group_color_CHR)) {
			closeAlert(session,'loadingAlert')
			return()
		}
		isolate({
			# set manager fields
			if (!is.empty(input$week_number_CHR))
				manager$setActiveWeekNumber_CHR(input$week_number_CHR)
			if (!is.empty(input$group_color_CHR))
				manager$setActiveGroupColor(input$group_color_CHR)
			# load group names
			if (manager$isGroupSubset()) {
				updateSelectInput("group_names_VCHR", manager$getProjectGroupNames())
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=FALSE))
			}
		})
	})
	
	# load data observer
	observe({
		if(is.null(input$load_data_BTN) || input$load_data_BTN==0)
			return()
		isolate({
			if (!manager$setActiveData())
				createAlert(
					session,'alert','loadingAlert', title='Error', append=FALSE, style='danger',
					content='Error loading group data from master dataset.\n\nPlease check that you have entered the correct details.\n\nIf you have still receive this message, please ask your tutor for help.'
				)
		})
	})
	
	## main viewing panels
	# variable sidebar panel observers
	observe({
		if (is.empty(input$response_CHR) & is.empty(input$predictor1_CHR) & is.empty(input$predictor2_CHR))
			return()
		isolate({
			# set variables
			if (!is.empty(input$predictor1_CHR))
				manager$setPredictor1Variable(input$predictor1_CHR)
			if (!is.empty(input$predictor2_CHR))
				manager$setPredictor2Variable(input$predictor2_CHR)
			if (!is.empty(input$response_CHR))
				manager$setResponseVariable(input$response_CHR)
			if (!is.empty(input$family_CHR))
				manager$setResponseFamily(input$family_CHR)
		
			# update results shown in panels
			if (!is.empty(manager$response_CHR)) {
				## run model
				manager$fitModel()
				manager$resetPlotParameters()
				
				## render main plot
				output$main_PLOT=renderPlot({manager$plot()})
				
				## render diagnostic plots
				output$diagnostics_PLOT=renderPlot({manager$modelDiagnostics()})
				
				## render model results
				output$model_TXT=renderText({manager$modelSummary()})
			
				## render data
				output$data_DF=renderDataFrame({manager$modelData()})
			}
		})
	})
	
	# plot options sidebar panel observers
	observe({
		if (is.empty(input$main_CHR) & is.empty(input$ylab_CHR) & is.empty(input$xlab_CHR) & is.empty(nbins_INT))
			return()
		isolate({
			# set variables
			if (!is.empty(input$main_CHR))
				manager$setPlotTitle(input$main_CHR)
			if (!is.empty(input$ylab_CHR))
				manager$setPlotYlabel(input$ylab_CHR)
			if (!is.empty(input$xlab_CHR))
				manager$setPlotXlabel(input$xlab_CHR)
			if (!is.empty(input$nbins_INT))
				manager$setPlotNbins(input$nbins_INT)
			# update plot
			output$main_PLOT=renderPlot({manager$plot()})
		})
	})	

})




