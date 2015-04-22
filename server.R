shinyServer(function(input,output,session) { 
	#### initalize
	# set defaults
	setwd(main_DIR)	
	manager=MANAGER$new()
	output$plot_type=renderText({'histogram'})
	output$panel_mode=renderText({'welcome'})
	output$model_TXT=renderText({''})
	output$main_PLOT=renderPlot({emptyPlot()})
	output$diagnostic_PLOT=renderPlot({emptyPlot()})
	output$data_DF=renderDataTable({emptyDataFrame()})
	
	#### reactive handlers
	## load data panels
	# project name observer
	observe({
		print(3)
		if (is.empty(input$project_name_CHR)) {
			closeAlert(session,'loadingAlert')
			return()
		}
		isolate({
			# set field
			manager$setActiveProjectName(input$project_name_CHR)
			# try loading master data
			if (manager$loadProjectDataFromFile()) {
				if (manager$isGroupSubset()) {
					closeAlert(session,'loadingMasterAlert')
					session$sendCustomMessage("setWidgetProperty",list(id="week_number_CHR",prop="disabled", status=FALSE))
					session$sendCustomMessage("setWidgetProperty",list(id="group_color_CHR",prop="disabled", status=FALSE))
					session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=FALSE))
				}
			} else {
				createAlert(
					session,'alert','loadingMasterAlert', title='Error', append=FALSE, type='danger',
					message='Error loading master data for project.\n\nPlease check that you have entered the correct project name. \n\nIf you have still receive this message, please ask your tutor for help.'
				)
				session$sendCustomMessage("setWidgetProperty",list(id="week_number_CHR",prop="disabled", status=TRUE))
				session$sendCustomMessage("setWidgetProperty",list(id="group_color_CHR",prop="disabled", status=TRUE))
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=TRUE))
			}
		})
	})
	
	# group subset checkbox observer
	observe({
		if (is.empty(input$group_subset_BOOL)) {
			closeAlert(session,'loadingGroupAlert')
			return()
		}
		isolate({
			manager$setGroupSubset(input$group_subset_BOOL)
			if (!input$group_subset_BOOL)
				session$sendCustomMessage("setWidgetProperty",list(id="load_data_BTN",prop="disabled", status=FALSE))
		})
	})

	# group names update widget observer
	observe({
		print(5)
		if(is.empty(input$week_number_CHR) & is.empty(input$group_color_CHR)) {
			closeAlert(session,'loadingGroupAlert')
			return()
		}
		isolate({
			# set manager fields
			if (!is.empty(input$week_number_CHR))
				manager$setActiveWeekNumber(input$week_number_CHR)
			if (!is.empty(input$group_color_CHR))
				manager$setActiveGroupColor(input$group_color_CHR)
			# load group names
			if (manager$isGroupSubset()) {
				updateSelectInput(session, "group_names_VCHR", choices=manager$getProjectGroupNames())				
				session$sendCustomMessage("setWidgetProperty",list(id="load_data_BTN",prop="disabled", status=TRUE))
				if (nrow(tmp)==0) {
					createAlert(
						session,'alert','loadingGroupAlert', title='Error', append=FALSE, type='danger',
						message='Error loading data for specified week and group color.\n\nPlease check that you have entered the correct details. \n\nIf you have still receive this message, please ask your tutor for help.'
					)
				}
			}
		})
	})
	observe({
		if (is.empty(input$group_names_VCHR))
			return()
		isolate({
			manager$setActiveGroupNames(input$group_names_VCHR)
			session$sendCustomMessage("setWidgetProperty",list(id="load_data_BTN",prop="disabled", status=FALSE))
		})
	})
	
	
	
	# load data observer
	observe({
		print(6)
		if(is.null(input$load_data_BTN) || input$load_data_BTN==0)
			return()
		isolate({
			if (!manager$setActiveData()) {
				print(6.1)
				createAlert(
					session,'alert','loadingAlert', title='Error', append=FALSE, type='danger',
					message='Error loading group data from master dataset.\n\nPlease check that you have entered the correct details.\n\nIf you have still receive this message, please ask your tutor for help.'
				)
				print(6.2)
			} else {
				# update variable selection widgets
				print(6.3)
				updateSelectInput(session, "response_CHR", choices=manager$getContinuousActiveColNames())
				updateSelectInput(session, "predictor1_CHR", choices=c('----', manager$getAllColActiveNames()))
				updateSelectInput(session, "predictor2_CHR", choices=c('----', manager$getCategoricalColActiveNames()))
				output$panel_mode=renderText({'active'})
				print(6.4)
			}
			
		})
	})
	
	## main viewing panels
	# variable sidebar panel observers
	observe({
		print(7)
		if (is.empty(input$response_CHR) & is.empty(input$predictor1_CHR) & is.empty(input$predictor2_CHR))
			return()
		local({
			# set variables
			print(7.1)
			if (!is.empty(input$predictor1_CHR))
				manager$setPredictor1Variable(input$predictor1_CHR)
			print(7.2)
			if (!is.empty(input$predictor2_CHR))
				manager$setPredictor2Variable(input$predictor2_CHR)
			print(7.3)
			if (!is.empty(input$response_CHR))
				manager$setResponseVariable(input$response_CHR)
			print(7.4)
			if (!is.empty(input$family_CHR))
				manager$setResponseFamily(input$family_CHR)
			
		
			# update results shown in panels
			if (manager$.responseVariable_CHR %in% manager$getContinuousActiveColNames()) {
				## run model
				print(7.6)
				manager$fitModel()
				print(7.7)
				manager$resetPlotParameters()
				print(7.8)
				
				local({
					## render main plot
					print(7.9)
					output$main_PLOT=renderPlot({print(manager$plot())})
					## render diagnostic plots
					print(7.91)
					output$diagnostics_PLOT=renderPlot({print(manager$modelDiagnostics())})
				})
				
				local({
				})
				
				## render model results
				print(7.92)
				local({
					output$model_TXT=renderText({manager$modelSummary()})
				})
				## render data
				print(7.93)
				local({
					output$data_DF=renderDataTable({manager$modelData()})
				})
				print(7.94)
			}
		})
	})
	
	# plot options sidebar panel observers
	observe({
		print(8)
		if (is.empty(input$main_CHR) & is.empty(input$ylab_CHR) & is.empty(input$xlab_CHR) & is.empty(input$nbins_DBL))
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
			if (is.validchr(manager$.responseVariable_CHR))
				output$main_PLOT=renderPlot({manager$plot()})
		})
	})	

})




