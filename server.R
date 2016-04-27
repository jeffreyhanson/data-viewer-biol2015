shinyServer(function(input,output,session) { 
	#### initalize
	# try(system('touch /srv/shiny-server/data-viewer-biol2015/restart.txt'), silent=TRUE)

	
	# set defaults
	setwd(main_DIR)
	manager=MANAGER$new()
	output$plot_type=renderText({'histogram'})
	output$panel_mode=renderText({'welcome'})
	output$group_subset_CHR=renderText({'FALSE'})
	output$model_TXT=renderText({''})
	output$main_PLOT=renderPlot({emptyPlot()})
	output$diagnostic_PLOT=renderPlot({emptyPlot()})
	output$data_DF=renderDataTable({emptyDataFrame()})
		
	# set initial states
	session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=TRUE))		
		
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
			manager$setActiveProjectName(input$project_name_CHR)
			# try loading master data
			if (manager$loadProjectDataFromFile()) {
				if (manager$isGroupSubset()) {
					closeAlert(session,'loadingMasterAlert')
					session$sendCustomMessage("setWidgetProperty",list(id="week_number_CHR",prop="disabled", status=FALSE))
					session$sendCustomMessage("setWidgetProperty",list(id="group_color_CHR",prop="disabled", status=FALSE))
					# session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=FALSE))
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
			output$group_subset_CHR=renderText({as.character(input$group_subset_BOOL)})
			if (!input$group_subset_BOOL)
				session$sendCustomMessage("setWidgetProperty",list(id="load_data_BTN",prop="disabled", status=FALSE))
		})
	})

	# group names update widget observer
	observe({
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
			assign('.fullData_DF', manager$.fullData_DF, envir=globalenv())
			assign('.activeWeekNumber_CHR', input$week_number_CHR, envir=globalenv())
			assign('.activeGroupColor_CHR', input$group_color_CHR, envir=globalenv())
			
			if (manager$isGroupSubset() & manager$isDataLoaded()) {
				tmp=manager$getProjectGroupNames()
				updateSelectInput(session, "group_names_VCHR", choices=tmp)
				session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=FALSE))
				session$sendCustomMessage("setWidgetProperty",list(id="load_data_BTN",prop="disabled", status=TRUE))
				if (length(tmp)==0) {
					session$sendCustomMessage("setWidgetProperty",list(id="group_names_VCHR",prop="disabled", status=TRUE))
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
		if(is.null(input$load_data_BTN) || input$load_data_BTN==0)
			return()
		isolate({
			if (!manager$setActiveData()) {
				createAlert(
					session,'alert','loadingAlert', title='Error', append=FALSE, type='danger',
					message='Error loading group data from master dataset.\n\nPlease check that you have entered the correct details.\n\nIf you have still receive this message, please ask your tutor for help.'
				)
				updateSelectInput(session, "response_CHR", choices=c(''))
				updateSelectInput(session, "predictor1_CHR", choices=c(''))
				updateSelectInput(session, "predictor2_CHR", choices=c(''))
				output$data_DF=renderDataTable({emptyDataFrame()})
			} else {
				# update variable selection widgets
				updateSelectInput(session, "response_CHR", choices=manager$getContinuousActiveColNames())
				updateSelectInput(session, "predictor1_CHR", choices=c('----', manager$getAllColActiveNames()))
				updateSelectInput(session, "predictor2_CHR", choices=c('----', manager$getCategoricalColActiveNames()))
				output$data_DF=renderDataTable({manager$.activeData_DF})
				output$panel_mode=renderText({'active'})
				# automatically change tab
				updateTabsetPanel(session, 'sidebarTabsetPanel', selected='vars_PNL')
				
			}
		})
	})
	
	## main viewing panels
	# variable sidebar panel observers
	observe({
		if (is.empty(input$response_CHR) & is.empty(input$predictor1_CHR) & is.empty(input$predictor2_CHR) & is.empty(input$family_CHR))
			return()
		local({
			# set variables
			if (!is.empty(input$predictor1_CHR))
				manager$setPredictor1Variable(input$predictor1_CHR)
			if (!is.empty(input$predictor2_CHR))
				manager$setPredictor2Variable(input$predictor2_CHR)
			if (!is.empty(input$response_CHR))
				manager$setResponseVariable(input$response_CHR)
			if (!is.empty(input$family_CHR))
				manager$setResponseFamily(input$family_CHR)
				
			# check that variable entries are valid
			if (!manager$is.validResponseFamily()) {
				createAlert(
					session,'variableAlert','responseFamilyAlert', title='Error', append=FALSE, type='danger',
					message='The error distribution specified is invalid for the specified response variable.\n\nPlease change either the response variable or the distribution.'
				)
				return()
			}
			closeAlert(session, 'responseFamilyAlert')
			
			# update results shown in panels
			if (manager$.responseVariable_CHR %in% manager$getContinuousActiveColNames()) {
				## run model
				manager$fitModel()
				manager$resetPlotParameters()
				local({
					## render main plot
					output$main_PLOT=renderPlot({
						manager$plot()
					})
					## render diagnostic plots
					output$diagnostics_PLOT=renderPlot({
						manager$modelDiagnostics()
					})
					## render model results
					currResults=manager$modelResults()
					output$data_SUMMARY_DT=renderDataTable({currResults$dataSummary})
					output$data_SUMMARY_UI=renderUI({
						div(
							h3('Data Summary'),
							dataTableOutput('data_SUMMARY_DT')
						)
					})
					output$model_SUMMARY_UI=renderUI({currResults$modelSummary})
					output$model_ANOVA_UI=renderUI({currResults$anova})
					output$model_POSTHOC_UI=renderUI({currResults$posthoc})
					output$model_UI=renderUI({
						div(
							uiOutput('data_SUMMARY_UI'),
							uiOutput('model_SUMMARY_UI'),
							uiOutput('model_ANOVA_UI'),
							uiOutput('model_POSTHOC_UI')
						)
					})
				})
			}
		})
	})	
})




