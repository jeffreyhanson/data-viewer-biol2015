shinyUI(fluidPage(
	tags$head(
		tags$link(rel="stylesheet", type="text/css", href="deps/list/style.css"),
		tags$link(rel="stylesheet", type="text/css", href="css/font-awesome.min.css"),
		tags$link(rel="stylesheet", type="text/css", href="deps/datatable/DataTables-1.10.5/media/css/jquery.dataTables.css"),
		tags$link(rel="stylesheet", type="text/css", href="deps/datatable/DataTables-1.10.5/extensions/FixedHeader/css/dataTables.fixedHeader.min.css"),
		tags$style("
		
		#group_names_WDGT {
			visibility: hidden;
		}
		
		")
	),
	tags$body(id="page-wrap",
		sidebarLayout(
			sidebarPanel(
				tabsetPanel(type = "tabs", 
					tabPanel("load_data_PNL",
						div(
							selectInput("week_number_CHR", "Week Number:", choices=week_numbers_VCHR, selected="week 1"),
							selectInput("project_name_CHR", "Project:", choices=project_names_VCHR, selected="mangrove herbivory"),
							checkboxInput("group_subset_BOOL", 'load data for specific group?', value = FALSE),
							selectInput_CUST("group_color_CHR", "Group Colour:", choices=group_colors_VCHR, selected="blue", multiple=-FALSE, widgetId="group_color_WDGT"),
							selectInput_CUST("group_names_VCHR", "Group Name:", choices=group_colors_VCHR, selected="blue", multiple=FALSE, widgetId="group_names_WDGT", selectize=TRUE),
							div(class="center", 
								tags$button(
									id="load_data_BTN",
									type="button",
									class="btn action-button btn-primary sbs-action-button btn-lg",
									"Load Data",
									title="Load data collected for a specific project"
								)
							),
							bsAlert("alert")
						)
					),
					tabPanel("vars_PNL",
						div(
							div(
								h2('Variables'),
								textInput("response_CHR", label="Response variable:", value=""),
								textInput("predictor1_CHR", label="Predictor variable:", value=""),
								textInput("predictor2_CHR", label="Predictor variable:", value="")
							),
							div(
								h2('Model Parameters'),
								selectInput("family_CHR", label="Error distribution:", choices=c(
									gaussian='Gaussian', 
									poisson='Poisson', 
									binomial='binomial', 
									quasipoisson='quasi-Poisson'),
									negativebinomial='negative binomial'
								),
							),
						)
					),
					tabPanel("opts_PNL",
						div(
							div(
								h2('Plot Options'),
								textInput("main_CHR", label="title:", value=""),
								textInput("ylab_CHR", label="y-axis label:", value=""),
								textInput("xlab_CHR", label="x-axis label:", value=""),
							)
							conditionalPanel(
								condition="output.plot_type == 'histogram'",
								div(
									sliderInput("nbins_INT", label="number bins:", min=1, max=10, round=TRUE)
								)
							),
							div(style="visibility:hidden",h5(textOutput('plot_type')))
						)
					)
				)
			),
			mainPanel(
				conditionalPanel(
					condition="output.panel_mode == 'plot'",
					tabsetPanel(type = "tabs", 
						tabPanel("plot_PNL",
							plotOutput("main_PLOT")
						), 
						tabPanel("diagnostics_PNL",
							plotOutput("diagnostic_PLOT")
						),
						tabPanel("model_PNL"
							textOutput("model_TXT")
						),
						tabPanel("data_PNL",
							dataTableOutput(outputId="data_DF")
						)
					)
				),
				div(style="visibility:hidden",h5(textOutput('panel_mode')))
			)
		)
	),
	tags$foot(
		tags$script(
			HTML('
				Shiny.addCustomMessageHandler("setWidgetProperty",
					function(message) {
						$("#"+message.id).prop(message.prop, message.status);
					}
				);
			')
		)
	)
))