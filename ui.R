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
					tabPanel(value="load_data_PNL", title="Load data",
						div(
							br(),
							selectInput("project_name_CHR", "Project:", choices=project_names_VCHR, selected="mangrove herbivory"),
							checkboxInput("group_subset_BOOL", ' load data for specific group?', value = FALSE),
							conditionalPanel(
								condition="input.group_subset_BOOL == true",
								div(
									selectInput("week_number_CHR", "Week Number:", choices=week_numbers_VCHR, selected="week 1"),
									selectInput("group_color_CHR", "Group Colour:", choices=group_colors_VCHR, selected="blue", multiple=-FALSE),
									selectInput("group_names_VCHR", "Group Name:", choices=c(''), selected="blue", multiple=FALSE, selectize=TRUE)
								)
							),
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
					tabPanel(value="vars_PNL", title="Variables",
						div(
							br(),
							div(
								selectInput("response_CHR", label="Response variable:", choices=c('')),
								selectInput("predictor1_CHR", label="Predictor variable:", choices=c('')),
								selectInput("predictor2_CHR", label="Predictor variable:", choices=c(''))
							),
							div(
								selectInput("family_CHR", label="Error distribution:", choices=c(
									"Gaussian"="gaussian", 
									"Poisson"="poisson", 
									"binomial"="binomial", 
									"quasi-Poisson"="quasipoisson",
									"negative binomial"="negativebinomial"
								))
							)
						)
					),
					tabPanel(value="opts_PNL", title="Options",
						div(
							br(),
							div(
								textInput("main_CHR", label="title:", value=""),
								textInput("ylab_CHR", label="y-axis label:", value=""),
								textInput("xlab_CHR", label="x-axis label:", value="")
							),
							conditionalPanel(
								condition="output.plot_type == 'histogram'",
								div(
									sliderInput("nbins_INT", label="number bins:", min=1, max=10, value=5, round=TRUE)
								)
							),
							div(style="visibility:hidden",h5(textOutput('plot_type')))
						)
					)
				)
			),
			mainPanel(
				conditionalPanel(
					condition="output.panel_mode == 'active'",
					tabsetPanel(type = "tabs", 
						tabPanel(value="plot_PNL", title="Plot",
							plotOutput("main_PLOT")
						), 
						tabPanel(value="diagnostics_PNL", title="Diagnostics",
							plotOutput("diagnostics_PLOT")
						),
						tabPanel(value="model_PNL", title="Results",
							textOutput("model_TXT")
						),
						tabPanel(value="data_PNL", title="Data",
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