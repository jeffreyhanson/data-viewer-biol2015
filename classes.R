MANAGER=setRefClass("MANAGER",
	fields=list(
		.activeWeekNumber_CHR="character",
		.activeProjectName_CHR="character",
		.activeGroupColor_CHR="character",
		.activeGroupNames_CHR="character",
		.groupSubset_BOOL='logical',
		.fullData_DF="data.table",
		.activeData_DF="data.table",
		
		.main_CHR='character',
		.ylab_CHR='character',
		.xlab_CHR='character',
		.zlab_CHR='character',
		.nbins_DBL='numeric',
		
		.responseVariable_CHR='character',
		.responseFamily_CHR='character',
		.predictorVariable1_CHR='character',
		.predictorVariable2_CHR='character',
	
		.model_FRM='formula',
		.model_GLM='glm'
		
	),
	methods=list(
		#### initialize methods
		initialize=function() {
			# loading fields
			.activeWeekNumber_CHR<<-character(0)
			.activeProjectName_CHR<<-character(0)
			.activeGroupColor_CHR<<-character(0)
			.activeGroupNames_CHR<<-character(0)
			.fullData_DF<<-data.table(0)
			.activeData_DF<<-data.table(0)
			.groupSubset_BOOL<<-FALSE
			# plotting fields
			.main_CHR<<-character(0)
			.ylab_CHR<<-character(0)
			.xlab_CHR<<-character(0)
			.nbins_DBL<<-numeric(0)
			# model fields
			.model_FRM<<-~1
			.model_GLM<<-structure(class='glm', .Data=NULL)
		},
		
		#### field validating methods
		isAllGroupFieldsValid=function() {
			return(!is.empty(.activeWeekNumber_CHR) & !is.empty(.activeProjectName_CHR) & !is.empty(.activeGroupColor_CHR) & !is.empty(.activeGroupNames_CHR))
		},
		isGroupSubset=function() {
			return(.groupSubset_BOOL)
		},
		
		#### disk interface methods
		loadProjectDataFromFile=function() {
			print(3.1)
			currPTH=dir(file.path("master", .activeProjectName_CHR), '^.*.csv', ignore.case=TRUE, full.names=TRUE)
			print(currPTH)
			print(3.2)
			if (length(currPTH)>0) {
				print(3.3)
				.fullData_DF<<-fread(sort(currPTH, decreasing=TRUE)[1])
				print(3.4)
				return(TRUE)
			} else {
				.fullData_DF<<-data.table(0)
				print(3.5)
				return(FALSE)
			}
		},

		### data loading methods
		getProjectGroupNames=function() {
			return(
				.fullProjectData_DF %>%
				filter(Week==.activeWeekNumber_CHR & `Group color`==.activeGroupColor_CHR) %>%
				select(`Group name`) %>%
				unique()
			)
		},
		setActiveWeekNumber=function(week_number) {
			.activeWeekNumber_CHR<<-week_number
		},		
		setActiveProjectName=function(project_name) {
			.activeProjectName_CHR<<-sub(" ", "_", project_name)
		},
		setActiveGroupColor=function(group_color) {
			.activeGroupColor_CHR<<-group_color
		},
		setActiveGroupNames=function(group_names) {
			.activeGroupNames_CHR<<-group_names
		},
		setGroupSubset=function(i) {
			.groupSubset_BOOL<<-i
		},
		setActiveData=function() {
			if (.groupSubset_BOOL) {
				.activeData_DF<<-filter(
					.fullData_DF,  
					Week==.activeWeekNumber_CHR &
					`Group Color`==  .activeGroupColor_CHR &
					`Group Name`==.activeGroupNames_CHR
				)
				return(nrow(.activeData_DF)>0)
			} else {
				.activeData_DF<<-.fullData_DF
				return(TRUE)
			}
		},
		
		#### active data frame methods
		getAllColActiveNames=function() {
			return(names(.activeData_DF))
		},
		getContinuousActiveColNames=function() {
			return(names(.activeData_DF)[sapply(.activeData_DF, is.numeric)])
		},
		getCategoricalColActiveNames=function() {
			return(names(.activeData_DF)[!sapply(.activeData_DF, is.numeric)])
		},
		
		### variable setter methods
		setResponseVariable=function(i) {
			.responseVariable_CHR<<-i
		},
		setResponseFamily=function(i) {
			.responseFamily_CHR<<-i
		},
		setPredictor1Variable=function(i) {
			.predictorVariable1_CHR<<-i
		},
		setPredictor2Variable=function(i) {
			.predictorVariable2_CHR<<-i
		},
		
		### plot setter methods
		setPlotTitle=function(i) {
			.main_CHR<<-i
		},
		setPlotYlabel=function(i) {
			.ylab_CHR<<-i
		},
		setPlotXlabel=function(i) {
			.xlab_CHR<<-i
		},
		setPlotZlabel=function(i) {
			.zlab_CHR<<-i
		},
		setPlotNbins=function(i) {
			.nbins_DBL<<-i
		},
		resetPlotParameters=function(i) {
			.main_CHR<<-''
			.ylab_CHR<<-.responseVariable_CHR
			.xlab_CHR<<-.predictorVariable1_CHR
			.zlab_CHR<<-.predictorVariable2_CHR
		},
		
		### model methods
		fitModel=function() {
			# construct formula
			print('fitting model')
			if (!is.validchr(.predictorVariable1_CHR)) {
				.model_FRM<<-as.formula(paste0('`',.responseVariable_CHR,'`~ 1'))
			} else {
				if (is.validchr(.predictorVariable2_CHR)) {
					.model_FRM<<-reformulate(response=.responseVariable_CHR, termlabels=.predictorVariable1_CHR)
				} else {
					if (is.numeric(.activeData_DF[[.predictorVariable2_CHR]]) & !is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
						tmp=.predictorVariable1_CHR
						.predictorVariable1_CHR<<-.predictorVariable2_CHR
						.predictorVariable2_CHR<<-tmp
					}
					.model_FRM<<-reformulate(response=.responseVariable_CHR, termlabels=c(.predictorVariable1_CHR, .predictorVariable2_CHR, paste0(.predictorVariable1_CHR, ':', .predictorVariable2_CHR)))
				}
			}
			# run model
			.model_GLM<<-glm(.model_FRM, data=.activeData_DF, family=.responseFamily_CHR)
		},
		
		### export methods
		plot=function() {
			print('plotting data')
			# no predictor variable plots
			if (!is.validchr(.predictorVariable1_CHR)) {
				# histogram plot
				pos=unname(predict(.model_GLM, newdata=data.frame(1), type='response'))
				print('pos')
				print(pos)
				x=ggplot(.activeData_DF, aes(x=.activeData_DF[[.responseVariable_CHR]])) +
					geom_histogram(
						aes(y=..density..),
						binwidth=abs(diff(range(.activeData_DF[[.responseVariable_CHR]])))/.nbins_DBL,
						fill='grey'
					) +
					geom_density(
						alpha=.2,
						fill="blue"
					) +
					geom_vline(aes(xintercept=pos), color="blue", linetype="dashed", size=1) +
					xlab(.responseVariable_CHR) +
					ylab("Density") +
					theme_classic()
				x$plot_env=environment()
				print(x)
			}
			
			# single predictor variable plots
			if (is.validchr(.predictorVariable1_CHR) & !is.validchr(.predictorVariable2_CHR)) {
				if (is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
					# regression plot
					x=ggplot(.activeData_DF, aes_string(x=.responseVariable_CHR, y=.predictorVariable2_CHR)) +
						geom_point(shape=1) +
						geom_smooth(method="glm", family=.responseFamily_CHR) +
						theme_classic()
					x$plot_env=environment()
					print(x)
				} else {
					# make predictions
					predDF=data.frame(.predictorVariable1_CHR=unique(.activeData_DF[[.predictorVariable1_CHR]]))
					names(predDF)=.predictorVariable1_CHR
					tmpDF=predict(.model_GLM, se.fit=TRUE)
					predDF[[.responseVariable_CHR]]=tmpDF$fit
					predDF$upper=tmpDF$fit+tmpDF$se.fit
					predDF$lower=tmpDF$fit-tmpDF$se.fit
					
					# bar plot
					x=ggplot(.activeData_DF, aes_string(x=.predictorVariable1_CHR, y=.responseVariable_CHR)) + 
						geom_bar(position=position_dodge(), stat="identity") +
						geom_errorbar(aes(ymin=predDF$lower, ymax=perdDF$upper), position=position_dodge(.9)) +
						theme_classic()
					x$plot_env=environment()
					print(x)

				}
			} 
			
			# two predictor variable plots
			if (is.validchr(.predictorVariable1_CHR) & is.validchr(.predictorVariable2_CHR)) {
				if (is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
					# ancova plot
					stop('not finished')
				} else {
					#two-way anova bar plot
					stop('not finished')
				}
			}
		},
		modelDiagnostics=function() {
			print('rendering model diagnostics')
			print(7.911)
			if (is.validchr(.predictorVariable2_CHR) && is.validchr(.activeData_DF[[.predictorVariable2_CHR]]) && 
			    is.validchr(.predictorVariable1_CHR) && is.validchr(.activeData_DF[[.predictorVariable1_CHR]])) {
				print(7.912)
				tmpDF=.activeData_DF
				tmpDF$interaction=interaction(.activeData_DF[[.predictorVariable1_CHR]], .activeData_DF[[.predictorVariable2_CHR]])
				x=autoplot(.model_GLM, data=.activeData_DF, color='interaction') + 
					theme_classic()
				x$plot_env=environment()
				print(x)
				print(7.913)
			} else if (is.validchr(.predictorVariable1_CHR) && is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
				print(7.914)
				x=autoplot(.model_GLM, data=.activeData_DF, color=.predictorVariable1_CHR) + theme_bw()
				x$plot_env=environment()
				print(x)
				print(7.915)
			} else {
				print(7.916)
				x=autoplot(.model_GLM) + theme_classic()
# 				x$plot_env=environment()
				print(x)
				print(7.917)
			}
			print(7.918)
		},
		modelResults=function() {
			return(
				paste(
					capture.output(anova(.model_GLM)),
					collapse='\n'
				)
			)
		},
		modelData=function() {
			if (is.validchr(.predictorVariable1_CHR)) {
				return(.activeData_DF[,.responseVariable_CHR,with=FALSE])
			} else {
				if (is.validchr(.predictorVariable2_CHR)) {
					return(.activeData_DF[,c(.responseVariable_CHR,.predictorVariable1_CHR),with=FALSE])
				} else {
					return(.activeData_DF[,c(.responseVariable_CHR,.predictorVariable1_CHR, .predictorVariable2_CHR),with=FALSE])
				}
			}			
		}
	)
)
 
