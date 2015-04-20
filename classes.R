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
		.nbins_INT='integer',
		
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
			.nbins_INT<<-integer(0)
			# model fields
			.model_GLM<<-structure(class='glm', .Data=NULL)
		},
		#### field validating methods
		isAllGroupFieldsValid=function() {
			return(!is.empty(.activeWeekNumber_CHR) & !is.empty(.activeProjectName_CHR) & !is.empty(.activeGroupColor_CHR) & !is.empty(.activeGroupNames_CHR))
		},
		isGroupSubset=function() {
			return(.groupSubset)
		}
		
		#### disk interface methods
		loadProjectDataFromFile=function() {
			currPTH=dir(file.path("master", .activeProjectName_CHR), '^.*.csv', ignore.case=TRUE, full.names=TRUE)
			if (length(currPTH)>0) {
				.fullData_DF<<-fread(sort(currPTH, descending=TRUE)[1])
				return(TRUE)
			} else {
				.fullData_DF<<-data.table(0)
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
		setActiveWeekNumber_CHR=function(week_number) {
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
		setActiveData=function() {
			if (.groupSubset) {
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
		
		### variable setter methods
		setResponseVariable=function(i) {
			.responseVariable_CHR<<-i
		},
		setResponseFamily=function(i) {
			.responseFamily<<-i
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
			.nbins_INT<<-i
		},
		resetPlotParameters=function(i) {
			.main_CHR<<-''
			.ylab_CHR<<-responseVariable_CHR
			.xlab_CHR<<-.predictorVariable1_CHR
			.zlab_CHR<<-.predictorVariable2_CHR
		},
		
		### model methods
		fitModel=function() {
			# construct formula
			if (is.empty(.predictorVariable1_CHR)) {
				currFormula=as.formula(paste0('~ ',responseVariable_CHR))
			} else {
				if (is.empty(.predictorVariable2_CHR)) {
					currFormula=reformulate(response=responseVariable_CHR, term.labels=.predictorVariable1_CHR)
				} else {
					if (is.numeric(.activeData_DF[[.predictorVariable2_CHR]]) & !is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
						tmp=.predictorVariable1_CHR
						.predictorVariable1_CHR<<-.predictorVariable2_CHR
						.predictorVariable2_CHR<<-tmp
					}
					currFormula=reformulate(response=responseVariable_CHR, term.labels=c(.predictorVariable1_CHR, .predictorVariable2_CHR, paste0(.predictorVariable1_CHR, ':', .predictorVariable2_CHR)))
				}
			}
			# run model
			.model_GLM<<-glm(currFormula, data=.activeData_DF, family=.responseFamily_CHR)
		},
		
		### export methods
		plot=function() {
			if (is.empty(.predictorVariable1_CHR)) {
				# histogram plot
				pos=unname(predict(.predictorVariable1_CHR, newdata=data.frame(1), type='response'))
				ggplot(.activeData_DF, aes(x=responseVariable_CHR)) +
					geom_histogram(binwidth=abs(diff(range(.activeData_DF[[responseVariable_CHR]])))/.nbins_INT) +
					geom_density(alpha=.2, fill="#FF6666") +
					geom_vline(aes(xintercept=pos), color="red", linetype="dashed", size=1) +
					theme_bw()
			} else if (is.empty(.predictorVariable2_CHR)) {
				if (is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
					# regression plot
					ggplot(.activeData_DF, aes(x=responseVariable_CHR, y=.predictorVariable2_CHR)) +
						geom_point(shape=1) +
						geom_smooth(method="glm", family=.responseFamily_CHR) +
						theme_bw()
				} else {
					# make predictions
					predDF=data.frame(.predictorVariable1_CHR=unique(.activeData_DF[[.predictorVariable1_CHR]]))
					names(predDF)=.predictorVariable1_CHR
					tmpDF=predict(.model_GLM, se.fit=TRUE)
					predDF[[responseVariable_CHR]]=tmpDF$fit
					predDF$upper=tmpDF$fit+tmpDF$se.fit
					predDF$lower=tmpDF$fit-tmpDF$se.fit
					
					# bar plot
					ggplot(.activeData_DF, aes(x=.predictorVariable1_CHR, y=responseVariable_CHR)) + 
						geom_bar(position=position_dodge(), stat="identity") +
						geom_errorbar(aes(ymin=predDF$lower, ymax=perdDF$upper), position=position_dodge(.9)) +
						theme_bw()
				}
			} else {
				stop('not finished')
			
			}
		},
		modelDiagnostics=function() {
			if (!is.empty(.predictorVariable2_CHR) && !is.numeric(.activeData_DF[[.predictorVariable2_CHR]]) && 
			    !is.empty(.predictorVariable1_CHR) && !is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
				tmpDF=.activeData_DF
				tmpDF$interaction=interaction(.activeData_DF[[.predictorVariable1_CHR]], .activeData_DF[[.predictorVariable2_CHR]])
				autoplot(.model_GLM, data=.activeData_DF, color='interaction') + theme_bw()
			} else if ( !is.empty(.predictorVariable1_CHR) && !is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
				autoplot(.model_GLM, data=.activeData_DF, color=.predictorVariable1_CHR) + theme_bw()
			} else {
				autoplot(.model_GLM) + theme_bw()
			}
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
			if (is.empty(.predictorVariable1_CHR)) {
				return(.activeData_DF[,responseVariable_CHR,with=FALSE])
			} else {
				if (is.empty(.predictorVariable2_CHR)) {
					return(.activeData_DF[,c(responseVariable_CHR,.predictorVariable1_CHR),with=FALSE])
				} else {
					return(.activeData_DF[,c(responseVariable_CHR,.predictorVariable1_CHR, .predictorVariable2_CHR),with=FALSE])
				}
			}			
		}
	)
)
 
