setOldClass('negbin')
setClassUnion('glmModel', c('glm','negbin'))

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
		.model_GLM='glmModel'
		
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
		
		isDataLoaded=function() {
			return(!identical(.fullData_DF, data.table(0)))
		},
		
		#### disk interface methods
		loadProjectDataFromFile=function() {
			currPTH=dir(file.path("master", .activeProjectName_CHR), '^.*.csv', ignore.case=TRUE, full.names=TRUE)
			if (length(currPTH)>0) {
				.fullData_DF<<-fread(sort(currPTH, decreasing=TRUE)[1])
				return(TRUE)
			} else {
				.fullData_DF<<-data.table(0)
				return(FALSE)
			}
		},

		### data loading methods
		getProjectGroupNames=function() {
			return(
				.fullData_DF %>%
				dplyr::filter(Week==.activeWeekNumber_CHR, `Group color`==.activeGroupColor_CHR) %>%
				dplyr::select(`Group`) %>%
				`[[`(1) %>%
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
					Week==.activeWeekNumber_CHR,
					`Group color`==  .activeGroupColor_CHR,
					`Group` %in% .activeGroupNames_CHR
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
		is.validResponseFamily=function() {
			switch(.responseFamily_CHR,
				'gaussian'={
					return(TRUE)
				},
				'poisson'={
					return (
						all(na.omit(.activeData_DF[[.responseVariable_CHR]])>=0) &&
						all(na.omit(.activeData_DF[[.responseVariable_CHR]])==na.omit(round(.activeData_DF[[.responseVariable_CHR]])))
					)
				},
				'binomial'={
					return(
						all(na.omit(.activeData_DF[[.responseVariable_CHR]])>=0) &&
						all(na.omit(.activeData_DF[[.responseVariable_CHR]])<=1)
					)
				
				},
				'quasipoisson'={
					return (
						all(na.omit(.activeData_DF[[.responseVariable_CHR]])>=0) &&
						all(na.omit(.activeData_DF[[.responseVariable_CHR]])==na.omit(round(.activeData_DF[[.responseVariable_CHR]])))
					)
				},
				'negativebinomial'={
					return (
						all(na.omit(.activeData_DF[[.responseVariable_CHR]])>=0) &&
						all(na.omit(.activeData_DF[[.responseVariable_CHR]])==na.omit(round(.activeData_DF[[.responseVariable_CHR]])))
					)
				}
			)
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
			if (!is.validchr(.predictorVariable1_CHR)) {
				.model_FRM<<-as.formula(paste0(make.names(.responseVariable_CHR),'~1'))
			} else {
				if (!is.validchr(.predictorVariable2_CHR)) {
					.model_FRM<<-reformulate(response=make.names(.responseVariable_CHR), termlabels=make.names(.predictorVariable1_CHR))
				} else {
					if (is.numeric(.activeData_DF[[.predictorVariable2_CHR]]) && !is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
						tmp=.predictorVariable1_CHR
						.predictorVariable1_CHR<<-.predictorVariable2_CHR
						.predictorVariable2_CHR<<-tmp
					}
					.model_FRM<<-reformulate(response=make.names(.responseVariable_CHR), termlabels=c(make.names(.predictorVariable1_CHR), make.names(.predictorVariable2_CHR), paste0(make.names(.predictorVariable1_CHR), ':', make.names(.predictorVariable2_CHR))))
				}
			}
			# run model			
			tmpDF=as.data.frame(.activeData_DF)
			names(tmpDF)=make.names(names(tmpDF))
			if (.responseFamily_CHR!='negativebinomial') {
				.model_GLM<<-glm(.model_FRM, data=tmpDF, family=.responseFamily_CHR)
			} else {
				.model_GLM<<-MASS::glm.nb(.model_FRM, data=tmpDF)
			}
		},
		
		### export methods
		plot=function() {
			# prepare data
			tmpDF=as.data.frame(.activeData_DF)
			setnames(tmpDF, names(tmpDF), make.names(names(tmpDF)))
			tmpDF=tmpDF[which(is.finite(tmpDF[[make.names(.responseVariable_CHR)]])),]
			
			# no predictor variable plots
			if (!is.validchr(.predictorVariable1_CHR)) {
				# histogram plot
				ggplot2Wrapper(
					ggplot(tmpDF, aes(x=tmpDF[[make.names(.responseVariable_CHR)]])) +
						geom_histogram(
							aes(y=..density..),
							fill='black',
							alpha=0.2
						) +
						geom_density(
							fill=alpha('blue', 0.2),
							color='transparent'
						) +
						geom_vline(
							aes(xintercept=unname(predict(.model_GLM, newdata=data.frame(1), type='response'))),
							color="blue",
							linetype="dashed",
							size=1
						) +
						xlab(.responseVariable_CHR) +
						ylab("Density") +
						theme_classic()
				)
			}
			
			# single predictor variable plots
			if (is.validchr(.predictorVariable1_CHR) & !is.validchr(.predictorVariable2_CHR)) {
				tmpDF=tmpDF[which(!is.na(tmpDF[[make.names(.predictorVariable1_CHR)]])),]
				if (is.numeric(tmpDF[[make.names(.predictorVariable1_CHR)]])) {
					# regression plot
					if (.responseFamily_CHR!='negativebinomial') {
						trend_line=geom_smooth(method="glm", se=TRUE, method.args=list(family=.responseFamily_CHR))
					} else {
						trend_line=geom_smooth(method="glm.nb", se=TRUE)
					}
					setnames(tmpDF, make.names(c(.responseVariable_CHR,.predictorVariable1_CHR)), c('resp_var', 'pred_var'))
					ggplot2Wrapper(
						ggplot(tmpDF, aes(x=pred_var, y=resp_var)) +
							geom_point(shape=1) +
							trend_line +
							xlab(.predictorVariable1_CHR) +
							ylab(.responseVariable_CHR) +
							theme_classic()
					)
				} else {
					# make predictions
					predDF=data.frame(.predictorVariable1_CHR=unique(.model_GLM$model[[make.names(.predictorVariable1_CHR)]]))
					names(predDF)=make.names(.predictorVariable1_CHR)
					tmpDF=predict(.model_GLM, newdata=predDF, se.fit=TRUE, type='response')
					predDF$resp_var=tmpDF$fit
					predDF$upper=tmpDF$fit+tmpDF$se.fit
					predDF$lower=tmpDF$fit-tmpDF$se.fit					
					names(predDF)[1]='pred_var'
					# bar plot
					ggplot2Wrapper(
						ggplot(predDF, aes(x=pred_var, y=resp_var)) + 
							geom_bar(position=position_dodge(), stat="identity", fill="black", alpha=0.2) +
							geom_errorbar(aes(ymin=predDF$lower, ymax=predDF$upper), position=position_dodge(.9), width=0.5) +
							xlab(.predictorVariable1_CHR) +
							ylab(.responseVariable_CHR) +
							theme_classic()
					)
				}
			}
			
			# two predictor variable plots
			if (is.validchr(.predictorVariable1_CHR) & is.validchr(.predictorVariable2_CHR)) {
				tmpDF=tmpDF[which(
						!is.na(tmpDF[[make.names(.predictorVariable1_CHR)]]) &
						!is.na(tmpDF[[make.names(.predictorVariable2_CHR)]])
				),]
				if (is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
					## ancova plot
					# make plot
					if (.responseFamily_CHR!='negativebinomial') {
						trend_line=geom_smooth(method="glm", method.args=list(family=.responseFamily_CHR))
					} else {
						trend_line=geom_smooth(method="glm.nb")
					}					
					ggplot2Wrapper(
						ggplot(tmpDF, aes_string(x = make.names(.predictorVariable1_CHR), y = make.names(.responseVariable_CHR), color = make.names(.predictorVariable2_CHR))) +
							scale_color_discrete(name= .predictorVariable2_CHR) +
							geom_point() +
							geom_smooth(method = "glm", method.args=list(family=.responseFamily_CHR)) +
							xlab(.predictorVariable1_CHR) +
							ylab(.responseVariable_CHR) +
							theme_classic()
					)
				} else {
					## two-way anova bar plot
					# make predictions
					comb_MTX=laply(strsplit(unique(paste0(.model_GLM$model[[make.names(.predictorVariable1_CHR)]],'%%%SEP%%%',.model_GLM$model[[make.names(.predictorVariable2_CHR)]])),'%%%SEP%%%'), function(x) {return(x)})
					predDF=data.frame(
						pred1_var=comb_MTX[,1],
						pred2_var=comb_MTX[,2]
					
					)
					names(predDF)=make.names(c(.predictorVariable1_CHR,.predictorVariable2_CHR))
					tmpDF=predict(.model_GLM, newdata=predDF, se.fit=TRUE, type='response')
					predDF$resp_var=tmpDF$fit
					predDF$upper=tmpDF$fit+tmpDF$se.fit
					predDF$lower=tmpDF$fit-tmpDF$se.fit
					names(predDF)[1:2]=c('pred1_var', 'pred2_var')
					# bar plot
					ggplot2Wrapper(
						ggplot(predDF, aes(x=pred1_var, y=resp_var, fill=pred2_var)) + 
							geom_bar(position=position_dodge(), stat="identity") +
							geom_errorbar(aes(ymin=predDF$lower, ymax=predDF$upper), position=position_dodge(.9), width=0.5) +
							xlab(.predictorVariable1_CHR) +
							ylab(.responseVariable_CHR) +
							labs(fill = .predictorVariable2_CHR) +
							theme_classic()
					)
				}
			}
		},
		modelDiagnostics=function() {
			if (!is.validchr(.predictorVariable1_CHR)) {
				autoplot(.model_GLM) + theme_classic()
			} else {
				if (!is.validchr(.predictorVariable2_CHR)) {
					if (is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
						autoplot(.model_GLM) + theme_classic()
					
					} else {
						autoplot(.model_GLM, data=.model_GLM$model, color=make.names(.predictorVariable1_CHR)) + theme_classic()
					}
				} else {
					if (is.numeric(.activeData_DF[[.predictorVariable1_CHR]])) {
						autoplot(.model_GLM, data=.model_GLM$model, color=make.names(.predictorVariable1_CHR)) + theme_classic()
					} else {
						tmpDF=as.data.frame(.activeData_DF)
						tmpDF$interaction=interaction(.activeData_DF[[.predictorVariable1_CHR]], .activeData_DF[[.predictorVariable2_CHR]])
						autoplot(.model_GLM, data=tmpDF, color='interaction') + theme_classic()
					}
				}
			}
		},
		modelResults=function() {
			return(
				list(
					dataSummary=glmDataSummaryUI(.model_GLM),
					modelSummary=glmModelSummaryUI(.model_GLM),
					anova=glmAnovaUI(.model_GLM),
					posthoc=glmPosHocUI(.model_GLM, .responseFamily_CHR)
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
 
