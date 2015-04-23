### html functions
selectInput_CUST=function (inputId, label, choices, selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL, widgetId=NULL) {
	choices <- shiny:::choicesWithNames(choices)
	if (is.null(selected)) {
	if (!multiple) 
		selected <- shiny:::firstChoice(choices)
	}
	else selected <- shiny:::validateSelected(selected, choices, inputId)
	if (!is.null(size) && selectize) {
		stop("'size' argument is incompatible with 'selectize=TRUE'.")
	}
	selectTag <- tags$select(id = inputId, class = if (!selectize) "form-control", size = size, shiny:::selectOptions(choices, selected))
	if (multiple) 
	selectTag$attribs$multiple <- "multiple"
	res <- div(
		id=if (!is.null(widgetId))
			widgetId,
		class = "form-group shiny-input-container",
		style = if (!is.null(width))
			paste0("width: ", shiny:::validateCssUnit(width)),
		shiny:::controlLabel(inputId, label), 
		div(selectTag)
	)
    if (!selectize) 
        return(res)
    return(shiny:::selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in% choices)))
}

### assorted functions
emptyPlot=function() {
	plot(0, type='n', bty='n', axes=FALSE, xlab='', ylab='')
}

emptyDataFrame=function() {
	return(data.frame(NoData='Error: No data loaded'))
}

is.empty=function(x) {
	return(length(x)==0)
}

is.validchr=function(x) {
	return(length(x)>0 && nchar(x)>0 && !identical(x, '----'))
}

ggplot2Wrapper=function(x, env=parent.frame()) {
	x$plot_env=env
	print(x)
}

uiWrapper=function(title='', text='') {
	return(
		div(
			h3(title),
			pre(paste(text, collapse='\n'))
		)
	)
}

glmAnovaUI=function(x) {
	txt=capture.output(anova(x, test='F'))
	return(uiWrapper('Backwards step-wise term deletion routine', txt))
}

glmModelSummaryUI=function(x) {
	txt=capture.output(summary(x))
	return(uiWrapper('Model Summary', txt))
}


glmDataSummaryUI=function(x) {
	## init
	inpDF=data.table(x$model)
	setnames(inpDF, names(inpDF), gsub('.', ' ', names(inpDF), fixed=TRUE))
	## generate table
	if (ncol(x$model)==2 && !is.numeric(x$model[[2]])) {
		# one way data summary 
		expDF=inpDF[,list(
			variable=eval(names(inpDF)[1]),
			mean=mean(get(names(inpDF)[1]), na.rm=TRUE),
			var=var(get(names(inpDF)[1]), na.rm=TRUE),
			sd=sd(get(names(inpDF)[1]), na.rm=TRUE),
			se=std.error(get(names(inpDF)[1]), na.rm=TRUE)
		), by=list(get(names(inpDF)[2]))]
		setnames(expDF, names(expDF), c(names(inpDF)[2], 'variable', 'mean', 'variance', 'standard deviation', 'standard error'))
	} else if (ncol(x$model)>2 && !is.numeric(x$model[[2]]) && !is.numeric(x$model[[3]])) {
		# two way data summary
		expDF=inpDF[,list(
			variable=eval(names(inpDF)[1]),
			mean=mean(get(names(inpDF)[1]), na.rm=TRUE),
			var=var(get(names(inpDF)[1]), na.rm=TRUE),
			sd=sd(get(names(inpDF)[1]), na.rm=TRUE),
			se=std.error(get(names(inpDF)[1]), na.rm=TRUE)
		), by=list(get(names(inpDF)[2]), get(names(inpDF)[3]))]
		setnames(expDF, names(expDF), c(names(inpDF)[2:3], 'variable', 'mean', 'variance', 'standard deviation', 'standard error'))
	} else {
		# single response summary
		expDF=inpDF[,list(
			variable=eval(names(inpDF)[1]),
			mean=mean(get(names(inpDF)[1]), na.rm=TRUE),
			var=var(get(names(inpDF)[1]), na.rm=TRUE),
			sd=sd(get(names(inpDF)[1]), na.rm=TRUE),
			se=std.error(get(names(inpDF)[1]), na.rm=TRUE)
		)]
		setnames(expDF, names(expDF), c('variable', 'mean', 'variance', 'standard deviation', 'standard error'))
	}
	## return datatable
	return(expDF)

}

glmPosHocUI=function(x) {
	## generate text to render
	if (ncol(x$model)==2 && !is.numeric(x$model[[2]])) {
		# one way anova
		x$model[[2]]=as.factor(x$model[[2]])
		txt=capture.output(
			summary(glht(x, linfct=do.call(mcp, structure(list("Tukey"), .Names=names(x$model)[2]))), adjusted('bonferroni'))
		)
	} else if (ncol(x$model)>2 && !is.numeric(x$model[[2]]) && !is.numeric(x$model[[3]])) {
		# two way anova
		newDF=data.frame(resp=x$model[[1]], pred=interaction(x$model[[2]], x$model[[3]]))
		setnames(newDF, names(newDF), c(names(x$model)[1], paste(names(x$model)[2:3], collapse='.and.')))
		x2=update(x, formula=reformulate(response=names(newDF)[1], termlabels=paste(names(x$model)[2:3], collapse='.and.')), data=newDF)
		txt=capture.output(
			summary(glht(x2, linfct=do.call(mcp, structure(list("Tukey"), .Names=names(x2$model)[2]))), adjusted('bonferroni'))
		)
	} else {
		# not valid for post-hoc
		return(div())
	}
	## return ui
	return(uiWrapper('Model Post-Hoc', txt))
}





