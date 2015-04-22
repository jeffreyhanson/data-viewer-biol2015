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
	plot(0, type='n', bty='n', axes=FALSE)
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






