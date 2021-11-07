library("dplyr")
library("fmsb")
library("shiny")

prepare_dataset <- function(path) {
    data <- read.csv(
        paste0(Sys.getenv("DATA_RESULTS"), path)
    )
    columns <- data[,1]
    data <- t(select(data, coefficient))
    colnames(data) <- columns
    minimum <- rep(min(data), ncol(data))
    maximum <- rep(max(data), ncol(data))
    results <- rbind(maximum, minimum, data)
    return(results)
}

alignment <- prepare_dataset(Sys.getenv("MODEL_ALIGNMENT"))
race <- prepare_dataset(Sys.getenv("MODEL_RACE"))
class <- prepare_dataset(Sys.getenv("MODEL_CLASS"))
title <- prepare_dataset(Sys.getenv("MODEL_TITLE"))
background <- prepare_dataset(Sys.getenv("MODEL_BACKGROUND"))
item <- prepare_dataset(Sys.getenv("MODEL_ITEM"))

server <- function(input, output, session) {
    dataset <- reactive({
        switch(
            input$dataset,
            "Alignment"=alignment,
            "Race"=race,
            "Class"=class,
            "Title"=title,
            "Background"=background,
            "Item"=item
        )
    })
    output$view <- renderPlot({
        radarchart(as.data.frame(dataset()))
    })
}

#app <- shinyApp(ui=ui, server=server)
#runApp(appDir=app, port=strtoi(Sys.getenv("PORT")))
