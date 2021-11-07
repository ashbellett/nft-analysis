library("shiny")

windowTitle <- "Ethermore NFT Analysis"
pageTitle <- "Impact of Ethermore NFT character attributes on token value"
choices <- c(
    "Alignment",
    "Race",
    "Class",
    "Title",
    "Background",
    "Item"
)
choicePrompt <- "Choose an attribute"
plotTitle <- "Relative importance of token attribute to token value"
plotCaption0 <- "Token value is taken as a token's mean sale price according to historical, on-chain NFT transactions."
plotCaption1 <- "Feature importance is taken as the coefficients of a linear regression model trained on the attributes to predict mean sale price."

ui <- fluidPage(
    titlePanel(
        h3(pageTitle),
        windowTitle=windowTitle
    ),
    sidebarPanel(
        selectInput(
            "dataset",
            choicePrompt,
            choices = choices
        )
    ),
    mainPanel(
        h4(plotTitle),
        plotOutput('view'),
        p(plotCaption0),
        p(plotCaption1)
    )
)
