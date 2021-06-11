# This is the demo word-predictor application. You can run the application by
# clicking 'Run App' above.
#
# The application allows users to enter a set of words. For the given words the
# application attempts to predict the top ten most likely words. These words are
# presented in a bar plot along with the respective probabilities.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/

library(shiny)
library(ggplot2)
library(wordpredictor)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Word Predictor"),
    # Horizontal rule
    hr(),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # The input field
            textInput("ngram", "Enter a n-gram:", value = "where is")
        ),

        # Show a plot of the possible predicted words
        mainPanel(
            # The predicted word
            uiOutput("next_word"),
            # The predicted word probability
            uiOutput("word_prob"),
            # Horizontal rule
            hr(),
            # The bar plot of possible next words
            plotOutput("next_word_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # The model file path
    sfp <- system.file("extdata", "def-model.RDS", package = "wordpredictor")
    # The ModelPredictor object is created
    mp <- ModelPredictor$new(sfp)
    # The predicted word information
    p <- NULL

    # The next word is predicted
    output$next_word <- renderUI({
        # If the user entered some text
        if (trimws(input$ngram) != "") {
            # The text entered by the user is split on space
            w <- trimws(input$ngram)
            # The next word is predicted
            p <- mp$predict_word(w, 10)
            # If the next word was not found
            if (!p$found) {
                # The next word and next word is set to an information
                # message
                nw <- span("Not Found", style = "color:red")
                # The next word probability is set to an information
                # message
                nwp <- span("N.A", style = "color:red")
                # The plot is set to empty
                output$next_word_plot <- renderPlot({})
                # The predicted next word
                nw <- tags$div("Predicted Word: ", tags$strong(nw))
                # The predicted next word probability
                nwp <- tags$div("Word Probability: ", tags$strong(nwp))
                # The next word probability is updated
                output$word_prob <- renderUI(nwp)
            }
            else {
                # The next word
                nw <- p$words[[1]]
                # The next word probability
                nwp <- p$probs[[1]]
                # The plot is updated
                output$next_word_plot <- renderPlot({
                    # A data frame containing the data to plot
                    df <- data.frame("word" = p$words, "prob" = p$probs)
                    # The data frame is sorted in descending order
                    df <- (df[order(df$prob, decreasing = T),])
                    # The words and their probabilities are plotted
                    g <- ggplot(data = df, aes(x = reorder(word, prob), y = prob)) +
                        geom_bar(stat = "identity", fill = "red") +
                        ggtitle("Predicted words and their probabilities") +
                        ylab("Probability") +
                        xlab("Word")
                    print(g)
                })
                # The predicted next word
                nw <- tags$div("Predicted Word: ", tags$strong(nw))
                # The predicted next word probability
                nwp <- tags$div("Word Probability: ", tags$strong(nwp))
                # The next word probability is updated
                output$word_prob <- renderUI(nwp)
            }
        }
        else {
            # The next word is set to ""
            nw <- tags$span()
            # The next word probability text is set to ""
            output$word_prob <- renderUI(tags$span())
            # The plot is set to empty
            output$next_word_plot <- renderPlot({})
        }
        return(nw)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
