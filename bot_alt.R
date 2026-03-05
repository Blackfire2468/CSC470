library(shiny)
library(shinychat)
library(stringr)

ui <- bslib::page_fillable(
  chat_ui(
    id = "chat",
    messages = str_c(readLines("initial.md"), collapse = "\n")
  ),
  fillable_mobile = TRUE
)

server <- function(input, output, session) {
  chat <-
    ellmer::chat_anthropic(
      system_prompt = "You help people learn about data sets i the R programming environment.",
      
    )
  
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

shinyApp(ui, server)

