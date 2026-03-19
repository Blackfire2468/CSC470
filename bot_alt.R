library(shiny)
library(shinychat)
library(stringr)
library(ellmer)
library(ggplot2)
library(dplyr)
library(base64enc)

read_system_prompt <- function(path = "prompt.Rmd") {
  lines <- readLines(path, warn = FALSE)
  
  start <- match("<!-- START_PROMPT -->", lines)
  end   <- match("<!-- END_PROMPT -->", lines)
  
  if (is.na(start) || is.na(end) || end <= start) {
    return(paste(lines, collapse = "\n"))
  }
  
  paste(lines[(start + 1):(end - 1)], collapse = "\n")
}

ui <- bslib::page_fillable(
  chat_ui(
    id = "chat",
    messages = str_c(readLines("initial.md"), collapse = "\n")
  ),
  fillable_mobile = TRUE
)

server <- function(input, output, session) {
  
  system_prompt_text <- read_system_prompt("prompt.Rmd")
  
  chat <- chat_anthropic(
    system_prompt = system_prompt_text
  )
  
  observeEvent(input$chat_user_input, {
    
    response <- chat$chat(input$chat_user_input)
    
    code_match <- str_match(response, "```r\\s*([\\s\\S]*?)```")
    
    if (!is.na(code_match[1,2])) {
      
      r_code <- code_match[1,2]
      temp_env <- new.env(parent = globalenv())
      
      result <- try(eval(parse(text = r_code), envir = temp_env), silent = TRUE)
      
      # If ggplot object → render to temp file and inject image
      if (inherits(result, "ggplot")) {
        
        file <- tempfile(fileext = ".png")
        ggsave(file, result, width = 7, height = 5)
        
        # Convert image to base64
        img_data <- base64enc::dataURI(file = file, mime = "image/png")
        
        chat_append(
          "chat",
          div(
            tags$img(
              src = img_data,
              style = "max-width:100%; border-radius:12px;"
            )
          )
        )
      }
      
    } else {
      # If normal text response
      chat_append("chat", response)
    }
  })
}

shinyApp(ui, server)
