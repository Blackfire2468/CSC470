library(shiny)
library(shinychat)
library(stringr)
library(ellmer)
library(ggplot2)
library(dplyr)
library(base64enc)

Sys.setenv(ANTHROPIC_API_KEY = readLines("api-key.txt"))

read_system_prompt <- function(path = "prompt.Rmd") {
  lines <- readLines(path, warn = FALSE)
  start <- match("<!-- START_PROMPT -->", lines)
  end   <- match("<!-- END_PROMPT -->", lines)
  if (is.na(start) || is.na(end) || end <= start) {
    return(paste(lines, collapse = "\n"))
  }
  paste(lines[(start + 1):(end - 1)], collapse = "\n")
}

# Find all installed packages that contain at least one data frame
get_packages_with_data <- function() {
  pkgs <- rownames(installed.packages())
  has_data <- vapply(pkgs, function(pkg) {
    tryCatch({
      objs <- ls(paste0("package:", pkg), all.names = FALSE)
      if (length(objs) == 0) {
        # Package not loaded yet — check its data() listing
        d <- tryCatch(
          data(package = pkg)$results,
          error = function(e) NULL
        )
        return(!is.null(d) && nrow(d) > 0)
      }
      any(vapply(objs, function(o) {
        tryCatch(is.data.frame(get(o, envir = as.environment(paste0("package:", pkg)))),
                 error = function(e) FALSE)
      }, logical(1)))
    }, error = function(e) {
      d <- tryCatch(data(package = pkg)$results, error = function(e) NULL)
      !is.null(d) && nrow(d) > 0
    })
  }, logical(1))
  sort(pkgs[has_data])
}

# Get data frame names available from a package
get_package_datasets <- function(pkg) {
  d <- tryCatch(data(package = pkg)$results, error = function(e) NULL)
  if (!is.null(d) && nrow(d) > 0) {
    return(sort(d[, "Item"]))
  }
  character(0)
}

# Load a specific dataset from a package and return the data frame
load_dataset <- function(pkg, dataset) {
  env <- new.env(parent = emptyenv())
  tryCatch({
    data(list = dataset, package = pkg, envir = env)
    obj <- get(ls(env)[1], envir = env)
    if (is.data.frame(obj)) obj else NULL
  }, error = function(e) NULL)
}

ui <- bslib::page_fillable(
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 260,
      title = "Data source",
      
      selectInput(
        "pkg_select",
        label = "Package",
        choices  = character(0),
        selected = NULL
      ),
      
      selectInput(
        "ds_select",
        label = "Dataset",
        choices  = character(0),
        selected = NULL
      ),
      
      uiOutput("dataset_info")
    ),
    
    chat_ui(
      id       = "chat",
      messages = str_c(readLines("initial.md"), collapse = "\n")
    )
  ),
  fillable_mobile = TRUE
)

server <- function(input, output, session) {
  
  # ── load package list once on startup ──────────────────────────────────────
  pkg_list <- get_packages_with_data()
  updateSelectInput(session, "pkg_select", choices = pkg_list, selected = pkg_list[1])
  
  # ── when package changes, refresh dataset dropdown ─────────────────────────
  observeEvent(input$pkg_select, {
    req(input$pkg_select)
    datasets <- get_package_datasets(input$pkg_select)
    updateSelectInput(session, "ds_select", choices = datasets,
                      selected = if (length(datasets)) datasets[1] else NULL)
  })
  
  # ── reactive: currently loaded data frame ──────────────────────────────────
  active_df <- reactive({
    req(input$pkg_select, input$ds_select)
    load_dataset(input$pkg_select, input$ds_select)
  })
  
  # ── sidebar info card ──────────────────────────────────────────────────────
  output$dataset_info <- renderUI({
    df <- active_df()
    if (is.null(df)) return(NULL)
    tags$div(
      style = paste(
        "margin-top:8px; padding:10px 12px;",
        "background:var(--bs-body-bg,#f8f9fa);",
        "border:1px solid #dee2e6; border-radius:8px; font-size:13px;"
      ),
      tags$strong(paste0(input$pkg_select, "::", input$ds_select)),
      tags$br(),
      tags$span(
        style = "color:#6c757d;",
        paste0(nrow(df), " rows \u00b7 ", ncol(df), " cols")
      )
    )
  })
  
  # ── chat setup ─────────────────────────────────────────────────────────────
  system_prompt_text <- read_system_prompt("prompt.Rmd")
  chat <- chat_anthropic(system_prompt = system_prompt_text)
  
  # ── when the user sends a message ──────────────────────────────────────────
  observeEvent(input$chat_user_input, {
    req(input$chat_user_input)
    
    df      <- active_df()
    user_q  <- input$chat_user_input
    
    # Build a context block so the LLM knows what data is available
    data_context <- if (!is.null(df)) {
      paste0(
        "The user has loaded the dataset `", input$ds_select,
        "` from the `", input$pkg_select, "` package.\n",
        "It has ", nrow(df), " rows and ", ncol(df), " columns: ",
        paste(names(df), collapse = ", "), ".\n",
        "When writing R code that uses this dataset, load it with:\n",
        "  data('", input$ds_select, "', package='", input$pkg_select, "')\n",
        "then refer to the object `", input$ds_select, "`."
      )
    } else {
      "No dataset is currently loaded."
    }
    
    # Prepend context invisibly to the user message
    augmented_msg <- paste0(
      "[DATA CONTEXT — do not show this to the user]\n",
      data_context, "\n\n",
      "User question: ", user_q
    )
    
    response <- chat$chat(augmented_msg)
    
    code_match <- str_match(response, "```r\\s*([\\s\\S]*?)```")
    
    if (!is.na(code_match[1, 2])) {
      
      r_code  <- code_match[1, 2]
      temp_env <- new.env(parent = globalenv())
      
      # Make the active dataset available in the eval environment
      if (!is.null(df)) {
        assign(input$ds_select, df, envir = temp_env)
      }
      
      result <- try(eval(parse(text = r_code), envir = temp_env), silent = TRUE)
      
      if (inherits(result, "ggplot")) {
        file     <- tempfile(fileext = ".png")
        ggsave(file, result, width = 7, height = 5)
        img_data <- base64enc::dataURI(file = file, mime = "image/png")
        chat_append(
          "chat",
          div(tags$img(src = img_data,
                       style = "max-width:100%; border-radius:12px;"))
        )
      } else {
        # Still show the text response even if code ran
        chat_append("chat", response)
      }
      
    } else {
      chat_append("chat", response)
    }
    
    # ── switch the sidebar to the dataset the bot referenced ─────────────────
    # If the response mentions a package::dataset pattern, switch the UI to it
    ref_match <- str_match(response, "`?(\\w+)::(\\w+)`?")
    if (!is.na(ref_match[1, 2])) {
      ref_pkg <- ref_match[1, 2]
      ref_ds  <- ref_match[1, 3]
      if (ref_pkg %in% pkg_list) {
        updateSelectInput(session, "pkg_select", selected = ref_pkg)
        # slight delay so dataset list updates first
        later::later(function() {
          updateSelectInput(session, "ds_select", selected = ref_ds)
        }, delay = 0.3)
      }
    }
  })
}

shinyApp(ui, server)