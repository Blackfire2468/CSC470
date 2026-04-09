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
    
    df     <- active_df()
    user_q <- input$chat_user_input
    
    data_context <- if (!is.null(df)) {
      paste0(
        "Currently loaded dataset: `", input$ds_select,
        "` from package `", input$pkg_select, "`.\n",
        "Dimensions: ", nrow(df), " rows x ", ncol(df), " columns.\n",
        "Columns: ", paste(names(df), collapse = ", "), ".\n",
        "In any R code you write, the data frame is already available as `",
        input$ds_select, "` — do NOT call data() or library() to load it."
      )
    } else {
      "No dataset is currently loaded."
    }
    
    augmented_msg <- paste0(
      "[DATA CONTEXT — do not repeat this to the user]\n",
      data_context, "\n\n",
      "User: ", user_q
    )
    
    response <- chat$chat(augmented_msg)
    
    # ── 1. Parse switch tag ────────────────────────────────────────────────────
    switch_match <- regmatches(
      response,
      regexpr(
        '<switch_dataset\\s+pkg=["\']([^"\']+)["\']\\s+ds=["\']([^"\']+)["\']\\s*/>',
        response,
        perl = TRUE
      )
    )
    
    new_pkg <- NULL
    new_ds  <- NULL
    
    if (length(switch_match) > 0) {
      # Extract attributes directly from the matched tag
      new_pkg <- sub('.*pkg=["\']([^"\']+)["\'].*', '\\1', switch_match)
      new_ds  <- sub('.*ds=["\']([^"\']+)["\'].*',  '\\1', switch_match)
      
      # Strip tag from displayed response
      response <- str_trim(
        gsub(
          '<switch_dataset\\s+pkg=["\']([^"\']+)["\']\\s+ds=["\']([^"\']+)["\']\\s*/>',
          '',
          response,
          perl = TRUE
        )
      )
      
      # Validate and apply the switch
      if (new_pkg %in% pkg_list) {
        new_datasets <- get_package_datasets(new_pkg)
        
        if (new_ds %in% new_datasets) {
          # Update package first, then immediately update dataset choices + selection
          updateSelectInput(session, "pkg_select", selected = new_pkg)
          updateSelectInput(session, "ds_select",
                            choices  = new_datasets,
                            selected = new_ds)
        } else {
          # Dataset name didn't match exactly — try case-insensitive fallback
          ci_match <- new_datasets[tolower(new_datasets) == tolower(new_ds)]
          if (length(ci_match) == 1) {
            new_ds <- ci_match
            updateSelectInput(session, "pkg_select", selected = new_pkg)
            updateSelectInput(session, "ds_select",
                              choices  = new_datasets,
                              selected = new_ds)
          }
        }
      }
    }
    
    # ── 2. Execute R code block ────────────────────────────────────────────────
    code_match <- regmatches(
      response,
      regexpr("(?i)```r\\s*([\\s\\S]*?)```", response, perl = TRUE)
    )
    
    if (length(code_match) > 0) {
      r_code   <- sub("(?i)```r\\s*", "", code_match, perl = TRUE)
      r_code   <- sub("```$", "", r_code)
      
      temp_env <- new.env(parent = globalenv())
      
      # Use the switched dataset if a switch just happened, otherwise current
      eval_pkg <- if (!is.null(new_pkg) && new_pkg %in% pkg_list) new_pkg else input$pkg_select
      eval_ds  <- if (!is.null(new_ds))  new_ds  else input$ds_select
      eval_df  <- load_dataset(eval_pkg, eval_ds)
      
      if (!is.null(eval_df)) assign(eval_ds, eval_df, envir = temp_env)
      
      result <- try(eval(parse(text = r_code), envir = temp_env), silent = TRUE)
      
      if (inherits(result, "ggplot")) {
        file     <- tempfile(fileext = ".png")
        ggsave(file, result, width = 7, height = 5)
        img_data <- base64enc::dataURI(file = file, mime = "image/png")
        chat_append(
          "chat",
          div(tags$img(src = img_data, style = "max-width:100%; border-radius:12px;"))
        )
        return()
      }
    }
    
    chat_append("chat", response)
  })

shinyApp(ui, server)