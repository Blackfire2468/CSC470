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

get_packages_with_data <- function() {
  pkgs <- rownames(installed.packages())
  has_data <- vapply(pkgs, function(pkg) {
    tryCatch({
      d <- data(package = pkg)$results
      !is.null(d) && nrow(d) > 0
    }, error = function(e) FALSE)
  }, logical(1))
  sort(pkgs[has_data])
}

get_package_datasets <- function(pkg) {
  d <- tryCatch(data(package = pkg)$results, error = function(e) NULL)
  if (!is.null(d) && nrow(d) > 0) return(sort(d[, "Item"]))
  character(0)
}

load_dataset <- function(pkg, dataset) {
  env <- new.env(parent = emptyenv())
  tryCatch({
    data(list = dataset, package = pkg, envir = env)
    obj_name <- ls(env)[1]
    if (length(obj_name) == 0 || is.na(obj_name)) return(NULL)
    obj <- get(obj_name, envir = env)
    if (is.data.frame(obj)) obj else NULL
  }, error = function(e) NULL)
}

parse_switch_tag <- function(response) {
  m <- regmatches(
    response,
    regexpr(
      '<switch_dataset\\s+pkg=["\']([^"\']+)["\']\\s+ds=["\']([^"\']+)["\']\\s*/>',
      response,
      perl = TRUE
    )
  )
  if (length(m) == 0) return(NULL)
  list(
    pkg = sub('.*pkg=["\']([^"\']+)["\'].*', '\\1', m, perl = TRUE),
    ds  = sub('.*ds=["\']([^"\']+)["\'].*',  '\\1', m, perl = TRUE)
  )
}

strip_switch_tag <- function(response) {
  str_trim(gsub(
    '<switch_dataset\\s+pkg=["\']([^"\']+)["\']\\s+ds=["\']([^"\']+)["\']\\s*/>',
    '', response, perl = TRUE
  ))
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- bslib::page_fillable(
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 260,
      title = "Data source",
      selectInput("pkg_select", label = "Package", choices = character(0), selected = NULL),
      selectInput("ds_select",  label = "Dataset", choices = character(0), selected = NULL),
      uiOutput("dataset_info")
    ),
    chat_ui(
      id       = "chat",
      messages = str_c(readLines("initial.md"), collapse = "\n")
    )
  ),
  fillable_mobile = TRUE
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Single source of truth for what is active — never read input$pkg_select
  # or input$ds_select directly after init; always use these reactiveValues
  rv <- reactiveValues(
    pkg      = NULL,
    ds       = NULL,
    pkg_list = character(0)
  )
  
  # ── startup: build package list and seed first selection ──────────────────
  observe({
    pkg_list <- get_packages_with_data()
    rv$pkg_list <- pkg_list
    
    if (length(pkg_list) > 0) {
      first_pkg      <- pkg_list[1]
      first_datasets <- get_package_datasets(first_pkg)
      first_ds       <- if (length(first_datasets) > 0) first_datasets[1] else NULL
      
      rv$pkg <- first_pkg
      rv$ds  <- first_ds
      
      updateSelectInput(session, "pkg_select",
                        choices  = pkg_list,
                        selected = first_pkg)
      updateSelectInput(session, "ds_select",
                        choices  = first_datasets,
                        selected = first_ds)
    }
  })
  
  # ── user manually changes package ─────────────────────────────────────────
  observeEvent(input$pkg_select, {
    req(input$pkg_select)
    # Only act on genuine user-driven changes, not programmatic ones we issued
    if (identical(input$pkg_select, rv$pkg)) return()
    
    new_datasets <- get_package_datasets(input$pkg_select)
    new_ds       <- if (length(new_datasets) > 0) new_datasets[1] else NULL
    
    rv$pkg <- input$pkg_select
    rv$ds  <- new_ds
    
    updateSelectInput(session, "ds_select",
                      choices  = new_datasets,
                      selected = new_ds)
  }, ignoreInit = TRUE)
  
  # ── user manually changes dataset ─────────────────────────────────────────
  observeEvent(input$ds_select, {
    req(input$ds_select)
    if (identical(input$ds_select, rv$ds)) return()
    rv$ds <- input$ds_select
  }, ignoreInit = TRUE)
  
  # ── active data frame (driven by rv, not inputs) ──────────────────────────
  active_df <- reactive({
    req(rv$pkg, rv$ds)
    load_dataset(rv$pkg, rv$ds)
  })
  
  # ── sidebar info card ──────────────────────────────────────────────────────
  output$dataset_info <- renderUI({
    req(rv$pkg, rv$ds)
    df <- active_df()
    if (is.null(df)) return(NULL)
    tags$div(
      style = paste(
        "margin-top:8px; padding:10px 12px;",
        "background:var(--bs-body-bg,#f8f9fa);",
        "border:1px solid #dee2e6; border-radius:8px; font-size:13px;"
      ),
      tags$strong(paste0(rv$pkg, "::", rv$ds)),
      tags$br(),
      tags$span(style = "color:#6c757d;",
                paste0(nrow(df), " rows \u00b7 ", ncol(df), " cols"))
    )
  })
  
  # ── apply a dataset switch (called from chat observer) ────────────────────
  do_switch <- function(new_pkg, new_ds) {
    new_datasets <- get_package_datasets(new_pkg)
    
    # Exact match first, then case-insensitive fallback
    matched_ds <- if (new_ds %in% new_datasets) {
      new_ds
    } else {
      ci <- new_datasets[tolower(new_datasets) == tolower(new_ds)]
      if (length(ci) == 1) ci else NULL
    }
    
    if (is.null(matched_ds)) {
      message("Switch requested but dataset not found: ", new_pkg, "::", new_ds)
      return(FALSE)
    }
    
    # Update source of truth first
    rv$pkg <- new_pkg
    rv$ds  <- matched_ds
    
    # Then push to UI
    updateSelectInput(session, "pkg_select",
                      choices  = rv$pkg_list,
                      selected = new_pkg)
    updateSelectInput(session, "ds_select",
                      choices  = new_datasets,
                      selected = matched_ds)
    
    message("Switched sidebar to: ", new_pkg, "::", matched_ds)
    return(TRUE)
  }
  
  # ── chat setup ─────────────────────────────────────────────────────────────
  system_prompt_text <- read_system_prompt("prompt.Rmd")
  chat_obj <- chat_anthropic(system_prompt = system_prompt_text)
  
  # ── handle user messages ───────────────────────────────────────────────────
  observeEvent(input$chat_user_input, {
    req(input$chat_user_input)
    
    cur_pkg <- rv$pkg
    cur_ds  <- rv$ds
    cur_df  <- if (!is.null(cur_pkg) && !is.null(cur_ds))
      load_dataset(cur_pkg, cur_ds)
    else NULL
    
    # Build data context from rv — always accurate even right after a switch
    data_context <- if (!is.null(cur_df)) {
      paste0(
        "Currently loaded dataset: `", cur_ds,
        "` from package `", cur_pkg, "`.\n",
        "Dimensions: ", nrow(cur_df), " rows x ", ncol(cur_df), " columns.\n",
        "Columns: ", paste(names(cur_df), collapse = ", "), ".\n",
        "In any R code you write, the data frame is already available as `",
        cur_ds, "` — do NOT call data() or library() to load it."
      )
    } else {
      "No dataset is currently loaded."
    }
    
    augmented_msg <- paste0(
      "[DATA CONTEXT — do not repeat this to the user]\n",
      data_context, "\n\n",
      "User: ", input$chat_user_input
    )
    
    response <- chat_obj$chat(augmented_msg)
    
    # ── parse and apply switch tag ─────────────────────────────────────────
    tag      <- parse_switch_tag(response)
    switched_pkg <- NULL
    switched_ds  <- NULL
    
    if (!is.null(tag)) {
      response <- strip_switch_tag(response)
      if (tag$pkg %in% rv$pkg_list) {
        ok <- do_switch(tag$pkg, tag$ds)
        if (ok) {
          switched_pkg <- rv$pkg
          switched_ds  <- rv$ds
        }
      }
    }
    
    # ── execute R code block ───────────────────────────────────────────────
    code_match <- regmatches(
      response,
      regexpr("(?i)```r\\s*([\\s\\S]*?)```", response, perl = TRUE)
    )
    
    if (length(code_match) > 0) {
      r_code <- sub("(?i)```r\\s*", "", code_match, perl = TRUE)
      r_code <- sub("```$", "", r_code)
      
      temp_env <- new.env(parent = globalenv())
      
      eval_pkg <- if (!is.null(switched_pkg)) switched_pkg else cur_pkg
      eval_ds  <- if (!is.null(switched_ds))  switched_ds  else cur_ds
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
}

shinyApp(ui, server)