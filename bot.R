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
  
  rv <- reactiveValues(
    pkg      = NULL,
    ds       = NULL,
    pkg_list = character(0)
  )
  
  # ── startup ────────────────────────────────────────────────────────────────
  observe({
    pkg_list <- get_packages_with_data()
    rv$pkg_list <- pkg_list
    if (length(pkg_list) > 0) {
      first_pkg      <- pkg_list[1]
      first_datasets <- get_package_datasets(first_pkg)
      first_ds       <- if (length(first_datasets) > 0) first_datasets[1] else NULL
      rv$pkg <- first_pkg
      rv$ds  <- first_ds
      updateSelectInput(session, "pkg_select", choices = pkg_list,       selected = first_pkg)
      updateSelectInput(session, "ds_select",  choices = first_datasets, selected = first_ds)
    }
  })
  
  # ── user manually changes package ─────────────────────────────────────────
  observeEvent(input$pkg_select, {
    req(input$pkg_select)
    if (identical(input$pkg_select, rv$pkg)) return()
    new_datasets <- get_package_datasets(input$pkg_select)
    new_ds       <- if (length(new_datasets) > 0) new_datasets[1] else NULL
    rv$pkg <- input$pkg_select
    rv$ds  <- new_ds
    updateSelectInput(session, "ds_select", choices = new_datasets, selected = new_ds)
  }, ignoreInit = TRUE)
  
  # ── user manually changes dataset ─────────────────────────────────────────
  observeEvent(input$ds_select, {
    req(input$ds_select)
    if (identical(input$ds_select, rv$ds)) return()
    rv$ds <- input$ds_select
  }, ignoreInit = TRUE)
  
  # ── active data frame ──────────────────────────────────────────────────────
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
                paste0(nrow(active_df()), " rows \u00b7 ", ncol(active_df()), " cols"))
    )
  })
  
  # ── switch sidebar helper ──────────────────────────────────────────────────
  do_switch <- function(new_pkg, new_ds) {
    new_datasets <- get_package_datasets(new_pkg)
    matched_ds <- if (new_ds %in% new_datasets) {
      new_ds
    } else {
      ci <- new_datasets[tolower(new_datasets) == tolower(new_ds)]
      if (length(ci) == 1) ci else NULL
    }
    if (is.null(matched_ds)) return(FALSE)
    rv$pkg <- new_pkg
    rv$ds  <- matched_ds
    updateSelectInput(session, "pkg_select", choices = rv$pkg_list,  selected = new_pkg)
    updateSelectInput(session, "ds_select",  choices = new_datasets, selected = matched_ds)
    TRUE
  }
  
  # ── define tools for the bot ───────────────────────────────────────────────
  
  # Tool 1: switch the active dataset
  tool_switch_dataset <- tool(
    name        = "switch_dataset",
    description = "Switch the active dataset shown in the sidebar. Call this whenever the user asks about a different dataset or package.",
    arguments   = list(
      pkg = type_string("The exact R package name, e.g. 'ggplot2', 'datasets', 'Lahman'"),
      ds  = type_string("The exact dataset name as it appears in R, e.g. 'mpg', 'iris', 'Batting'")
    ),
    fun = function(pkg, ds) {
      if (!pkg %in% rv$pkg_list) {
        return(paste0("Package '", pkg, "' not found in installed packages with data."))
      }
      ok <- do_switch(pkg, ds)
      if (ok) {
        df <- load_dataset(rv$pkg, rv$ds)
        if (!is.null(df)) {
          paste0(
            "Switched to ", pkg, "::", rv$ds, ". ",
            "It has ", nrow(df), " rows and ", ncol(df),
            " columns: ", paste(names(df), collapse = ", "), "."
          )
        } else {
          paste0("Switched to ", pkg, "::", rv$ds, " but could not load the data frame.")
        }
      } else {
        paste0("Could not find dataset '", ds, "' in package '", pkg, "'.")
      }
    }
  )
  
  # Tool 2: get dataset info / structure
  tool_dataset_info <- tool(
    name        = "get_dataset_info",
    description = "Get the structure, column names, types, and a sample of rows from the currently active dataset or any specified dataset.",
    arguments   = list(
      pkg = type_string("Package name. Use 'current' to use whatever is active in the sidebar."),
      ds  = type_string("Dataset name. Use 'current' to use whatever is active in the sidebar.")
    ),
    fun = function(pkg, ds) {
      use_pkg <- if (pkg == "current") rv$pkg else pkg
      use_ds  <- if (ds  == "current") rv$ds  else ds
      if (is.null(use_pkg) || is.null(use_ds)) return("No dataset is currently loaded.")
      df <- load_dataset(use_pkg, use_ds)
      if (is.null(df)) return(paste0("Could not load ", use_pkg, "::", use_ds))
      col_info <- vapply(names(df), function(col) {
        paste0(col, " (", class(df[[col]])[1], ")")
      }, character(1))
      paste0(
        "Dataset: ", use_pkg, "::", use_ds, "\n",
        "Rows: ", nrow(df), "\n",
        "Columns: ", ncol(df), "\n",
        "Column details: ", paste(col_info, collapse = ", "), "\n",
        "First 3 rows:\n", paste(capture.output(print(head(df, 3))), collapse = "\n")
      )
    }
  )
  
  # Tool 3: run a summary / computation and return text
  tool_summarise <- tool(
    name        = "summarise_data",
    description = "Run a statistical summary or grouped computation on the active dataset. Returns a text summary. Use this for counts, averages, breakdowns, top values.",
    arguments   = list(
      code = type_string("Valid R code that uses a variable called `df` (the active data frame) and returns a data frame or named vector of results.")
    ),
    fun = function(code) {
      df <- load_dataset(rv$pkg, rv$ds)
      if (is.null(df)) return("No dataset loaded.")
      env <- new.env(parent = baseenv())
      # make common packages available
      env$df      <- df
      env$dplyr   <- asNamespace("dplyr")
      suppressPackageStartupMessages({
        env$`%>%` <- magrittr::`%>%`
        env$n      <- dplyr::n
        env$summarise <- dplyr::summarise
        env$group_by  <- dplyr::group_by
        env$arrange   <- dplyr::arrange
        env$filter    <- dplyr::filter
        env$mutate    <- dplyr::mutate
        env$select    <- dplyr::select
        env$desc      <- dplyr::desc
      })
      result <- tryCatch(
        eval(parse(text = code), envir = env),
        error = function(e) paste0("Error: ", conditionMessage(e))
      )
      if (is.character(result)) return(result)
      paste(capture.output(print(result)), collapse = "\n")
    }
  )
  
  # Tool 4: make a ggplot and render it into the chat
  tool_plot <- tool(
    name        = "make_plot",
    description = "Create a ggplot visualisation from the active dataset and display it in the chat. Use for trends, distributions, relationships, comparisons.",
    arguments   = list(
      code = type_string("Valid R code that uses a variable called `df` (the active data frame) and returns a ggplot object.")
    ),
    fun = function(code) {
      df <- load_dataset(rv$pkg, rv$ds)
      if (is.null(df)) return("No dataset loaded.")
      env <- new.env(parent = baseenv())
      env$df <- df
      # expose ggplot2 and dplyr
      for (nm in getNamespaceExports("ggplot2")) {
        tryCatch(assign(nm, get(nm, envir = asNamespace("ggplot2")), envir = env),
                 error = function(e) NULL)
      }
      for (nm in getNamespaceExports("dplyr")) {
        tryCatch(assign(nm, get(nm, envir = asNamespace("dplyr")), envir = env),
                 error = function(e) NULL)
      }
      result <- tryCatch(
        eval(parse(text = code), envir = env),
        error = function(e) paste0("Error: ", conditionMessage(e))
      )
      if (is.character(result)) return(result)
      if (!inherits(result, "ggplot")) return("Code did not return a ggplot object.")
      file     <- tempfile(fileext = ".png")
      ggsave(file, result, width = 7, height = 5, dpi = 150)
      img_data <- base64enc::dataURI(file = file, mime = "image/png")
      # inject the image into chat directly from the tool
      chat_append(
        "chat",
        div(tags$img(src = img_data, style = "max-width:100%; border-radius:12px;"))
      )
      "Plot rendered successfully."
    }
  )
  
  # Tool 5: list available datasets for a package
  tool_list_datasets <- tool(
    name        = "list_datasets",
    description = "List all available datasets in a given package.",
    arguments   = list(
      pkg = type_string("The R package name to list datasets for.")
    ),
    fun = function(pkg) {
      if (!pkg %in% rv$pkg_list) return(paste0("Package '", pkg, "' not available."))
      ds <- get_package_datasets(pkg)
      if (length(ds) == 0) return(paste0("No datasets found in '", pkg, "'."))
      paste0("Datasets in '", pkg, "': ", paste(ds, collapse = ", "))
    }
  )
  
  # ── chat setup with tools ──────────────────────────────────────────────────
  system_prompt_text <- read_system_prompt("prompt.Rmd")
  
  chat_obj <- chat_anthropic(system_prompt = system_prompt_text)
  
  # Register all tools
  chat_obj$register_tool(tool_switch_dataset)
  chat_obj$register_tool(tool_dataset_info)
  chat_obj$register_tool(tool_summarise)
  chat_obj$register_tool(tool_plot)
  chat_obj$register_tool(tool_list_datasets)
  
  # ── handle user messages ───────────────────────────────────────────────────
  observeEvent(input$chat_user_input, {
    req(input$chat_user_input)
    
    cur_df <- load_dataset(rv$pkg, rv$ds)
    
    data_context <- if (!is.null(cur_df)) {
      paste0(
        "Currently active dataset: `", rv$ds,
        "` from package `", rv$pkg, "`.\n",
        "Dimensions: ", nrow(cur_df), " rows x ", ncol(cur_df), " columns.\n",
        "Columns: ", paste(names(cur_df), collapse = ", "), "."
      )
    } else {
      "No dataset is currently loaded."
    }
    
    augmented_msg <- paste0(
      "[DATA CONTEXT — do not repeat this to the user]\n",
      data_context, "\n\n",
      "User: ", input$chat_user_input
    )
    
    # chat_obj handles tool calls automatically in a loop until done
    response <- chat_obj$chat(augmented_msg)
    
    # Response is now pure text — no code blocks to parse, no switch tags needed
    if (!is.null(response) && nzchar(str_trim(response))) {
      chat_append("chat", response)
    }
  })
}

shinyApp(ui, server)