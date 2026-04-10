library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(tidytext)
library(tidyr)
library(forcats)
library(scales)
library(DT)
library(htmltools)

crl_long <- read_csv("crl_long2.csv", show_col_types = FALSE )
domain_unique <- read_csv("domain_unique.csv", show_col_types = FALSE)

crl_long <- crl_long %>%
  mutate(
    year = as.character(year),
    application_type = as.character(application_type),
    deficiency_domain = as.character(deficiency_domain),
    deficiency_subtype = as.character(deficiency_subtype)
  )

domain_unique <- domain_unique %>%
  mutate(
    year = as.character(year),
    application_type = as.character(application_type),
    deficiency_domain = as.character(deficiency_domain)
  )

# optional: keep/replace with your own palette if you already have one
domain_colors <- c(
  "CMC" = "#5B8E7D",
  "Clinical" = "#4F6D7A",
  "Nonclinical" = "#C06C84",
  "Regulatory" = "#7A6C5D",
  "Statistics" = "#8C6BB1",
  "Patent Certifications" = "#D4A373"
)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f4ee;
      }

      .well {
        background-color: #fcfbf8;
      }

      .summary-card {
        background: white;
        border-radius: 16px;
        padding: 18px 20px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.06);
        border-left: 6px solid #5B8E7D;
        margin-bottom: 12px;
        min-height: 120px;
      }

      .summary-label {
        font-size: 14px;
        color: #5f6b6d;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-weight: 700;
      }

      .summary-value {
        font-size: 30px;
        font-weight: 800;
        color: #24323a;
        margin-top: 8px;
      }

      .note-box {
        background: #fcfbf8;
        border-left: 5px solid #5B8E7D;
        padding: 16px 18px;
        border-radius: 10px;
        margin-bottom: 20px;
      }

      .method-box {
        background: white;
        padding: 18px;
        border-radius: 14px;
        box-shadow: 0 4px 14px rgba(0,0,0,0.05);
        margin-bottom: 18px;
      }

      .tab-content {
        padding-top: 10px;
      }

      .btn-default, .btn-primary {
        border-radius: 10px !important;
      }

      h4 {
        font-weight: 700;
      }
    "))
  ),
  
  div(
    style = "margin-bottom: 18px;",
    h2("FDA Complete Response Letter Dashboard", style = "font-weight: 800; margin-bottom: 4px;"),
    p(
      "Patterns in deficiency domains, subtype structure, and CRL composition across applications",
      style = "color: #5f6b6d; font-size: 15px; margin-top: 0;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "app_type",
        "Application Type",
        choices = c("All", "NDA", "BLA"),
        selected = "All"
      ),
      selectInput(
        "year",
        "Year",
        choices = c("All", sort(unique(crl_long$year))),
        selected = "All"
      ),
      selectInput(
        "domain",
        "Deficiency Domain",
        choices = c("All", "CMC", "Clinical", "Nonclinical", "Regulatory", "Statistics", "Patent Certifications"),
        selected = "All"
      ),
      radioButtons(
        "trend_metric",
        "Overview Trend Metric",
        choices = c("Count", "Percent"),
        selected = "Count",
        inline = TRUE
      ),
      
      hr(),
      h4("Download Summaries"),
      downloadButton("download_domain", "Download Domain Table"),
      br(), br(),
      downloadButton("download_subtype", "Download Subtype Table"),
      br(), br(),
      downloadButton("download_structure", "Download Structure Table")
    ),
    
    mainPanel(
      div(
        class = "note-box",
        HTML("<strong>Dashboard Note:</strong> Main results emphasize interpretable domain, subtype, and structural patterns. More granular supplementary analyses were retained outside the dashboard due to sparsity and limited interpretability.")
      ),
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          
          div(
            class = "note-box",
            h4("How to Read This Dashboard"),
            tags$ul(
              tags$li("Filters update all results."),
              tags$li("Overview shows domain patterns and trends."),
              tags$li("Subtype Analysis shows subtype composition."),
              tags$li("CRL Composition summarizes CRL structure."),
              tags$li("CMC Deep Dive focuses on CMC deficiencies.")
            ),
            p(strong("Note:"), " Percentages are calculated within the filtered data.")
          ),
          
          fluidRow(
            column(3, uiOutput("n_def_card")),
            column(3, uiOutput("n_crl_domain_card")),
            column(3, uiOutput("n_events_card")),
            column(3, uiOutput("top_domain_card"))
          ),
          
          br(),
          conditionalPanel(
            condition = "input.domain == 'All'",
            plotOutput("domain_plot", height = "450px")
          ),
          br(),
          plotOutput("domain_trend_plot", height = "420px"),
          br(),
          DTOutput("domain_table")
        ),
        
        tabPanel(
          "Subtype Analysis",
          br(),
          
          fluidRow(
            column(6, uiOutput("selected_domain_card")),
            column(6, uiOutput("top_subtype_card"))
          ),
          
          br(),
          plotOutput("subtype_plot", height = "500px"),
          br(),
          DTOutput("subtype_table")
        ),
        
        tabPanel(
          "CMC Deep Dive",
          br(),
          
          div(
            class = "note-box",
            h4("CMC Deep Dive"),
            p("This tab focuses specifically on Chemistry, Manufacturing, and Controls (CMC) deficiencies within the currently selected application type and year filters."),
            p("Because CMC is the most frequently observed domain, this view provides a more detailed look at subtype composition.")
          ),
          
          fluidRow(
            column(4, uiOutput("cmc_n_def_card")),
            column(4, uiOutput("cmc_n_domain_card")),
            column(4, uiOutput("cmc_top_subtype_card"))
          ),
          
          br(),
          plotOutput("cmc_plot", height = "450px"),
          br(),
          DTOutput("cmc_table")
        ),
        
        tabPanel(
          "CRL Composition",
          br(),
          plotOutput("structure_plot", height = "450px"),
          br(),
          DTOutput("cooccur_table")
        ),
        
        tabPanel(
          "Methods / Notes",
          fluidPage(
            tags$div(
              style = "padding: 20px; max-width: 1100px;",
              
              tags$h2("Methods / Notes"),
              tags$p(strong("STOP DATE:"), " 2/2/2026"),
              tags$p("Click each section to expand definitions."),
              tags$hr(),
              
              tags$details(
                open = TRUE,
                tags$summary(tags$strong("CRL_EVENTS")),
                tags$div(
                  style = "padding-top: 10px;",
                  tags$ul(
                    tags$li(strong("EVENT_ID:"), " ID number given by me for reference between tables."),
                    tags$li(strong("APPLICATION_NUMBER:"), " Number given by the FDA for the drug; can have multiple applications under one number."),
                    tags$li(strong("APPLICATION_TYPE:")),
                    tags$ul(
                      tags$li(strong("NDA:"), " New Drug Application"),
                      tags$li(strong("BLA:"), " Biologics License Application")
                    ),
                    tags$li(strong("CRL_DATE:"), " The date the CRL was given."),
                    tags$li(strong("YEAR:"), " The year the CRL was given.")
                  )
                )
              ),
              
              tags$br(),
              
              tags$details(
                open = TRUE,
                tags$summary(tags$strong("CRL_DEFICIENCIES")),
                tags$div(
                  style = "padding-top: 10px;",
                  tags$p("Deficiencies were classified based on the primary issues identified by the FDA for each individual deficiency statement and the action required to resolve it."),
                  tags$p("Deficiencies were coded at the event level, allowing for analysis at both the deficiency level and the CRL (application) level."),
                  tags$ul(
                    tags$li(strong("EVENT_ID")),
                    tags$li(strong("DEFICIENCY_DOMAIN: Clinical, Nonclinical, CMC, Statistics, Regulatory, Patent Certifications"))
                  )
                )
              ),
              
              tags$br(),
              
              tags$details(
                tags$summary(tags$strong("Clinical Subtypes")),
                tags$div(
                  style = "padding-top: 10px;",
                  tags$ul(
                    tags$li(strong("RISK-BENEFIT:"), " FDA determines that the overall benefits of the drug do not outweigh its risks despite evidence of efficacy and/or safety data."),
                    tags$li(strong("EFFICACY:"), " Failure to demonstrate substantial evidence of effectiveness, including failure to meet primary endpoints or lack of adequate and well-controlled trials."),
                    tags$li(strong("SAFETY:"), " Insufficient safety data, unacceptable adverse events, or inability to adequately characterize safety at proposed doses or populations."),
                    tags$li(strong("PHARMACOLOGY:"), " Inadequate pharmacokinetic/pharmacodynamic (PK/PD), bioavailability, dose-response, or exposure data needed to support dosing, effectiveness, or safety."),
                    tags$li(strong("STUDY DESIGN:"), " Flaws in trial design, including inappropriate endpoints, population selection, comparators, or trial structure that limit interpretability."),
                    tags$li(strong("DATA INTEGRITY:"), " Issues with data reliability, including missing data, inconsistent datasets, protocol deviations, or inability to verify results."),
                    tags$li(strong("HUMAN FACTORS:"), " Risks related to product use, including medication errors, labeling comprehension, device usability, or user interface issues."),
                    tags$li(strong("REDACTED:"), " Clinical deficiency identified, but insufficient detail available (e.g. redacted CRLs).")
                  )
                )
              ),
              
              tags$br(),
              
              tags$details(
                tags$summary(tags$strong("Nonclinical Subtypes")),
                tags$div(
                  style = "padding-top: 10px;",
                  tags$ul(
                    tags$li(strong("IMPURITY QUALIFICATION:"), " Inadequate data to support safety of impurities or leachables exceeding regulatory thresholds."),
                    tags$li(strong("INSUFFICIENT NONCLINICAL SUPPORT:"), " Missing or inadequate nonclinical studies required to support clinical use (general, not impurity-specific)."),
                    tags$li(strong("TOXICOLOGY:"), " Identified or potential toxic effects requiring additional toxicology studies or raising safety concerns."),
                    tags$li(strong("REDACTED:"), " Nonclinical deficiency identified, but insufficient detail available (e.g. redacted CRLs).")
                  )
                )
              ),
              
              tags$br(),
              
              tags$details(
                tags$summary(tags$strong("CMC Subtypes")),
                tags$div(
                  style = "padding-top: 10px;",
                  tags$ul(
                    tags$li(strong("FACILITY:"), " cGMP violations, inspection findings, or deficiencies related to manufacturing sites or facilities."),
                    tags$li(strong("MICROBIOLOGY:"), " Issues related to sterility, microbial contamination, preservative effectiveness, or aseptic processing."),
                    tags$li(strong("STABILITY:"), " Insufficient or inadequate stability data, including missing time points, batch data, or shelf-life justification."),
                    tags$li(strong("SPECIFICATIONS:"), " Inadequate or missing acceptance criteria, test methods, or quality attribute limits (e.g., assay, impurities, strength)."),
                    tags$li(strong("MANUFACTURING CONTROLS:"), " Issues with manufacturing processes, validation, batch records, controls, or process consistency."),
                    tags$li(strong("BIOPHARMACEUTICS:"), " Inadequate bridging, comparability, bioequivalence, or in vitro/in vivo performance linking to a reference product."),
                    tags$li(strong("REDACTED:"), " CMC deficiency identified, but insufficient detail available (e.g. redacted CRLs).")
                  )
                )
              ),
              
              tags$br(),
              
              tags$details(
                tags$summary(tags$strong("Statistics Subtypes")),
                tags$div(
                  style = "padding-top: 10px;",
                  tags$ul(
                    tags$li(strong("SAP (Statistical Analysis Plan) DEFICIENCIES:"), " Issues with the statistical analysis plan, including lack of prespecification, multiplicity control, or improper statistical methodology."),
                    tags$li(strong("MISSING/INADEQUATE ANALYSES:"), " Required analyses were not conducted, incomplete, or insufficiently reported."),
                    tags$li(strong("INSUFFICIENT EVIDENCE/NOT PERSUASIVE:"), " Statistical results are inconclusive or not convincing, even if analyses were performed."),
                    tags$li(strong("REDACTED:"), " Statistical deficiency identified, but insufficient detail available (e.g. redacted CRLs).")
                  )
                )
              ),
              
              tags$br(),
              
              tags$details(
                tags$summary(tags$strong("Regulatory Subtypes")),
                tags$div(
                  style = "padding-top: 10px;",
                  tags$ul(
                    tags$li(strong("ADMINISTRATIVE/FILING:"), " Application deficiencies related to submission requirements or missing documentation."),
                    tags$li(strong("LABELING:"), " Issues with prescribing information, formatting, or content of labeling."),
                    tags$li(strong("REMS/PMRs:"), " Need for or inadequacy of risk management strategies or post-marketing requirements."),
                    tags$li(strong("PREA:"), " Pediatric Study requirements not met."),
                    tags$li(strong("REDACTED:"), " Regulatory deficiency identified, but insufficient detail available (e.g. redacted CRLs).")
                  )
                )
              ),
              
              tags$br(),
              
              tags$details(
                tags$summary(tags$strong("Patent Certifications")),
                tags$div(
                  style = "padding-top: 10px;",
                  tags$ul(
                    tags$li(strong("PATENT CERTIFICATIONS:"), " Issues related to patent certification requirements under regulatory pathways.")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_long <- reactive({
    df <- crl_long
    
    if(input$app_type != "All") {
      df <- df %>% filter(application_type == input$app_type)
    }
    
    if(input$year != "All") {
      df <- df %>% filter(year == input$year)
    }
    
    if(input$domain != "All") {
      df <- df %>% filter(deficiency_domain == input$domain)
    }
    
    df
  })
  
  filtered_domain <- reactive({
    df <- domain_unique
    
    if(input$app_type != "All") {
      df <- df %>% filter(application_type == input$app_type)
    }
    
    if(input$year != "All") {
      df <- df %>% filter(year == input$year)
    }
    
    if(input$domain != "All") {
      df <- df %>% filter(deficiency_domain == input$domain)
    }
    
    df
  })
  
  filtered_structure <- reactive({
    df <- domain_unique
    
    if (input$app_type != "All") {
      df <- df %>% filter(application_type == input$app_type)
    }
    
    if (input$year != "All") {
      df <- df %>% filter(year == input$year)
    }
    
    df
  })
  
  filtered_cmc_long <- reactive({
    df <- crl_long
    
    if(input$app_type != "All") {
      df <- df %>% filter(application_type == input$app_type)
    }
    
    if(input$year != "All") {
      df <- df %>% filter(year == input$year)
    }
    
    df %>% filter(deficiency_domain == "CMC")
  })
  
  filtered_cmc_domain <- reactive({
    df <- domain_unique
    
    if(input$app_type != "All") {
      df <- df %>% filter(application_type == input$app_type)
    }
    
    if(input$year != "All") {
      df <- df %>% filter(year == input$year)
    }
    
    df %>% filter(deficiency_domain == "CMC")
  })
  
  trend_data <- reactive({
    
    if (input$domain == "All") {
      df <- domain_unique
      
      if (input$app_type != "All") {
        df <- df %>% filter(application_type == input$app_type)
      }
      
      if (input$year != "All") {
        df <- df %>% filter(year == input$year)
      }
      
      df <- df %>% filter(as.numeric(year) >= 2017)
      
      out <- df %>%
        count(year, deficiency_domain, name = "n")
      
      if (input$trend_metric == "Percent") {
        out <- out %>%
          group_by(year) %>%
          mutate(value = n / sum(n)) %>%
          ungroup()
      } else {
        out <- out %>%
          mutate(value = n)
      }
      
    } else {
      df <- domain_unique
      
      if (input$app_type != "All") {
        df <- df %>% filter(application_type == input$app_type)
      }
      
      if (input$year != "All") {
        df <- df %>% filter(year == input$year)
      }

      df <- df %>% filter(as.numeric(year) >= 2017)
      
      out <- df %>%
        filter(deficiency_domain == input$domain) %>%
        count(year, name = "n")
      
      if (input$trend_metric == "Percent") {
        year_totals <- domain_unique
        
        if (input$app_type != "All") {
          year_totals <- year_totals %>% filter(application_type == input$app_type)
        }
        
        if (input$year != "All") {
          year_totals <- year_totals %>% filter(year == input$year)
        }
        
        year_totals <- year_totals %>%
          filter(as.numeric(year) >= 2017) %>%
          count(year, name = "year_total")
        
        out <- out %>%
          left_join(year_totals, by = "year") %>%
          mutate(value = n / year_total)
      } else {
        out <- out %>%
          mutate(value = n)
      }
    }
    
    out %>%
      mutate(year = factor(year, levels = sort(unique(year))))
  })
  
  make_card <- function(label, value, border_color = "#5B8E7D") {
    div(
      class = "summary-card",
      style = paste0("border-left-color:", border_color, ";"),
      div(class = "summary-label", label),
      div(class = "summary-value", value)
    )
  }
  
  output$n_def <- renderText({
    nrow(filtered_long())
  })
  
  output$n_crl_domain <- renderText({
    nrow(filtered_domain())
  })
  
  output$n_events <- renderText({
    filtered_long() %>%
      distinct(event_id) %>%
      nrow()
  })
  
  output$n_def_card <- renderUI({
    make_card("Total Deficiencies", scales::comma(nrow(filtered_long())), "#5B8E7D")
  })
  
  output$n_crl_domain_card <- renderUI({
    make_card("Total CRL-Domain Records", scales::comma(nrow(filtered_domain())), "#4F6D7A")
  })
  
  output$n_events_card <- renderUI({
    val <- filtered_long() %>% distinct(event_id) %>% nrow()
    make_card("Unique CRLs", scales::comma(val), "#8C6BB1")
  })
  
  output$top_domain_card <- renderUI({
    df <- filtered_domain() %>%
      count(deficiency_domain, sort = TRUE)
    
    top_domain <- if (nrow(df) == 0) "None" else df$deficiency_domain[1]
    make_card("Top Domain", top_domain, "#D4A373")
  })
  
  output$selected_domain_card <- renderUI({
    make_card("Selected Domain", input$domain, "#7A6C5D")
  })
  
  output$top_subtype_card <- renderUI({
    df <- filtered_long() %>%
      filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
      count(deficiency_subtype, sort = TRUE)
    
    top_subtype <- if (nrow(df) == 0) "None" else df$deficiency_subtype[1]
    make_card("Top Subtype", top_subtype, "#C06C84")
  })
  
  output$cmc_n_def_card <- renderUI({
    make_card("CMC Deficiencies", scales::comma(nrow(filtered_cmc_long())), "#5B8E7D")
  })
  
  output$cmc_n_domain_card <- renderUI({
    make_card("CMC CRL-Domain Records", scales::comma(nrow(filtered_cmc_domain())), "#4F6D7A")
  })
  
  output$cmc_top_subtype_card <- renderUI({
    df <- filtered_cmc_long() %>%
      filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
      count(deficiency_subtype, sort = TRUE)
    
    top_subtype <- if (nrow(df) == 0) "None" else df$deficiency_subtype[1]
    make_card("Top CMC Subtype", top_subtype, "#D4A373")
  })
  
  output$domain_table <- renderDT({
    table_df <- filtered_domain() %>%
      count(deficiency_domain, name = "count") %>%
      mutate(percent = paste0(round(100 * count / sum(count), 2), "%")) %>%
      arrange(desc(count))
    
    req(nrow(table_df) > 0)
    
    datatable(
      table_df,
      rownames = FALSE,
      options = list(
        pageLength = 6,
        dom = 'tip',
        autoWidth = TRUE
      )
    )
  })
  
  output$domain_plot <- renderPlot({
    plot_df <- filtered_domain() %>%
      count(deficiency_domain, name = "n") %>%
      mutate(
        pct = n / sum(n),
        deficiency_domain = fct_reorder(deficiency_domain, n)
      )
    
    req(nrow(plot_df) > 0)
    
    ggplot(plot_df, aes(x = deficiency_domain, y = pct, fill = deficiency_domain)) +
      geom_col(width = 0.72, show.legend = FALSE) +
      geom_text(
        aes(label = percent(pct, accuracy = 0.1)),
        hjust = -0.1,
        size = 4
      ) +
      coord_flip() +
      scale_fill_manual(values = domain_colors, drop = FALSE) +
      scale_y_continuous(
        labels = percent_format(),
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        x = NULL,
        y = "Percent of CRL-Domain Records",
        title = "Distribution of Deficiency Domains Across CRLs"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      )
  })
  
  output$domain_trend_plot <- renderPlot({
    plot_df <- trend_data()
    
    req(nrow(plot_df) > 0)
    
    if (input$domain == "All") {
      ggplot(
        plot_df,
        aes(x = year, y = value, color = deficiency_domain, group = deficiency_domain)
      ) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 2.2) +
        scale_color_manual(values = domain_colors, drop = FALSE) +
        scale_y_continuous(
          labels = if (input$trend_metric == "Percent") percent_format() else comma_format(),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(
          x = "Year",
          y = if (input$trend_metric == "Percent") "Percent of CRL-Domain Records" else "Count of CRL-Domain Records",
          title = if (input$trend_metric == "Percent") {
            "Deficiency Domain Trends Over Time (%)"
          } else {
            "Deficiency Domain Trends Over Time"
          },
          color = "Domain"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.title = element_text(face = "bold")
        )
      
    } else {
      fill_col <- if (input$domain %in% names(domain_colors)) domain_colors[[input$domain]] else "#5B8E7D"
      
      ggplot(plot_df, aes(x = year, y = value, group = 1)) +
        geom_line(linewidth = 1.2, color = fill_col) +
        geom_point(size = 2.4, color = fill_col) +
        scale_y_continuous(
          labels = if (input$trend_metric == "Percent") percent_format() else comma_format(),
          expand = expansion(mult = c(0, 0.08))
        ) +
        labs(
          x = "Year",
          y = if (input$trend_metric == "Percent") "Percent of CRL-Domain Records" else "Count of CRL-Domain Records",
          title = paste(
            if (input$trend_metric == "Percent") "Temporal Distribution of" else "Yearly Trend for",
            input$domain
          )
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
    }
  })
  
  output$subtype_plot <- renderPlot({
    if (input$domain == "All") {
      plot_df <- filtered_long() %>%
        filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
        count(deficiency_domain, deficiency_subtype, name = "n") %>%
        group_by(deficiency_domain) %>%
        mutate(pct = n / sum(n)) %>%
        ungroup()
      
      req(nrow(plot_df) > 0)
      
      ggplot(
        plot_df,
        aes(
          x = reorder_within(deficiency_subtype, n, deficiency_domain),
          y = pct,
          fill = deficiency_domain
        )
      ) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        facet_wrap(~ deficiency_domain, scales = "free_y") +
        scale_x_reordered() +
        scale_fill_manual(values = domain_colors, drop = FALSE) +
        scale_y_continuous(labels = percent_format()) +
        labs(
          x = NULL,
          y = "Percent within Domain",
          title = "Subtype Composition Across All Domains"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          strip.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()
        )
      
    } else {
      plot_df <- filtered_long() %>%
        filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
        count(deficiency_subtype, name = "n", sort = TRUE) %>%
        mutate(
          pct = n / sum(n),
          deficiency_subtype = factor(deficiency_subtype, levels = rev(deficiency_subtype))
        )
      
      fill_col <- if (input$domain %in% names(domain_colors)) domain_colors[[input$domain]] else "#5B8E7D"
      
      ggplot(plot_df, aes(x = deficiency_subtype, y = pct)) +
        geom_col(fill = fill_col, width = 0.72) +
        geom_text(
          aes(label = percent(pct, accuracy = 0.1)),
          hjust = -0.1,
          size = 4
        ) +
        coord_flip() +
        scale_y_continuous(
          labels = percent_format(),
          expand = expansion(mult = c(0, 0.12))
        ) +
        labs(
          x = NULL,
          y = "Percent within Selected Domain",
          title = paste("Subtype Composition:", input$domain)
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          strip.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()
        )
    }
  })
  
  output$subtype_table <- renderDT({
    
    if (input$domain == "All") {
      table_df <- filtered_long() %>%
        filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
        count(deficiency_domain, deficiency_subtype, name = "count", sort = TRUE) %>%
        group_by(deficiency_domain) %>%
        mutate(percent = paste0(round(100 * count / sum(count), 2), "%")) %>%
        ungroup()
      
    } else {
      table_df <- filtered_long() %>%
        filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
        count(deficiency_subtype, name = "count", sort = TRUE) %>%
        mutate(percent = paste0(round(100 * count / sum(count), 2), "%"))
    }
    req(nrow(table_df) > 0)
    datatable(
      table_df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  output$cmc_plot <- renderPlot({
    plot_df <- filtered_cmc_long() %>%
      filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
      count(deficiency_subtype, name = "n", sort = TRUE) %>%
      mutate(
        pct = n / sum(n),
        deficiency_subtype = factor(deficiency_subtype, levels = rev(deficiency_subtype))
      )
    
    req(nrow(plot_df) > 0)
    
    ggplot(plot_df, aes(x = deficiency_subtype, y = pct)) +
      geom_col(fill = domain_colors["CMC"], width = 0.72) +
      geom_text(
        aes(label = percent(pct, accuracy = 0.1)),
        hjust = -0.1,
        size = 4
      ) +
      coord_flip() +
      scale_y_continuous(
        labels = percent_format(),
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        x = NULL,
        y = "Percent within CMC",
        title = "CMC Subtype Composition"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      )
  })
  
  output$cmc_table <- renderDT({
    table_df <- filtered_cmc_long() %>%
      filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
      count(deficiency_subtype, name = "count", sort = TRUE) %>%
      mutate(percent = paste0(round(100 * count / sum(count), 2), "%"))
    
    req(nrow(table_df) > 0)
    
    datatable(
      table_df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  output$structure_plot <- renderPlot({
    structure_df <- filtered_structure() %>%
      count(event_id, name = "num_domains") %>%
      mutate(
        structure = ifelse(num_domains == 1, "Single-Domain", "Multi-Domain")
      ) %>%
      count(structure, name = "n") %>%
      mutate(pct = n / sum(n))
    
    req(nrow(structure_df) > 0)
    
    ggplot(structure_df, aes(x = structure, y = pct, fill = structure)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(
        aes(label = percent(pct, accuracy = 0.1)),
        vjust = -0.4,
        size = 4.5
      ) +
      scale_y_continuous(
        labels = percent_format(),
        expand = expansion(mult = c(0, 0.12))
      ) +
      scale_fill_manual(values = c("Single-Domain" = "#5B8E7D", "Multi-Domain" = "#C06C84")) +
      labs(
        x = "CRL Structure",
        y = "Percent of CRLs",
        title = "Single vs Multi-Domain CRLs"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$cooccur_table <- renderDT({
    combo_df <- filtered_structure() %>%
      distinct(event_id, deficiency_domain) %>%
      arrange(event_id, deficiency_domain) %>%
      group_by(event_id) %>%
      summarise(
        pattern = paste(deficiency_domain, collapse = ", "),
        .groups = "drop"
      ) %>%
      count(pattern, name = "count") %>%
      mutate(percent = paste0(round(100 * count / sum(count), 2), "%")) %>%
      arrange(desc(count))
    
    req(nrow(combo_df) > 0)
    
    datatable(
      combo_df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  output$download_domain <- downloadHandler(
    filename = function() {
      paste0("domain_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_domain() %>%
        count(deficiency_domain, name = "count") %>%
        mutate(percent = round(100 * count / sum(count), 2)) %>%
        arrange(desc(count))
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_subtype <- downloadHandler(
    filename = function() {
      paste0("subtype_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (input$domain == "All") {
        df <- filtered_long() %>%
          filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
          count(deficiency_domain, deficiency_subtype, name = "count", sort = TRUE) %>%
          group_by(deficiency_domain) %>%
          mutate(percent = round(100 * count / sum(count), 2)) %>%
          ungroup()
      } else {
        df <- filtered_long() %>%
          filter(!is.na(deficiency_subtype), deficiency_subtype != "") %>%
          count(deficiency_subtype, name = "count", sort = TRUE) %>%
          mutate(percent = round(100 * count / sum(count), 2))
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_structure <- downloadHandler(
    filename = function() {
      paste0("structure_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_structure() %>%
        distinct(event_id, deficiency_domain) %>%
        arrange(event_id, deficiency_domain) %>%
        group_by(event_id) %>%
        summarise(
          pattern = paste(deficiency_domain, collapse = ", "),
          .groups = "drop"
        ) %>%
        count(pattern, name = "count") %>%
        mutate(percent = round(100 * count / sum(count), 2)) %>%
        arrange(desc(count))
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)