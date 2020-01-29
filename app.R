# Load packages ----
library(shiny)
library(animation)
library(scales)
library(viridis)
library(dplyr)
library(data.table)
library(ggthemes)
library(plotrix)
library(ggplot2)
library(DT)
library(htmlwidgets)
library(shinyjs)

# Source helpers ----
source("source.R")

# Load data ----
source("set_up.R")

# inlineCSS
#includeCSS("ibm.css")

# User interface ----
ui <- fluidPage(theme = "ibm.css",
  titlePanel("Model: Oscillation Hypothesis"),
  br(),
  
  navbarPage(
    "",
    tabPanel("Model description",
    navlistPanel("",
               tabPanel("Overall",
                        fluidRow(
                          br(),
                        column(12, align = "center",
                         img(src = "methods_1.png", height = 260, width = 750) #390, 1125
                          ),
                         
                         column(12, 
                                div(p("In this study, we used an Individual-Based Model to investigate the patterns of species richness and dynamics on host use when individuals evolve in a spatial landscape with different hosts. The model is based on the interacting life cycle and dynamics of phytophagous insects and their host plants, but its principles can be extended to other parasite-host systems.",
                                  align = "justify"), style = "font-size:160%")
                         )
                        ) # fluidRow
               ), # Methods tabPanel 1

               tabPanel("Events schedulling",
                         fluidRow(
                           br(),
                           column(12, align = "center",
                                  img(src = "methods_events.png", height = 420, width = 720) ) # column
                         ) # fluidRow       
                ) # tabPanel events schedulling
                
               ) # navlistPage
    ), # Methods tabPanel
    
    navbarMenu("Simulations",
               
    tabPanel("Select simulation",
             sidebarLayout(
               sidebarPanel(
               
             sliderInput("alpha_sim", "Selection strength (Alpha)",
                         min = 1, max = 7,
                         value = 7),
             
             sliderInput("radius_sim", "Dispersal radius (R)",
                         min = 1, max = 11,
                         value = 3, step = 2),
             
             actionButton(inputId = "load_sim", label = "Load simulation"),
             
             br(),
             
             fluidRow(
             column(12, align = "center",
              
             br(),             
             
             strong(p("You have chosen the following parameters:")),
            
             strong(span(
             textOutput("loaded_alpha_sim"),
             
             textOutput("loaded_radius_sim"),
             
             style = "color:red")),
             
             br(),
             
             tableOutput("par_table")
             ) # column
             ) # fluidRow
             ), # sidebarPanel
             
             mainPanel(

               fluidRow(
                 
               column(6, align = "center", 
                        plotOutput("sim_alpha_plot", width = "350px", height="350px")
                ), # column
                 
               column(6, align = "center", 
               plotOutput("sim_radius_plot", width = "350px", height="350px")
               ) # column

               ), # fluidRow
               
               br(),
               
               fluidRow(
                 column(12, align = "center",
                 div(
                    textOutput("alpha_summary1"),
                    textOutput("alpha_summary2"),
                    textOutput("radius_summary")
                    , style = "font-size:120%" 
                    ) #div
                 ) # column
               
               ) # fluidRow
             ) # mainPanel
             
             ) # sidebarLayout
             
             ), # tabPanel "Select simulation"
                          
    tabPanel("Spatial distribution",
             fluidRow(
               column(4,
                      wellPanel(      
                        h4("Spatial position of individuals"),
                        sliderInput("slider_spatial", label = "Generation",
                                    min = 1, max = max_gen, value = 0, step = 5,
                                    animate = animationOptions(interval = 100, loop = TRUE)),
                        
                        radioButtons("radio_tab1", label = "Display individual's:",
                                     choices = list("Species", "Phenotype", "Host", 
                                                    "Phenotype-Host difference"), 
                                     selected = "Species")
                      ),
                      plotOutput("plot.diver", height= "300px")
               ),
               #),
               
               column(8, align = "center",
                      plotOutput("plot", width = "500px", height="500px"),
                      br(),
                      plotOutput("plot.legend", width = "500px", height="320px")
                     # strong(verbatimTextOutput("summary_spatial"))
                      )
               
             )
             
    ), # tabPanel 1
    
    tabPanel("Phenotype",
             
             sidebarLayout(
               sidebarPanel(
                 
                 sliderInput("slider_phenotype", label = h3("Generation", align = "center"),
                             min = 1, max = max_gen, value = 0, step = 5,
                             animate = animationOptions(interval = 100, loop = TRUE)),
                 br(),
                 br(),
                 sliderInput("slider_phen_ymax", label = "Set 'Number of Individuals' limit",
                             min = 1, max = 1000, value = 300),
                 br(),
                 sliderInput("slider_phen_xlim", label = "Set 'phenotype' limits",
                             min = 0, max = 100, value = c(15,85)),
                 br(),
                 actionButton("reset_phen", "Reset graph settings")
                 
               ), #sidebarPanel
               
               mainPanel(
                 fluidRow(      
                   h3("Phenotype distribution through generations", align = "center"),
                   column(12, align = "center",
                          plotOutput("plot2", width = "500px", height="500px") )
                 )
               ) # mainPanel
             ) # sidebarLayout
             
             
    ), # tabPanel 2
    
    tabPanel("Lineage details",
             sidebarLayout(
               sidebarPanel(

                 numericInput("select_lin", "Select a lineage",
                              min = 1, max = 50, value = 1),
                 
                 selectInput("select", label = "Select attribute", 
                             choices = list("Phenotype x time", "Abundance x time",
                                              "Host range x time"), selected = "Phenotype x time"),
                 
                 uiOutput("ui_tab3_side")
                 
               ), # sidebarPanel
               
               mainPanel(
                 uiOutput("ui_tab3_main")
               ) #mainPanel
             ) # sidebarLayout
             ) #tabPanel 3
    
    ), #navbarMenu
    ####tabPanel("Index analysis",
             
    navbarMenu("Index Analysis",
                 
       tabPanel("Index values",
                
       sidebarLayout(
         
       sidebarPanel(
         fluidRow(
         
         selectInput("select_index", label ="Select Index", 
                     choices = list("Oscillation Index" = "OI", "Host Specialization Index" = "HSI", "Host Expansion Index" = "HEI"), 
                     selected = "Oscillation Index"), 
         
         radioButtons("select_lin_index", label = "Choose lineage abundance at speciation",
                      choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
                      selected = "Least abundant lineage"),
         
         radioButtons("radio_nonzero", label = "Choose range of values to display",
                      choices = list("Positive indexes only", "All indexes", "Negative indexes only"), 
                      selected = "Positive indexes only"),
         
         checkboxInput("checkbox_showtext_1", label = "Display mean values", value = TRUE),
         
         checkboxInput("refresh_max_1", label = "Custom plot range", value = FALSE),
         
         uiOutput("custom_limits")
         
         )
        # verbatimTextOutput("check_value")
         
       ), #sidebarPanel
       
       mainPanel(
         fluidRow(
           
         column(7, align = "center",
         plotOutput("plot.index.means", width = "500px", height="600px")
          ),
         
         column(5, align = "center",
                plotOutput("plot.boxplot.index.r", height="300px"),
                
                plotOutput("plot.boxplot.index.alpha", height="300px")
         ) # column
         ) # fluidRow
       ) #mainPanel
       
       ) #sidebarLayout
       
       ), # tabPanel indexvalues
       
       tabPanel("Proportion of events",     
             sidebarLayout(
               sidebarPanel(
                 h1("Proportion of events"),
                 
                 selectInput("select_index_2", label ="Select Index", 
                             choices = list("Oscillation Index" = "OI", "Host Specialization Index" = "HSI", "Host Expansion Index" = "HEI"), 
                             selected = "Oscillation Index"),
                 # qqq
                 radioButtons("select_lin_index_2", label = "Choose lineage abundance at speciation",
                              choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
                              selected = "Least abundant lineage"),
                 
                 #uiOutput("ui_tab4_side_1"), # Select small, big or both
                 radioButtons("radio_index_sign_2", label = "Choose index sign",
                              choices = list("Positive index", "Index equal to 0", "Negative index"), 
                              selected = "Positive index"),
                 
                 #uiOutput("ui_tab4_side_2"), # Select positive, equal or negative

                 checkboxInput("checkbox_showtext_2", label = "Display mean values", value = TRUE),
                 
                 column(10, align = "center", 
                 sliderInput("decimal_2", "Set upper limit:",
                             min = 0, max = 1.0,
                             value = 1.0, step = 0.01)
                 ), #column
                 #),
                 
                 actionButton(inputId = "refresh_max_2", label = "Adjust new upper limit")
                 
                 
               ),
               mainPanel(
                 fluidRow(
                   
                   column(7, align = "center",
                 plotOutput("plot_prop", width = "500px", height="600px")
                   ), #column
                 
                 column(5, align = "center",
                      plotOutput("plot.boxplot.r", height="300px"),
                      
                      plotOutput("plot.boxplot.alpha", height="300px")
                        )
                 
                 ) #fluidRow
                 
               )
             ) # sidebarLayout
       ), # proportion tabPanel
       
      tabPanel("Summary",
               sidebarLayout(
                 sidebarPanel(
  
                   radioButtons("radio_phase_model", label = "Choose phase of the model:",
                                choices = list("All speciation events", "First speciation event only"), 
                                selected = "All speciation events"),
                   
                   h4("Histogram display:"),
                   
                   selectInput("select_index_3", label ="Select Index", 
                               choices = list("Oscillation Index" = "OI", "Host Specialization Index" = "HSI", "Host Expansion Index" = "HEI"), 
                               selected = "Oscillation Index"),
                   
                   radioButtons("select_lin_index_3", label = "Choose lineage abundance at speciation",
                                choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
                                selected = "Least abundant lineage"),
                   
                   radioButtons("radio_index_sign_3", label = "Choose index sign",
                                choices = list("Positive index", "Index equal to 0", "Negative index"), 
                                selected = "Positive index")
                 ), #sidebarPanel
               
               mainPanel(
               fluidRow(
               column(8, offset = 2, align = "center", 
                      dataTableOutput("prop_table")
                     ),
               
               column(12, align = "center", 
                      plotOutput("hist_prop", width = "300px", height="300px")
                      ) #column
               
               ) #fluidRow
               ) # mainPanel
               ) #sidebarLayout
               ) #tabPanel 
    ) # navbarMenu 2
       
    #) #tabPanel
    
    # ) # navbarMenu
  
  ) # navbarPage
 ) # ui

# Server logic
server <- function(input, output, clientData, session) {
  
  # Spatial x time
  time_spatial = reactive(input$slider_spatial)
  
  type <- reactive({
    switch(input$radio_tab1,
           "Species" = {"sp_id"},
           "Phenotype" = {"phenotype"},
           "Host" = {"host"},
           "Phenotype-Host difference" = {"dif"}
           )
  })
  
  
  legend.pal <- reactive({
    switch(input$radio_tab1,
           "Species" = {pal_spatial2()},
           "Phenotype" = {pal_z2()},
           "Host" = { pal_host2()},
           "Phenotype-Host difference" = {pal_dif2()}
    )
  })
  
  output$plot <- renderPlot({
    plot.spatial(rv$spp2, time_spatial(), pal_spatial2(), pal_z2(), pal_host2(), pal_dif2(), type = type())
  })
  # 123
  output$plot.legend <- renderPlot({
    plot.legend(rv$spp2, legend.pal(), input$radio_tab1)
  })
  
  output$plot.diver <- renderPlot({
    plot.diver(diversity(), time_spatial())
  })
  # 456
  output$summary_spatial  <- renderText({
    paste0("Species richness: ", diversity()$d[time_spatial()], ". Number of individuals: ", abundance()$a[time_spatial()])
    })

  # Phenotype x Time
  time_phenotype = reactive(input$slider_phenotype)
  ymax_phen = reactive(input$slider_phen_ymax)
  xlim_inf = reactive(input$slider_phen_xlim[1])
  xlim_sup = reactive(input$slider_phen_xlim[2])
  
  output$plot2 <- renderPlot({
    plot.phen(rv$spp2, time_phenotype(), pal_spatial2(), ymax = ymax_phen(), xlim = c(xlim_inf(),xlim_sup()))
  })
  
  observeEvent(input$reset_phen, {
       updateSliderInput(session, "slider_phen_ymax",
                      label =  "Set 'Number of Individuals' limit",
                      value = 300)
    
    updateSliderInput(session, "slider_phen_xlim",
                      # label =  "Set 'phenotype' limits",
                      value = c(25,70))
  })

  # Phenotype of a species x time
  output$ui_tab3_main  <- renderUI({
    switch(input$select,
           "Phenotype x time" = {
             column(12, align = "center",
                       plotOutput("plot.phen_sp") )},
           "Abundance x time" = {
             column(12, align = "center",
                    plotOutput("plot.abund_sp"),
                    br(),
                    textOutput("tab3_abund_text_1"),
                    textOutput("tab3_abund_text_2"),
                    textOutput("tab3_abund_text_3")
             ) # column
           },
           
           "Host range x time" = {
             plotOutput("plot.host_sp")
           }
           ) # switch
                    
                    }) # renderUi
  
  output$ui_tab3_side  <- renderUI({
    switch(input$select,
           "Phenotype x time" = {
             sliderInput("ymax_tab3_side_p", "Set 'Y axis' range",
                         min = 1, max = 100, value = c(15,85))
             
           },
           "Abundance x time" = {
             sliderInput("ymax_tab3_side_a", "Set 'Y axis' limit",
                         min = 1, max = 2500, value = 2000)
           },
           "Host range x time" =
             sliderInput("ymax_tab3_side_h", "Set 'Y axis' limit",
                         min = 1, max = 15, value = 14)
    ) # switch
      
  })
  
  sp_tab3 <- reactive(input$select_lin) # species chosen at tab 3
  
  abund.spp <- reactive(abund.list()[[sp_tab3()]]) # Select item from abundance list according to species
  
  sp_start <- reactive(abund.spp()[1,1]) 
  sp_end <- reactive(max(abund.spp()[,1]))
  sp_duration <- reactive(sp_end() - sp_start()) 
  sp_max_abund <- reactive(max(abund.list()[[1]][,2]))
  
  output$plot.phen_sp <- renderPlot({
    plot.phen.sp(phen.all2(), sp_tab3(), yrange = c(input$ymax_tab3_side_p[1],input$ymax_tab3_side_p[2]))
  })
   
  output$plot.abund_sp <- renderPlot({
    plot.abund.sp(abund.list(), sps(), sp_tab3(), ymax = input$ymax_tab3_side_a)
  })
  
  output$tab3_abund_text_1 <- renderText({ 
    paste0("Lineage start at generation ", sp_start(), ", ended at generation ", sp_end())
  })
  
  output$tab3_abund_text_2 <- renderText({ 
    paste0("Length of duration: ", sp_duration())
  })
  
  output$tab3_abund_text_3 <- renderText({ 
    paste0("Maximum abundance: ", sp_max_abund())
  })
  
  output$plot.host_sp <- renderPlot({
    plot.host.sp(host.list(), sp_tab3(), sps(), ymax = input$ymax_tab3_side_h)
  })
  
  ###
  
  # Current selected simulation
  
  rv <- reactiveValues(loaded_alpha = 7)
  rv <- reactiveValues(loaded_radius = 3)
  rv <- reactiveValues(loaded_alpha = 7)
  rv <- reactiveValues(index = 2)
  rv <- reactiveValues(spp2 = spp.list[[2]])
  #rv <- reactiveValues(index_table = oi.df)
  
  alpha <- reactive({input$alpha_sim})
  radius <- reactive({input$radius_sim})
  
  # UPDATE SPP
  observeEvent(input$load_sim, {
    rv$loaded_alpha <- alpha()
    rv$loaded_radius <- radius()
    rv$index <- which(par[seq(2,84,2),]$alfa == alphas[ abs(rv$loaded_alpha - 8)  ] & par[seq(2,84,2),]$R == radius())[1]
   ## rv$spp2 <- spp.list[[(input$load_sim%%2) + 1]]
    rv$spp2 <- spp.list[[rv$index]]
  })
  
  pal.list <- reactive(return.pallete.list(rv$spp2))
  pal_spatial2 = reactive(pal.list()$spatial)
  pal_z2 <- reactive(pal.list()$z)
  pal_host2 <- reactive(pal.list()$host)
  pal_dif2 <- reactive(pal.list()$dif)
  
  phen.all2 <- reactive(phen.all.list[[rv$index]]) # Initial values
  abund.list <- reactive(get.abund.list(rv$spp2, sps()))
  max_abund <- reactive(max(abund.list[[1]]))
  
  host.list <- reactive(get.host.range(rv$spp2))
  max_host <- reactive(max(host.list()$h))
  
  sps <- reactive(unique(rv$spp2$sp))
  
  diversity = reactive(rv$spp2 %>% group_by(ngen) %>% summarize(d = n_distinct(sp)))
  abundance = reactive(rv$spp2 %>% group_by(ngen) %>% summarize(a = n()))
  
  # QQQ
  
  observe({ 
    updateNumericInput(session, "select_lin",
                     label = paste("Select a lineage. Max: ", max(rv$spp2$sp)),
                     value = 1, min = 1, max = max(rv$spp2$sp))
  })

  observe({ 
    updateSliderInput(session, "ymax_tab3_side_a",
                      label =  "Set 'Y axis' limit",
                      value = sp_max_abund())
  })
  
  observe({ 
    updateSliderInput(session, "ymax_tab3_side_h",
                      label =  "Set 'Y axis' limit",
                      value = max_host())
  })
  
  output$loaded_alpha_sim  <- renderText({
    paste0("Alpha: ", sprintf( alphas[abs(rv$loaded_alpha - 8)], fmt = paste0("%#.", show_n_digits[rv$loaded_alpha], "f")))
  })
  
  output$loaded_radius_sim  <- renderText({
    paste0("Dispersal radius: ", rv$loaded_radius)
  })
  
  ###
  
  output$sim_radius_plot <- renderPlot({
    draw.radius(input$radius_sim, rv$loaded_radius)
  })
  
  output$sim_alpha_plot <- renderPlot({
      plot.alpha(input$alpha_sim, alphas, rv$loaded_alpha)
  })
  
  alpha_sim_num <- reactive( alphas[ abs(input$alpha_sim - 8)] )
  alpha_sim_num_log <- reactive( log(alphas[ abs(input$alpha_sim - 8)] ))
  
  output$alpha_summary1  <- renderText({
    paste0("Selection strength (alpha) is: ", sprintf(alpha_sim_num(), fmt = paste0("%#.", show_n_digits[input$alpha_sim], "f")))
  })
  
  output$alpha_summary2  <- renderText({
    paste0("Log (alpha) is: ", round(alpha_sim_num_log(),3))
  })
  
  output$radius_summary  <- renderText({
    paste0("Dispersal radius is: ", input$radius_sim)
  })
  
  output$par_table <- renderTable({par_table},
    rownames =  FALSE, digits = 0, align = "c", striped = TRUE)
  
  output$print <- renderPrint({
    #index_name()
    # head(abund.list()[[1]])
  })

  ###
  decimal_limit_range <- reactive({
    return(range(means_table()$mean))
  })
  
  observeEvent(input$refresh_custom, {
                 updateSliderInput(session, "decimal_1",
                                   value = decimal_limit_range() + c(-0.01, 0.01))
                                })
  
  index_table <- reactive({
  switch(input$select_index,
         "OI" = {
           if(lin_abundance()!="both") {
              return(oi.df) } else {
                return(oi.both)
              }
           },
         "HSI" = {
           if(lin_abundance()!="both") {
             return(hsi) } else {
               return(hsi.both)
             }
           return(hsi)
           },
         "HEI" = {return(hei)})
  })
  
  
  index_table_2 <- reactive({
    switch(input$select_index_2,
           "OI" = {
             if(lin_abundance_2()!="both") {
               return(oi.df) } else {
                 return(oi.both)
               }
           },
           "HSI" = {
             if(lin_abundance_2()!="both") {
               return(hsi) } else {
                 return(hsi.both)
               }
             return(hsi)
           },
           "HEI" = {return(hei)})
  })
  
  
  index_table_3 <- reactive({
    switch(input$select_index_3,
           "OI" = {
             if(input$radio_phase_model == "All speciation events"){
             if(lin_abundance_3()!="both") {
               return(oi.df) } else {
                 return(oi.both)
               }
             } else {
             if(lin_abundance_3()!="both") {
               return(oi.first) } else {
                 return(oi.first.both)
               }
             }
             
             
           },
           "HSI" = {
             
             if(input$radio_phase_model == "All speciation events"){ 
             if(lin_abundance_3()!="both") {
               return(hsi) } else {
                 return(hsi.both)
               }
             } else {
             if(lin_abundance_3()!="both") {
               return(hsi.first) } else {
                 return(hsi.first.both)
               }
             }
             
           },
           "HEI" = {
             if(input$radio_phase_model == "All speciation events"){ 
             return(hei)
             } else {
               return(hei.first)
             }
             })
  })
  
  lin_abundance <- reactive({
    switch(input$select_lin_index,
           "Least abundant lineage" = {return("small")},
           "Most abundant lineage" = {return("big")},
           "All lineages" = {return("both")})
  })
  
  lin_abundance_2 <- reactive({
    switch(input$select_lin_index_2,
           "Least abundant lineage" = {return("small")},
           "Most abundant lineage" = {return("big")},
           "All lineages" = {return("both")})
  })
  
  lin_abundance_3 <- reactive({
    switch(input$select_lin_index_3,
           "Least abundant lineage" = {return("small")},
           "Most abundant lineage" = {return("big")},
           "All lineages" = {return("both")})
  })
  
  # rrr
  index_sign <- reactive({
    switch(input$radio_index_sign,
           "Positive index" = {return("positive")},
           "Index equal to 0" = {return("equal")},
           "Negative index" = {return("negative")})
  })  

  index_sign_2 <- reactive({
    switch(input$radio_index_sign_2,
           "Positive index" = {return("positive")},
           "Index equal to 0" = {return("equal")},
           "Negative index" = {return("negative")})
  })  
  
  index_sign_3 <- reactive({
    switch(input$radio_index_sign_3,
           "Positive index" = {return("positive")},
           "Index equal to 0" = {return("equal")},
           "Negative index" = {return("negative")})
  })  
  
  new_decimal_max_2 <- reactive({
    return(as.numeric(get.max.prop(index_table_2(), lin_abundance_2())[index_sign_2()]))
  })
  
  prop_freq <- reactive({
    return(get.proportion(index_table_2(), lin_abundance_2(), index_sign_2(), index_name = input$select_index_2))
    })
  
  
  observeEvent(input$refresh_max,{
    
   updateSliderInput(session, "decimal_2",
                   value = new_decimal_max_2())
  }
  )
  
  output$plot_prop <- renderPlot({
    plot.proportion(index_table_2(), lin_abundance = lin_abundance_2(), index_name = input$select_index_2, limit = input$decimal_2, alphas, sign = index_sign_2(), show.text = input$checkbox_showtext_2)  
  })

  # output$ui_tab4_side_1  <- renderUI({
  #   selectInput("select_lin_index", label = "", 
  #               choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
  #               selected = "Least abundant lineage")
  # })
  
  
  observe({
    switch(input$select_index,
           "OI" = {
             updateRadioButtons(session, "select_lin_index",
                                choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
                                selected = "Least abundant lineage")
           },
           "HSI" = {
             updateRadioButtons(session, "select_lin_index",
                                choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
                                selected = "Least abundant lineage")
           },
           "HEI" = {
             updateRadioButtons(session, "select_lin_index",
                                choices = list("All lineages"), 
                                selected = "All lineages")
           }
           
    ) #switch
  }) #observe
  
  observe({
    switch(input$select_index,
           "OI" = {
             updateRadioButtons(session, "radio_nonzero",
                                choices = list("Positive indexes only", "All indexes", "Negative indexes only"), 
                                selected = "Positive indexes only")
           },
           "HSI" = {
             updateRadioButtons(session, "radio_nonzero",
                                choices = list("Positive indexes only", "All indexes"), 
                                selected = "Positive indexes only")
           },
           "HEI" = {
             updateRadioButtons(session, "radio_nonzero",
                                choices = list("Positive indexes only", "All indexes", "Negative indexes only"), 
                                selected = "Positive indexes only")
           }
           
    ) #switch
  }) #observe
  
  
  observe({
    switch(input$select_index_2,
            "OI" = {
  updateRadioButtons(session, "radio_index_sign_2",
                     label = "Choose index sign:",
                     choices = list("Positive index", "Index equal to 0", "Negative index"), 
                     selected = "Positive index")
            },
  "HSI" = {
    updateRadioButtons(session, "radio_index_sign_2",
                       label = "Choose index sign:",
                       choices = list("Positive index", "Index equal to 0"), 
                       selected = "Positive index")
  },
  "HEI" = {
    updateRadioButtons(session, "radio_index_sign_2",
                       label = "Choose index sign:",
                       choices = list("Positive index", "Index equal to 0", "Negative index"), 
                       selected = "Positive index")
  }
  
    ) #switch
  }) #observe

  observe({
    switch(input$select_index_2,
    "OI" = {
    updateRadioButtons(session, "select_lin_index_2",
                                label = "Choose lineage abundance at speciation",
                                choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
                                selected = "Least abundant lineage")
           },
  "HSI" = {
    updateRadioButtons(session, "select_lin_index_2",
                       label = "Choose lineage abundance at speciation",
                       choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
                       selected = "Least abundant lineage")
  },
  "HEI" = {
    updateRadioButtons(session, "select_lin_index_2",
                       label = "Choose lineage abundance at speciation",
                       choices = list("All lineages"), 
                       selected = "All lineages")
  }
    ) #switch
  }) #observe
  
  observe({
    switch(input$select_index_3,
           "OI" = {
             updateRadioButtons(session, "select_lin_index_3",
                                choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
                                selected = "Least abundant lineage")
           },
           "HSI" = {
             updateRadioButtons(session, "select_lin_index_3",
                                choices = list("Least abundant lineage", "Most abundant lineage", "All lineages"), 
                                selected = "Least abundant lineage")
           },
           "HEI" = {
             updateRadioButtons(session, "select_lin_index_3",
                                choices = list("All lineages"), 
                                selected = "All lineages")
           }
           
    ) #switch
  }) #observe
  
  observe({
    switch(input$select_index_3,
           "OI" = {
             updateRadioButtons(session, "radio_index_sign_3",
                                choices = list("Positive index", "Index equal to 0", "Negative index"), 
                                selected = "Positive index")
           },
           "HSI" = {
             updateRadioButtons(session, "radio_index_sign_3",
                                choices = list("Positive index", "Index equal to 0"), 
                                selected = "Positive index")
           },
           "HEI" = {
             updateRadioButtons(session, "radio_index_sign_3",
                                choices = list("Positive index", "Index equal to 0", "Negative index"), 
                                selected = "Positive index")
           }
           
    ) #switch
  }) #observe
  
  df.prop.final <- reactive({
    if(input$radio_phase_model == "All speciation events"){ 
      return(df.prop)
    } else{
      return(df.prop.first)
    }
  })
  
  output$check_value <- renderPrint({ 
  #lin_abundance()
  print(df.prop.final())
    #head(index_table())
    })
  
  ###  
  output$plot.boxplot.r <- renderPlot({
    boxplot.R(prop_freq(), index_name = input$select_index_2, ylimit = input$decimal_2)
  })
  
  output$plot.boxplot.alpha <- renderPlot({
    boxplot.alpha(prop_freq(), index_name = input$select_index_2, alphas, ylimit = input$decimal_2)
  })
 
  ####
  

  index_final_table <- reactive({
    switch(input$radio_nonzero,
           "Positive indexes only" = {
             return(filter.index.means(index_table(), lin_abundance(), index_name = input$select_index, filter = "positive"))
           },
           "All indexes" = {
             return(index_table())
             },
           "Negative indexes only" = {
             return(filter.index.means(index_table(), lin_abundance(), index_name = input$select_index, filter = "negative"))
           }
    ) # switch
  })
  
  means_table <- reactive({
    return(get.index.means(index_final_table(), lin_abundance(), index_name = input$select_index))
  })
  
  output$plot.index.means <- renderPlot({
   plot.index.means(means_table(), alphas, index_name = input$select_index, 
                    ylimit = input$decimal_1, show.text = input$checkbox_showtext_1, adjusty = input$refresh_max_1)
  })
  
  output$custom_limits <- renderUI({
    if(input$refresh_max_1 == TRUE){
      
      column(8, align = "center", offset = 2,
      sliderInput("decimal_1", "Set index limits:",
                         min = -0.45, max = 1.25,
                         value = c(-0.45, 1.25), step = 0.01),
      
       actionButton(inputId = "refresh_custom", label = "Adjust to current limits"),
      
       br()       
      ) #column  
            
    } else{
      
    }
  })
  
  output$plot.boxplot.index.r <- renderPlot({
    boxplot.index.r(means_table(), index_name = input$select_index, input$decimal_1, adjusty = input$refresh_max_1)
  })
  
  output$plot.boxplot.index.alpha <- renderPlot({
    boxplot.index.alpha(means_table(), index_name = input$select_index, alphas, input$decimal_1, adjusty = input$refresh_max_1)
  })

  output$prop_table <- DT::renderDataTable({ 
    datatable(df.prop.final(), container = sketch, rownames = FALSE, 
              options = list(info = FALSE, paging = FALSE, searching = FALSE,
              columnDefs = list(list(className = 'dt-center', targets = "_all")))
              )
  })

  output$hist_prop <- renderPlot({
    hist.prop(index_table_3(), lin_abundance_3(), index_sign_3(), input$select_index_3)
  })
  
}

?renderTable

# Run the app
shinyApp(ui, server)


