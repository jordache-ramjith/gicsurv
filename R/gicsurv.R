
#' app function
#'
#' This function is to run the shiny app that will estimate covariate effects 
#' and tabulate and visualize estimates of the baseline hazard and survival.
#'
#'
#' @param NULL once run, the shiny app will be prompted
#' @return Estimates of covariate effects and baseline estimates and plots
#' @export

# Now create the UI -------------------------------------------------------

gicsurv <- function(...){
  
  library(shiny)
  library(tidyr)
  
  options(shiny.maxRequestSize = 50 * 1024 ^ 2)
  
  
  waiting_screen <- tagList(
    waiter::spin_ball(),
    h4("Cool stuff loading but may take a minute or more depending on how much data and covariates it has to work with... please be patient.")
  ) 
  
  
ui <- fluidPage(theme = shinythemes::shinytheme("superhero"),
                titlePanel("Generalized interval-censoring survival analysis"),
                waiter::useWaiter(), 
                tabsetPanel(type="pills",
                            id = "inTabset3",
                            tabPanel(
                              "Import Data",
                              value = "panel3c",
                              importUI("import")),
                            tabPanel(
                              "Covariate effects",
                              value = "arm1",
                              h2(),
                              column(2,
                                     downloadButton('downloadcovs',"Download the effects", class = "btn-success")),
                              column(
                                width = 10,
                                uiOutput("spinnertab"),
                                
                              )
                            ),
                            tabPanel(
                              "Baseline estimates and plots",
                              value="plots3d",
                              column(
                                width = 3,
                                numericInput("maxtime","Maximum time in plot",value=100,min=1,step=1e25),
                                numericInput("nboot","Number of simulations for confidence interval estimates", value=1000, min=500, step=500),
                                textInput("xlabel","Text for time axis",value="Time"),h5(),
                                selectInput('dev',"File type",choices=c("png","jpeg","pdf"),selected = "png"),
                                downloadButton('downloadPlot', 'Download plot', class = "btn-success"),h5(),
                                downloadButton('download',"Download the estimates", class = "btn-success")
                                
                              ),
                              column(width=9,
                                     column(12,
                                            uiOutput("spinner")),
                                     column(12,
                                            shiny::dataTableOutput("pred_table"))
                              )
                            ),
                            tabPanel("READ ME",
                                     value="methods",
                                     column(12,
                                            column(4, actionButton("gohome","Home",icon=icon("home"), class = "btn-info"),h5(),
                                                   downloadButton('downloadex',"Download example data", class = "btn-success") ),
                                            column(8,HTML("<h2>Purpose of the app</h2>
<p>This app has been developed for interactive use of the methods presented in generalized interval-censored survival analysis (Ramjith &amp; Bousema, 2023). To this extent it allows users to analyze survival (time-to-event) data with</p>
<ol>
<li>exact times,</li>
<li>right-censored times,</li>
<li>interval-censored start times,</li>
<li>interval-censored event times,</li>
<li>double-interval-censored times (i.e. interval-censored start and event times).</li>
</ol>
<p>All combinations of the above censoring (or none) can be present in the same dataset. The user can choose to estimate covariate effects, presented in terms of the hazard ratios, 95% CI and p-values, as well as visualizing the baseline hazard and survival curves (with or without covariates).</p>
<h2>Import data</h2>
<p>The <code>Import data</code> tab allows users to upload their own data for analysis in the app. Only <code>XLS/XLSX</code> files can be uploaded. Once the data is uploaded (never stored in the app), the user must select several parameters from the variables in their data: three time variables, a status variable and possibly covariates (not required). Further, the user can select the number of knots, where 10 (recommended) is the default option. Remember that more knots require a longer computational time. In the methodology for double-interval-censoring, time0 (= 0) is the time at last visit before the start event had occurred. And all other times are calculated as the time since this time.</p>
<ul>
<li><code>time1</code>: The right boundary of the start/origin event interval. For data where the start time is known exactly, this can be set to zero.</li>
<li><code>time2</code>: The last visit before the event of interest had occurred. For data where the event times are known exactly, <code>time2</code> and <code>time3</code> have the same values.</li>
<li><code>time3</code>: This is the first visit where the event of interest was known to have occurred. For right-censored data, this can either be defined as <code>Inf</code> or set to the same values as <code>time2</code>.</li>
<li><code>status</code>: right-censoring indicator. 1 if right-censored and 0 if not.</li>
</ul>
<h2>Analysis, tables and plots</h2>
<p>Once the data has been imported and variables and parameters have been defined, the <code>Run analysis</code> should be clicked. If covariate(s) had been selected, then the user will automatically be taken to the <code>Covariate effects</code> tab where a summary table is shown. The table can be downloaded in a csv format by clicking the <code>Download the effects</code> button. The user can proceed to the <code>Baseline estimates and plots</code> tab. If no covariates were selected, then the user would be automatically directed to this tab after clicking <code>Run analysis</code>. In the tab there are options for the user to select that effect the resulting predictions table and plot that are displayed.</p>
<ul>
<li><code>Maximum time in the plot</code>: this can be changed so that a longer/shorter time is visualized. Note this time can not exceed the maximum time in the uploaded data, else it will be reset to the data’s maximum time.</li>
<li><code>Number of simulations for confidence interval estimates</code>: The 95% confidence intervals for the hazard, cumulative hazard and survival estimates are computed through simulating a number of parameters from a normal distribution given the mean as the parameters estimated in the model and the covariance matrix of the spline parameters. A larger number of simulations is associated with better precision surrounding the standard error of the estimates.</li>
<li><code>Text for time axis</code>: to change the x-axis (time) labeling in the plot.</li>
<li><code>File type</code>: before you download the plot, you may change the filetype to either png (default), pdf or jpeg. All plots are generated in 600 dpi.</li>
</ul>
<p>Thereafter the plot and/or the predicted data can be downloaded. The predicted data can easily be used to obtain the median survival time (and 95% CI), or the user can use the downloaded data in any other program to make their own plot.</p>
<p>Lastly, on the left panel of this page, we provide some example data that may be downloaded in excel format and can be used in the app or for a visual understanding of how to construct the data to be analyzed.</p>
<p>To reference this app, please make use of the reference below.</p>
<h2>Reference</h2>
<p>Ramjith, J. &amp; Bousema, T. A generalized interval-censoring survival regression model. Journal unknown. 2023.</p>"))
                                     )
                            )
                )
                
)







##############


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #K2 <- reactiveVal()
  
  #bs_themer()
  
  thematic::thematic_shiny()
  thematic::thematic_on()
  ggplot2::theme_set(ggprism::theme_prism())
  
  data <- importSE("import")
 
  
  xvars <- varselect_server("select", data = data)
  
  allout <- reactiveValues()
  
  cow <- reactive({
    !is.null(xvars$covs())
  })
  
  
  

  
  
  
  
  observeEvent(input$do4, {
    
    if(is.null(data())){
      shinyalert::shinyalert("You can not calculate and plot the baseline estimates of the hazard and survival functions without first importing your data and selecting the correct variables. Your time variables must be numerical and your status variable must be binary.",timer=8000,type = "warning",
                size = "l")
    }
    
    req(data())
    
    allout$warner = warning_calc(
      data=data(),
      time1 = xvars$time1(),
      time2 = xvars$time2(),
      time3 = xvars$time3(),
      status = xvars$status()
    )
    
    if(allout$warner){
      shinyalert::shinyalert("You can not calculate and plot the baseline estimates of the hazard and survival functions without first importing your data and selecting the correct variables. Your time variables must be numerical and your status variable must be binary.",type = "warning",timer=8000,size="l")
    }
    
    req(!allout$warner)
    
    if(cow()){
      
      waiter::waiter_show(html = waiting_screen, color = "#2a374a")
      
      allout$calc2_obj <- calculation_analysis(
        data(),
        time1 = xvars$time1(),
        time2 = xvars$time2(),
        time3 = xvars$time3(),
        status = xvars$status(),
        covs = xvars$covs(),
        K = input$K
      )
      
      waiter::waiter_hide() 
      
      updateTabsetPanel(session, "inTabset3", "arm1")
      
      allout$calc_obj <-
        output_analysis(data(),
                        results = allout$calc2_obj,
                        covs = xvars$covs(),
                        K = input$K)
      
      allout$calc_obj2 = allout$calc_obj %>%
        dplyr::mutate(HR = paste0(HR, " (95% CI: ",lower,", ",upper,")")) %>%
        dplyr::select(Variables,HR,pval)
      
      colnames(allout$calc_obj2) = c("Variables", "HR (95% CI)", "p-value")
      
      
      
      output$calc2_table <-  renderTable({
        
        allout$calc_obj2
        
      })
      
    } else {
      
      waiter::waiter_show(html = waiting_screen, color = "#2a374a")
      
      allout$calc2_obj <- calculation_analysis(
        data(),
        time1 = xvars$time1(),
        time2 = xvars$time2(),
        time3 = xvars$time3(),
        status = xvars$status(),
        covs = NULL,
        K = input$K
      )
      
      waiter::waiter_hide()
      
      updateTabsetPanel(session, "inTabset3", "plots3d")
      
      allout$calc_obj2 = data.frame(x="No covariates selected but you can still calculate baseline estimates and plot the baseline hazard and survival curves. Go to the tab 'Baseline estimates and plot'.") 
      
      output$calc2_table <-  renderTable({
        
        allout$calc_obj2
      })
      
      
      
    }
    
    output$spinnertab <- renderUI({
      shinycssloaders::withSpinner(tableOutput("calc2_table"),type=3, color.background="#2a374a")
    })
    
    
  })
  
  observe({
    if(!is.null(allout$calc2_obj)){
      if(cow()){
        allout$pred_obj <-
          pred_analysis(results = allout$calc2_obj,
                        data(),
                        time1 = xvars$time1(),
                        time2 = xvars$time2(),
                        time3 = xvars$time3(),
                        status = xvars$status(),
                        covs = xvars$covs(),
                        maxtime = input$maxtime,
                        nboot = input$nboot,
                        K = input$K)
        

        
        
        allout$plot_obj <-
          function(){
            plot1 = ggplot2::ggplot() +
              ggplot2::geom_line(
                data = allout$pred_obj,
                ggplot2::aes(x = time, y = haz, col="hazard"),
                linewidth = 1.2
              ) +
              ggplot2::ylab("Hazard") +
              ggplot2::geom_ribbon(
                data = allout$pred_obj,
                ggplot2::aes(x = time, ymin = haz.lwr, ymax = haz.upr, fill="hazard"),
                alpha = 0.3
              ) 
            
            
            plot3 = ggplot2::ggplot() +
              ggplot2::geom_line(data = allout$pred_obj, ggplot2::aes(x = time, y = surv,col="survival"), linewidth = 1.2) +
              ggplot2::scale_y_continuous(limits = c(0, 1)) +
              ggplot2::ylab("Survival") +
              ggplot2::geom_ribbon(data = allout$pred_obj, ggplot2::aes(x = time, y = surv, ymin = surv.lwr, ymax = surv.upr, fill="survival"),
                          alpha = 0.3) +
              ggplot2::xlab(input$xlabel)
            
            plotout = ggpubr::ggarrange(
              plot1+ggplot2::theme(axis.title.x = ggplot2::element_blank()),
              plot3,
              align = "h",
              nrow=2,
              ncol=1,
              legend = "none"
            )
            
            return(plotout)
            
          }
        
        
        allout$plot_obj2 <-
          function(){
            plot1 = ggplot2::ggplot() +
              ggplot2::geom_line(
                data = allout$pred_obj,
                ggplot2::aes(x = time, y = haz),
                col = "black",
                linewidth = 1.2
              ) +
              ggplot2::ylab("Hazard") +
              ggplot2::geom_ribbon(
                data = allout$pred_obj,
                ggplot2::aes(x = time, ymin = haz.lwr, ymax = haz.upr),
                fill = "grey",
                alpha = 0.6
              )+
              ggprism::theme_prism()
            
            
            plot3 = ggplot2::ggplot(data = allout$pred_obj, ggplot2::aes(x = time, y = surv)) +
              ggplot2::geom_line(col = "black", linewidth = 1.2) +
              ggplot2::scale_y_continuous(limits = c(0, 1)) +
              ggplot2::ylab("Survival") +
              ggplot2::geom_ribbon(ggplot2::aes(ymin = surv.lwr, ymax = surv.upr),
                          fill = "grey",
                          alpha = 0.6) +
              ggplot2::xlab(input$xlabel)+
              ggprism::theme_prism()
            
            plotout = ggpubr::ggarrange(
              plot1+ggplot2::theme(axis.title.x = ggplot2::element_blank()),
              plot3,
              align = "h",
              nrow=2,
              ncol=1
            )
            
            return(plotout)
            
          }
        
        
        output$plot_curve <-  renderPlot({
          allout$plot_obj()
        })
        
        output$spinner <- renderUI({
          shinycssloaders::withSpinner(plotOutput("plot_curve"),type=3, color.background="#2a374a")
        })
        
      } else {
        
        allout$pred_obj <-
          pred_analysis(results = allout$calc2_obj,
                        data(),
                        time1 = xvars$time1(),
                        time2 = xvars$time2(),
                        time3 = xvars$time3(),
                        status = xvars$status(),
                        covs = NULL,
                        maxtime = input$maxtime,
                        nboot = input$nboot,
                        K = input$K)
        
        
        
        
        allout$plot_obj <-
          function(){
            plot1 = ggplot2::ggplot() +
              ggplot2::geom_line(
                data = allout$pred_obj,
                ggplot2::aes(x = time, y = haz, col="hazard"),
                linewidth = 1.2
              ) +
              ggplot2::ylab("Hazard") +
              ggplot2::geom_ribbon(
                data = allout$pred_obj,
                ggplot2::aes(x = time, ymin = haz.lwr, ymax = haz.upr, fill="hazard"),
                alpha = 0.3
              ) 
            
            
            plot3 = ggplot2::ggplot() +
              ggplot2::geom_line(data = allout$pred_obj, ggplot2::aes(x = time, y = surv,col="survival"), linewidth = 1.2) +
              ggplot2::scale_y_continuous(limits = c(0, 1)) +
              ggplot2::ylab("Survival") +
              ggplot2::geom_ribbon(data = allout$pred_obj, ggplot2::aes(x = time, y = surv, ymin = surv.lwr, ymax = surv.upr, fill="survival"),
                          alpha = 0.3) +
              ggplot2::xlab(input$xlabel)
            
            plotout = ggpubr::ggarrange(
              plot1+ggplot2::theme(axis.title.x = ggplot2::element_blank()),
              plot3,
              align = "h",
              nrow=2,
              ncol=1,
              legend = "none"
            )
            
            return(plotout)
            
          }
        
        
        allout$plot_obj2 <-
          function(){
            plot1 = ggplot2::ggplot() +
              ggplot2::geom_line(
                data = allout$pred_obj,
                ggplot2::aes(x = time, y = haz),
                col = "black",
                linewidth = 1.2
              ) +
              ggplot2::ylab("Hazard") +
              ggplot2::geom_ribbon(
                data = allout$pred_obj,
                ggplot2::aes(x = time, ymin = haz.lwr, ymax = haz.upr),
                fill = "grey",
                alpha = 0.6
              )+
              ggprism::theme_prism()
            
            
            plot3 = ggplot2::ggplot(data = allout$pred_obj, ggplot2::aes(x = time, y = surv)) +
              ggplot2::geom_line(col = "black", linewidth = 1.2) +
              ggplot2::scale_y_continuous(limits = c(0, 1)) +
              ggplot2::ylab("Survival") +
              ggplot2::geom_ribbon(ggplot2::aes(ymin = surv.lwr, ymax = surv.upr),
                          fill = "grey",
                          alpha = 0.6) +
              ggplot2::xlab(input$xlabel)+
              ggprism::theme_prism()
            
            plotout = ggpubr::ggarrange(
              plot1+ggplot2::theme(axis.title.x = ggplot2::element_blank()),
              plot3,
              align = "h",
              nrow=2,
              ncol=1
            )
            
            return(plotout)
          }
        
        
        output$plot_curve <-  renderPlot({
          allout$plot_obj()
        })
        
        
        output$spinner <- renderUI({
          shinycssloaders::withSpinner(plotOutput("plot_curve"),type=3, color.background ="#2a374a")
        })
        
      }
      
      
      
      output$pred_table <-  shiny::renderDataTable(
        format(allout$pred_obj, digits = 2, nsmall=4), options = list(pageLength = 10, info = FALSE, lengthMenu = list(c(10,50,100,200, -1), c("10","50","100","200","All")),scrollX = TRUE) 
      )
      
      
    }
  })
  

  
  observeEvent(input$swaptabs, {
    updateTabsetPanel(session, "inTabset3", 'methods')
  })
  
  observeEvent(input$gohome, {
    updateTabsetPanel(session, "inTabset3", 'panel3c')
  })
  
  output$downloadcovs <- downloadHandler(
    filename = function(){"estimated_effects.csv"}, 
    content = function(fname){
      utils::write.csv(allout$calc_obj, fname, row.names = FALSE)
    }
  )
  
  
  output$download <- downloadHandler(
    filename = function(){"baseline_estimates.csv"}, 
    content = function(fname){
      utils::write.csv(allout$pred_obj, fname, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0('baseline_plots.',input$dev) },
    content = function(file) {
      ggplot2::ggsave(file, plot = allout$plot_obj2(), device = paste0(input$dev), dpi=300,width=8,height=8)
    }
  )
  
  
  #testdata <- read_excel("simdat_ex.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "sheet1")
  openxlsx::writeData(wb, sheet = 1, x = testdata, startCol = 1, startRow = 1)
  
  
  output$downloadex <- downloadHandler(
    filename = function(){"testdata.xlsx"}, 
    content = function(file) {
      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
}
