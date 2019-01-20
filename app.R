
library(shiny)
library(ggplot2)
library(tidyr)
library(ggbeeswarm)
library(car)
library(rethinking)
library(cowplot)
library(ggridges)
library(dplyr) # for arrange

# Custom Functions ----
new_precis <- function(input.df = temp,
                       HDPI.prob = 0.89){
  
  output.df <- as.data.frame(matrix(NA, 
                                    nrow = ncol(input.df), 
                                    ncol = 5))
  
  for (i in seq(1, ncol(input.df))){
    output.df[i, 1] <- names(input.df)[i]
    
    output.df[i, 2:5] <- c(
      round(mean(temp.diffs[,i]), digits = 2),
      round(sd(temp.diffs[,i]), digits = 2),
      rethinking::HPDI(temp.diffs[,i], prob = HDPI.prob)
    )  
  }
  
  names(output.df) <- c("Comparison", "Mean", "StdDev", 
                        paste("|", as.character(HDPI.prob)),
                        paste(as.character(HDPI.prob), "|"))
  
  return(output.df)
}


# shinyApp(
ui <- tagList(
  navbarPage(
    "Continuous ~ Catagorical",

    ## Tab 1 ====
    tabPanel(
      "Upload and visualization",
      titlePanel("Upload and visualize data"),
      sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Choose CSV File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),

          checkboxInput("gather", label = "Convert to long format", value = TRUE),

          numericInput("FirstGather", label = "First data column", value = 2),

          numericInput("LastGather", label = "Last data column", value = 6),

          checkboxInput("ColorBy", label = "Color by key", value = TRUE),
          ## Horizontal line
          ## tags$hr(),

          textInput("ivar", label = ("Independent Variable"), value = ""),

          textInput("dvar", label = ("Dependent Variable"), value = "")
        ),

        mainPanel(
          tableOutput("contents"),

          plotOutput("user_plot")
        )
      )
    ),

    ## Tab 2 ====
    tabPanel(
      "Bayesian Analysis",
      sidebarPanel(
        textInput("CtrlEffect", label = "Control for a factor?", value = "No"),
        checkboxInput("RunModel", label = "Ready to run model?", value = F),
        numericInput("HPDIProb", label = "Highest posterior density interval probability:", value = 0.89),
        
        tags$hr(),
        
        checkboxInput("ManualPriors", label = "Manually specify priors?", value = F),
        numericInput("a_mu", label = "Factor mu", value = 73),
        numericInput("a_sigma", label = "Factor sigma", value = 13),
        numericInput("b_mu", label = "Controled factor mu", value = 0),
        numericInput("b_sigma", label = "Controled factor sigma", value = 5),
        numericInput("sigma_min", label = "Sigma min", value = 0),
        numericInput("sigma_max", label = "Sigma max", value = 40)
      ),
      mainPanel(
        # plots
        plotOutput("PosteriorDensityRidges"),
        tableOutput("PosteriorStats"),
        tags$style(
          type = "text/css",
          "#PosteriorStats {white-space: pre-wrap;}"
        ),
        plotOutput("SubtractedPosteriors"),
        tableOutput("SubtractedPosteriorsStats"),
        tags$style(
          type = "text/css",
          "#SubtractedPosteriorsStats {white-space: pre-wrap;}"
        )
      )
    )
  )
)

# Server ----

server <- function(input, output) {

  ## For Tab 1 ====
  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
      df <- read.csv(input$file1$datapath)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
    )
    return(head(df))
  })

  output$user_plot <- renderPlot({
    req(input$file1)
    req(input$ColorBy)

    df <- read.csv(input$file1$datapath) # repeat from above

    if (input$gather == TRUE) {
      df <- gather(df, "Key", "Value", seq(
        from = input$FirstGather,
        to = input$LastGather,
        by = 1
      ))
    }

    if (input$ColorBy == FALSE) {
      ggplot(df, aes_string(x = "Key", y = "Value")) +
        ggbeeswarm::geom_quasirandom() +
        labs(x = input$ivar, y = input$dvar)
    } else {
      ggplot(df, aes_string(x = "Key", y = "Value", color = "Key")) +
        ggbeeswarm::geom_quasirandom() +
        labs(x = input$ivar, y = input$dvar)
    }
  })

  ## For Tab 2 ====

  # use observe to pull the processing out of the assignment of ui objects
  observe({
    req(input$file1)
    req(input$FirstGather)
    req(input$LastGather)

    req(input$CtrlEffect)
    req(input$RunModel)

    df <- read.csv(input$file1$datapath) # repeat from above

    if (input$gather == TRUE) {
      df <- gather(df, "Key", "Value", seq(
        from = input$FirstGather,
        to = input$LastGather,
        by = 1
      ))
    }
    
    
    # TODO: later need to automatically pick priors
    df <- as.data.frame(df) # ensure we're not working with tibbles
    
    
    if (input$ManualPriors == T){
      a.mu <<- as.numeric(input$a_mu)
      a_sigma <<- as.numeric(input$a_sigma)
      b_mu <<- as.numeric(input$b_mu)
      b_sigma <<- as.numeric(input$b_sigma)
      sigma_min <<- as.numeric(input$sigma_min)
      sigma_max <<- as.numeric(input$sigma_max)
      
    } else {
      
      #FIXME how can we calculate reasonable priors?
      
      #a_mu <- 73
      #a_sigma <- 13 
      a_mu <<- median(df$Value)
      a_sigma <<- sd(df$Value)
      
      if (input$CtrlEffect != "No"){
        #df[[as.character(input$CtrlEffect)]]

        #b_mu <- 0
        #b_sigma <- 5       
        b_mu <<- 5
        b_sigma <<- as.numeric(sd(df$Value)/2)  
      }
      
      #sigma_min <- 0
      #sigma_max <- 40
      sigma_min <<- as.numeric(0)
      sigma_max <<- as.numeric(2*(max(df$Value) - min(df$Value)))     

    }
    
    ### Fit model with MAP ####
    
    df$KEY_ID <- rethinking::coerce_index(df$Key)
    
    
    print(input$CtrlEffect)
    if (input$CtrlEffect != "No") {
      # FIXME if someone types in a non valid name this could break
      # Add an else that controls for individual effect
      # use coerce_index to make sure that we know what the controled factor is.
      df$CTRL_BY <- rethinking::coerce_index(df[[as.character(input$CtrlEffect)]])
      print("running model with factor controlled")
      model <- rethinking::map(
        alist(
          Value ~ dnorm(mu, sigma),
          mu <- a[KEY_ID] + b[CTRL_BY],
          a[KEY_ID] ~ dnorm(a_mu, a_sigma),
          b[CTRL_BY] ~ dnorm(b_mu, b_sigma),
          sigma ~ dunif(sigma_min, sigma_max)
        ),
        data = df
      )
    } else if (input$CtrlEffect == "No") {
      print("running model without factor controlled")
      model <- rethinking::map(
        alist(
          Value ~ dnorm(mu, sigma),
          mu <- a[KEY_ID],
          a[KEY_ID] ~ dnorm(a_mu, a_sigma),
          sigma ~ dunif(sigma_min, sigma_max)
        ),
        data = df
      )
    } else {
      warning("Controlled effect is neither 'No' nor a column name")
    }


    post <- rethinking::extract.samples(model)
    ## plots of posterior
    temp <- df[, c("Key", "KEY_ID")] %>% unique()
    ordered.names <- dplyr::arrange(temp, KEY_ID)[, 1]

    temp <- post$a %>% as.data.frame()
    names(temp) <- ordered.names
    ### First plot ####
    output$PosteriorDensityRidges <- renderPlot({
      ggplot(
        gather(temp, KEY, VALUE, seq(1, ncol(temp))),
        aes(x = VALUE, y = KEY, fill = KEY)
      ) +
        ggridges::geom_density_ridges(alpha = 0.5)
    })

    ### Prep plots of differences of posteriors ####

    temp.diffs <- as.data.frame(matrix(NA,
      nrow = nrow(temp),
      ncol = sum(seq(1, (ncol(temp) - 1)))
    ))

    empty.col <- 1
    for (i in seq(1, length(ordered.names) - 1)) {
      for (j in seq(i + 1, length(ordered.names))) {
        temp.diffs[, empty.col] <- temp[, i] - temp[, j]
        names(temp.diffs)[empty.col] <- paste0(ordered.names[i], "-", ordered.names[j])
        #print(paste(i, j))
        empty.col <- empty.col + 1
      }
    }

    ### Make a nice figure that can be produced from one difference column.
    # Shade HDPI for 67, 89, 97 because they're all prime
    plt.list <- purrr::map(seq(1, ncol(temp.diffs)), function(i) {
      # i=1
      # ref : http://rstudio-pubs-static.s3.amazonaws.com/5475_d63ad1667701424c9a1292ee766b45bb.html

      temp.diffs.plt <- with(density(temp.diffs[, i]), data.frame(x, y))
      # names(temp.diffs.plt) <- c(names(temp.diffs)[i], "Density")
      HPDI.67 <- rethinking::HPDI(temp.diffs[, i], prob = 0.67)
      HPDI.89 <- rethinking::HPDI(temp.diffs[, i], prob = 0.89)
      HPDI.97 <- rethinking::HPDI(temp.diffs[, i], prob = 0.97)
      CMODE <- rethinking::chainmode(temp.diffs[, i])

      ggplot(temp.diffs.plt, aes_string(x = "x", y = "y")) +
        geom_area(aes(x = ifelse(x > HPDI.97[1] & x < HPDI.97[2], x, 0)),
          fill = "firebrick", alpha = 0.4
        ) +
        geom_area(aes(x = ifelse(x > HPDI.89[1] & x < HPDI.89[2], x, 0)),
          fill = "firebrick", alpha = 0.4
        ) +
        geom_area(aes(x = ifelse(x > HPDI.67[1] & x < HPDI.67[2], x, 0)),
          fill = "firebrick", alpha = 0.4
        ) +
        geom_vline(xintercept = CMODE, size = 1, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 0, size = 1, linetype = "dashed", color = "blue") +
        geom_line(size = 1) +
        scale_y_continuous(limits = c(0, max(temp.diffs.plt$y))) +
        labs(x = names(temp.diffs)[i], y = "Density")
    })

    ### Multi-plot figure ####
    output$SubtractedPosteriors <- renderPlot({
      cowplot::plot_grid(plotlist = plt.list, ncol = 3)
    })

    ### table of posteriors ####
    output$PosteriorStats <- renderTable({
      new_precis(temp)
    })

    ### table  of differences of posteriors ####
    output$SubtractedPosteriorsStats <- renderTable({
      new_precis(temp.diffs)
    })
  })
}


# M <- read.csv("C:/Users/Daniel/Desktop/class_diving_data.csv")
# M <- gather(M, "Key", "Value", 2:6)
# fm <- lm(Value ~ Key, data = M)

# ss <- agricolae::HSD.test(fm, trt = "Key")
# ss$groups

shiny::shinyApp(ui = ui, server = server)
