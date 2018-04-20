library(shiny)
library(keras)
library(bmp)

##
## ---- ui ----
##
ui <-
  fluidPage(
    h4("Click on plot to start drawing, click again to pause"),
    column(6,
      fluidRow(
        actionButton("reset", "reset"),
        actionButton("guess", "guess"),
        strong("My guess:"),
        verbatimTextOutput("txtGuess", placeholder=TRUE)
      ),
      plotOutput("plot", width="280px", height="280px", click="plot_click",
                 hover=hoverOpts(id="hover", delay=100, delayType="throttle", clip=TRUE, nullOutside=TRUE)),
      strong("What I will send to the NN:"),
      plotOutput("rasterGuessImg")
    ),
    column(6,
      strong("What I will send to the NN:"),
      verbatimTextOutput("txtGuessImg", placeholder=TRUE)
    )
  )

##
## ---- server ----
##
server <- function(input, output, session) {
  
  # vectors with the drawn x, y coordinates
  vals <- reactiveValues(x=NULL, y=NULL)
  draw <- reactiveVal(FALSE)
  
  ## ---- enter/exit draw mode (record track) ----
  observeEvent(input$plot_click, {
    draw(!draw())
    vals$x <- c(vals$x, NA) # terminate segment in plot
    vals$y <- c(vals$y, NA) # terminate segment in plot
  })
  
  ## ---- reset plot ----
  observeEvent(input$reset, {
    draw(FALSE)
    vals$x <- NULL
    vals$y <- NULL
  })
  
  ## ---- record mouse path when draw is active ----
  observeEvent(input$hover, {
    if(draw()) {
      vals$x <- c(vals$x, input$hover$x)
      vals$y <- c(vals$y, input$hover$y)
    }
  })
  
  ## ---- render image and scale down to a 28x28 matrix
  img <- reactive({
    input$guess
    
    # save plot as a bitmap
    isolate({
      f <- tempfile()
      bmp(f, width=280, height=280)
      opar <- par(mar=c(0, 0, 0, 0))
      plot(vals$x, vals$y, xlim=c(0, 28), ylim=c(0, 28), xlab=NA, ylab=NA, axes=FALSE, type="l", lwd=25)
      par(opar)
      dev.off()
    })
    
    # read the bitmap into a matrix of 280x280, and reduce to a matrix of 28x28
    img <- read.bmp(f)
    img <- 1 - img # invert colors. Theoretically it's greyscale (0, 1)
    img28 <-
      sapply(seq(from=1, to=280, by=10), function(i) {
        sapply(seq(from=1, to=280, by=10), function(j) {
          mean(img[i:i+9, j:j+9])
        })
      })
    file.remove(f)
    
    t(img28)
  })
  
  ## ---- display drawing in the plot canvas ----
  output$plot= renderPlot({
    plot(vals$x, vals$y, xlim=c(0, 28), ylim=c(0, 28), xlab=NA, ylab=NA, las=1, type="l", lwd=8)
  })
  
  ## ---- guess what you drew ----
  model <-  withProgress(load_model_hdf5("model.hdf5"), value=0, message="Loading model")
  output$txtGuess <- renderText({
    # predict the flattened 28x28 image
    model %>% predict_classes(array_reshape(img(), c(1, 28*28)))
  })
  
  ## ---- debug output img ----
  output$txtGuessImg <- renderText({
    out <- ""
    for(i in 1:nrow(img())) {
      out <- c(out, ifelse(img()[i, ], "#", " "), "\n")
    }
    paste(out, collapse="")
  })
 
  output$rasterGuessImg <- renderPlot({
    plot.new()
    rasterImage(img(), 0, 0, 1, 1)
  }) 
}

##
## ---- run app ----
##
shinyApp(ui, server)
