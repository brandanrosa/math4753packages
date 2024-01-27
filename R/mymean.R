#' @title Alternative Mean
#'
#' @param x a numeric vector
#' @param trim value between 0 and 0.5
#' @param graph box, hist, or both
#'
#' @importFrom grDevices rgb
#' @importFrom graphics boxplot
#' @importFrom graphics hist
#' @importFrom graphics layout
#' @importFrom stats na.omit
#' @importFrom utils write.csv
#'
#' @return a graph, list with arithmetic means and summary. Also a trimmed data csv file
#' @export
#'
#' @examples
#' \dontrun{mymean(x = rnorm(30), trim = 0.1, graph = "both)}

mymean <- function(x, trim = 0, graph = "NULL"){
  y <- na.omit(x)
  l <- length(y)
  trmy <- floor(trim * l)
  sy <- sort(y)
  if(trim != 0){
    ssy <- sy[-c(1:trmy, l:(l - (trmy - 1)))]
    mean <- sum(ssy) / length(ssy)
  } else({
    ssy <- y
    mean <-  sum(y) / length(y)
  })

  myhist <- function(){
    h <- hist(ssy, plot = FALSE)
    r <- h$density/max(h$density)
    hist(ssy,
         col =  rgb(r,r^2,0),
         main = paste("Trim =", trim),
         xlab = "Trimmed x")
  }

  mybox <- function(){
    b <- boxplot(ssy, plot = FALSE)
    boxplot(ssy,
            col = "Blue",
            range = 3,
            main = paste("Trim =", trim),
            xlab = "Trimmed x")
  }

  switch(graph,

         hist =  {
           myhist()
         },

         box = {
           mybox()
         },
         both = {# can you see a better way to code this?
           layout(matrix(1:2, nrow = 1))
           myhist()
           mybox()
         },

         NULL = NULL,
         stop("MUST be one of \"hist\", \"box\", \"both\" or \"NULL\" ", call. = FALSE)
  )

# Would really like to make this conditional... filewrite = T/F... etc
  write.csv(x = ssy,
            file = paste0(Sys.Date(),"TRIMMEDx.csv"),
            row.names = FALSE)

  list(mean = mean, summary = summary(ssy))
}
