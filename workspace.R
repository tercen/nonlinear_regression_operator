library(tercen)
library(dplyr)
library(drc)

options("tercen.workflowId" = "edb692c87397bd85e5bca26eba005ca8")
options("tercen.stepId"     = "50b04de7-e785-449d-b367-9a5c0fd27312")

getOption("tercen.workflowId")
getOption("tercen.stepId")

do.nlm <- function(df, function.type) {
  
  if(function.type == "Three-parameter log-logistic") {
    function.type <- "LL.3"
    par_names <- c("b", "d", "e")
  } 
  if(function.type == "Michaelis-Menten") { 
    function.type <- "MM.2"
    par_names <- c("d", "e")
  } 

  out <- data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1]
  )
  eval(parse(text = paste0("ff <- ", function.type, "()")))
  mod <- try(drm(.y ~ .x, fct = ff, data = df))
  
  if(!inherits(mod, 'try-error')) {
    coef <- mod$coefficients
    names(coef) <- gsub(pattern = ":\\(Intercept\\)", "", names(coef))
    out <- cbind(out, t(coef))
    
    x.pred <- seq(min(df$.x), max(df$.x), length.out = 100)
    y.pred <- predict(mod, newdata = data.frame(x.pred))
    out <- cbind(out, x.pred, y.pred)
    
    if(function.type == "LL.3" | function.type == "MM.2") {
      f <- function(x, y) y - predict(mod, data.frame(.x = x))[1]
      for(i in c(0.5, 0.9, 0.99)) {
        x <- try(uniroot(f, c(0, 1e6), y = out$d[1] * i)$root, silent = TRUE)
        if(inherits(x, 'try-error')) x <- NA
        eval(parse(text = paste0("out$X", i * 100, " <- x")))
        eval(parse(text = paste0("out$Y", i * 100, " <- out$d[1] * i")))
      }
    }
  } else {
    if(length(unique(df$.y)) == 1) {
      x.pred <- seq(min(df$.x), max(df$.x), length.out = 100)
      nas <- list(
        x.pred = x.pred, y.pred = df$y[1],
        X50 = NA, X90 = NA, X99 = NA,
        Y50 = df$y[1], Y90 = df$y[1], Y99 = df$y[1])
      nas[par_names] <- NA
      out <- cbind(out, nas)
    } else {
      nas <- list(
        x.pred = NA, y.pred = NA,
        X50 = NA, X90 = NA, X99 = NA,
        Y50 = NA, Y90 = NA, Y99 = NA)
      nas[par_names] <- NA
      out <- cbind(out, nas)
      
    }
  }
  return(out)
}

ctx <- tercenCtx()

if(inherits(try(ctx$select(".x")), 'try-error')) stop("x axis is missing.")
if(inherits(try(ctx$select(".y")), 'try-error')) stop("y axis is missing.")

function.type <- "Three-parameter log-logistic"
if(!is.null(ctx$op.value('function.type'))) function.type <- (ctx$op.value('function.type'))

ctx %>% 
  dplyr::select(.x, .y, .ri, .ci) %>% 
  group_by(.ri, .ci) %>%
  do(do.nlm(., function.type)) %>% 
  ctx$addNamespace() %>%
  ctx$save()

