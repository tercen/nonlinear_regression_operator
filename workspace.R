library(tercen)
library(dplyr)
library(drc)

options("tercen.workflowId" = "2553cb89b6ec3bc593e238e0df01901f")
options("tercen.stepId"     = "34c0d45e-1212-477f-a95a-174778f917ef")

getOption("tercen.workflowId")
getOption("tercen.stepId")

do.nlm <- function(df, function.type) {
  
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
    if(function.type == "LL.3") {
      f <- function(x, y) y - predict(mod, data.frame(.x = x))[1]
      x50 <- uniroot(f, c(0, 1e6), y = out$d[1] * 0.50)$root
      x90 <- uniroot(f, c(0, 1e6), y = out$d[1] * 0.90)$root
      x99 <- uniroot(f, c(0, 1e6), y = out$d[1] * 0.99)$root
      x99.9 <- uniroot(f, c(0, 1e6), y = out$d[1] * 0.999)$root
      out <- cbind(out, x50, x90, x99, x99.9)
      
    }
  } 
  return(out)
}

ctx <- tercenCtx()

if(inherits(try(ctx$select(".x")), 'try-error')) stop("x axis is missing.")
if(inherits(try(ctx$select(".y")), 'try-error')) stop("y axis is missing.")

function.type <- "LL.3"
if(!is.null(ctx$op.value('function.type'))) function.type <- (ctx$op.value('function.type'))

ctx %>% 
  dplyr::select(.x, .y, .ri, .ci) %>% 
  group_by(.ri, .ci) %>% do(do.nlm(., function.type)) %>% 
  ctx$addNamespace() %>%
  ctx$save()
