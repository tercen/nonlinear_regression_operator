library(tercen)
library(dplyr)
library(drc)

do.nlm <- function(df, function.type) {
  
  out <- data.frame(
    .ri = df$.ri[1],
    .ci = df$.ci[1]
  )
  eval(parse(text = paste0("ff <- ", function.type, "()")))
  mod <- try(drm(.y ~ .x, fct = ff, data = df), silent = TRUE)
  
  if(!inherits(mod, 'try-error')) {
    coef <- mod$coefficients
    names(coef) <- gsub(pattern = ":\\(Intercept\\)", "", names(coef))
    out <- cbind(out, t(coef))
    
    x.pred <- seq(min(df$.x), max(df$.x), length.out = 100)
    y.pred <- predict(mod, newdata = data.frame(x.pred))
    out <- cbind(out, x.pred, y.pred)

    if(function.type == "LL.3") {
      f <- function(x, y) y - predict(mod, data.frame(.x = x))[1]
      for(i in c(0.5, 0.9, 0.99, 0.999)) {
        x <- try(uniroot(f, c(0, 1e6), y = out$d[1] * i)$root, silent = TRUE)
        if(inherits(x, 'try-error')) x <- NA
        eval(parse(text = paste0("x_", i * 100, " <- x")))
      }
      out <- cbind(out, x_50, x_90, x_99, x_99.9)
    }
  } else {
    nas <- list(b = NA, d = NA, e = NA,x.pred = NA, y.pred = NA, x_50 = NA, x_90 = NA, x_99 = NA, x_99.9 = NA)
    out <- cbind(out, nas)
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
