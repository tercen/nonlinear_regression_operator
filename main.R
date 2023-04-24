suppressPackageStartupMessages({
  library(tercen)
  library(dplyr)
  library(dtplyr)
  library(drc)
  library(data.table)
})

ctx <- tercenCtx()

if(!ctx$hasNumericXAxis) stop("x axis is missing.")
if(length(ctx$yAxis) < 1) stop("y axis is missing.")

function.type <- ctx$op.value('function.type', as.character, "Four-parameter log-logistic")
n.predictions <- ctx$op.value('n.predictions', as.double, 100)
response.output <- ctx$op.value('response.output', as.character, "50, 90, 99")
response.output <- as.numeric(trimws(strsplit(response.output, ",")[[1]]))

dt_in <- ctx %>% 
  dplyr::select(.x, .y, .ri, .ci) %>%
  data.table::as.data.table()

df_result <- dt_in[, 
  {
      
      if(function.type == "Three-parameter log-logistic") {
        function.type <- "LL.3"
        par_names <- c("b", "d", "e")
      } 
      if(function.type == "Four-parameter log-logistic") {
        function.type <- "LL.4"
        par_names <- c("b", "c", "d", "e")
      } 
      if(function.type == "Michaelis-Menten") { 
        function.type <- "MM.2"
        par_names <- c("d", "e")
      } 
      
      eval(parse(text = paste0("ff <- ", function.type, "()")))
      mod <- try(drm(.y ~ .x, fct = ff))
      
      if(!inherits(mod, 'try-error')) {
        coef <- mod$coefficients
        names(coef) <- gsub(pattern = ":\\(Intercept\\)", "", names(coef))
        out <- as.data.frame(t(coef))
        
        x.pred <- seq(min(.x), max(.x), length.out = n.predictions)
        y.pred <- predict(mod, newdata = data.frame(x.pred))
        out <- cbind(out, x.pred, y.pred)
        
        if(function.type %in% c("LL.3", "LL.4", "MM.2")) {
          f <- function(x, y) y - predict(mod, data.frame(.x = x))[1]
          for(i in response.output) {
            x <- try(uniroot(f, c(0, 1e6), y = out$d[1] * i / 100)$root, silent = TRUE)
            if(inherits(x, 'try-error')) x <- NA
            eval(parse(text = paste0("out$X", i, " <- x")))
            eval(parse(text = paste0("out$Y", i, " <- out$d[1] * i")))
          }
        }
      } else {
        if(length(unique(.y)) == 1) {
          x.pred <- seq(min(.x), max(.x), length.out = n.predictions)
          nas <- list(
            x.pred = x.pred, y.pred = y[1],
            X50 = NA, X90 = NA, X99 = NA,
            Y50 = y[1], Y90 = y[1], Y99 = y[1])
          nas[par_names] <- NA
          out <- data.frame(nas)
        } else {
          nas <- list(
            x.pred = NA, y.pred = NA,
            X50 = NA, X90 = NA, X99 = NA,
            Y50 = NA, Y90 = NA, Y99 = NA)
          nas[par_names] <- NA
          out <- data.frame(nas)
          
        }
      }
  out
    
  }, by = c(".ri", ".ci")
]

sum.table <- df_result %>%
  dplyr::select(-x.pred, -y.pred) %>%
  unique() %>% 
  arrange(.ri, .ci) %>%
  as_tibble() %>%
  ctx$addNamespace() 

pred.table <- df_result %>%
  dplyr::select(.ri, .ci, x.pred, y.pred) %>%
  arrange(.ri, .ci) %>%
  as_tibble() %>%
  ctx$addNamespace()

ctx$save(list(sum.table, pred.table))

# 2. Flexible 50. Handle NAs
