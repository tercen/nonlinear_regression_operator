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
relative.response <- ctx$op.value('relative.response', as.logical, FALSE)

dose.transformation <- ctx$op.value('dose.transformation', as.character, "None") # log10, none
dt <- switch(dose.transformation,
             Log = exp(1),
             Log10 = 10,
             None = NULL)

model.function <- switch(
  function.type,
  "Three-parameter log-logistic" = "LL.3",
  "Four-parameter log-logistic" = "LL.4",
  "Michaelis-Menten" = "MM.2"
)
par_names <- switch(
  function.type,
  "Three-parameter log-logistic" = c("b", "d", "e"),
  "Four-parameter log-logistic" = c("b", "c", "d", "e"),
  "Michaelis-Menten" = c("d", "e")
)

dt_in <- ctx %>% 
  dplyr::select(.x, .y, .ri, .ci) %>%
  data.table::as.data.table()

get_pseudo_r2 <- function(mod) {
  predicted <- mod$predres[, "Predicted values"]
  actual <- mod$predres[, "Residuals"] + predicted
  rss <- sum((predicted - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  1 - rss/tss
}

limits <- c(0, 1e6)
x_rng <- range(dt_in$.x) * 1.1
if(x_rng[1] < limits[1]) limits[1] <- x_rng[1]
if(x_rng[2] > limits[2]) limits[2] <- x_rng[2]

df_result <- dt_in[, 
  {
      mod <- try(drm(
        .y ~ .x, fct = match.fun(model.function)(), logDose = dt
      ), silent = TRUE)
      
      if(!inherits(mod, 'try-error')) {
        coef <- mod$coefficients
        names(coef) <- gsub(pattern = ":\\(Intercept\\)", "", names(coef))
        out <- as.data.frame(t(coef))
        
        x.pred <- seq(min(.x), max(.x), length.out = n.predictions)
        y.pred <- predict(mod, newdata = data.frame(x.pred))
        out <- cbind(out, x.pred, y.pred)
        
        out["pseudo_R2"] <- get_pseudo_r2(mod)
        
        if(model.function %in% c("LL.3", "LL.4", "MM.2")) {
          f <- function(x, y) y - predict(mod, data.frame(.x = x))[1]
          for(i in response.output) {
            y_ed <- ifelse(
              relative.response & model.function == "LL.4",
              (out$c[1] + out$d[1]) * i / 100,
              out$d[1] * i / 100
            )
            x <- try(uniroot(f, limits, y = y_ed)$root, silent = TRUE)
            if(inherits(x, 'try-error')) x <- NA_real_
            vn <- paste0("X", i)
            out[[paste0("X", i)]] <- x
            out[[paste0("Y", i)]] <- as.double(out$d[1] * i)
          }
        }
      } else {
        if(length(unique(.y)) == 1) {
          x.pred <- seq(min(.x), max(.x), length.out = n.predictions)
          out <- data.frame(x.pred = x.pred, y.pred = .y[1])
          out[paste0("X", response.output)] <- NA_real_
          out[paste0("Y", response.output)] <- .y[1]
        } else {
          out <- data.frame(x.pred = NA_real_, y.pred = NA_real_)
          out[paste0("X", response.output)] <- NA_real_
          out[paste0("Y", response.output)] <- NA_real_
        }
        out[par_names] <- NA_real_
        out["pseudo_R2"] <- NA_real_
        out <- out[c(par_names, "x.pred", "y.pred", "pseudo_R2", paste0("X", response.output), paste0("Y", response.output))]
      }
  out
  }, by = c(".ri", ".ci")
]

if(model.function == "LL.4") {
  df_result <- df_result %>%
    mutate(Span = d - e)
} 

sum.table <- df_result %>%
  dplyr::select(-x.pred, -y.pred) %>%
  unique() %>% 
  arrange(.ri, .ci) %>%
  as_tibble() %>%
  ctx$addNamespace() 

pred.table <- df_result %>%
  dplyr::select(.ri, .ci, x.pred, y.pred) %>%
  dplyr::rename(x_pred = x.pred, y_pred = y.pred) %>%
  arrange(.ri, .ci) %>%
  as_tibble() %>%
  ctx$addNamespace()

ctx$save(list(sum.table, pred.table))
