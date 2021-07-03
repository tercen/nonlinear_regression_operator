# Nonlinear regression operator

##### Description

The `nonlinear_regression` operator fits a non linear function to observed data.

##### Usage

Input projection|.
---|---
`y-axis`| measurement value (e.g. response)
`x-axis`| explanatory value (e.g. dose)

Output relations|.
---|---
`function.type`| Model to be fitted (`fct` parameter from the `drc` R package). Default is the three parmeters log-logistic function `LL.3`

Output relations|.
---|---
`parameters`| numeric, fitted parameters (depend on the fitted model)
`x-pred`| predicted x-values
`y-pred`| predicted y-values

##### Details

[Here is a good introduction to non-linear regression](https://www.statforbiology.com/nonlinearregression/usefulequations).

See [Linear regression on Wikipedia](https://en.wikipedia.org/wiki/Linear_regression) and
`lm` [R function documentation](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm).

##### See Also

[lm](https://github.com/tercen/lm_operator)

