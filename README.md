# Nonlinear regression operator

##### Description

The `nonlinear_regression` operator fits a non linear function to observed data.

Model available are: - the `Three-parameter log-logistic` ( also know as the 3PL nonlinear regression model)  
                     - the `Michaelis-Menten` often used in enzyme kinetics  

##### Usage

Input projection|.
---|---
`y-axis`| measurement value (e.g. response)
`x-axis`| explanatory value (e.g. dose)

Output relations|.
---|---
`function.type`| Model to be fitted. Any of `Three-parameter log-logistic` or `Michaelis-Menten`.

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

[lm_operator](https://github.com/tercen/lm_operator)

