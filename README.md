# Nonlinear regression operator

##### Description

The `nonlinear_regression` operator fits a non linear function to observed data.

Different models are available:   
- the `Three-parameter log-logistic` (also know as the 3PL nonlinear regression model)  
- the `Four-parameter log-logistic`
- the `Michaelis-Menten` often used in enzyme kinetics  

##### Usage

Input projection|.
---|---
`y-axis`| measurement value (e.g. response)
`x-axis`| explanatory value (e.g. dose)

Output relations|.
---|---
`function.type`| Model to be fitted. Any of `Three-parameter log-logistic`, `Four-parameter log-logistic` or `Michaelis-Menten`.
`n.predictions`| Number of predicted values to generate.
`response.output`| Comma-separated list of percentages of maximal response to generate predicted values for (for example,Y50 will give you the EC50 value as X50, Y50 being hald the maximal response value).

Output relations|.
---|---
`parameters`| numeric, fitted parameters (depend on the fitted model)
`x-pred`| predicted x-values
`y-pred`| predicted y-values

##### Details

[Here is a good introduction to non-linear regression](https://www.statforbiology.com/nonlinearregression/usefulequations).

See [Nonlinear regression on Wikipedia](https://en.wikipedia.org/wiki/Nonlinear_regression).

##### See Also

[lm_operator](https://github.com/tercen/lm_operator)

