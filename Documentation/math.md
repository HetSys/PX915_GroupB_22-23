# Maths and Theory

A half single particle model by Group B for the 2023 PX915 group project.

## 1 Mathematics and Algorithms

A Crank-Nicolson semi-implicit finite difference scheme is used  to obtain concentration of lithium in the sphere, c(iapp, r), at each time step by approximating the partial differential equation (PDE) solution. 

This is accurate to second order both spatially and temporally, i.e. $O(\Delta r^2)$ \& $O(\Delta t^2)$.

$$
\frac{c_i^{j+1} - c_i^j}{\Delta t} = \frac{D}{2r_i^2} \Bigg[
\left(r_i^2 \frac{c_{i+1}^{j+1} - 2c_i^{j+1} + c_{i-1}^{j+1}}{\Delta r^2} + r_i^2 \frac{c_{i+1}^j - 2c_i^j + c_{i-1}^j}{\Delta r^2}\right) + \nonumber \\ r_i \left(\frac{c_{i+1}^{j+1} - c_{i-1}^{j+1}}{2\Delta r} + \frac{c_{i+1}^{j} - c_{i-1}^{j}}{2\Delta r} \right) \Bigg]
$$

####################################################

A Crank-Nicolson semi-implicit finite difference scheme will be used to approximate the partial differential equation (PDE) solution to obtain the concentration of lithium in the sphere, `$c(i_app, r)$`, at each time step. This is accurate to second order both spatially and temporally, i.e. <!-- end of the list --> `$O(Delta r^2)$` & `$O(Delta t^2)$`.

A key benefit of the Crank-Nicolson scheme is that it is unconditionally stable for the spherically symmetric diffusion equation (the PDE of interest), thus not restricting the user's choice of step size spatially or temporally. Although, due to being second order, the accuracy may be impacted by a larger step size.

The implicit nature of the Crank-Nicolson scheme makes it ideal for numerically approximating solutions to both stiff and highly nonlinear diffusion problems, providing the user a strong base from which to update this solution framework.

Additionally, the Crank-Nicolson scheme is consistent meaning that 'the error of the numerical method converges to zero as the grid spacing, or time step, reduces to zero from above, under certain regularity conditions on the solution.'

The consistency and stability of the Crank-Nicolson scheme means it satisfies the Lax equivalence theorem, which states that a numerical method is convergent if and only if it is both consistent and stable.

Specific Equation:

<!-- end of the list -->

> $\frac{{c_i^{j+1} - c_i^j}}{{\Delta t}} = \frac{{D}}{{2r_i^2}} \left[ \left(r_i^2 \frac{{c_{i+1}^{j+1} - 2c_i^{j+1} + c_{i-1}^{j+1}}}{{\Delta r^2}} + r_i^2 \frac{{c_{i+1}^j - 2c_i^j + c_{i-1}^j}}{{\Delta r^2}}\right) + r_i \left( \frac{{c_{i+1}^{j+1} - c_{i-1}^{j+1}}}{{2\Delta r}} + \frac{{c_{i+1}^{j} - c_{i-1}^{j}}}{{2\Delta r}} \right) \right]$

Generally:
<!-- end of the list -->
> $\frac{u_i^{j+1} - u_i^j}{\Delta t} = \frac{1}{2} \left[ F^{j+1}_i(u,r,t,\frac{\partial u}{\partial r},\frac{\partial ^2 u}{\partial r^2}) + F^j_i(u,r,t,\frac{\partial u}{\partial r},\frac{\partial ^2 u}{\partial r^2}) \right].$

This is an average of standard forward and backward Euler methods.

Boundary conditions will be treated through the use of ghost nodes, which assign values for the function of interest to regions just beyond the domain of the problem in an attempt to approximate the first derivatives present in the Neumann boundary conditions specified. For example,

<!-- end of the list -->

> $\[\frac{\partial c}{\partial r}\Bigg|_{r=0} \approx \frac{c_1 - c_{-1}}{2\Delta r} = 0 \iff c_1 = c_{-1}.\]$


[\frac{\partial c}{\partial r}\Bigg|{r=0} \approx \frac{c_1 - c{-1}}{2\Delta r} = 0 \iff c_1 = c_{-1}.]

The accuracy of the scheme will be chosen such that it is equivalent to the interior solved points.

Finally, a general initial condition is specified of the form
<!-- end of the list -->
> $c = c_0 \textrm{ at } t=0.$

The form of c_0 is assumed to be constant in R.

