# Maths and Theory

A half single particle model by Group B for the 2023 PX915 group project.

## 1 Mathematics and Algorithms

A Crank-Nicolson semi-implicit finite difference scheme is used  to obtain concentration of lithium in the sphere, c(iapp, r), at each time step by approximating the partial differential equation (PDE) solution. 

This is accurate to second order both spatially and temporally, i.e. $O(\Delta r^2)$ \& $O(\Delta t^2)$.

$$
\frac{c_i^{j+1} - c_i^j}{\Delta t} = \frac{D}{2r_i^2} \Bigg[
\left(r_i^2 \frac{c_{i+1}^{j+1} - 2c_i^{j+1} + c_{i-1}^{j+1}}{\Delta r^2} + r_i^2 \frac{c_{i+1}^j - 2c_i^j + c_{i-1}^j}{\Delta r^2}\right) + \nonumber \\ r_i \left(\frac{c_{i+1}^{j+1} - c_{i-1}^{j+1}}{2\Delta r} + \frac{c_{i+1}^{j} - c_{i-1}^{j}}{2\Delta r} \right) \Bigg]
$$
