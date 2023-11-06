# -*- coding: utf-8 -*-
"""
Created on Sun Nov 18 14:37:30 2018

@author: Bruno
"""

#==============================================================================
# Recursive Methods - Optimal Growth
#==============================================================================

import os
import numpy as np
from scipy.optimize import fminbound



#==============================================================================
# Exercise: U(c) = c^(1-sigma)/(1-sigma)
#==============================================================================


def bellman_operator(w, grid, β, u, f, shocks, Tw=None, compute_policy=0):
    #? === Apply linear interpolation to w === #
    w_func = lambda x: np.interp(x, grid, w)        
    # w = vector of values for w(x) for x in grid !! interpolation if z is between to points of the grid for w(z)

    #? == Initialize Tw if necessary == #
    if Tw is None:
        Tw = np.empty_like(w)

    if compute_policy:
        σ = np.empty_like(w)

    #? == set Tw[i] = max_c { u(c) + β E w(f(y  - c) z)} == #
    for i, y in enumerate(grid):
        def objective(c, y=y):
            return - u(c) - β * np.mean(w_func(f(y - c) * shocks))      # mean is here bc code uses shock ... y - c > delta = 1 in this code
        c_star = fminbound(objective, 1e-10, y)     # minus bc uses fmin (want to maximize)
        if compute_policy:
            #σ[i] = c_star
            #σ[i] = y - c_star #capital at date t
            σ[i] = f(y-c_star) #y_(t+1) as a function of y_t
        Tw[i] = - objective(c_star)

    if compute_policy:
        return Tw, σ
    else:
        return Tw





def solve_optgrowth(initial_w, grid, β, u, f, shocks, tol=1e-4, max_iter=500):

    w = initial_w  # Set initial condition
    error = tol + 1
    i = 0

    #? == Create storage array for bellman_operator. Reduces  memory
    # allocation and speeds code up == #
    Tw = np.empty(len(grid))

    # Iterate to find solution
    while error > tol and i < max_iter:
        w_new = bellman_operator(w,
                                 grid,
                                 β,
                                 u,
                                 f,
                                 shocks,
                                 Tw) 
        #* w_new = Tw
        error = np.max(np.abs(w_new - w))
        w[:] = w_new
        i += 1
        print("Iteration "+str(i)+'\n Error is '+str(error)+'\n')
        
    # Computes policy final step
    policy = bellman_operator(w,
                         grid,
                         β,
                         u,
                         f,
                         shocks,
                         Tw,
                         compute_policy=1)[1]
 
    return [w, policy]




class CES_OG:
    """
    Constant elasticity of substitution optimal growth model, Cobb-Douglas production and
    multiplicative lognormal shock, so that
    
        F(K,L) = K^α.L^(1-α) > L normalized at 1
        F(K,1) = K^α

        y = f(k, z) = z k^α

    with z ~ LN(μ, s).

    The class holds parameters and true value and policy functions.
    """

    def __init__(self, α=0.4, β=0.96, μ=0, s=0.1, sigma=0.9):

        self.α, self.β, self.μ, self.s, self.sigma = α, β, μ, s, sigma 
        #* β > take the real interest rate (or from micro data)
        #* alpha > look at capital and labour share : compute w.L/Y and r.K/Y using Y is homogenous of order 1 F_L(.)L + F_K(.)K = F(L,K)
        #* U(c) = C^(1-sigma) - 1 / 1 - sigma 
        #* >>> sigma measure or risk aversion : either invest in bonds or equity : infer from portfolio how risk adverse they are 
        # (RBC work but for it to work need high labour elasticity (1st puzzle and there's also the equity premium puzzle))

        #? == Some useful constants == #
        self.ab = α * β
        self.c1 = np.log(1 - self.ab) / (1 - β)
        self.c2 = (μ + α * np.log(self.ab)) / (1 - α)
        self.c3 = 1 / (1 - β)
        self.c4 = 1 / (1 - self.ab)

    def u(self, c):
        " Utility "
        return (c**(1-self.sigma)-1)/(1-self.sigma)


    def f(self, k):
        " Deterministic part of production function.  "
        return k**self.α






# Creation of the model
ces = CES_OG()
# == Unpack parameters / functions for convenience == #
α, β, μ, s, sigma = ces.α, ces.β, ces.μ, ces.s, ces.sigma


### Setup of the grid
grid_max = 1         # Largest grid point
grid_size = 200      # Number of grid points
shock_size = 250     # Number of shock draws in Monte Carlo integral
grid = np.linspace(1e-5, grid_max, grid_size)

# Initial conditions and shocks
shocks = np.exp(μ + s * np.random.randn(shock_size))
initial_w = 5 * np.log(grid)


# Computation of the value function
solve = solve_optgrowth(initial_w, grid, β, u=ces.u,
                               f=ces.f, shocks=shocks, tol=1e-4, max_iter=500)

value_approx = solve[0]
policy_function = solve[1]


#==============================================================================
# Plotting value function
#==============================================================================
import matplotlib.pyplot as plt

fig, ax = plt.subplots(figsize=(9, 5))
ax.set_ylim(min(value_approx), max(value_approx))
ax.plot(grid, value_approx, lw=2, alpha=0.6, label='approximate value function')
ax.set_xlabel('c')
ax.set_ylabel('v')
ax.legend(loc='lower right')
plt.show()


#==============================================================================
# Plotting Policy function
#==============================================================================

fig, ax = plt.subplots(figsize=(9, 5))
ax.set_ylim(min(policy_function), max(policy_function))
ax.plot(grid, policy_function, lw=2, alpha=0.6, label='approximate policy function')

# 45° line
ax.plot(grid, grid, lw=2, alpha=0.6, label='45 degrees line')

ax.set_xlabel('y_t')
ax.set_ylabel('y_(t+1)')
ax.legend(loc='lower right')
plt.show()

