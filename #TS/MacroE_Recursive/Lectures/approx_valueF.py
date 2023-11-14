import numpy as np
from scipy.optimize import fminbound

# U(c) = c^(1-sigma)/(1-sigma)
# f(k) = (1-delta)*k + k**alpha

def bellman_operator(w, Tw, grid, beta, u, f, policy=0):
    w_func = lambda x: np.interp(x, grid, w)
    if policy:
        g = np.empty_like(w)
          
    #? pr tt point de la grid T(w[i]) = max_(k' in [O,y]) { u(y-k') + beta.w_func(k') } where y = f(grid[i])
    # " w[i] = w(grid[i]) "
    for i, k in enumerate(grid):
        y = f(k)
        def objective(k_prime,y=y):
            return - ( u(y-k_prime) + beta*w_func(k_prime) )
        k_star = fminbound(objective, x1=1e-10, x2=f(k)) #[x1,x2] = Gamma(k)
        Tw[i] = - objective(k_star)
        
        if policy:
            g[i] = k_star
    
    if policy:
        return Tw,g
    else:
        return Tw

def solve_optimal_growth(w_initial, grid, beta, u, f, err_max=1e-3, max_iter=500):
    
    w = w_initial
    i = 0
    
    init = bellman_operator(w=w_initial, Tw=np.empty(len(grid)), beta=beta, u=u, f=f,policy=1)[0]
    Tw, policy_function = init[0],init[1] 
    error = np.max(np.abs(Tw - w_initial))
    error_track = np.empty(shape=max_iter)
    error_track[i] = error
    
    if error > err_max:
        w[:] = Tw
        i+=1
    
        while error > err_max or i<=max_iter : 
            Bellman_operation = bellman_operator(w=w, Tw=Tw, beta=beta, u=u, f=f, policy=1)
            w_bellman = Bellman_operation[0]
            error = np.max(np.abs(w_bellman - w))
            error_track[i] = error
            w[:] = w_bellman
            i+=1 
        policy_function[:] = Bellman_operation[1]
        return [w,policy_function]
    else:
        return [Tw, policy_function] #initial guess is correct

class optimal_growth:
    """
    Constant elasticity of substitution optimal growth model, Cobb-Douglas production
    Capital depreciation at rate delta
        F(K,L) = K^α.L^(1-α) > L normalized at 1
        F(K,1) = K^α
        From constraint:
        > f(k) = (1-delta)k + k^α
    """
    def __init__(self, alpha=0.4, beta=0.96, delta=0.03, sigma=0.9):
        self.alpha, self.beta, self.delta, self.sigma = alpha, beta, delta, sigma
        #* β > take the real interest rate (or from micro data)
        #* alpha > look at capital and labour share : compute w.L/Y and r.K/Y using Y is homogenous of order 1 F_L(.)L + F_K(.)K = F(L,K)
        #* CES utility : U(c) = C^(1-sigma) - 1 / 1 - sigma
        #* >>> sigma measure or risk aversion : either invest in bonds or equity : infer from portfolio how risk adverse they are 

    def u(self, c):
        " CES Utility "
        return (c**(1-self.sigma)-1)/(1-self.sigma)

    def f(self, k):
        " From constraint : c_t = f(k_t) - k_t+1 "
        return (1-self.delta)*k + k**self.alpha
    

ces = optimal_growth()
alpha, beta, delta, sigma = ces.alpha, ces.beta, ces.delta, ces.sigma

grid_max = 1    #! Normalisation de X = [O,kmax] "k=K/kmax"
grid_size = 250
grid = np.linspace(1e-10,grid_max,grid_size)

# TODO: take initial random guess v in C(X)
initw = np.zeros(grid.shape)

# TODO: solve
solve = solve_optimal_growth(w_initial=initw, grid=grid, beta=beta, u=ces.u, f=ces.f, err_max=1e-3, max_iter=500)

value_funct_approx = solve[0]
policy_corresp_function = solve[1]