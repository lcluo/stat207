##########
# 14.11 ##
##########

dat = read.table('data/CH14PR11.txt')
names(dat) = c('x', 'n', 'y'); dat
n = nrow(dat)

fr.foo = function(x, b0, b1) (1 + exp(-b0-b1*x))^(-1)

### b
model = glm(y/n ~ x, family = binomial, data = dat, 
            weights = n); model
b0 = coef(model)[1]; b0
b1 = coef(model)[2]; b1
summary(model)
s = summary(model)$coef[,2][2]; s

##########
# 14.17 ##
##########

### c
# deviance() gives -2*log-likelihood
G.sq = deviance(model.red) - deviance(model.full); G.sq

##########
# 14.23 ##
##########

# calculate observed and expected frequencies (counts)
obs1 = dat$y; obs1
obs0 = dat$n[1] - dat$y; obs0
exp1 = dat$n[1]*sapply(dat$x, function(x.j) 
  fr.foo(x.j, b0, b1)); exp1
exp0 = dat$n[1] - exp1; exp0

chisq.star = sum((obs0 - exp0)^2/exp0) + 
  sum((obs1 - exp1)^2/exp1); chisq.star
pchisq(chisq.star, n-2, lower = FALSE)

##########
# 14.14 ##
##########

dat = read.table('data/CH14PR14.txt')
names(dat) = c('y', 'x1', 'x2', 'x3'); head(dat)
n = nrow(dat); n
# center x1 so that x1 and x1^2 are uncorrelated, 
# same for x2
dat.new = dat
dat.new$x1 = dat$x1 - mean(dat$x1)
dat.new$x1.2 = dat.new$x1^2
dat.new$x2 = dat$x2 - mean(dat$x2)
dat.new$x2.2 = dat.new$x2^2
head(dat.new)

##########
# 14.22 ##
##########

### a
alpha = .1

# get p-val of newly added variable, order matters;
# x1 + x2 -> p-val(x2) but x2 + x1 -> p-val(x1)
wrap.foo = function(formula, dat.new. = dat.new) 
{
  model = glm(formula, family = binomial, data = dat.new.)
  p.val = summary(model)$coef
  p.val[nrow(p.val), 4]
}

tmp = c(wrap.foo(y ~ x1), 
        wrap.foo(y ~ x2), 
        wrap.foo(y ~ x1:x2), 
        wrap.foo(y ~ x1.2), 
        wrap.foo(y ~ x2.2), 
        wrap.foo(y ~ x1.2:x2.2))
# if any p-vals are < alpha, add variable with smallest p-val
# and continue...
any(tmp < alpha)
which.min(tmp)

tmp = c(wrap.foo(y ~ x1 + x2), 
        wrap.foo(y ~ x1 + x1:x2), 
        ...)
# and so on...

### b
# tweak wrap.foo().
# if any p-vals are > alpha, drop variable with largest p-val
# and continue...

### c
model = glm(y ~ x1 + x2 + x1:x2 + x1.2 + x2.2 + x1.2:x2.2, 
            family = binomial, data = dat.new)
step(model)

### d
# SBC = BIC
step(model, k = log(n))
