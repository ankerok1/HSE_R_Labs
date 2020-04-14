library(igraph)
data = read.table("Internet_AS.dat")
data = t(as.matrix(data))
graph = graph(data)
# getting DPF and CDF
xmax = max(degree(graph))
degree_range = as.numeric(1:xmax)
degree_dist = degree.distribution(graph, cumulative = FALSE)[degree_range]




# deleting zero values 
nonzero.position = which(degree_dist > 0)
degree_dist = degree_dist[nonzero.position]
degree_range = degree_range[nonzero.position]
cum_degree_range = degree_range + 1
compl_cum_degree_range = cum_degree_range

cum_degree_dist = cumsum(degree_dist)
compl_cum_degree_dist = degree.disribution(graph,cumulative = TRUE)[cum_degree_range]
compl_cum_degree_dist = compl_cum_degree_dist[cum_degree_range]

# plotting two graphs side by side 
par(mfrow=c(2,1))
plot(degree_dist, log = "xy", main = "PDF", xlab = "node degree (log)", ylab = "frequency (log)")

plot(cum_degree_dist, log = "xy", main = "CDF", xlab = "node degree (log)", ylab = "frequency (log)")

# fitting linear regression 1 - PDF
reg = lm(log(degree_dist) ~log(degrdee_range))
coefficients <-coef(reg)
func_powerlaw <- function(x)exp(coefficients[[1]] + coefficients[[2]] *log(x))
alpha <- -coefficients[[2]]
# plotting PDF and its estimation
plot(degree_dist ~ degree_range, log = "xy", main = "PDF", xlab = "node degree (log)", ylab = "frequency (log)")
curve(func_powerlaw, col = "red", add = TRUE, n =length(degree_range))

# fitting linear regression 2 - CDF
reg = lm(log(cum_degree_dist) ~log(degree_range))
coefficients <-coef(reg)
func_powerlaw <- function(x)exp(coefficients[[1]] + coefficients[[2]] *log(x))
alpha <- -coefficients[[2]]
# plotting CDF and its estimation
plot(cum_degree_dist ~ degree_range, log = "xy", main = "CDF", xlab = "node degree (log)", ylab = "frequency (log)")
curve(func_powerlaw, col = "red", add = TRUE, n =length(degree_range))


d = degree(graph)
fit = power.law.fit(d, NULL, implementation = "plfit")
alpha = fit$alpha
xmin = fit$xmin
C = (alpha-1)*xmin^(alpha-1	)
alpha

par(mfrow=c(2,1))
fit_pdf <- function(x)return(C*x^(-alpha))
plot(degree_range,degree_dist, log = "xy", main = "PDF", xlab = "node degree (lo)", ylab = "frequency")
#lines(x, x^(-alpha), col="green")
par(new=TRUE)
curve(fit_pdf,from=xmin,to=xmax, log="xy", col = "red", add = FALSE, n =length(degree_range),main = "", xlab = "", ylab = "", axes=FALSE)
#x=xmin:xmax
#plot(log(x), log(C * x ^ (-alpha)), col="red", xlab="", ylab="", yaxt="n")
#lines(log(x), log(C * x ^ (-alpha)), col="brown")


initial <-read.table("Wiki-vote.txt")
initial <-t(as.matrix(initial))
graph <-graph(initial)

sum(is.loop(graph), na.rm=TRUE)

reciprocity(graph)*ecount(graph)/2

degree_dist <-degree.distribution(graph)
plot(degree.distribution(graph, mode='all')[which(degree_dist  > 0)], log ='xy', main ='PDF', xlab ='degree', ylab ='frequency')

sum(degree(graph) > 1, na.rm=TRUE)

components <-clusters(graph, mode = "strong")
#print(components$csize)
print(components$no)