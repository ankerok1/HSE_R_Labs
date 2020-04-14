library(igraph)
data = read.table("Internet_AS.dat")
wdata = t(as.matrix(data))
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
