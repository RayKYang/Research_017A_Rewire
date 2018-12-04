

# pre-acquisition
X1 <- c("A", "A", "A", "A", "A", "B", "B", "B", "E", "B", "C", "C", "C", "D", "D", "D", "E", "F", "F", "G", "I", "I", "J", "L", "L", "M")
X2 <- c("B", "E", "F", "G", "H", "C", "D", "E", "G", "H", "I", "J", "K", "L", "M", "N", "F", "G", "H", "H", "J", "K", "K", "M", "N", "N")
a <- c(t(data.frame(X1, X2)))
vec <- c(t(a))
g <- igraph::make_graph(vec, directed = F) %>% igraph::simplify()
plot(g, vertex.size=15)

# pre-acquisition R_n_D
X1_ <- c("A", "A", "B", "B", "E", "B", "C", "C", "C", "D", "D", "D", "E", "L", "L", "M")
X2_ <- c("E", "H", "C", "D", "G", "H", "I", "J", "K", "L", "M", "N", "F", "M", "N", "N")
a_ <- c(t(data.frame(X1_, X2_)))
g_ <- igraph::make_graph(c(t(a_)), directed = F) %>% igraph::simplify()
plot(g_, vertex.size=15)

A_Ego <- igraph::make_ego_graph(g, order = 1, nodes = "B")[[1]] # 
plot(A_Ego)
igraph::cohesion(A_Ego)


order_vec <- 1:length(names(igraph::V(g)))
names(order_vec) <- names(igraph::V(g))
new_order <- order_vec[names(igraph::constraint(g_))]

result <- data.frame(Ego_Cohesion = unlist(purrr::map(igraph::make_ego_graph(g, order = 1, nodes = igraph::V(g)[new_order]), igraph::cohesion)),
                    RnD_Brokerage = round(regrrr::scale_01(-1*igraph::constraint(g_)), 3),
                    Acquirer_Proximity = igraph::shortest.paths(g, v = which(names(igraph::V(g.post)) == "A"))[new_order])
row_order <- LETTERS[1:14][which(!LETTERS[1:14] %in% c("A", "C", "D", "L", "M", "N"))]
knitr::kable(result[row_order,])

# post-acquisition
X1 <- c("A", "A", "A", "A", "A", "B", "B", "E", "B", "A", "A", "A", "D", "D", "D", "E", "F", "F", "G", "I", "I", "J", "L", "L", "M")
X2 <- c("B", "E", "F", "G", "H", "D", "E", "G", "H", "I", "J", "K", "L", "M", "N", "F", "G", "H", "H", "J", "K", "K", "M", "N", "N")
a <- c(t(data.frame(X1, X2)))
vec <- c(t(a))
g <- igraph::make_graph(vec, directed = F) %>% igraph::simplify()
plot(g, vertex.size=15)

# post-acquisition R_n_D
X1_ <- c("A", "A", "B", "B", "E", "B", "A", "A", "A", "D", "D", "D", "E", "L", "L", "M")
X2_ <- c("E", "H", "A", "D", "G", "H", "I", "J", "K", "L", "M", "N", "F", "M", "N", "N")
a_ <- c(t(data.frame(X1_, X2_)))
g_ <- igraph::make_graph(c(t(a_)), directed = F) %>% igraph::simplify()
plot(g_, vertex.size=15)

result <- data.frame(Acquirer_Proximity = igraph::shortest.paths(g, v = which(names(igraph::V(g.post)) == "A")))
row_order <- LETTERS[1:14][which(!LETTERS[1:14] %in% c("A", "C", "D", "L", "M", "N"))]
knitr::kable(result[row_order,])


