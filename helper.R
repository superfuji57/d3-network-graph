library(RSiteCatalyst)
library(d3Network)
user.name <- ##
secret <- ##
SCAuth(user.name, secret)

#### Get Pathing data using ::anything:: wildcards
# Results are limited by the API to 50000
pathpattern <- c("::anything::", "::anything::")

queue_pathing_pages <- QueuePathing("schosso",
                                    "2014-09-01",
                                    "2014-09-08",
                                    metric="pageviews",
                                    element= "page",
                                    pathpattern,
                                    top = 500)

#### Remove Enter and Exit site values
#This information is important for analysis, but not related to website structure
graph_links <- subset(queue_pathing_pages, step.1 != "Entered Site" & step.2 != "Exited Site")

#### First pass - Simple Network
# Setting standAlone = TRUE creates a full HTML file to view graph
# Set equal to FALSE to just get the d3 JavaScript
simpleoutput1 = "/Users/awon/Documents/d3-network-graph/simpleoutput.html"
d3SimpleNetwork(graph_links, Source = "step.1", Target = "step.2", height = 700,
                width = 950, fontsize = 12, linkDistance = 50, charge = -50,
                linkColour = "#666", nodeColour = "#3182bd",
                nodeClickColour = "#E34A33", textColour = "3182bd", opacity = 0.6,
                standAlone = TRUE, file = simpleoutput1)

# Prune edges
simpleoutput2 = "~/documents/d3-network-graph/simpleoutput2.html"
d3SimpleNetwork(subset(graph_links, count > 250), Source = "step.1", Target = "step.2", height = 600,
                width = 750, fontsize = 12, linkDistance = 50, charge = -100,
                linkColour = "#666", nodeColour = "#3182bd",
                nodeClickColour = "#E34A33", textColour = "#3182bd", opacity = 0.6,
                standAlone = TRUE, file = simpleoutput2)
### Force directed network
#Limit to more than 5 occurences like in a simple network
fd_graph_links <- subset(graph_links, count > 500)

#get unique values of page name to create nodes df
# create an index value, starting at 0
fd_nodes <- as.data.frame(unique(c(fd_graph_links$step.1, fd_graph_links$step.2)))
names(fd_nodes) <- "name"
fd_nodes$nodevalue <- as.numeric(row.names(fd_nodes)) - 1

#Create groupings for node colors
#This is user-specific in terms of how to create these groupings
# Due to few number of pages/topics, I am manually coding this

grouping <- function(string) {
        if(grepl("(flyer)", string, perl=TRUE)){
                return(1)
        }else if(grepl("(product)", string, perl = TRUE)){
                return(2)
        }else{
                return(3)
        }
}

fd_nodes$group <- sapply(fd_nodes$name, grouping)

fd_graph_links <- merge(fd_graph_links, fd_nodes[,1:2], by.x = "step.1", by.y = "name")
names(fd_graph_links) <- c("step.1", "step.2", "value", "source")

fd_graph_links <- merge(fd_graph_links, fd_nodes[,1:2], by.x="step.2", by.y="name")
names(fd_graph_links) <- c("step.1", "step.2", "value", "source", "target")

d3output = "~/documents/d3-network-graph/fd_graph.html"
# Create force-directed graph

d3ForceNetwork(Links = fd_graph_links, Nodes = fd_nodes, Source = "source",
               Target = "target", NodeID = "name",
               Group = "group", opacity = 0.8, Value = "value",
               file = d3output, 
               charge = -90,
               fontsize=12, zoom=T)

