urlBase <- "https://graph.facebook.com/"
apiVersion <- "v2.10"

installRequiredPackages <- function() {
    install.packages("devtools")
    install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
    install.packages("tm")
    install.packages("RCurl")
    install.packages("wordcloud")
    install.packages("igraph")
    install.packages("lubridate")
    install.packages("grep")
}

setEnv <- function() {
    setwd("/Users/xinhuang/Google Drive/CSC 522 R Language Programming /Final Project/source")
    require(devtools)
    require (Rfacebook)
    require(rjson)
    require(tm)
    require(RCurl)
    require(wordcloud)
    require(igraph)
    require("scales")
    
    library(plyr)
    library(dplyr)
    library(tidyr)
    library(lattice)
    library(ggplot2)
    library(lubridate)
    library(grep)
    library(rgl)
    library(rglwidget)
    library(wordcloud)
}

writeResponsToFile <- function(data, fileName) {
    cat(toJSON(data), file = paste(fileName, "json", sep = "."))
}

setConnector <- function(isToken, isOath = FALSE, isToCreateNewOath = FALSE) {
    appSetting <- fromJSON(file = "app_setting.json")
    if (isOath) {
        if (isToCreateNewOath) {
            fbOauth <- fbOAuth(app_id = appSetting$app_id, 
                               app_secret = appSetting$app_secret, 
                               extended_permissions = TRUE)
            save(fb_oauth, file = "fb_oauth")
            return(fbOauth)
        } else {
            return(fbOauth <- load("fb_oauth"))
        }
    } else {
        # Connect to Facebook API via Authentication Toke
        return(appSetting$token)
    }
}

setEnv()
fbToken <- setConnector(TRUE)

# Get personal data
path <- "me?fields=birthday,name,age_range,gender,languages,hometown,email,relationship_status"
response <- callAPI(paste(urlBase, path, sep=""), fbToken, api = "v2.10")


# get list of friends of selected user Xin
testFriendsAPIs <- function() {
    ownerInfo <- getUsers("me", fbToken, private_info=TRUE)
    friendsList <- getFriends(token = fbToken, simplify = FALSE)
    friendsList
    friendsInfo <- getUsers(friendsList$id, fbToken, private_info = TRUE)
    friendsInfo
}
testFriendsAPIs()

clusterFriendsNetworkData <- function() {
    friendsGraph <- getNetwork(fbToken, format = "adj.matrix")
    # preparing node list and layout with igraph
    network <- graph.adjacency(friendsGraph, mode="undirected") 
    # finc communities in graph
    fc <- fastgreedy.community(network)
    set.seed(123)
    
    # prepare data to plot 
    # determine the placement of the vertices for drawing a graph
    layoutCor <- layout.fruchterman.reingold(network, niter=1000, coolexp=0.5) 
    clusteredData <- data.frame(layoutCor) 
    names(clusteredData) <- c("x", "y")
    clusteredData$cluster <- factor(fc$membership)
    
    # add edges to the graph
    edgelist <- get.edgelist(network, names = FALSE)
    edges <- data.frame(clusteredData[edgelist[ , 1], c("x", "y")], clusteredData[edgelist[ , 2], c("x", "y")])
    names(edges) <- c("x1", "y1", "x2", "y2")
    
    clusteredData$degree <- degree(network)
    which.max(degree(network)) ## who do I have more friends in common with?
    central.nodes <- lapply(communities(fc), function(x) x[which.max(clusteredData$degree[x])])
    central.names <- fc$names[unlist(central.nodes)] ## names of central nodes
    
    ## labels I give to each cluster
    labels <- c("From Same Country", "Works at Apple.Inc", "Other Cluster 1", "Other Cluster 2", "Other Cluster 3")
    clusteredData$label <- NA
    clusteredData$label[unlist(central.nodes)] <- labels
    
    return (clusteredData)
}

visualizeFriendsNetwork <- function() {
    clusteredData <- clusterFriendsNetworkData()
    
    p <- ggplot(clusteredData, aes(x = x, y = y, color = cluster)) + 
                    ggtitle("Friends Network of Xin")
    pq <- p + geom_segment(
        aes(x = x1, y = y1, xend = x2, yend = y2), 
        data = edges, size = 0.5, color = "white", alpha = 1/3) +
        geom_point(color = "grey20", aes(fill = cluster), shape = 21, size = 2) +
        scale_fill_discrete(labels = labels) +
        theme(
            plot.title = element_text(color = "white", face = "bold"),
            panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill="black"),
            axis.line = element_blank(), axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), panel.border = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            legend.background = element_rect(colour = F, fill = "black"),
            legend.key = element_rect(fill = "black", colour = F),
            legend.title = element_text(color = "white"),
            legend.text = element_text(color = "white")) +
        guides(fill = guide_legend(override.aes = list(size=3)))
    
    # save plot
    ggsave(pq, file="1 FriendsNetwork.png")
}

visualizeFriendsNetwork()


# Basic Analyze of Facebook Page: Vox
# convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

# Ananlyze growth of selected new pages
visualizePageGrowth <- function(pageName = "vox", graphName) {
    page <- getPage(page = "FoxNews", token = fbToken, n = 2000, api = apiVersion)
    page$datatime <- format.facebook.date(page$created_time)
    page$month <- month(page$datatime)
    page$year <- year(page$datatime)
    
    # aggreate like/comments/shares grouped by years and month
    pageAggr <- page %>% 
        group_by(month, year) %>% 
           summarise(likesCount = mean(likes_count), commentsCount = mean(comments_count), sharesCount = mean(shares_count))
    pageAggr$date <- paste(pageAggr$year, '-', pageAggr$month, sep = "")
    
    pageAggr <- pageAggr %>% 
        gather("type", "count", - month, - year, -date)
    
    # plot the outcome
    p <- ggplot(pageAggr, 
                aes(date, count, group = type, colour = factor(type))) +
        ggtitle(graphName) + 
        geom_point() + 
        geom_line() + 
        theme(axis.text=element_text(size=5))
    p
    ggsave(plot = last_plot(), graphName, width = 20, limitsize = FALSE)
}

# VOX
visualizePageGrowth(pageName = "vox", graphName = "2 Average Replies for per post of page VOX")
#Fox News
visualizePageGrowth(pageName = "FoxNews", graphName = "3 Average Replies for per post of page Fox News")
# CNN News
visualizePageGrowth(pageName = "cnn", graphName = "4 Average Replies for per post of page CNN News")

# Anyalze the responce fo certain post
# Still take FOX News news page as target 
anaylzeMostRepliedPost <- function() {
    # get most discussed post of among the last 2000 post from Fox News
    page <- getPage("FoxNews", fbToken, n = 2000)
    page <- page[order(page$likes_count), ]
    
    # post about Trump
    pageTrump <- page[grep("Trump", page$message), ]
    reactionsTrump <- getReactions(pageTrump$id, token = fbToken, api = apiVersion)
    
    # post abot Hillary 
    pageHillary <- page[grep("Hillary", page$message), ]
    reactionsHillary <- getReactions(pageTrump$id, token = fbToken, api = apiVersion)

    # plot the reactions for Trump
    pageAggrTrump <- reactionsTrump %>% 
        gather("type", "count", -id)
    # add the datetime information 
    pageAggrTrump <- merge(x = pageAggrTrump, y = pageTrump, by = "id", all.x = TRUE)
    
    p <- ggplot(pageAggrTrump, 
                aes(created_time, count, group = type.x, fill = factor(type.x))) +
        ggtitle("Reaction Distrubtion for Post related to Trump") + 
        geom_bar(stat="identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    p
    
    # plot the reactions for Hillary
    pageAggrHillary <- reactionsHillary %>% 
        gather("type", "count", -id)
    # add the datetime information 
    pageAggrHillary <- merge(x = pageAggrHillary, y = pageHillary, by = "id", all.x = TRUE)
    p <- ggplot(pageAggrHillary, 
                aes(created_time, count, group = type.x, fill = factor(type.x))) +
        ggtitle("Reaction Distrubtion for Post related to Hillary") + 
        geom_bar(stat="identity") +
        scale_y_continuous(limits = c(0, 5000)) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    p
    
    # Text Minining for the comments from Trump
    # Extract all the comment messages from most replaied post
    postTrump <- pageTrump[which(pageTrump$likes_count == max(pageTrump$likes_count)), ]
    postTrump <- getPost(post = postTrump$id, n = 3000, token = fbToken)
    postMessage <- (postTrump$comments)$message
    textMiningUtil(postMessage)
    
    postHillary <- pageHillary[which(pageHillary$likes_count == max(pageHillary$likes_count)), ]
    postHillary <- getPost(post = postHillary$id, n = 3000, token = fbToken)
    postMessage <- (postHillary$comments)$message
    textMiningUtil(postMessage)
}

textMiningUtil <- function(postMessage) {
    docs <- Corpus(VectorSource(postMessage))
    toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    
    # convert the text to lower case
    docs <- tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
    docs <- tm_map(docs, content_transformer(tolower))
    # remove numbers 
    docs <- tm_map(docs, removeNumbers)
    # remove English common stop words
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # remove punctuatios 
    docs <- tm_map(docs, removePunctuation)
    # remove extra white space 
    docs <- tm_map(docs, stripWhitespace)
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    head(d, 20)
    
    p <- wordcloud(words = d$word, freq = d$freq, min.freq = 1, 
                   max.words = 300, random.order = FALSE, 
                   rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
    p
}





