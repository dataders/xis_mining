a <- "considerable personal engagement with the creative process"
b <- "considerable personal engagement with the creative"

highest.hits <- lapply(all.pruned, `[[`, 1,2) 
names <- names(highest.hits)
realthing <- unlist(highest.hits, names) %>%
        sort(decreasing = TRUE)


most.unique <- names(head(realthing, 10))

all.pruned[["10002030"]]

0.017794872/3.4

"10002505" "10001604" "10001733" "10001915" "10002074" "10001276" "10001540" "10001569" "10002398" "10002058"
plo(summary(1000*realthing))

boxplot(realthing)