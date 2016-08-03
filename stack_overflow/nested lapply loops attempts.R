greg.top1k <- setNames(greg.top1000$LenNorm, greg.top1000$Words)
ngrams_sorted <- names(greg.top1k)
so_far <- c()
for (ngram in ngrams_sorted) {
        for (better_ngram in so_far) {
                print("so_far loop runs!")
                browser()
                if (is.null(better_ngram)) {
                        so_far <- append(so_far,ngram)
                        break}
                ifelse(overlap(better_ngram, ngram),
                       {
                               greg.top1k[ngram] <- 0
                               break
                       },
                       {so_far <- append(so_far, ngram)})
        }
}
sort(greg.top1k, decreasing = TRUE)
rm(ngram,so_far, better_ngram)


rm(ngram,so_far, better_ngram)
greg.top1k <- setNames(greg.top1000$LenNorm, greg.top1000$Words)
ngrams_sorted <- names(greg.top1k)
so_far <- c()
for (ngram in ngrams_sorted) {
        browser()
        ifelse(length(so_far) == 0,
               #cond if TRUE
               {so_far <- append(so_far,ngram)},
               #cond if FALSE
               {for (better_ngram in so_far) {
                       print("so_far loop runs!")
                       ifelse(overlap(better_ngram, ngram),
                              #cond if TRUE
                              {greg.top1k[ngram] <- 0},
                              #cond if FALSE
                              {so_far <<- append(so_far, ngram)})
               } #end for loop condition
               }) #end if else
} #end total loop
sort(greg.top1k, decreasing = TRUE)
