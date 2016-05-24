

# House comparisons ----------------------------------------------------

ggplot(data = MB.AP.db, aes(y=avg.CriMean.t2, x=HOUSE, label=HOUSE)) +
        geom_boxplot() +
        ggtitle("Average T2 Criterion Levels by House") +
        xlab("House") +
        ylab("Average T2 Criterion Levels")

ggsave("MAP/CriMean.t2_byHouse.png", width = 6, height = 6)


ggplot(data = MB.AP.db, aes(x=HOUSE)) +
        geom_bar() +
        facet_grid(.~NATIONALITY)

ggsave("MAP/CriMean.t2_byHouse.png", width = 6, height = 6)