

ggplot(data = all, aes(x=avg.CriMean.t1, y=Lang.RITScore.FALL, label=Lang.RITScore.FALL)) +
        stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
        geom_smooth(method="lm",se=FALSE) +
        geom_point() +
        facet_wrap(~GRADE.LEVEL) +
        ggtitle("T1 Avg Criterion Levels by Fall '15 MAP Language Usage Score Percentile") +
        xlab("Trimester One Criterion Level Average") +
        ylab("Fall '15 MAP Language Usage Percentile")

ggsave("MAP/t1grades~fallLangpercentile_bygrade.png", width = 6, height = 6)