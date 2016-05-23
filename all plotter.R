

# nationalality comparisons -----------------------------------------------
#nationalality comparisons

Taiwan        Korean            HK           USA      Canadian    Australian         Dutch     Malaysian        German     Argentine   Philippines 
20            18             9             9             7             4             4             4             3             2             2 
countries <- c("Taiwan", "Korean", "HK", "USA", "Canadian")
all.5countries <- all %>% filter(NATIONALITY %in% countries)

ggplot(data =all.5countries , aes(x=career_pct,  y=Math.RITScore.SPRING, label=Math.RITScore.SPRING)) +
        stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
        geom_smooth(method="lm",se=FALSE) +
        geom_point() +
        facet_wrap(~NATIONALITY) +
        ggtitle("T2 Avg Criterion Levels by Spring '16 MAP Math Score Percentile") +
        xlab("Trimester Two Criterion Level Average") +
        ylab("Spring '16 MAP Math Percentile") +
        theme_bw()

ggsave("MAP/t2.grades~Spring.Math.Percentile_bycountry.png", width = 9, height = 6)


#1st language comparisons ------------------------------------------------

langs <- c("English", "Korean", "Mandarin")
all.3lang <- all %>% filter(X1st.Language %in% langs)

ggplot(data =all.3lang , aes(x=avg.CriMean.t2,  y=Math.RITScore.SPRING, label=Math.RITScore.SPRING)) +
        stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
        geom_smooth(method="lm",se=FALSE) +
        geom_point() +
        facet_wrap(~X1st.Language) +
        ggtitle("T2 Avg Criterion Levels by Spring '16 MAP Math Score Percentile") +
        xlab("Trimester Two Criterion Level Average") +
        ylab("Spring '16 MAP Math Percentile") +
        theme_bw()

ggsave("MAP/t2.grades~Spring.Math.Percentile_byLang.png", width = 9, height = 3)


# grade level comparisons -------------------------------------------------

ggplot(data = all, aes(x=Years.XIS.int, y=Math.RITScore.SPRING, label=Math.RITScore.SPRING)) +
        stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
        geom_smooth(method="lm",se=FALSE) +
        geom_point() +
        facet_wrap(~GRADE.LEVEL) +
        ggtitle("T2 Avg Criterion Levels by Spring '16 MAP Math Score Percentile") +
        xlab("Trimester Two Criterion Level Average") +
        ylab("Spring '16 MAP Math Percentile")

ggsave("MAP/t2.grades~Spring.Math.Percentile_bygrade.png", width = 6, height = 6)


ggplot(data = all, aes(x=Years.XIS.int, y=Lang.RITScore.SPRING, label=Lang.RITScore.SPRING)) +
        stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
        geom_smooth(method="lm",se=FALSE) +
        geom_point() +
        facet_wrap(~GRADE.LEVEL) +
        ggtitle("Years at XIS by Spring '16 MAP Lang Score Percentile") +
        xlab("Years at XIS") +
        ylab("Spring '16 MAP Lang Percentile")

ggsave("MAP/Years.At.XIS~Spring.Lang.Percentile_bygrade.png", width = 6, height = 6)




