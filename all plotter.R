
thing <- lm(Math_RITScore ~ avg.CriMean.t2, all)
browser()
ggplot(all, aes(x=Math_RITScore, y=avg.CriMean.t2)) +
        geom_point() +    # Use hollow circles
        #scale_fill_brewer( palette = "Blues") + # Use a slightly darker palette than normal
        geom_smooth(method=lm,   # Add linear regression lines
                    se=FALSE,    # Don't add shaded confidence region
                    fullrange=TRUE) + # Extend regression lines
        geom_text(x = 8, y = 6, label = lm_eqn(thing), parse = TRUE) +
        facet_wrap(~GRADE.LEVEL)


