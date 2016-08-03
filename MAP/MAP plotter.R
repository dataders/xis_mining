

plotter2 <- function(data, ind, dep, facet) {
eq <- lm(as.formula(paste(ind,"~", dep)), data)
txt.ind <- quantile(paste(data,"$",ind), probs = 0.75)
txt.dep <- quantile(paste(data,"$",ind), probs = 0.75)
ggplot(data, aes_string(x = ind, y = dep)) +
        geom_point() +
        geom_smooth(    method=lm,   # Add linear regression lines
                        se=FALSE,    # Don't add shaded confidence region
                        fullrange=TRUE) + # Extend regression lines
        geom_text(x = txt.ind, y = txt.dep, label = lm_eqn(eq), parse = TRUE) +
        facet_wrap(as.formula(sprintf('~%s',facet)))
}


        
plotter(xis.db, "RITScore", "Percentile","Discipline")

ggsave("MAP/duration~RIT_byDiscipline.png", width = 6, height = 6)
        
        
        
        
        
        geom_text(x = 8, y = 6, label = lm_eqn(thing), parse = TRUE) +
