library(irr)
IPublic <- icc(calificaciones, model = "twoway", type = "consistency", unit = "average")
IPublic$value
IPublic$Fvalue
IPublic$p.value
IPublic$lbound
IPublic$ubound
