# calc CI ranges, beta df must have mean and sd cols
getCIVecs = function(beta){
	beta$lo95 = beta$mean - qnorm(.975)*beta$sd
	beta$hi95 = beta$mean + qnorm(.975)*beta$sd
	beta$lo90 = beta$mean - qnorm(.95)*beta$sd  
	beta$hi90 = beta$mean + qnorm(.95)*beta$sd
	return(beta)
}

# colors for sig
coefp_colors = c(
	"Positive"=rgb(54, 144, 192, maxColorValue=255),
	"Negative"= rgb(222, 45, 38, maxColorValue=255),
	"Positive at 90"=rgb(158, 202, 225, maxColorValue=255),
	"Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
	"Insignificant" = rgb(150, 150, 150, maxColorValue=255)
	)

# add sig col to beta df gen from getCIVecs
getSigVec = function(beta){
	beta$sig = NA
	beta$sig[beta$lo90 > 0 & beta$lo95 < 0] = "Positive at 90"
	beta$sig[beta$lo95 > 0] = "Positive"
	beta$sig[beta$hi90 < 0 & beta$hi95 > 0] = "Negative at 90"
	beta$sig[beta$hi95 < 0] = "Negative"
	beta$sig[beta$lo90 < 0 & beta$hi90 > 0] = "Insignificant"
	return(beta)
}