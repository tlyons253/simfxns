

#' broodsummary.hpd.
#'
#' internal functionto calculate HPD and append true data-generating parmeters to the results
#' of simulations from JAGS.
#'@export

broodsummary.hpd<-function(x,broodsize,S,D){


	true.vals<-data.frame(param=c('trueD','trueS','h1','p1'),
							trueval=c(D,S,0.8,-1))

	hpd1<-coda::as.mcmc(do.call(rbind,x$samples))

	hpd2<-coda::HPDinterval(hpd1)

	hpd3<-data.frame(param=rownames(hpd2),hpd2)

	rownames(hpd3)<-NULL

	summary.mat<-x$summary

	summary.dat<-data.frame(param=rownames(summary.mat),summary.mat)%>%
					dplyr::select(param,Mean=mean,SD=sd,Median=X50.,Rhat,CRL=X2.5.,CRH=X97.5.)%>%
	        dplyr::left_join(.,hpd3,by='param')%>%
	        dplyr::left_join(.,true.vals,by='param')%>%
	        dplyr::mutate(brood=broodsize,
							          Det=D)

	return(summary.dat)




}


#'process.simfile.
#'
#'process.simfile used to read-in and organize the contents of a folder. Folder should contain the output .rds files
#'from jagsUI.
#'
#'@param results_path path to the folder containing .rds files .
#'@param results_pattern regex expression to pull subset of .rds files based on naming pattern.
#'@param sim_broodszie the number of broods used when simulating the data.
#'@param sim_S the survival parameter value used in data generation.
#'@param sim_D the detection parameter value used in data generation.
#'@param sim_prior the type of prior used in the model.
#'@export

#make the process file more generic in the future, allow free summary function and
#	generic ways to provide  simualation parameter names and values


process.simfile<-function(results_path, results_pattern,

						sim_broodsize, sim_S, sim_D, sim_prior){

filepath<-results_path




x<-list.files(path=filepath,pattern=results_pattern, full=TRUE)
x


y<-lapply(x,readRDS)


 z<-do.call(rbind,lapply(y,broodsummary.hpd,broodsize=sim_broodsize,S=sim_S,D=sim_D))


	z<-	dplyr::mutate(z, prior=sim_prior)


 return(z)

				}





