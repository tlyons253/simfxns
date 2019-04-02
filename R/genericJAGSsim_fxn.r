
#'repeat.sims.


#' A function to streamline simulations using JAGS in R
#' runs jagsUI.




#'@param SIMdata.fxn a	function to simulate your data.

#'@param SIMdata.params parameter values passed to your data-generating function.

#'@param PTM vector of parameters to monitor in jags.

#'@param JAGdata.fxn function to produce a data object to be fed to jags
#'				at minimum, needs at least 1 argument (the simulated data).

#'@param JAG.initial function/list passed to jags containing initial values or steps to produce initial values.



#'@param chains to jagsUI.
#'@param thin to jagsUI.
#'@param iter to jagsUI.
#'@param burn to jagsUI.
#'@param adapt  to jagsUI.
#'@param outfolder.path path to folder where simulation results should be written.
#'@param out_Label label to add to each rds file.

#'@param JAGmodel.path path to location of JAG model file.





#'@export
repeat.sims<-function(SIMdata.fxn,
                      SIMdata.params,
                      JAGdata.fxn,
                      PTM,
                      JAG.initial,
                      chains,thin,iter,burn,adapt,
                      outfolder.path,
                      out_label='',
                      JAGmodel.path,
                      n.sims
){



  #things that don't change across sims
  params<-PTM

  nc<-chains

  ni<-iter

  nt<-thin

  nb<-burn

  na<-adapt

  n.sims<-n.sims

  # Setup a simulation




  repeat{			#the repeat/break syntax takes the place of a loop


    files<-list.files(path=outfolder.path,full=TRUE)			#list files in output folder
    inds<-file.info(files)$size<5000					#index of files where the size < 5KB
    file.remove(files[inds])						#delete failed runs

    sims.complete<-length(files)

    #count number of sims in folder
    if (sims.complete>=n.sims) break		#if the number of files meets the desired # of simulations, stop the function


    #simulate data and call JAGS


    sim.data<-do.call('SIMdata.fxn',SIMdata.params)

    mydata<-JAGdata.fxn(sim.data)



    inits<-JAG.initial(sim.data)

    ##############################
    #####		Calling JAGS
    #####	wrapping the call in try returns no data allows JAGS to
    #####	move to the next simulation upon failure, and saves an RDS of 1kb which gets deleted above
    ###############################


    jag <- try({jagsUI::jags(data=mydata,inits=inits,
                     parameters.to.save=params,
                     model.file=JAGmodel.path,
                     n.chains=nc,
                     n.thin=nt,
                     n.iter=ni,
                     n.burnin=nb,
                     n.adapt=na,
                     DIC=F,
                     parallel=TRUE)
    })

    saveRDS(jag,paste0(outfolder.path,'/sim_',out_label,'_',sims.complete+1,'.rds'))


  }



}




