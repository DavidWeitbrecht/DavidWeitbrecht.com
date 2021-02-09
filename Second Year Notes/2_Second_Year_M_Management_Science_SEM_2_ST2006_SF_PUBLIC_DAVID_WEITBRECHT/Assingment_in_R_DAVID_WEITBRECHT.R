LoadingTimeF = function(rand){			#Create A function for Loading time
	if(rand< 0.3){
		result =5
			}
		else if((rand<0.8)&(rand>0.3)){
		result =10
			}
		else {
		result=15
			}
	return(result)
}


WeighingTimeF = function(rand){			#Create A function for weighing time
	if(rand< 0.7){
			result =12
			}
	 else {
			 result=16
			}
	return(result)
}

TravelTimeF = function(rand){				#Create A function traveling time
	if(rand< 0.4){
				result =40
			}
	else if(rand<0.7) {
				result =60
			}
	else if(rand<0.9) {
				result = 80
		}
	else{
				result =100
		}
  return(result)
}


end =1000								#End time of sweep

TotalLoadingTime =0   						#Total loading time varibale
MaxQLoading = 0   						#Max size of loading Queue
MaxQWeighting = 0   						#Max size of weighing Queue
TotalWeighingTime = 0  						#Total time weighing scales used.
TotalTravelTime= 0  				 		#Total time where trucks are travelling.
TotalDeliveries=0							#Total Number of deliveries completed.



time = 0									#initialise time to 0 to start sweep
LoadingQ = 3 								#Number of people currently in loading Queue
TrucksBeingLoaded = 2 							#Number of trucks currently being loaded 0,1 or 2
WeighingQ = 0								#Number of trucks waiting to be weighed.
TrucksBeingWeighed = 1							#Number of trucks being weighed, 0 or 1
TrucksTravelling =0

TimeCurrentWeighingCommenced =0					#time when weighing comenced
TimeCurrentLoadingCommenced1Truck =0				#time when one truck is being weighed
TimeCurrentLoadingCommenced2Trucks =0				#time when two trucks are being weighed

FEL = data.frame(times = c(TravelTimeF(runif(1,0,1)),LoadingTimeF(runif(1,0,1)),WeighingTimeF(runif(1,0,1)), end ), event = c('ArrivalAtLoader','LoadingEnded','WeighingEnded', 'End'))

							#Create future event list with travel times, loading times, weighing time and 4 events: arrival at loader, loading ended, weighing ended and end.

event = 'LoadingEnded'				#initialise sweep with arrival of truck at loader.
print(c('starting sweep'),1)			#Starting sweep of 250 time units

while(event!= 'End'){				#Run simulation while there is no end event, i.e untill 250 time units have been reached.
	
	FEL = FEL[order(FEL[,1]),]		#Order future event list so successive events appear as succesive rows.
	time = FEL[1,1]				#Update clock to new time of next event
	print(c('Main time =',time))		#Print out main time
	event = FEL[1,2]				#Record event type from future event list
	FEL[1,] = NA				#Remove current event from future event list.


	if(event=='LoadingEnded'){							#Event logic if loader is ended -> join weighing q or go on weighing scales.
		print(c('Event= LoadingEnded',time))				#Print event (loading ended at time)
			
			
			if(LoadingQ ==0){							#Event logic,loading queue 0 and 1 truck being loaded

				if(TrucksBeingLoaded==1){				#Event Logic for 1 truck being loaded.
					TrucksBeingLoaded=TrucksBeingLoaded-1	#No trucks in queue -> reduce trucks being loaded by 1.
					TotalLoadingTime= TotalLoadingTime + time -TimeCurrentLoadingCommenced1Truck		
												#Update total loading time for one truck being loaded.
					TimeCurrentLoadingCommenced1Truck =0	#Reset timer for 1 truck being loaded to 0.
				}								#End Event Logic for 1 truck being loaded.

				else if (TrucksBeingLoaded==2){			#Event logic for 2 trucks being loaded.
					TrucksBeingLoaded=TrucksBeingLoaded-1	#No trucks in queue, reduce trucks being loaded by 1.
					TotalLoadingTime = TotalLoadingTime + time + time - TimeCurrentLoadingCommenced2Trucks - TimeCurrentLoadingCommenced2Trucks
												#Update total time for 2 loaders in use ^^^^
					TimeCurrentLoadingCommenced2Trucks =0	#Reset loading time for 2 trucks to 0.
					TimeCurrentLoadingCommenced1Truck = time	#Only 1 truck being loaded now -> set loading timer for 1 truck to time.
				}								#End of event logic for 2 trucks being loaded.
			}									#End Event Logic for loading queue of 0.

			else if(LoadingQ>0) {						#Event Logic for Loading queue bigger than 0.											

				if(TrucksBeingLoaded==1){				#Event Logic for 1 truck being loaded.
					LoadingQ= LoadingQ-1				#Reduce loading Q by 1
					TimeCurrentLoadingCommenced1Truck = time	#Set new loading time for one truck to time
					NewDepEvent = data.frame(times=c(time+ LoadingTimeF(runif(1,0,1))),event = c('LoadingEnded'))
												#Create new loading ended event because of new truck being loaded.	
					FEL=rbind(FEL,NewDepEvent)			#Update Future event list to hold new event.
				}								#End Event Logic for 1 truck being loaded.

				else if ( TrucksBeingLoaded==2){			#Event Logic for 2 truck being loaded.
					LoadingQ= LoadingQ-1				#Reduce loading Q by 1
					TotalLoadingTime = TotalLoadingTime + time + time - TimeCurrentLoadingCommenced2Trucks - TimeCurrentLoadingCommenced2Trucks	
												#Update total loading time to include time 2 trucks being loaded ^^
					TimeCurrentLoadingCommenced2Trucks = time	#Reset loading timer for 2 trucks to current time.								
					NewDepEvent = data.frame(times=c(time+ LoadingTimeF(runif(1,0,1))),event = c('LoadingEnded'))	
												#Create new loading ended event because of free loaded.
					FEL=rbind(FEL,NewDepEvent)			#Update the future event list to include this event.	
				}								#End Event Logic for 2 truck being loaded
										
			}									#End of Event logic for loading queue >0.

									
	
												#Trucks finished loading go to weighing scales 		

			if(TrucksBeingWeighed==1){					#Event logic for scales in use
				WeighingQ=WeighingQ +1					#Extend weighing q by 1.
			}									#End event logic for scales in use.
			
			else if (WeighingQ==0){						#Event Logic for weighing Scales free and no queue.
				TrucksBeingWeighed =1					#Set Weighing scales to busy.
				TimeCurrentWeighingCommenced=time			#Set commenement time of start of weighing.
				NE = data.frame(times=c(time+ WeighingTimeF(runif(1,0,1))),event = c('WeighingEnded'))			
												#create new departure event for weighing.
				FEL=rbind(FEL,NE)						#Add departure event to future event list.
			}									#End Event Logic for weighing Scales free and no queue.

			else if (WeighingQ>0){						#Event logic for weighing Scales free and weighing queue >0
					WeighingQ= WeighingQ -1				#Reduce weighing queue by 1.
					TrucksBeingWeighed =1				#Set Weighing scales to busy.
					TimeCurrentWeighingCommenced=time		#Set commenement time of start of weighing.
					NewDepartureEvent = data.frame(times=c(time+ WeighingTimeF(runif(1,0,1))),event = c('WeighingEnded'))			
												#create new departure event for weighing.
					FEL=rbind(FEL,NewDepartureEvent)		#Add departure event to future event list.	
			}									#End Event logic for weighing Scales free and weighing queue >0

}												#Event logic if loader is ended -> join weighing q or go on weighing scales.

	if(event =='WeighingEnded'){							#Event logic if weighing is finished -> Generate travel time for arrival at loader.
			print(c('Event = weighing ended, time ',time))
			TotalWeighingTime= TotalWeighingTime+ time -TimeCurrentWeighingCommenced		
												#Update time weighing scales in use

			if(WeighingQ ==0){						#Event Logic for no weighing queue.
			TrucksBeingWeighed = 0						#Queue empty, set scales to idle
			}									#End Event Logic for no weighing queue.

			else if(WeighingQ>0){						#Event logic if loader is ended -> join weighing q or go on weighing scales.
				WeighingQ = WeighingQ -1				#Reduce weighing que by 1
				TimeCurrentWeighingCommenced =time			#Update time of new service commencement.
				NE= data.frame(times = c(time + WeighingTimeF(runif(1,0,1))), event=c('WeighingEnded'))			
												#Create new weighing time event
				FEL=rbind(FEL,NE)						#Update the future event list to include departure event.
			}									#End Event logic if loader is ended -> join weighing q or go on weighing scales.

			NEN= data.frame(times=c(time+TravelTimeF(runif(1,0,1))),event = c('ArrivalAtLoader'))
												#Create new trvel time and arrival at loader event.	
			FEL=rbind(FEL,NEN)						#Updae future event list to include arrival at loader.
			TrucksTravelling = TrucksTravelling+1			#Increment trucks currently travelling by one.		
	}											#End event logic if weighing is finished -> Generate travel time for arrival at loader.

	if(event =='ArrivalAtLoader'){						#Event Logic if truck arrives at loader -> enter loading or joing loading q.
				print(c('Event = arrival at loader, time ',time))
			TotalDeliveries=TotalDeliveries+1				#Update total delivers completed -> +1 delivery completed at arrival
			TrucksTravelling = TrucksTravelling -1			#Reduce trucks travelling by one.
			
			if(TrucksBeingLoaded==2){					#Event logic if both loaders are busy.
				LoadingQ= LoadingQ+1					#Both Loaders busy, increase loading q by 1.
			}									#End Event logic if both loaders are busy.
			
			else if (LoadingQ==0){					#Event Logic loading queue .


				if(TrucksBeingLoaded==0){				#Event Logic no trucks being loaded
					TrucksBeingLoaded = TrucksBeingLoaded+1	#Make extra loader busy
					TimeCurrentLoadingCommenced1Truck = time	#Record start of loading time for one truck.
					NE = data.frame(times = c(time+LoadingTimeF(runif(1,0,1))), event=c('LoadingEnded'))		
												#Create new departure event for loading ended.
					FEL=rbind(FEL,NE)					#Add new dep event to future event list.
				}								#End Event Logic no trucks being loaded.
				else if(TrucksBeingLoaded==1){			#Event Logic for 1 truck being loaded.
					TrucksBeingLoaded = TrucksBeingLoaded+1	#Make extra loader busy
					TotalLoadingTime = TotalLoadingTime - TimeCurrentLoadingCommenced1Truck + time
												#Update total loading time to include time for one truck being loaded.
					TimeCurrentLoadingCommenced1Truck =0		#Reset loading time for one truck to 0.
					TimeCurrentLoadingCommenced2Trucks = time		#Record start of service time for 2 trucks loading.
					NE = data.frame(times = c(time+LoadingTimeF(runif(1,0,1))), event=c('LoadingEnded'))		
													#Create new departure event for loading ended.
					FEL=rbind(FEL,NE)						#Add new dep event to future event list.
				}								#End Event Logic for 1 truck being loaded.

			}									#End Event Logic for 0 loading queue.

			else if ((LoadingQ>0)&(TrucksBeingLoaded==0)){		#Event Logic for lodaing queue >0 and no trucks being loaded.
				TrucksBeingLoaded = TrucksBeingLoaded+1		#Make extra loader busy
				LoadingQ=LoadingQ-1					#Reduce loading queue by 1.
				TimeCurrentLoadingCommenced1Truck = time		#Start loading timer for 1 truck being loaded.		
				NEE = data.frame(times = c(time+LoadingTimeF(runif(1,0,1))), event=c('LoadingEnded'))		
												#Create new departure event for loading ended.
				FEL=rbind(FEL,NEE)					#Add new dep event to future event list.	
			}									#End Event Logic for lodaing queue >0 and no trucks being loaded.

			else if ((LoadingQ>0)&(TrucksBeingLoaded==1)){		#Event Logic for loading queue> 0 and 1 loader idle.
				TrucksBeingLoaded = TrucksBeingLoaded+1		#Make extra loader busy
				LoadingQ=LoadingQ-1					#Reduce loading queue by 1.
				TotalLoadingTime = TotalLoadingTime - TimeCurrentLoadingCommenced1Truck + time		
												#Update total loading time to include loading time from one truck.
				TimeCurrentLoadingCommenced1Truck =0		#Reset loading time for one truck to 0.
				TimeCurrentLoadingCommenced2Trucks = time		#Record start of service time for 2 trucks loading.		
				NEE = data.frame(times = c(time+LoadingTimeF(runif(1,0,1))), event=c('LoadingEnded'))		
												#Create new departure event for loading ended.
				FEL=rbind(FEL,NEE)					#Add new dep event to future event list.

			}									#End Event Logic for loading queue> 0 and 1 loader idle.

	}											#End Event Logic if truck arrives at loader -> enter loading or joing loading q.

	if(event == 'End'){								#Event logic for event = end -> stop simulation.
			print(c('Event = end, time =',time))
			if(TrucksBeingLoaded==1){					#Event Logic if truck arrives at loader -> enter loading or joing loading q.
				TotalLoadingTime=TotalLoadingTime + time - TimeCurrentLoadingCommenced1Truck		
												#Truck being loaded -> Update total loading time.
			}									#End Event Logic if truck arrives at loader -> enter loading or joing loading q.

			if(TrucksBeingLoaded==2){					#Event Logic for 2 trucks being loaded.
				TotalLoadingTime=TotalLoadingTime + time+ time -TimeCurrentLoadingCommenced2Trucks - TimeCurrentLoadingCommenced2Trucks		
												#Two loader busy -> update time in use.	
			}									#End Event Logic for 2 trucks being loaded.

			if(TrucksBeingWeighed ==1){					#Event Logic for weighing scales in use.
				TotalWeighingTime= TotalWeighingTime+ time -TimeCurrentWeighingCommenced	
												#Weighing scales busy -> update time in use.
			}	
	}  											#End Event logic for event = end -> stop simulation.
	
	FEL=na.omit(FEL)									#Remove any empty reocrds from Future event list

	print(c('Trucks being loaded:',TrucksBeingLoaded))			#Print out system state.
	print(c('Trucks in loading queue:',LoadingQ))
	print(c('Trucks in weighing queue:',WeighingQ))
	print(c('Trucks being weighed:',TrucksBeingWeighed)) 
	print(c('Trucks travelling:',TrucksTravelling))
	total = TrucksBeingLoaded+LoadingQ+WeighingQ+TrucksBeingWeighed+TrucksTravelling
	print(c('Total trucks:',total))
	print(c('Total Deliveries Completed:', TotalDeliveries))
	print(c('Total Weighing Time:',TotalWeighingTime))
	print(c('Total Loading Time:', TotalLoadingTime))
	writeLines("  ")
}												#End sweep