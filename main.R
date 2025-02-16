
library(behavr)
library(scopr)
library(sleepr)
library(ggetho)

source("parameters.R")
source("library.R")
source("01_plots.R")

create_directory_structure()

metadata <- data.table::fread(file.path(
  DATA_DIR,
  paste("metadata_ID", ID, ".csv", sep = ""))
)

# clean empty rows
metadata <- metadata[complete.cases(metadata), ]

# make sure date is read as a character
metadata$date <- as.character(metadata$date)
metadata <- metadata[status == "OK"]

#4: Linking, link metadata with ethoscope
metadata <- scopr::link_ethoscope_metadata(metadata, result_dir = result_dir)

dt <- scopr::load_ethoscope(
  metadata,
  FUN = sleepr::sleep_annotation,
  reference_hour = NA,
  curate = FALSE,
  cache = ethoscope_cache,
  verbose = TRUE,
  velocity_correction_coef = 0.0042,
  min_time_immobile = 300,
  time_window_length = 10
)


dt_curated <- dt[t <= behavr::days(stop_day_experiment + 1), ]
dt_curated[, day := floor(t / behavr::days(1))]


## Quality control per etoscope
ethoscope_codes <- unique(sapply(metadata$id, function(x) {substr(x, 21, 26)}))
output_folder <- file.path(DATA_DIR, "output", "quality_control")

if(a_print_quality_control){
  lapply(1:length(ethoscope_codes), function(i) {
    ethoscope_ids <- unique(grep(pattern = ethoscope_codes[i], x = dt$id, value = T))
    output_plot <- file.path(
      output_folder,
      paste("ID", ID, "_graph_", ethoscope_codes[i], ".tiff", sep = "")
    )
    quality_control_per_ethoscope(
      dt_curated[id %in% ethoscope_ids, ],
      title = ethoscope_codes[i],
      output_path = output_path
    )
  })
}


output_folder <- file.path(DATA_DIR, "output")

####### sleep fraction analysis ######

#A1: mean fraction of sleep
output_dt <- analyse_mean_fraction_of_sleep(
  dt_curated, output_folder, ID, do_print = a_print_graphs
)


#A2: mean fraction of sleep day vs night
output_dt <- analyse_mean_fraction_of_sleep_day_vs_night(
  dt_curated, output_dt, output_folder, ID, do_print = a_print_graphs
)

#A3: Bout analysis and sleep architecture
## check difference to A4 mean duration!
## NOTE! Empty


#A4 to calculate the "latency to sleep"
output_dt <- analyse_latency_to_sleep(
  dt_curated, output_dt,
  start_day_experiment, stop_day_experiment,
  output_folder, ID,
  do_print = a_print_graphs
)

###motor performance
#A5: beam crosses in 24h
output_dt <- analyse_beam_crosses(dt_curated, output_dt)

#A6 + A7 beam crosses per active minute
output_dt <- analyse_activity(dt_curated, output_dt)

#A8 average velocity
output_dt <- analyse_velocity(dt_curated, output_dt)

#A9 morning anticipation = (a-b)/(a+b),
output_dt <- analyse_morning_anticipation(dt_curated, output_dt)
#  a = number of transitions in 6h to 9h
#  b = number of transitions in 3h to 6h

#A10 time of the peak
# add evening peak?
output_dt <- analyse_peak(dt_curated, output_dt)

#A11 peak activity (after9h, for 10min)
##check final values! a lot in 41.3333....

  A11_peak<-data.frame("id"=character(),"peak_activity_moving"=character(),"peak_activity_beam_crosses"=character(),"peak_activity_velocity"=character())

  i=1
  while(i <= length(valid_ids)){
    print(valid_ids[i])
    temp_for_peak<-data.frame("day"=character(),"peakPlus10_moving"=character(),"peakPlus10_beamCrosses"=character(),"peakPlus10_velocity"=character())
    temp_dt_curated_forPeak<-dt_curated[id==valid_ids[i], c(2:8)]

    day=start_day_experiment
    while(day <= stop_day_experiment-1){
  ##check daystop-1
      shift<-for_peak[[i,day+1]]

      short_temp_dt_curated_forPeak=temp_dt_curated_forPeak[t %between% c(days(day)+shift,days(day)+shift+590), "moving"]
      test=sum(short_temp_dt_curated_forPeak$moving, na.rm = TRUE)>0


      if(test){
        tablePeakPlus10_moving<-table(temp_dt_curated_forPeak[t %between% c(days(day)+shift,days(day)+shift+590), "moving"])[TRUE]
        tablePeakPlus10_beam_crosses<-sum(temp_dt_curated_forPeak[t %between% c(days(day)+shift,days(day)+shift+590), "beam_crosses"])[TRUE]
        tablePeakPlus10_velocity<-(sum(temp_dt_curated_forPeak[t %between% c(days(day)+shift,days(day)+shift+590), "max_velocity"])/60)[TRUE]

        temp_for_peak<-rbind(temp_for_peak,data.frame("day"=day,
                                                      "peakPlus10_moving"=((tablePeakPlus10_moving["TRUE"])),
                                                      "peakPlus10_beamCrosses"=tablePeakPlus10_beam_crosses,
                                                      "peakPlus10_velocity"=tablePeakPlus10_velocity))
      }else{
        tablePeakPlus10_moving<-NaN
        tablePeakPlus10_beam_crosses<-NaN
        tablePeakPlus10_velocity<-NaN

        temp_for_peak<-rbind(temp_for_peak,data.frame("day"=day,
                                                      "peakPlus10_moving"=((tablePeakPlus10_moving)),
                                                      "peakPlus10_beamCrosses"=tablePeakPlus10_moving,
                                                      "peakPlus10_velocity"=tablePeakPlus10_moving))
      }
      #print(i)
      #print(day)
      day<-day+1

    }

    A11_peak<-rbind(A11_peak,data.frame("id"=valid_ids[i],
                                        "peak_activity_moving"=sum(temp_for_peak$peakPlus10_moving, na.rm=TRUE)/day,
                                        "peak_activity_beam_crosses"=sum(temp_for_peak$peakPlus10_beamCrosses, na.rm=TRUE)/day,
                                        "peak_activity_velocity"=sum(temp_for_peak$peakPlus10_velocity, na.rm=TRUE)/day))
    i<-i+1
  }

  output_dt<-cbind(output_dt,
                   "A11_peak_moving"=A11_peak$peak_activity_moving,
                   "A11_peak_beam_crosses"=A11_peak$peak_activity_beam_crosses,
                   "A11_peak_velocity"=A11_peak$peak_activity_velocity)

  if(a_print_graphs){
    print(ggplot(output_dt, aes(x=genotype, y=A11_peak_moving, fill=genotype)) +
            geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
            geom_jitter(alpha=.5, width = 0.15) +
            stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
            scale_y_continuous(name= "A11_peak_moving"))
    ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A11_peak_moving",".png", sep=""))

    print(ggplot(output_dt, aes(x=genotype, y=A11_peak_beam_crosses, fill=genotype)) +
            geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
            geom_jitter(alpha=.5, width = 0.15) +
            stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
            scale_y_continuous(name= "A11_peak_beam_crosses"))
    ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A11_peak_beam_crosses",".png", sep=""))

    print(ggplot(output_dt, aes(x=genotype, y=A11_peak_velocity, fill=genotype)) +
            geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
            geom_jitter(alpha=.5, width = 0.15) +
            stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
            scale_y_continuous(name= "A11_peak_velocity"))
    ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A11_peak_velocity",".png", sep=""))
  }

  if(a_remove_temp_dataTables){
    rm(a, A10_peak_time, short_temp_dt_curated_forPeak, for_peak, temp_for_peak, day, i, temp_dt_curated_forPeak,
       tablePeakPlus10_beam_crosses, tablePeakPlus10_moving, tablePeakPlus10_velocity, shift)
  }



#B1: micromovements and other motor parameters
  ## velocity if awake
  dt_curated3<-dt_curated[moving == TRUE]

  B1_velocity<-data.frame("id"=character(),"mean_velocity_if_awake"=character())

  i=1
  while(i <= length(unique_flies)){
    temp_dt_curated_foractiveMinutes<-dt_curated3[id==unique_flies[i], list(max_velocity)]
    B1_velocity<-rbind(B1_velocity,data.frame("id"=unique_flies[i],
                                              "mean_velocity_if_awake"=sum(temp_dt_curated_foractiveMinutes[["max_velocity"]])/
                                                                           length(temp_dt_curated_foractiveMinutes[["max_velocity"]])))

    i<-i+1
  }

  output_dt<-cbind(output_dt,"B1_velocity_if_awake"=B1_velocity$mean_velocity_if_awake)

  if(a_print_graphs){
    print(ggplot(output_dt, aes(x=genotype, y=B1_velocity_if_awake, fill=genotype)) +
            geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
            geom_jitter(alpha=.5, width = 0.15) +
            stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
            scale_y_continuous(name= "B1_velocity_if_awake"))
    ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","B1_velocity_if_awake",".png", sep=""))
  }

  if(a_remove_temp_dataTables){
     rm(B1_velocity, temp_dt_curated_foractiveMinutes, i)
  }

#B2 frequency of velocity per fly
  #flies in the genotypes
  genotypes<-unique(output_dt$genotype, incomparables = FALSE)
  control_genotype=subset(metadata$genotype, metadata$control_genotype == TRUE)
  velocity=seq(1,round(max(dt_curated3$max_velocity),1), by=0.1)

  ##! get row numbers for control genotype
  B2_velocity_frequency<-data.frame("velocity"=velocity)
  B2_velocity_frequency_temp<-data.frame()
  i=1
  while(i <= length(unique_flies)){
    if(metadata[id == unique_flies[i],genotype] == control_genotype[1]){
    #if(control_genotype[1] == genotypes[x]){
      B2_velocity_frequency_temp<-data.frame(rep(NaN, as.numeric(length(B2_velocity_frequency$velocity))))

      freq_velocity_table<-data.frame(table(round(dt_curated3[id==unique_flies[i],5],1)))
      names(B2_velocity_frequency_temp)<-"temp"

      ##fill data table B2 with the velocities
      j=1
      while(j<length(freq_velocity_table$Var1)){
        a<-which(B2_velocity_frequency$velocity == freq_velocity_table$Var1[j], arr.ind=TRUE)
        B2_velocity_frequency_temp$temp[a]<-freq_velocity_table$Freq[j]
        j<-j+1
      }

      names(B2_velocity_frequency_temp)<-unique_flies[i]
      B2_velocity_frequency<-cbind(B2_velocity_frequency, B2_velocity_frequency_temp)
    }
    i<-i+1
  }

  if(a_filter_values){
    B2_velocity_frequency[55,14]<-NaN
    B2_velocity_frequency[185,21]<-NaN
  }

  #calculate mean of control_genotype
  B2_velocity_frequency_mean<-data.table()

  i=1
  while(i<=length(B2_velocity_frequency$velocity)){
    B2_velocity_frequency_minus_1st<-B2_velocity_frequency
    B2_velocity_frequency_minus_1st$velocity<-NULL
    B2_velocity_frequency_mean<-rbind(B2_velocity_frequency_mean, rowMeans(B2_velocity_frequency_minus_1st[i,], na.rm=TRUE))
    i<-i+1
  }

  names(B2_velocity_frequency_mean)<-"B2_velocity_frequency_mean"
  B2_velocity_frequency<-cbind(B2_velocity_frequency, B2_velocity_frequency_mean)

  #calculate the minimum/peak/minimum_after_peak around velocity 9 in control_genotype==TRUE
  threshold=10
  threshold_row=which(B2_velocity_frequency$velocity == threshold, arr.ind=TRUE)
  min_B2_before_threshold_freq<-min(B2_velocity_frequency$B2_velocity_frequency_mean[1:threshold_row])
  min_B2_before_threshold_row<-which(B2_velocity_frequency$B2_velocity_frequency_mean == min_B2_before_threshold_freq, arr.ind=TRUE)
  min_B2_before_threshold_velocity<-B2_velocity_frequency$velocity[min_B2_before_threshold_row-1]

  peak_B2_around_threshold_freq<-max(B2_velocity_frequency$B2_velocity_frequency_mean[min_B2_before_threshold_row:
                                                                        length(B2_velocity_frequency$B2_velocity_frequency_mean)],na.rm=TRUE)
  peak_B2_around_threshold_row<-which(B2_velocity_frequency$B2_velocity_frequency_mean == peak_B2_around_threshold_freq, arr.ind=TRUE)
  peak_B2_around_threshold_velocity<-B2_velocity_frequency$velocity[peak_B2_around_threshold_row]

  last_after_peak_B2_row<-peak_B2_around_threshold_row+(peak_B2_around_threshold_row-min_B2_before_threshold_row)
  last_after_peak_B2_velocity<-B2_velocity_frequency$velocity[last_after_peak_B2_row]

  #rm(B2_velocity_frequency_mean, B2_velocity_frequency_minus_1st, B2_velocity_frequency, B2_velocity_frequency_temp)


  ##create new cicle
  velocity=seq(1,round(max(dt_curated3$max_velocity),1), by=0.1)

  x=1
  while(x <= length(genotypes)){
  #while(x <= 1){
    B2_velocity_frequency<-data.frame("velocity"=velocity)
    B2_velocity_frequency_temp<-data.frame()
    count=0
    i=1
    while(i <= length(unique_flies)){
      if(metadata[id == unique_flies[i],genotype] == genotypes[x]){
        #if(control_genotype[1] == genotypes[x]){
        B2_velocity_frequency_temp<-data.frame(rep(0, as.numeric(length(B2_velocity_frequency$velocity))))

        freq_velocity_table<-data.frame(table(round(dt_curated3[id==unique_flies[i],5],1)))
        names(B2_velocity_frequency_temp)<-"temp"

        ##fill data table B2 with the velocities
        j=1
        while(j<length(freq_velocity_table$Var1)){
          # print(freq_velocity_table$Var1[j])
          a<-which(B2_velocity_frequency$velocity == freq_velocity_table$Var1[j], arr.ind=TRUE)
          B2_velocity_frequency_temp$temp[a]<-freq_velocity_table$Freq[j]
          j<-j+1
        }

        names(B2_velocity_frequency_temp)<-unique_flies[i]
        B2_velocity_frequency<-cbind(B2_velocity_frequency, B2_velocity_frequency_temp)
        count=count+1
      }
      #B2_velocity_frequency$velocity_frequency.Var1=(B2_velocity_frequency$velocity_frequency.Var1/nr_flies)
      i<-i+1
    }

    B2_velocity_frequency<-cbind(c(genotypes[[x]]), B2_velocity_frequency)
    parameter="freq_velocity_table"
    write.csv(B2_velocity_frequency, file(paste(DATA_DIR,"/output/freq_velocity_table/ID",ID,"_B2_",parameter,"_",x,".csv",sep="")))

    print(paste("B2_velocity_",genotypes[x], sep=""))
    print(count)
    x<-x+1
  }

  rm(B2_velocity_frequency, B2_velocity_frequency_temp, freq_velocity_table, a, count, i, j, parameter, size_velocity, threshold, threshold_row, x, velocity)

  #micromovement, walking and running for all genotypes

  B2_velocity_frequency_output<-data.frame("id"=character(),
                                             "average_micromovement_norm_frequency"=character(),
                                             "average_walking_norm_frequency"=character(),
                                             "average_running_norm_frequency"=character())
  i=1
  while(i <= length(unique_flies)){
    freq_velocity_table<-data.frame(table(round(dt_curated3[id==unique_flies[i],5],1)))
    names(freq_velocity_table)<-c("velocity","frequency")

    freq_velocity_table$product<-as.numeric(as.character(freq_velocity_table$velocity))*as.numeric(freq_velocity_table$frequency)

    average_micromovement_velocity<-sum(freq_velocity_table$product[1:min_B2_before_threshold_row-1])/
      sum(as.numeric(as.character(freq_velocity_table$velocity[1:min_B2_before_threshold_row-1])))

    average_walking_velocity<-sum(freq_velocity_table$product[min_B2_before_threshold_row:last_after_peak_B2_row])/
      sum(as.numeric(as.character(freq_velocity_table$velocity[min_B2_before_threshold_row:last_after_peak_B2_row])))

    average_running_velocity<-sum(freq_velocity_table$product[last_after_peak_B2_row:length(freq_velocity_table$product)])/
      sum(as.numeric(as.character(freq_velocity_table$velocity[last_after_peak_B2_row:length(freq_velocity_table$product)])))

    if(is.na(average_micromovement_velocity)){
     print(paste("flies without movement:",as.character(unique_flies[i])),sep="")
    }else{
      if(is.na(average_walking_velocity) & average_micromovement_velocity>0){
        average_walking_velocity<-0
        print(paste("flies with no walking:",as.character(unique_flies[i])),sep="")
      }
      if(is.na(average_running_velocity) & average_micromovement_velocity>0){
        average_running_velocity<-0
        print(paste("flies with no running:",as.character(unique_flies[i])),sep="")
      }
    }

    B2_velocity_frequency_output<-rbind(B2_velocity_frequency_output,data.frame("id"=unique_flies[i],
                                              "average_micromovement_norm_frequency"=average_micromovement_velocity,
                                              "average_walking_norm_frequency"=average_walking_velocity,
                                              "average_running_norm_frequency"=average_running_velocity))
    i<-i+1
  }

  output_dt<-cbind(output_dt, "B2_average_micromovement_norm_frequency"=B2_velocity_frequency_output$average_micromovement_norm_frequency,
                   "B2_average_walking_norm_frequency"=B2_velocity_frequency_output$average_walking_norm_frequency,
                   "B2_average_running_norm_frequency"=B2_velocity_frequency_output$average_running_norm_frequency)

  if(a_print_graphs){
    print(ggplot(output_dt, aes(x=genotype, y=B2_average_micromovement_norm_frequency, fill=genotype)) +
            geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
            geom_jitter(alpha=.5, width = 0.15) +
            stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
            scale_y_continuous(name= "B2_average_micromovement_norm_frequency"))
    ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","B2_average_micromovement_norm_frequency",".png", sep=""))

    print(ggplot(output_dt, aes(x=genotype, y=B2_average_walking_norm_frequency, fill=genotype)) +
            geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
            geom_jitter(alpha=.5, width = 0.15) +
            stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
            scale_y_continuous(name= "B2_average_walking_norm_frequency"))
    ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","B2_average_walking_norm_frequency",".png", sep=""))

    print(ggplot(output_dt, aes(x=genotype, y=B2_average_running_norm_frequency, fill=genotype)) +
            geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
            geom_jitter(alpha=.5, width = 0.15) +
            stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
            scale_y_continuous(name= "B2_average_running_norm_frequency"))
    ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","B2_average_running_norm_frequency",".png", sep=""))
  }

  #if(a_remove_temp_dataTables){
    #rm(freq_velocity_table, B2_velocity_frequency, B2_velocity_frequency_mean, B2_velocity_frequency_minus_1st, B2_velocity_frequency_temp, B2_velocity_frequency_output, average_micromovement_velocity, average_running_velocity, average_walking_velocity, control_genotype, i, last_after_peak_B2_row,
     # last_after_peak_B2_velocity, min_B2_before_threshold_freq, min_B2_before_threshold_row, min_B2_before_threshold_velocity, peak_B2_around_threshold_freq, peak_B2_around_threshold_row,
     # peak_B2_around_threshold_velocity)
 # }

#B3_activity_immediatelly_after_awake
  if(a_calculate_velocity_after_awakening){
    #calculate time of awakening from bout_dt table
    time_awake_dt<-data.frame(bout_dt$t+bout_dt$duration-10)
    names(time_awake_dt)<-"time_awake"
    bout_dt<-cbind(bout_dt,time_awake_dt)

    #limit to 10min ==600sec
    seconds_after_awake_limit = 60

    ##!for each fly:
    mean_1st_minute_after_awake=c()
i=1
bout_nr_previous_flies=0
    while(i<=length(unique_flies)){
      print(paste(i,"/",length(unique_flies),"  B3_activity_immediatelly_after_awake_",unique_flies[i],sep=""))
      temp_bout_dt<-data.frame(bout_dt[id==unique_flies[i],])
      awakening_max_velocity_per_fly<-data.frame(seq(from = -60, to = seconds_after_awake_limit, by =10))
      names(awakening_max_velocity_per_fly)<-"t_awakening"


      ######403350 is missing

      j=1
      while(j<=length(temp_bout_dt$id)){
        print(j)
        print(temp_bout_dt$t[j])
        print(temp_bout_dt$duration[j])
        print(temp_bout_dt$time_awake[j])
        after_awake_dt_temp<-data.frame(dt_curated[id==unique_flies[i]
                                                  & t %between% c(time_awake_dt$time_awake[j+bout_nr_previous_flies]-60,
                                                                  time_awake_dt$time_awake[j+bout_nr_previous_flies]+seconds_after_awake_limit), max_velocity])
        if(length(after_awake_dt_temp[[1]])< 7+seconds_after_awake_limit/10){
          print(paste("ERROR in: ",i,"_bout_",j, sep=""))
        }else{
          names(after_awake_dt_temp)<-paste(i,"_bout_",j, sep="")
          awakening_max_velocity_per_fly<-cbind(awakening_max_velocity_per_fly,after_awake_dt_temp)
        }
        j=j+1
      }
      #names(awakening_max_velocity_per_fly)[1]<-"t_from_sleep"
      ##average of the 1st minute


      temp_1st_minute_after_awake=c()
      #avg_1st_minute<-mean(awakening_max_velocity_per_fly[t_from_sleep %between% c(0,60), ])
      test<-data.frame(awakening_max_velocity_per_fly[c(8:14),])

      k=2   #not counting with the time column
      while (k<length(awakening_max_velocity_per_fly)){
        temp_1st_minute_after_awake<-c(temp_1st_minute_after_awake,mean(awakening_max_velocity_per_fly[c(8:14),k]))
        k=k+1
      }
      mean_1st_minute_after_awake<-c(mean_1st_minute_after_awake,mean(temp_1st_minute_after_awake))

      if(0){
        #to have csv file of every awake of every fly
        parameter="B13_awakening_max_velocity_per_fly"
        write.csv(awakening_max_velocity_per_fly, file(paste(DATA_DIR,"/output/ID",ID,"_",parameter,"_Nr_",i,".csv",sep="")))
      }

      bout_nr_previous_flies=bout_nr_previous_flies+j
      i=i+1
    }
    output_dt<-cbind(output_dt,"B3_activity_immediatelly_after_awake"=mean_1st_minute_after_awake)

    if(a_print_graphs){
      print(ggplot(output_dt, aes(x=genotype, y=B3_activity_immediatelly_after_awake, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "B3_activity_immediatelly_after_awake"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","B3_activity_immediatelly_after_awake",".png", sep=""))
    }
  }

#B4: total distance from delta_X
 #calculate delta_x for each x position in dt_curated
  delta_t=(max(dt_curated$t)-min(dt_curated$t))/days(1)

  #create temp table for delta_x
  dt_curated3<-dt_curated
  #x<-as.numeric(length(dt_curated$x))
  #dt_x<-data.frame(dt_curated$x[2:x])
  #dt_x<-cbind(dt_x, dt_curated$x[1:x-1])
  #names(dt_x)<-c("xf","xi")
  #dt_x$delta_x<-dt_x$xf-dt_x$xi
  #dt_x<-rbind(c(0,0,NaN),dt_x)

  #dt_curated3<-cbind(dt_curated3, "delta_x"= dt_x$delta_x)

  #calculate the sum of delta_x per fly
  total_distance=c()
  i=1
  while(i <= length(valid_ids)){
    temp_dt_curated_delta_x<-abs(dt_curated3[id==valid_ids[i], "sum_movement"])
    #each ROI 5.5 cm = 0.055 m; by day
    total_distance<-c(total_distance,sum(temp_dt_curated_delta_x$sum_movement[2:length(temp_dt_curated_delta_x$sum_movement)], na.rm=TRUE)*0.055/(delta_t))
    i=i+1
  }
  output_dt<-cbind(output_dt,"B4_total_distance"=total_distance)

  if(a_print_graphs){
    print(ggplot(output_dt, aes(x=genotype, y=B4_total_distance, fill=genotype)) +
            geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
            geom_jitter(alpha=.5, width = 0.15) +
            stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
            scale_y_continuous(name= "B4_total_distance"))
    ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","B4_total_distance",".png", sep=""))
  }

  if(a_remove_temp_dataTables){
    rm(dt_x, total_distance, temp_dt_curated_delta_x, i)
  }



##EXPORT
  

  col_names_output_dt_size<-length(output_dt[1,])

  i<-37
  while(i<=col_names_output_dt_size){
    col_names_output_dt<-colnames(output_dt)
    print(col_names_output_dt[i])
    # print(i)

    export_table <- as.data.frame(matrix(0, nrow=1))

    f=1
    max_rows=0
    while(f<=length(genotypes)){
      temp_for_extract<-subset (output_dt, genotype == genotypes[f], select = col_names_output_dt[i])
      #str(temp_for_extract)
      #print(length(temp_for_extract[,i]))
      if(length(temp_for_extract[[1]])>max_rows){
        max_rows<-length(temp_for_extract[[1]])
      }
      f<-f+1
    }

    f=1
    while(f<=length(genotypes)){
      temp_for_extract<-subset (output_dt, genotype == genotypes[f], select = col_names_output_dt[i])
      j=length(temp_for_extract[[1]])+1
      if(length(temp_for_extract[[1]])<max_rows){
        while(j<=max_rows){
          temp_for_extract<-rbind(temp_for_extract,as.character(col_names_output_dt[i]), fill=TRUE)
          j<-j+1
        }
      }
      export_table <- cbind(export_table,temp_for_extract[,1])
      f<-f+1
    }

    export_table<-export_table[,1:length(genotypes)+1]
    export_table <- cbind(export_table,c(rep(NaN, max_rows)))
    genotypesPLUStitle<-c(genotypes,col_names_output_dt[i])
    names(export_table)<-genotypesPLUStitle

    parameter<-col_names_output_dt[i]
    

    write.csv(export_table, file(paste(DATA_DIR,"/output/ID",ID,"_",parameter,".csv",sep="")))
    i<-i+1
  }


rm (export_table)



#EXTRAS
if(0){

  summary_dt <- rejoin(dt_curated[,
                                  .(A14_rotation_fraction = mean(interactions)),
                                  by=id])

if(a_print_graphs){
  ggplot(summary_dt, aes(x=genotype, y=A14_rotation_fraction, fill=genotype)) +
    geom_boxplot(outlier.colour = NA) +
    geom_jitter(alpha=.5) +
    scale_y_continuous(name= "Fraction of rotations",labels = scales::percent)
  ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","dddddd",".png", sep=""))
}
}

save.image(file = paste(DATA_DIR,"/output_ID",ID,".RData", sep = ""))
print("END")
