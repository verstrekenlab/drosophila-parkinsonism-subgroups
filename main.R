
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


# #A4 to calculate the "latency to sleep"
# output_dt <- analyse_latency_to_sleep(
#   dt_curated, output_dt,
#   start_day_experiment, stop_day_experiment,
#   output_folder, ID,
#   do_print = a_print_graphs
# )

###motor performance
#A5: beam crosses in 24h
# output_dt <- analyse_beam_crosses(dt_curated, output_dt)

#A6 + A7 beam crosses per active minute
# output_dt <- analyse_activity(dt_curated, output_dt)

#A8 average velocity
# output_dt <- analyse_velocity(dt_curated, output_dt)

#A9 morning anticipation = (a-b)/(a+b),
# output_dt <- analyse_morning_anticipation(dt_curated, output_dt)
#  a = number of transitions in 6h to 9h
#  b = number of transitions in 3h to 6h

#A10 time of the peak
# add evening peak?
# output_dt <- analyse_peak(dt_curated, output_dt)



#B1: motor parameters
output_dt <- analyse_mean_velocity(dt_curated, output_dt)


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
