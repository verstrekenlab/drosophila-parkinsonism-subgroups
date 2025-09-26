# #install.packages("remotes")
# remotes::install_github("r-lib/processx")
# install.packages("devtools")
# library(devtools)

# devtools::install_github("shaliulab/behavr@0272386cd83535278d7d15c735853c2f88dae7c0")
# devtools::install_github("shaliulab/scopr@ed206b13a49b7782b033c6ac6a410f396a3899d5")
# devtools::install_github("shaliulab/sleepr@7775e1541c93d01c0475b34be3f70570a53983b2")
# remotes::install_github("shaliulab/behavr@0272386cd83535278d7d15c735853c2f88dae7c0")
# remotes::install_github("shaliulab/scopr@ed206b13a49b7782b033c6ac6a410f396a3899d5")
# remotes::install_github("shaliulab/sleepr@7775e1541c93d01c0475b34be3f70570a53983b2")
# library(behavr)
# library(scopr)
# library(sleepr)

# devtools::install('C:\\Users\\u0122996\\Documents\\opt\\fslbehavr')
# devtools::install('C:\\Users\\u0122996\\Documents\\opt\\fslscopr')
# devtools::install('C:\\Users\\u0122996\\Documents\\opt\\fslsleepr')
# library(fslbehavr)
# library(fslscopr)
# library(fslsleepr)


# devtools::install_github("shaliulab/sleepr@7775e1541c93d01c0475b34be3f70570a53983b2")

analyse_ID_batch_old <- function(batch_id, testing = FALSE) {

  # library(ggetho)

  #1:setting workspace
    #memory.limit(size = 90000)

    ID=batch_id

    start_day_experiment=1
    stop_day_experiment=5
    
    a_print_graphs=FALSE
    a_print_quality_control=TRUE
    a_print_graphs_individual_arousal=TRUE

    arousal=TRUE
    a_calculate_velocity_after_awakening=TRUE

    a_remove_temp_dataTables=TRUE
    a_change_single_sleep_events=FALSE
    a_filter_values=FALSE
    
    
    #DATA_DIR <- paste("C:/temp/Ethoscope_temp_analysis/ID",ID, sep="")
    #DATA_DIR <- paste("F:/Ethoscope raw data/data from 190819/ID",ID, sep="")

    NATALIE_CACHE <- "/ethoscope_data/natalie_cache"

    DATA_DIR <- paste(here::here(), "/ID",ID, sep="")
    #DATA_DIR <- paste("C:/Users/u0122996/Documents/ID",ID, sep="")
    #setwd("L:/GBW-0004_CMEVIB_OMERO/0002_PAVE/Natalie/C2_ethoscope/ethoscope_results/ID",ID, sep="")
  
    RAW_DATA <- paste(DATA_DIR,"/raw_data", sep = "")
    # Instead use a local drive (not a network drive)
    dir.create(paste(DATA_DIR,"/output", sep = ""), showWarnings = F)
    dir.create(paste(DATA_DIR,"/output/arousal", sep = ""), showWarnings = F)
    #dir.create(paste(DATA_DIR,"/output/extra", sep = ""), showWarnings = F)
    dir.create(paste(DATA_DIR,"/output/quality_control", sep = ""), showWarnings = F)
    dir.create(paste(DATA_DIR,"/output/freq_velocity_table", sep = ""), showWarnings = F)

    #setwd(DATA_DIR)


    list_db<-list.files(RAW_DATA, recursive = T, pattern = ".db")
    print(list_db, quote = TRUE, row.names = FALSE)

  #2: From experiment design to metadata,
  #2-1 Loading libraries


  #3: loading and reading metadata file
    metadata <- data.table::fread(file.path(DATA_DIR, paste("metadata_ID",ID,".csv",sep="")))

    # clean empty rows
    metadata <- metadata[complete.cases(metadata),]

    # make sure date is read as a character
    metadata$date <- as.character(metadata$date)
    #4: Linking, link metadata with ethoscope
    metadata <- scopr::link_ethoscope_metadata(metadata, result_dir = RAW_DATA)



  #5: Loading raw data without dead or escaped animals [e.g status == "Ok","Dead", Escaped]
    metadata_subset <- metadata[status == "OK"]

    if (testing) metadata_subset <- metadata_subset[1:3, ]

    #metadata_subset <- metadata_subset[1:2,]
    #metadata_subset$reference_hour +1
  # metadata_subset <- metadata_subset[machine_name == 'ETHOSCOPE_PV_01' & region_id %in% 1:3,]


  #6: setting Zeitgeber (ZT0)
  ##lights on = 10h00 CET = 8h00 UTC = reference_hour=8.0 !!summertime
  ##lights on = 10h00 CET = 9h00 UTC = reference_hour=9.0 !!wintertime
    # For the old analysis distance is calculated in python with velocity correction coef of 0.006
  # sleep_annotation_006 <-sleepr::sleep_annotation()
  # sleep_annotation_006
    
    # dt_raw <- scopr::load_ethoscope(
    # metadata_subset,
    #       #max_time = days(2),
    # reference_hour=NA,
    #   #cache = "C:/Users/u0122996/Documents/cache",
    #   cache = "L:/GBW-0004_CMEVIB_OMERO/0002_PAVE/Natalie/C2_ethoscope/ethoscope_results/cache",
    # verbose=TRUE
    # )  
    
    #dt_raw[xmv(region_id)==1&t>=82800&t<=435600, sum(10**(xy_dist_log10x1000/1000))]
    #[t %between% c(days(start_day_experiment)-3600, days(stop_day_experiment)+3600)]
    # dt_raw[xmv(id)=="2020-12-11_16-48-18_101bbd|01"& t %between% c(days(start_day_experiment)-3600, days(stop_day_experiment)+3600), sum(10**(xy_dist_log10x1000/1000))]


  dt <- scopr::load_ethoscope(
    metadata_subset,
    FUN = list(
      sleepr::sleep_annotation
      #sleepr::sum_movement_detector
    ),
    #max_time = days(2),
    reference_hour=NA,
    curate=FALSE,
    #cache = "C:/Users/u0122996/Documents/cache",
    cache = NATALIE_CACHE,
    verbose=TRUE,
    velocity_correction_coef = 0.0042,
    min_time_immobile = 300,
    time_window_length = 10
  )

  #saveRDS(object = dt_fsl, file = 'dt_fsl.rds')
  #saveRDS(object = dt, file = 'dt.rds')

  #table(dt_fsl$asleep == dt$asleep)

  #dt_fsl <- readRDS('dt_fsl.rds')


  dt_movement <- scopr::load_ethoscope(
    metadata_subset,
    FUN = list(
      #sleepr::sleep_annotation
      sleepr::sum_movement_detector
    ),
    #max_time = days(2),
    reference_hour=NA,
    #cache = "C:/Users/u0122996/Documents/cache",
    cache = NATALIE_CACHE,
    verbose=TRUE,
    velocity_correction_coef = 0.0042,
    min_time_immobile = 300,
    time_window_length = 10
  )
  # dt[as.Date(xmv(datetime)) == as.Date('2020-09-22'), t := t - days(1)]
  dt_movement$interval<-NULL
  dt$interval<-NULL
  dt_movement$micromovement<-NULL
  dt2 <- behavr::merge_behavr_all(dt,dt_movement)
  #dt2$new<-3
  #dt2<- dt2[,c(setdiff(colnames(dt2), c("sum_movement")),"sum_movement"),with=FALSE]

  summary(dt2)

  #dt_stiched <- stitch_on(dt, on = "fly_no")
  # summary(dt_stiched)


    #ggetho(dt_stiched[xmv(machine_name) == 'ETHOSCOPE_PV_10',], aes(y = asleep)) +
    # stat_pop_etho() +
    # facet_grid(fly_no ~ .) +
      #stat_ld_annotations()

    # for new analysis using x and y as floats and calculate distance in R (by Antonio)
  #  dt <- fslscopr::load_ethoscope(metadata_subset,
  # FUN = list(fslsleepr::fsl_sleep_annotation),
    #FUN = list(fslsleepr::fsl_sleep_annotation,
    #          fslsleepr::custom_annotation_closure(custom_function = fslsleepr::distance_sum),
      #         fslsleepr::custom_annotation_closure(custom_function = fslsleepr::velocity_avg)
    #),

  #   reference_hour=9.0,
  #  cache = "C:/temp/Ethoscope_temp_analysis/cache",
    #cache = "L:/GBW-0004_CMEVIB_OMERO/0002_PAVE/Natalie/C2_ethoscope/ethoscope_results/cache",
  #  verbose=TRUE)
  #
  # summary(dt)

  if(stop_day_experiment>floor(max(dt2$t)/(24*3600))){
    print("stop day changed")
    stop_day_experiment=floor(max(dt2$t)/(24*3600))
  }


  if(a_remove_temp_dataTables){
    rm(metadata_subset)
  }

  #exceptions
  ##################


  #8***DATA_Subset**** # activate when important e.g exclude first day; sleep deprivation
    dt <- dt2[t %between% c(days(0), days(stop_day_experiment+1.25))]

  #9: data curation and removing dead animals automaticly.
  # we make a summary table of all lifespan for each animals

  # valid_ids=c()
  # unique_flies<-unique(dt_stiched$id, incomparables = FALSE)
  # unique_flies<-as.vector(unique_flies)

    valid_ids=c()
    unique_flies<-unique(dt$id, incomparables = FALSE)
    unique_flies<-as.vector(unique_flies)

    ##! work from here
  if(0){
    removed_id=c()
    unique_id_dt_factor=c()
    total_fraction_most_common_position=c()

    #criteria to exclude dead flies:
    criteria_fraction_most_common_position=0.7
    criteria_number_frames=days(0.45)

    i=1
    while(i<=length(unique_flies)){
      temp_dt_for_curation<-dt[id==unique_flies[i] & t %between% c(days(5.5), days(6))]
      length(temp_dt_for_curation$id)
      fraction_most_common_position<-max(table(temp_dt_for_curation$x))/sum(table(temp_dt_for_curation$x))

      if(0){
        print("-----------------------")
        print(i)
        print(unique_flies[[i]])
        print(fraction_most_common_position)
        print(sum(table(temp_dt_for_curation$x)))
      }

      total_fraction_most_common_position<-c(total_fraction_most_common_position,fraction_most_common_position)

      if(fraction_most_common_position<criteria_fraction_most_common_position & 10*length(temp_dt_for_curation$id)>=criteria_number_frames){
        valid_ids <- c(valid_ids, unique_flies[[i]])
      } else {
        removed_id <- c(removed_id, unique_flies[[i]])
      }

      i<-i+1
    }

    unique_id_dt_factor
    total_fraction_most_common_position
    valid_ids
    removed_id
    #rm(i,total_fraction_most_common_position,unique_id_dt_factor,)

  }else{
    i=1
    while(i<=length(unique_flies)){
      valid_ids <- c(valid_ids, unique_flies[[i]])
      i<-i+1
    }
  }
    ###manually remove flies from id
  if(0){
    valid_ids<-valid_ids[!valid_ids %in% "2019-01-07_16-59-06_111e27|01"]      #by single fly
    valid_ids<-valid_ids[!valid_ids %in% grep ("111e27", valid_ids, value=T)]  #by ethoscope
    valid_ids
  }

  #10 remove flies with less than 1% activity in 24 h
  # dt_curated <- dt_stiched[dt_stiched$id %in% valid_ids,]

    dt_curated <- dt[dt$id %in% valid_ids,]

    if(a_remove_temp_dataTables){
      rm(i)
    }

    #animals that were removed:

    setdiff(dt[, id, meta=T],
            dt_curated[, id, meta=T])


  #11: Adding some phase information
    dt_curated[, phase := ifelse(t %% hours(24) < hours(12), "L", "D")]

  #***Plotting and analysis****
  ##quality control
    # browser()
    SD=table(metadata$optomotor)

      a<-character()
      unique_ethoscopes<-unique(metadata[status == 'OK']$id,incomparables = FALSE)

      i=1
      while(i<=length(unique_ethoscopes)){
        a<-c(a,substr(unique_ethoscopes[i],21,26))
        a
        i<-i+1
      }
      rm(unique_ethoscopes)

      ethoscope_list<-unique(a,incomparables = FALSE)
      ethoscope_list
      if(a_print_quality_control){
        graphics.off()
        e=1
        while(e<=length(ethoscope_list)){
          print(e)
          #while(e<=1){
          #selected_ethoscope<-dt_curated[dt_curated$id %in% as.character(ethoscope_list[e]),]
          #selected_ethoscope<-dt_curated[dt_curated$id==unique(grep (ethoscope_list[e], dt_curated$id, value=T)),]
          # browser()
          unique_id <- unique(grep(pattern = ethoscope_list[e], x = dt_curated$id, value=T))

          # if(length(unique_id) == 0) {
          #   e<-e+1
          #   print(paste0('Skipping ', e))
          #   next()
          # }

          selected_ethoscope <- dt_curated[id %in% unique_id,]


          B<-unique(selected_ethoscope$id,incomparables = FALSE)
          C<-unique(dt_curated$id,incomparables = FALSE)
          # tiff(file=paste(DATA_DIR,"/output/quality_control/ID",ID,"_graph_",ethoscope_list[e], ".tiff", sep=""),width=17.15,height=17.15,units="cm", res=1200, pointsize=10, compression = "lzw")
          # print(ggetho(selected_ethoscope, aes(y=asleep)) +
          # #print(ggetho(selected_ethoscope, aes(y=moving)) +
          #         stat_pop_etho() +
          #         stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
          #         facet_grid(fly_no ~ .) +
          #         ggtitle (ethoscope_list[e]) )
          # dev.off()

          #ggsave(paste(DATA_DIR,"/output/quality_control/ID",ID,"_graph_ethoscope_",ethoscope_list[e],".png", sep=""))
          e<-e+1
        }
          }

  ##graphs
    if(a_print_graphs){
      
      selected_ethoscope2<-dt_curated

      print(ggetho(selected_ethoscope2, aes(y=asleep, colour=genotype)) +
        stat_pop_etho() +
        stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
        facet_grid(fly_no ~ .))


      print(ggetho(selected_ethoscope2, aes(y=moving, colour=genotype)) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
              facet_grid(fly_no ~ .))

      if(SD[names(SD)=="YES"]>0){
        print(ggetho(selected_ethoscope2, aes(y=interactions, colour=genotype)) +
              stat_pop_etho() +
              stat_ld_annotations() +
              facet_grid(fly_no ~ .))
      }

      print(ggetho(selected_ethoscope2, aes(y=max_velocity, colour=genotype)) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
              facet_grid(fly_no ~ .))


      #summary with offset
      print(ggetho(selected_ethoscope2, aes(y=asleep, colour=genotype), time_wrap = hours(24), time_offset=hours(18)) +
        stat_pop_etho() +
        stat_ld_annotations() +
        scale_y_continuous(name= "Fraction of time sleeping",labels = scales::percent))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A0_percentage_sleep_over_time",".png", sep=""))

      print(ggetho(selected_ethoscope2, aes(y=moving, colour=genotype), time_wrap = hours(24), time_offset=hours(18)) +
        stat_pop_etho() +
        stat_ld_annotations() +
        scale_y_continuous(name= "Fraction of time moving",limits = c(0,1),labels = scales::percent))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A0_percentage_activity_over_time",".png", sep=""))

      print(ggetho(selected_ethoscope2, aes(y=max_velocity, colour=genotype), time_wrap = hours(24), time_offset=hours(18)) +
        stat_pop_etho() +
        stat_ld_annotations() +
        scale_y_continuous(name= "max_velocity",labels = scales::percent))

      print( ggetho(selected_ethoscope2, aes(y=beam_crosses, colour=genotype), time_wrap = hours(24), time_offset=hours(18)) +
        stat_pop_etho() +
        stat_ld_annotations() +
        scale_y_continuous(name= "beam crosses"))

      rm(selected_ethoscope, selected_ethoscope2)
    }



  ####################################################################################################################################################################
  ####################################################################################################################################################################
  ####################################################################################################################################################################

  ####### sleep fraction analysis ######

    #duplicate dt_curated for A12_arousal
    dt_curated2<-dt_curated

    #crop 5 days
    dt_curated<-dt_curated[t %between% c(days(start_day_experiment)-3600, days(stop_day_experiment)+3600)]


  #A1: mean fraction of sleep

    summary_dt <- rejoin(dt_curated[,
                        .(A1_sleep_fraction = mean(asleep)),
                        by=id])
    output_dt<-summary_dt

    unique_flies<-unique(dt_curated$id, incomparables = FALSE)
    unique_flies<-as.vector(unique_flies)

    if(a_print_graphs){
      ggplot(output_dt, aes(x=genotype, y=A1_sleep_fraction, fill=genotype)) +
        geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
        geom_jitter(alpha=.5, width = 0.15) +
        stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
        scale_y_continuous(name= "A1_sleep_fraction",labels = scales::percent)
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A1_sleep_fraction",".png", sep=""))
    }

  #A2: mean fraction of sleep day vs night

    summary_dt <- rejoin(dt_curated[,
                        .(A2_sleep_fraction_l = mean(asleep[phase == "L"]),
                          A2_sleep_fraction_d = mean(asleep[phase == "D"])),
                        ,by=id])

    output_dt$A2_sleep_fraction_day<-summary_dt[,"A2_sleep_fraction_l"]
    output_dt$A2_sleep_fraction_night<-summary_dt[,"A2_sleep_fraction_d"]

    if(a_print_graphs){
      print(ggplot(output_dt, aes(x=genotype, y=A2_sleep_fraction_day, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A2_sleep_fraction_day",labels = scales::percent))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A2_sleep_fraction_day",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A2_sleep_fraction_night, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A2_sleep_fraction_night",labels = scales::percent))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A2_sleep_fraction_night",".png", sep=""))
    }


  #A3: Bout analysis and sleep architecture
  ## cehck difference to A4 mean duration!



  #A4 to calculate the "latency to sleep"
  ##TOTAL
    bout_dt <- bout_analysis(asleep, dt_curated)
    bout_dt <- bout_dt[asleep == TRUE, -"asleep"]

    A4_latency<-data.frame("id"=character(),"length_longest_bout"=character(),
                          "n_bouts"=character(),"mean_bout_length"=character(),"sum_sleep_minutes"=character())

    i=1
    while(i <= length(valid_ids)){
      temp_for_latency<-data.frame("day"=character())
      #print(valid_ids[i])
      ###fill above
      temp_dt_curated_forLatency<-bout_dt[id==valid_ids[i], c(1:3)]

      if(metadata[id==valid_ids[i], "optomotor"]=="YES"){
        day=as.numeric(as.Date(substr(metadata$SD_end_datetime[[1]],1,10))-as.Date(substr(metadata$datetime[[1]],1,10)))
        #print(day)
      }else{
        day=start_day_experiment
      # print(day)
      }

      while(day <= stop_day_experiment){
        bout_dt_second_day <- temp_dt_curated_forLatency[t %between%  c(days(day), days(day) + days(1))]

        if(length(bout_dt_second_day$t)==0){
          print("aaa")
          bout_summary <- bout_dt_second_day[,.(
            bout_length = NaN,
            length_longest_bout = NaN,
            n_bouts = NaN,
            mean_bout_length = NaN,
            sum_sleep_minutes = NaN)]
        }else{
          print("bbb")
        bout_dt_second_day[, t:= t - days(day)]
        bout_summary <- bout_dt_second_day[,.(
          length_longest_bout = max(duration)/60,
          n_bouts = .N,
          mean_bout_length = mean(duration)/60,
          sum_sleep_minutes = sum((duration)/60))]
          temp_for_latency<-rbind(temp_for_latency,data.frame("day"=day,
                                                          "length_longest_bout"=bout_summary$length_longest_bout,
                                                          "n_bouts"=bout_summary$n_bouts,
                                                          "mean_bout_length"=bout_summary$mean_bout_length,
                                                          "sum_sleep_minutes"=bout_summary$sum_sleep_minutes))
        }
        day<-day+1
      }
  ##add t for longest bout
      A4_latency<-rbind(A4_latency,data.frame("id"=valid_ids[i],
                                              "length_longest_bout"=mean(temp_for_latency$length_longest_bout),
                                              "n_bouts"=mean(temp_for_latency$n_bouts),
                                              "mean_bout_length"=mean(temp_for_latency$mean_bout_length),
                                              "sum_sleep_minutes"=mean(temp_for_latency$sum_sleep_minutes)))
      i<-i+1
    }

    output_dt$A4_sum_sleep_minutes<-A4_latency[, "sum_sleep_minutes"]
    output_dt$A4_n_bouts<-A4_latency[, "n_bouts"]
    output_dt$A4_mean_bout_length<-A4_latency[, "mean_bout_length"]
    output_dt$A4_length_longest_bout<-A4_latency[, "length_longest_bout"]

    if(a_print_graphs){
      print(ggplot(output_dt, aes(x=genotype, y=A4_sum_sleep_minutes, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_sum_sleep_minutes"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_sum_sleep_minutes",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A4_n_bouts, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_n_bouts"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_n_bouts",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A4_mean_bout_length, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_mean_bout_length"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_mean_bout_length",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A4_length_longest_bout, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_length_longest_bout"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_length_longest_bout",".png", sep=""))
    }


  ##night
    A4_latency_mean<-data.frame("id"=character(),"latency"=character(),"latency_to_longest_bout"=character(),
                          "length_longest_bout_night"=character(),"n_bouts_night"=character(),"mean_bout_length_night"=character(),
                          "sum_sleep_minutes_night"=character(), "sum_sleep_minutes_day"=character(), "latency_to_longest_bout_night"=character())

    i=1
    while(i <= length(valid_ids)){
      A4_latency<-data.frame("day"=character(),"latency"=character(),"latency_to_longest_bout"=character(),
                            "length_longest_bout_night"=character(),"n_bouts_night"=character(),"mean_bout_length_night"=character(),
                            "sum_sleep_minutes_night"=character(), "sum_sleep_minutes_day"=character(), "latency_to_longest_bout_night"=character())

      #print(valid_ids[i])
    # while(i <= 2){
      temp_for_latency_2<-data.frame("day"=character())
      ###fill above
      temp_dt_curated_forLatency<-bout_dt[id==valid_ids[i], c(1:3)]

      if(metadata[id==valid_ids[i], "optomotor"]=="YES"){
        day=as.numeric(as.Date(substr(metadata$SD_end_datetime[[1]],1,10))-as.Date(substr(metadata$datetime[[1]],1,10)))
        #print(day)
      }else{
        day=start_day_experiment
        # print(day)
      }
    #day=1
      while(day < stop_day_experiment){
        #print(paste(i,"day:",day,sep=""))
        bout_dt_second_day <- temp_dt_curated_forLatency[t %between%  c(days(day+0.5), days(day+1))]
        test=length(bout_dt_second_day$duration)>0

        if(test){
          bout_dt_second_day <- bout_dt_second_day[, t := t - days(day+0.5)]
          bout_summary <- bout_dt_second_day[,.(
            latency = (t[1])/60,
            #latency_to_longest_bout = (t[which.max(duration)])/60,
            length_longest_bout_night = max(duration)/60,
            n_bouts_night = .N,
            mean_bout_length_night = mean(duration)/60,
            sum_sleep_minutes_night = sum((duration)/60))]

          #length_longest_bout_night in seconds
          a=as.character(bout_summary$length_longest_bout_night*60)

          if(length(bout_dt_second_day[bout_dt_second_day$duration==as.character(bout_summary$length_longest_bout_night*60),1])>1){
            f<-bout_dt_second_day[bout_dt_second_day$duration == a,]
            latency_to_longest_bout_night<-((min(f$t)/60))
          } else {
            latency_to_longest_bout_night<-((bout_dt_second_day[bout_dt_second_day$duration ==a,t])/60)
          }

          # print("#####")
          # print(paste(valid_ids[i], day, latency_to_longest_bout_night))
          # print("#####")

          temp_for_latency_2<-rbind(temp_for_latency_2,data.frame("day"=day,
                                                              "latency"=bout_summary$latency,
                                                              "length_longest_bout_night"=bout_summary$length_longest_bout_night,
                                                              "n_bouts_night"=bout_summary$n_bouts,
                                                              "mean_bout_length_night"=bout_summary$mean_bout_length,
                                                              "sum_sleep_minutes_night"=bout_summary$sum_sleep_minutes_night,
                                                              "sum_sleep_minutes_day"=(output_dt$A4_sum_sleep_minutes[i]-bout_summary$sum_sleep_minutes_night),
                                                              "latency_to_longest_bout_night"=latency_to_longest_bout_night))
        }
        day<-day+1


        A4_latency<-rbind(A4_latency,data.frame("day"=day,
                                                "latency"=mean(temp_for_latency_2$latency),
                                                "length_longest_bout_night"=mean(temp_for_latency_2$length_longest_bout_night),
                                                "n_bouts_night"=mean(temp_for_latency_2$n_bouts),
                                                "mean_bout_length_night"=mean(temp_for_latency_2$mean_bout_length),
                                                "sum_sleep_minutes_night"=mean(temp_for_latency_2$sum_sleep_minutes_night),
                                                "sum_sleep_minutes_day"=mean(temp_for_latency_2$sum_sleep_minutes_day),
                                                "latency_to_longest_bout_night"=mean(temp_for_latency_2$latency_to_longest_bout_night)))
      }
      print(paste("Fly ", i, A4_latency$latency_to_longest_bout_night, collapse = " "))
      A4_latency_mean<-rbind(A4_latency_mean,data.frame("id"=valid_ids[i],
                                                  "latency"=mean(A4_latency$latency),
                                                  "length_longest_bout_night"=mean(A4_latency$length_longest_bout_night),
                                                  "n_bouts_night"=mean(A4_latency$n_bouts_night),
                                                  "mean_bout_length_night"=mean(A4_latency$mean_bout_length_night),
                                                  "sum_sleep_minutes_night"=mean(A4_latency$sum_sleep_minutes_night),
                                                  "sum_sleep_minutes_day"=mean(A4_latency$sum_sleep_minutes_day),
                                                  "latency_to_longest_bout_night"=mean(A4_latency$latency_to_longest_bout_night)))
      i<-i+1
    }

    output_dt$A4_sum_sleep_minutes_day<-A4_latency_mean[, "sum_sleep_minutes_day"]
    output_dt$A4_sum_sleep_minutes_night<-A4_latency_mean[, "sum_sleep_minutes_night"]
    output_dt$A4_n_bouts_night<-A4_latency_mean[, "n_bouts_night"]
    output_dt$A4_mean_bout_length_night<-A4_latency_mean[, "mean_bout_length_night"]
    output_dt$A4_length_longest_bout_night<-A4_latency_mean[, "length_longest_bout_night"]
    output_dt$A4_latency<-A4_latency_mean[, "latency"]
    output_dt$A4_latency_to_longest_bout_night<-A4_latency_mean[, "latency_to_longest_bout_night"]

    print(output_dt$A4_latency_to_longest_bout_night)

    if(a_print_graphs){
      print(ggplot(output_dt, aes(x=genotype, y=A4_latency_to_longest_bout_night, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_latency_to_longest_bout_night"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_latency_to_longest_bout_night",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A4_sum_sleep_minutes_night, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_sum_sleep_minutes_night"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_sum_sleep_minutes_night",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A4_n_bouts_night, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_n_bouts_night"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_n_bouts_night",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A4_mean_bout_length_night, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_mean_bout_length_night"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_mean_bout_length_night",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A4_length_longest_bout_night, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_length_longest_bout_night"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_length_longest_bout_night",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A4_latency, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_latency"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_latency",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A4_latency_to_longest_bout_night, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A4_latency_to_longest_bout_night"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A4_latency_to_longest_bout_night",".png", sep=""))
    }

    if(a_remove_temp_dataTables){
      rm(A4_latency,A4_latency_mean, temp_for_latency, temp_for_latency_2, temp_dt_curated_forLatency, day, i, a, latency_to_longest_bout_night, bout_dt_second_day, bout_summary)
    }


  ###motor performance

  #A5: beam crosses in 24h
    A5_sum_beam_crosses_mean<-data.frame("id"=character(),"sum_beam_crosses_mean"=character())
    i=1


    while(i <= length(unique_flies)){
      A5_beam_cross<-data.frame("day"=character(),"sum_beam_crosses_day"=character())
      day=start_day_experiment
      j=0

      while(day <= stop_day_experiment){
        temp_dt_curated_forBeamCrosses<-dt_curated[id==unique_flies[i] , list(t, beam_crosses)]
        temp_dt_curated_forBeamCrosses<-temp_dt_curated_forBeamCrosses[t %between% c(days(day),days(day+1)), c("t","beam_crosses")]
        test=sum(temp_dt_curated_forBeamCrosses$beam_crosses, na.rm = TRUE)>0

        if(test){
          A5_beam_cross<-rbind(A5_beam_cross,data.frame("day"=day,
                                                        "sum_beam_crosses_day"=sum(temp_dt_curated_forBeamCrosses$beam_crosses, na.rm = TRUE)))
          j<-j+1
        }else{
          A5_beam_cross<-rbind(A5_beam_cross,data.frame("day"=day,
                                                        "sum_beam_crosses_day"=NaN))
        }
        day<-day+1
      }
      sum_beam_crosses_mean=sum(A5_beam_cross$sum_beam_crosses_day, na.rm=TRUE)/j

      A5_sum_beam_crosses_mean<-rbind(A5_sum_beam_crosses_mean,data.frame("id"=unique_flies[i],
                                                                          "sum_beam_crosses_mean"=sum_beam_crosses_mean))

      i<-i+1
    }

    output_dt<-cbind(output_dt,"A5_sum_beam_crosses_mean"=A5_sum_beam_crosses_mean$sum_beam_crosses_mean)

  if(a_print_graphs){
    print(ggplot(output_dt, aes(x=genotype, y=A5_sum_beam_crosses_mean, fill=genotype)) +
            geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
            geom_jitter(alpha=.5, width = 0.15) +
            stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
            scale_y_continuous(name= "A5_sum_beam_crosses_mean"))
    ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A5_sum_beam_crosses_mean",".png", sep=""))
  }

    if(a_remove_temp_dataTables){
      rm(A5_beam_cross, temp_dt_curated_forBeamCrosses, i, j, test, sum_beam_crosses_mean)
    }

  #A6 + A7 beam crosses per active minute
    A6_A7_active_minute<-data.frame("id"=character(),
                                    "A6_sum_active_minutes"=character(),
                                    "A7_beam_crosses_per_active_minute_mean"=character())

    i=1
    while(i <= length(unique_flies)){
      A6_active_minute<-data.frame("day"=character(),"sum_active_minutes_day"=character())
      A7_active_minute<-data.frame("day"=character(),"beam_crosses_per_active_minute_day"=character())
      day=start_day_experiment
      j=0

      while(day <= stop_day_experiment){
        temp_dt_curated_foractiveMinutes<-dt_curated[id==unique_flies[i] , list(t, beam_crosses, moving)]
        temp_dt_curated_foractiveMinutes<-temp_dt_curated_foractiveMinutes[t %between% c(days(day),days(day+1)), c("t","beam_crosses", "moving")]
        test=sum(temp_dt_curated_foractiveMinutes$beam_crosses, na.rm = TRUE)>0

        if(test){
          A6_active_minute<-rbind(A6_active_minute,data.frame("day"=day,
                    "sum_active_minutes_day"=(table(temp_dt_curated_foractiveMinutes[t %between% c(days(day),days(day+1)), "moving"])["TRUE"])/6))
          j<-j+1
        }else{
          A6_active_minute<-rbind(A6_active_minute,data.frame("day"=day,
                                                        "sum_active_minutes_day"=NaN))
        }
        day<-day+1
      }

      sum_active_minute_mean=sum(A6_active_minute$sum_active_minutes_day, na.rm=TRUE)/j

      beam_crosses_per_active_minute_mean=A5_sum_beam_crosses_mean$sum_beam_crosses_mean[i]/sum_active_minute_mean




      A6_A7_active_minute<-rbind(A6_A7_active_minute,data.frame("id"=unique_flies[i],
                                                          "A6_sum_active_minutes"=sum_active_minute_mean,
                                                          "A7_beam_crosses_per_active_minute_mean"=beam_crosses_per_active_minute_mean))

      i<-i+1
    }

    output_dt<-cbind(output_dt,"A6_sum_active_minutes_mean"=A6_A7_active_minute$A6_sum_active_minutes)
    output_dt<-cbind(output_dt,"A7_beam_crosses_per_active_minute_mean"=A6_A7_active_minute$A7_beam_crosses_per_active_minute_mean)

    if(a_print_graphs){
      print(ggplot(output_dt, aes(x=genotype, y=A6_sum_active_minutes_mean, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A6_sum_active_minutes_mean"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A6_sum_active_minutes_mean",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A7_beam_crosses_per_active_minute_mean, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A7_beam_crosses_per_active_minute_mean"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A7_beam_crosses_per_active_minute_mean",".png", sep=""))
    }

    if(a_remove_temp_dataTables){
      rm(A6_active_minute, A5_sum_beam_crosses_mean, A6_A7_active_minute, A7_active_minute, temp_dt_curated_foractiveMinutes, i, j, beam_crosses_per_active_minute_mean, sum_active_minute_mean,test, day)
    }

  #A8 average velocity

    A8_velocity<-data.frame("id"=character(),"mean_velocity_if_awake"=character())
    i=1
    while(i <= length(unique_flies)){
      temp_dt_curated_foractiveMinutes<-dt_curated[id==unique_flies[i], c(2:8)]
      A8_velocity<-rbind(A8_velocity,data.frame("id"=unique_flies[i],
                                                "mean_velocity_if_awake"=sum(temp_dt_curated_foractiveMinutes[["max_velocity"]]*
                                                temp_dt_curated_foractiveMinutes[["moving"]])/length(temp_dt_curated_foractiveMinutes[["moving"]])))
      i<-i+1
    }

    output_dt<-cbind(output_dt,"A8_velocity"=A8_velocity$mean_velocity_if_awake)

    if(a_print_graphs){
      print(ggplot(output_dt, aes(x=genotype, y=A8_velocity, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A8_velocity"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A8_velocity",".png", sep=""))
    }

    if(a_remove_temp_dataTables){
      rm(A8_velocity, temp_dt_curated_foractiveMinutes, i)
    }


  #A9 morning anticipation = (a-b)/(a+b),
    #  a = number of transitions in 6h to 9h
    #  b = number of transitions in 3h to 6h

    inverted_day_diff<-1/(stop_day_experiment-(start_day_experiment))
    A9_anticipation<-data.frame("id"=character(),"morning_anticipation"=character(),"evening_anticipation"=character())
    i=1

    while(i <= length(unique_flies)){
      day=start_day_experiment+1

      temp_for_anticipation<-data.frame("day"=character(),"morning_anticipation"=character(),"evening_anticipation"=character())

      temp_dt_curated_foranticipation<-dt_curated[id==unique_flies[i], c(2:8)]

      while(day <= stop_day_experiment){

        # aa<-table(temp_dt_curated_foranticipation[t %between% c((days(day)-(3*3600)),days(day)), "moving"])
        mydf <- temp_dt_curated_foranticipation[t %between% c((days(day)-(3*3600)),days(day)),]
        print(nrow(mydf))
        a<-table(temp_dt_curated_foranticipation[t %between% c((days(day)-(3*3600)),days(day)), "moving"])[TRUE]
        b<-table(temp_dt_curated_foranticipation[t %between% c((days(day)-(6*3600)),(days(day)-(3*3600))), "moving"])[TRUE]
        c<-table(temp_dt_curated_foranticipation[t %between% c((days(day)-(15*3600)),(days(day)-(12*3600))), "moving"])[TRUE]
        d<-table(temp_dt_curated_foranticipation[t %between% c((days(day)-(18*3600)),(days(day)-(15*3600))), "moving"])[TRUE]

        # print("look here")
        # print(a["TRUE"])
        # print("done")
        

      if(is.table(a) && is.table(b) && is.table(c) && is.table(d)){
        temp_for_anticipation<-rbind(temp_for_anticipation,data.frame("day"=day,
                                                          "morning_anticipation"=(a["TRUE"]-b["TRUE"])/(a["TRUE"]+b["TRUE"]),
                                                          "evening_anticipation"= (c["TRUE"]-d["TRUE"])/(c["TRUE"]+d["TRUE"])))
        # print("######")
        # print(paste(unique_flies[i], day, tail(temp_for_anticipation$morning_anticipation, 1)))                                         
        # print("######")
      }
        day<-day+1
      }

      if(length(temp_for_anticipation$morning_anticipation)>0){
        A9_anticipation<-rbind(A9_anticipation,data.frame("id"=unique_flies[i],
                                                        "morning_anticipation"=inverted_day_diff*100*sum(temp_for_anticipation$morning_anticipation),
                                                        "evening_anticipation"= inverted_day_diff*100*sum(temp_for_anticipation$evening_anticipation)))
                                                  
      }else{
        A9_anticipation<-rbind(A9_anticipation,data.frame("id"=unique_flies[i],
                                                          "morning_anticipation"=NaN,
                                                          "evening_anticipation"=NaN))
      }

      print("######")
      print(paste(unique_flies[i], day, tail(A9_anticipation$morning_anticipation, 1)))                                         
      print("######")

      i<-i+1
    }

    output_dt<-cbind(output_dt,
                    "A9_morning_anticipation"=A9_anticipation$morning_anticipation,
                    "A9_evening_anticipation"=A9_anticipation$evening_anticipation)

    if(a_print_graphs){
      print(ggplot(output_dt, aes(x=genotype, y=A9_morning_anticipation, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A9_morning_anticipation"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A9_morning_anticipation",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A9_evening_anticipation, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A9_evening_anticipation"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A9_evening_anticipation",".png", sep=""))
    }

  # a<-as.vector(output_dt[genotype=="w1118",c("A9_morning_anticipation")])
  # sum(a$A9_morning_anticipation, na.rm = TRUE)/length(a$A9_morning_anticipation[!is.na(a$A9_morning_anticipation)])
    if(a_remove_temp_dataTables){
      rm(day,a,b,c,d, A9_anticipation,temp_for_anticipation, temp_dt_curated_foranticipation)
    }


  #A10 time of the peak

    ##add evening peak

    A10_peak_time<-data.frame("id"=character(),"peak_time"=character(),"diff_to86400"=character())
    for_peak<-data.frame("id"=character(),"day1"=character(),"day2"=character(),"day3"=character(),"day4"=character(), "day5"=character())

    i=1

    while(i <= length(valid_ids)){
      temp_for_peak<-data.frame("day"=character(),"peak_time_day"=character())
      temp_dt_curated_forPeak<-dt_curated[id==valid_ids[i], c(2:8)]

      day=start_day_experiment
      while(day <= stop_day_experiment-1){
        a<-temp_dt_curated_forPeak[t %between% c(days(day)-3600 ,days(day)+3600)]

        #to remove very high max.velocity values, (artifacts?) by removing all values >20average in max_velocity. from now, work in column "mean_20x":
        avg_max_velocity<-mean(a$max_velocity)
        mean_20x <- a$max_velocity
        replace(mean_20x, mean_20x>avg_max_velocity, NaN)
        mean_20x[mean_20x>20*avg_max_velocity]<-NaN
        a<-cbind(a,"mean_20x"=mean_20x)

        max_activity<-max(a$mean_20x, na.rm=TRUE)
        #print(max_activity)
        t_max_activity<-(a[a$mean_20x %in% max_activity, "t"])
        temp_for_peak<-rbind(temp_for_peak,data.frame("day"=day,
                                      "peak_time_day"=t_max_activity/day))

        day<-day+1
      }

      for_peak<-rbind(for_peak,data.frame("id"=valid_ids[i],
                                              "day1"=86400-temp_for_peak[1,2],
                                              "day2"=86400-temp_for_peak[2,2],
                                              "day3"=86400-temp_for_peak[3,2],
                                              "day4"=86400-temp_for_peak[4,2],
                                              "day5"=86400-temp_for_peak[5,2]))

      A10_peak_time<-rbind(A10_peak_time,data.frame("id"=valid_ids[i],
                                          "peak_time"=mean(temp_for_peak$t),
                                          "diff_to86400"=((86400-mean(temp_for_peak$t))/60)))
      i<-i+1
    }

    output_dt<-cbind(output_dt,
                    "A10_peak_time"=A10_peak_time$peak_time,
                    "A10_diff_to_ZT0"=A10_peak_time$diff_to86400)

    if(a_print_graphs){
      print(ggplot(output_dt, aes(x=genotype, y=A10_peak_time, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A10_peak_time"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A10_peak_time",".png", sep=""))

      print(ggplot(output_dt, aes(x=genotype, y=A10_diff_to_ZT0, fill=genotype)) +
              geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
              geom_jitter(alpha=.5, width = 0.15) +
              stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
              scale_y_continuous(name= "A10_diff_to_ZT0"))
      ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","A10_diff_to_ZT0",".png", sep=""))
    }


    if(a_remove_temp_dataTables){
      rm(i, mean_20x, max_activity, inverted_day_diff, day, avg_max_velocity, t_max_activity)
    }

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

  # #B3_activity_immediatelly_after_awake
  #   if(a_calculate_velocity_after_awakening){
  #     #calculate time of awakening from bout_dt table
  #     time_awake_dt<-data.frame(bout_dt$t+bout_dt$duration-10)
  #     names(time_awake_dt)<-"time_awake"
  #     bout_dt<-cbind(bout_dt,time_awake_dt)

  #     #limit to 10min ==600sec
  #     seconds_after_awake_limit = 60

  #     ##!for each fly:
  #     mean_1st_minute_after_awake=c()
  # i=1
  # bout_nr_previous_flies=0
  #     while(i<=length(unique_flies)){
  #       print(paste(i,"/",length(unique_flies),"  B3_activity_immediatelly_after_awake_",unique_flies[i],sep=""))
  #       temp_bout_dt<-data.frame(bout_dt[id==unique_flies[i],])
  #       awakening_max_velocity_per_fly<-data.frame(seq(from = -60, to = seconds_after_awake_limit, by =10))
  #       names(awakening_max_velocity_per_fly)<-"t_awakening"


  #       ######403350 is missing

  #       j=1
  #       while(j<=length(temp_bout_dt$id)){
  #         print(j)
  #         print(temp_bout_dt$t[j])
  #         print(temp_bout_dt$duration[j])
  #         print(temp_bout_dt$time_awake[j])
  #         after_awake_dt_temp<-data.frame(dt_curated[id==unique_flies[i]
  #                                                   & t %between% c(time_awake_dt$time_awake[j+bout_nr_previous_flies]-60,
  #                                                                   time_awake_dt$time_awake[j+bout_nr_previous_flies]+seconds_after_awake_limit), max_velocity])
  #         if(length(after_awake_dt_temp[[1]])< 7+seconds_after_awake_limit/10){
  #           print(paste("ERROR in: ",i,"_bout_",j, sep=""))
  #         }else{
  #           names(after_awake_dt_temp)<-paste(i,"_bout_",j, sep="")
  #           print(dim(awakening_max_velocity_per_fly))
  #           print(dim(after_awake_dt_temp))
            
  #           awakening_max_velocity_per_fly<-cbind(awakening_max_velocity_per_fly, after_awake_dt_temp)
  #         }
  #         j=j+1
  #       }
  #       #names(awakening_max_velocity_per_fly)[1]<-"t_from_sleep"
  #       ##average of the 1st minute


  #       temp_1st_minute_after_awake=c()
  #       #avg_1st_minute<-mean(awakening_max_velocity_per_fly[t_from_sleep %between% c(0,60), ])
  #       test<-data.frame(awakening_max_velocity_per_fly[c(8:14),])

  #       k=2   #not counting with the time column
  #       while (k<length(awakening_max_velocity_per_fly)){
  #         temp_1st_minute_after_awake<-c(temp_1st_minute_after_awake,mean(awakening_max_velocity_per_fly[c(8:14),k]))
  #         k=k+1
  #       }
  #       mean_1st_minute_after_awake<-c(mean_1st_minute_after_awake,mean(temp_1st_minute_after_awake))

  #       if(0){
  #         #to have csv file of every awake of every fly
  #         parameter="B13_awakening_max_velocity_per_fly"
  #         write.csv(awakening_max_velocity_per_fly, file(paste(DATA_DIR,"/output/ID",ID,"_",parameter,"_Nr_",i,".csv",sep="")))
  #       }

  #       bout_nr_previous_flies=bout_nr_previous_flies+j
  #       i=i+1
  #     }
  #     output_dt<-cbind(output_dt,"B3_activity_immediatelly_after_awake"=mean_1st_minute_after_awake)

  #     if(a_print_graphs){
  #       print(ggplot(output_dt, aes(x=genotype, y=B3_activity_immediatelly_after_awake, fill=genotype)) +
  #               geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
  #               geom_jitter(alpha=.5, width = 0.15) +
  #               stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
  #               scale_y_continuous(name= "B3_activity_immediatelly_after_awake"))
  #       ggsave(paste(DATA_DIR,"/output/ID",ID,"_graph_","B3_activity_immediatelly_after_awake",".png", sep=""))
  #     }
  #   }

  #B4: total distance from delta_X
  #calculate delta_x for each x position in dt_curated
    delta_t=(max(dt_curated$t)-min(dt_curated$t))/days(1)
    print(delta_t)

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

      # if (col_names_output_dt[i]=="A4_latency_to_longest_bout_night") {
      #   f=1
      #   temp_for_extract<-subset (output_dt, genotype == genotypes[f], select = col_names_output_dt[i])
      #   print(temp_for_extract)
      # }

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
}