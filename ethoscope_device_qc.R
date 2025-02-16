#' Visualize data segregated by ethoscope
#' This is useful to spot potential batch effects driven by each ethoscope device
quality_control_per_ethoscope <- function(dt, title, output_path){

    graphics.off()

    tiff(
        file=output_path,
        width=17.15,height=17.15,units="cm", res=1200, pointsize=10, compression = "lzw"
    )
    print(
        ggetho(dt, aes(y=asleep)) +
            stat_pop_etho() +
            stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
            facet_grid(fly_no ~ .) +
            ggtitle(title)
    )
    dev.off()
    #ggsave(paste(DATA_DIR,"/output/quality_control/ID",ID,"_graph_ethoscope_",ethoscope_list[e],".png", sep=""))

}