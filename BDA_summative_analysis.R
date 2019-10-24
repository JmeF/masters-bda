setwd("C:/Users/Jme/SkyDrive/Documents/Projects/R/RProjects/OxBigData/summative")

options(scipen = 10)

library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(gridExtra)
library(scales)

#devtools::install_github("bvidgen/rBDA", force = F)
library(rBDA)

##### setup #####

full_df = read.csv("Topic_BDA_2019-03-10.csv",stringsAsFactors = FALSE, header = TRUE)


view_name = "Views"
reply_name = "Replies"
author_name = "Authors"
con_name = "Conversation length"
run_name = "Average time between posts"

names(full_df)[names(full_df) == 'topic_view_count'] <- 'view'
names(full_df)[names(full_df) == 'topic_reply_count'] <- 'reply'
names(full_df)[names(full_df) == 'seconds_conversation_length'] <- 'con_len'
names(full_df)[names(full_df) == 'seconds_running_average_response'] <- 'run_av'

#need to convert to POSIXct
full_df$unix_first_post = as.POSIXct(full_df$unix_first_post,origin="1970-01-01",tz="GMT")
full_df$unix_last_post = as.POSIXct(full_df$unix_last_post,origin="1970-01-01",tz="GMT")


#set 0 values for length == NA
full_df$con_len[full_df$con_len == 0] <- NA
full_df$run_av[full_df$run_av == 0] <- NA
full_df$reply[full_df$reply == 0] <- NA


#make a df of only replied to topics
full_df = full_df[is.na(full_df$con_len) == F,]

#Make a color pallete
q_colors =  15 # for no particular reason
v_colors =  viridis::viridis(q_colors, option = "A")
pal(15)

###### Functions #########
## Gini function

getGini = function(var){
  ineq::ineq(var,type='Gini')
}

## descriptive functions
make_loglog_plot = function(data,var,variable_name,density_funct,disttype){
  #hist
  hist = ggplot(data, aes(var)) +
    geom_histogram(bins = 100,
                   color = 'grey',
                   fill = 'grey',
                   alpha = 0.6) +
    scale_x_log10(labels = trans_format("log10",math_format(10^.x))) +
    scale_y_log10(labels = trans_format("log10",math_format(10^.x))) +
    ggtitle(paste('Histogram of',variable_name)) +
    xlab(paste("Log",variable_name)) +
    ylab('Log frequency') +
    annotation_logticks() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.ticks = element_blank())
  
  # # CDF
  # my.ecdf = stats::ecdf(var)
  # df.ecdf = data.frame(x1 = sort(var), # these are the values of our variable - sorted into order from smallest to highest. We want them in order because we are calculating the *cumulative* values
  #                      y2 = my.ecdf(sort(var)))
  # CDF = ggplot(data = df.ecdf,
  #              aes(x1, y2) ) +
  #   geom_line() +
  #   geom_point(color = "black") +
  #   ggtitle(paste('ECDF of',variable_name)) +
  #   xlab(paste("Log",variable_name)) +
  #   ylab('Log cumulative probability') +
  #   scale_x_log10() +
  #   scale_y_log10() +
  #   annotation_logticks()  +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.background = element_blank(),
  #         axis.line = element_line(colour = "black"),
  #         axis.ticks = element_blank()) # +
  # 
  # gridExtra::grid.arrange(hist, CDF, ncol = 2)
  return(hist)
}
make_xlog_density_plot = function(data,var,variable_name,density_funct,disttype,band_width,variable_title_name = variable_name){
  #hist
  density_hist = ggplot(data, aes(var)) +
    geom_histogram(aes(y = ..density.. ), # this stipulates that we want the density instead of the count on y-axis - otherwise the histogram and PDF will have different scales
                   bins = 100,
                   color = 'grey',
                   fill = 'grey',
                   alpha = 0.6) +
    geom_density(alpha = 0.20,
                 fill = v_colors[13],
                 color = v_colors[13],
                 bw = band_width) +
    scale_x_log10(labels = trans_format("log10",math_format(10^.x))) +
    ggtitle(paste('Density plot of',variable_title_name)) +
    xlab(paste("log",variable_name)) +
    ylab('Density') +
    annotation_logticks(sides ='b') +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.ticks = element_blank())
  
  #show(hist)
  fit <- MASS::fitdistr(na.omit(var),
                        densfun = density_funct) # use ?fitdistr to find out more about this function
  params = fit$estimate
  params # our estimated values
  
  # Create a dataframe with both the actual and estimated values
  est.df = data.frame(x3 = sort(na.omit(var)), # we use the same values of x as before
                      y4 = sort(disttype(na.omit(var),
                                         mean = params[1],
                                         sd = params[2]))) # we generate the values of the theoretical normal distribution using pnorm. pnorm is a random sampling function which calculates values for a CDF. It tells you the cumulative probability of each of the x values. We use the params that we fitted using the MASS package as well as the variable we fitted them to (df.normal$x)
  
  # CDF
  my.ecdf = stats::ecdf(var)
  df.ecdf = data.frame(x1 = sort(var), # these are the values of our variable - sorted into order from smallest to highest. We want them in order because we are calculating the *cumulative* values
                       y2 = my.ecdf(sort(var)))
  CDF = ggplot(data = df.ecdf,
               aes(x1, y2) ) +
    geom_line() +
    geom_point(color = "grey",alpha = 0.6) +
    ggtitle(paste('CDF of',variable_name)) +
    xlab(paste("log",variable_name)) +
    ylab('Cumulative probability') +
    scale_x_log10(labels = trans_format("log10",math_format(10^.x)))  +
    geom_line(data = est.df,
              aes(x3, y4),
              color = v_colors[12],
              size = 0.75)+
    annotation_logticks(sides ='b') +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.ticks = element_blank())
  #show(CDF)
  plots = list("density_hist" = density_hist,
               "CDF" = CDF)
  return(plots)
}

## bivariate functions

#log sequence function for bins
plot.heat <- function(data = full_df,var1,var1name,var2,var2name){
  data %>% 
    ggplot(aes(var1, var2)) +
    ggplot2::geom_hex(aes(fill=log10(..count..)),bins = 50) + # adjust the number of bins to get more/fewer hexagons
    viridis::scale_fill_viridis(name = "log count",option = "A") +
    xlab(paste("log",var1name)) +
    ylab(paste("log",var2name)) +
    ggtitle(paste(var2name,"vs",var1name)) +
    scale_x_log10(var1name,
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(var2name,
                  labels = trans_format("log10", math_format(10^.x))) +
    annotation_logticks() +
    theme(text = element_text(size=8),
          plot.title = element_text(size=10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size=6),
          axis.ticks = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  
}
bin_log_means <- function(from,to,bins){
  test.range = to - from #get the range
  jump = test.range/bins # get the size of jumps required on a log10 scale
  begin = from + jump # work out where the first bin middle should be
  end = to # work out where the last bin middle should be #OR include [- jump] 
  sequence = seq(begin, end, length.out = bins) #sequence of these values de-logged
  return(sequence)
}
fit.log.model <- function(var1,var2,bins){
  
  #Make new df
  df.new = data.frame(x = log10(var1),
                      y = log10(var2))
  results = error_bars(df.new, s = bin_log_means(min(df.new$x),max(df.new$x),bins), draw = 'both') #,s = bin_means(min(df.new$x),max(df.new$x),bins)
  bin_df = results$df.summary
  orig_df = results$df 
  
  #Bin fit
  bin_fit = lm(ymean ~ xmean, na.action = na.omit, data = bin_df)
  bin_coeff = data.frame(summary(bin_fit)$coefficients)
  bin_exponent = bin_coeff$Estimate[2]
  bin_rsquare = summary(bin_fit)$r.squared
  
  #regular fit
  orig_fit = lm(y ~ x, na.action = na.omit, data = orig_df)
  orig_coeff = data.frame(summary(orig_fit)$coefficients)
  orig_exponent = orig_coeff$Estimate[2]
  orig_rsquare = summary(orig_fit)$r.squared
  
  
  print(paste("Original exponent:",orig_exponent," Orig R2:",orig_rsquare))
  print(paste("Bin exponent:",bin_exponent," Bin R2:",bin_rsquare))
  
  object_out = list("bin_df"=bin_df,"orig_df"=orig_df,
                    "orig_fit" = orig_fit,
                    "bin_fit" = bin_fit,
                    "bin_exponent" = bin_exponent,
                    "bin_rsquare" = bin_rsquare,
                    "orig_exponent" = orig_exponent,
                    "orig_rsquare" = orig_rsquare)
  
  return(object_out)
}
plot.log.bin.bivar <- function(bin_df,orig_df,var1name,var2name,xmin,xmax){ #,bin_fit,orig_fit
  ggplot() +
    geom_point(data = orig_df, # put the original data on the plot - gives an idea also of how many values are in each bin
               aes(x, y),
               color = v_colors[14],
               alpha = 0.05,
               size = 0.1) +
    geom_smooth(data = orig_df,
                aes(x,y,color = 'Not binned'),
                method = 'lm',
                se = F,
                size = 1,
                alpha = 0.5) +
    geom_errorbar(data = bin_df, 
                  aes(x = xmean,
                      ymin = ymean_minus_sd,
                      ymax = ymean_plus_sd,
                      width= 0.1),
                  color = 'darkgrey',
                  alpha = 0.9) +
    geom_point(data = bin_df, # each point shows a bin - with the mean x value and the mean y value
               aes(xmean, ymean),
               color = v_colors[5], #"blue"
               alpha = 0.6,
               size = 2) + 
    geom_smooth(data = bin_df,
                aes(xmean, ymean,color = 'Binned'),
                method = 'lm',
                se = F,
                size = 1.2) + # fit a line to the binned values
    
    scale_color_manual(name="Linear models",values = c("Binned"=v_colors[4],"Not binned"=v_colors[12]))  + #red orange
    scale_x_continuous(var1name,labels = math_format(10^.x),limits = c(xmin,xmax),
                       breaks = seq(xmin,xmax,1)) +
    scale_y_continuous(var2name,labels = math_format(10^.x)) +
    ggtitle(paste(var2name,"vs",var1name)) +
    annotation_logticks() +
    theme(text = element_text(size=8),
          plot.title = element_text(size=10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size=6),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="right")
}


plot.log.nonbin.bivar <- function(data,var1,var2,var1name,var2name,xlimit){ #,bin_fit,orig_fit
  ggplot(data = data, # put the original data on the plot - gives an idea also of how many values are in each bin
         aes(log10(var1),log10(var2))) +
    geom_point(color = 'lightblue',
               # alpha = 0.7,
               size = 0.1) +
    geom_smooth(method = 'lm',
                se = F,
                size = 1,
                # alpha = 0.9,
                color = 'orange') +
    # geom_errorbar(data = bin_df, #issues here with drawing
    #               aes(x = xmean,
    #                   ymin = ymean_minus_sd,
    #                   ymax = ymean_plus_sd,
    #                   width= 0.1),
    #               color = 'darkgrey',
    #               alpha = 0.9) +
    # geom_point(data = bin_df, # each point shows a bin - with the mean x value and the mean y value
    #            aes(xmean, ymean),
    #            color = "blue",
    #            alpha = 0.9,
  #            size = 2) + 
  # geom_smooth(data = bin_df,
  #             aes(xmean, ymean,color = 'Binned'),
  #             method = 'lm',
  #             se = F,
  #             size = 1.2) + # fit a line to the binned values
  
  # scale_color_manual(name="Linear models",values = c("Binned"="red","Not binned"="orange"))  +
  scale_x_continuous(var1name,labels = math_format(10^.x),limits = xlimit) + #,limits = xlimit
    scale_y_continuous(var2name,labels = math_format(10^.x)) +
    ggtitle(paste(var2name,"vs",var1name)) +
    annotation_logticks() +
    theme(text = element_text(size=8),
          plot.title = element_text(size=10),
          axis.title = element_text(size = 8),
          axis.text = element_text(size=6),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="right")
}


get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


#### ANlaysis #####
###inequality #####
#views
getGini(full_df$view)
# #replies
# getGini(full_df$reply)
# # authors
# getGini(full_df$author)
# # con_len
# getGini(full_df$con_len)
# #Average time
# getGini(full_df$run_av)



###### Descriptive plots ######

### Views - log normal -- do a K-S test
views_xlog_plots = make_xlog_density_plot(full_df,full_df$view,view_name,"log-normal",plnorm,0.1)
views_xlog = gridExtra::grid.arrange(views_xlog_plots$density_hist, views_xlog_plots$CDF, ncol = 2)
ggsave("views_dist_xlog.png",views_xlog, units = "cm", width = 16, height = 8)

# ### reply - power-law (but potentially log-normal)
# reply_loglog = make_loglog_plot(full_df,full_df$reply,reply_name)
# 
# ### Authors - likely power-law (but potentially log-normal)
# authors_loglog = make_loglog_plot(full_df,full_df$author,author_name)
# 
# ### con length -- kind of log-normal
# con_xlog_density = make_xlog_density_plot(full_df,full_df$con_len,con_name,"log-normal",plnorm,0.2,"Con length")
# con_xlog_density$density_hist
# 
# ### run-av -- log normal
# run_xlog_density = make_xlog_density_plot(full_df,full_df$run_av,run_name,"log-normal",plnorm,0.2,"Av. time")
# run_xlog_density$density_hist
# 
# ind.dist.plot = gridExtra::grid.arrange(reply_loglog,authors_loglog, con_xlog_density$density_hist,run_xlog_density$density_hist, ncol = 2)
# ggsave("other_dist_plots.png",ind.dist.plot, units = "cm",width = 16, height = 16)


##### Bivariate analysis #####

### heat plots (reason for binning)
VvR_heat = plot.heat(data = full_df,full_df$reply,reply_name,full_df$view,view_name)
VvA_heat = plot.heat(data = full_df, full_df$author,author_name,full_df$view,view_name)
VvC_heat = plot.heat(data = full_df,full_df$con_len,con_name,full_df$view,view_name)
VvRa_heat = plot.heat(data = full_df, full_df$run_av,run_name,full_df$view,view_name)

top_leg = get_legend(VvA_heat)

#make 2 heat plot sets
#RvA
RvAheat.plots = grid.arrange(arrangeGrob(VvR_heat + theme(legend.position="none"),
                                             VvA_heat + theme(legend.position="none"),
                                       top_leg,nrow=1,widths = c(10,10,2)),nrow=1)

ggsave("RvAheat_plots.png",RvAheat.plots, units = "cm",width = 16, height = 8)

# CvRa
bottom_leg = get_legend(VvC_heat)

CvRaheat.plots = grid.arrange(arrangeGrob(VvC_heat + theme(legend.position="none"),
                                VvRa_heat + theme(legend.position="none"),
                              bottom_leg, nrow=1,widths = c(10,10,2)), nrow=1)

# heat.plots = gridExtra::grid.arrange(VvR_heat,VvA_heat,VvC_heat,VvRa_heat, ncol = 2)
ggsave("CVRaheat_plots.png",CvRaheat.plots, units = "cm",width = 16, height = 8)


### bivariate plots 

## Views + replies + authors

#views and replies
VvR_log_fit = fit.log.model(var1 = full_df$reply,var2 =full_df$view,bins = 15)
# VvR_log_fit$orig_exponent
# VvR_log_fit$orig_rsquare
# VvR_log_fit$bin_exponent

#views and authors
VvA_log_fit = fit.log.model(var1 = full_df$author,var2 = full_df$view,bins = 15)
# VvA_log_fit$orig_exponent
# VvA_log_fit$orig_rsquare
# VvA_log_fit$bin_exponent

#plot replies and authors
VvR_logbin_plot = plot.log.bin.bivar(VvR_log_fit$bin_df,VvR_log_fit$orig_df,reply_name,view_name,xmin = 0,xmax = 4)
VvA_logbin_plot = plot.log.bin.bivar(VvA_log_fit$bin_df,VvA_log_fit$orig_fit, author_name,view_name,xmin = 0,xmax = 4)

views.by.reply.author = grid.arrange(arrangeGrob(VvR_logbin_plot + theme(legend.position="none"),
                           VvA_logbin_plot + theme(legend.position="none"),nrow = 1))

ggsave("VbyR_A_plots.png",views.by.reply.author, units = "cm",width = 16, height = 8)

# against each other # more people in a conversation - more posts
RvA_log_fit = fit.log.model(var1 = full_df$author,var2 = full_df$reply,bins = 15)
# RvA_log_plot = plot.log.nonbin.bivar(full_df,full_df$author,full_df$reply,author_name,reply_name,xlimit = c(0,4))
RvA_logbin_plot = plot.log.bin.bivar(RvA_log_fit$bin_df,RvA_log_fit$orig_df,author_name,reply_name,xmin = 0,xmax = 4)

ggsave("RbyA_plots.png",RvA_logbin_plot, units = "cm",width = 8, height = 16)

#All together now
V_RAleg = get_legend(VvA_logbin_plot)

views_reply_author_plot = grid.arrange(arrangeGrob(VvR_logbin_plot + theme(legend.position="none"),
                                                 VvA_logbin_plot + theme(legend.position="none"),nrow = 1),
                                     arrangeGrob(RvA_logbin_plot + theme(legend.position = "none"),
                                                 V_RAleg,nrow = 1),ncol = 1) #,heights = c(9,10)

ggsave("VbyRbyA_plots.png",views_reply_author_plot, units = "cm",width = 12, height = 12)


# Other two variables

###views and conversation length
VvC_log_fit = fit.log.model(var1 = full_df$con_len,var2 = full_df$view,bins = 15) # some serious issue here
#views and running average
VvRa_log_fit = fit.log.model(var1 = full_df$run_av,var2 =full_df$view,bins = 15)

#plot conversation length and average time
VvC_log_bin_plot = plot.log.bin.bivar(VvC_log_fit$bin_df,VvC_log_fit$orig_df, con_name, view_name,xmin = 1,xmax = 9)
VvRa_log_bin_plot = plot.log.bin.bivar(VvRa_log_fit$bin_df,VvRa_log_fit$orig_df,run_name,view_name,xmin = 1,xmax = 9)

views.by.con.av = grid.arrange(arrangeGrob(VvC_log_bin_plot + theme(legend.position="none"),
                                           VvRa_log_bin_plot + theme(legend.position="none"),nrow = 1))
ggsave("VbyC_Ra_plots.png",views.by.con.av, units = "cm",width = 16, height = 8)


# by each other
#running average and con length
RavC_log_fit = fit.log.model(var1 = full_df$con_len,var2 = full_df$run_av,bins = 15) # some serious issue here
RavC_log_bin_plot = plot.log.bin.bivar(RavC_log_fit$bin_df,RavC_log_fit$orig_df, con_name, "Average reply time",xmin = 1,xmax = 9)


ggsave("RabyC_plots.png",RavC_log_bin_plot, units = "cm",width = 8, height = 8)


#All together now
V_RaCleg = get_legend(VvC_log_bin_plot)

views_con_av_plot = grid.arrange(arrangeGrob(VvC_log_bin_plot + theme(legend.position="none"),
                                             VvRa_log_bin_plot + theme(legend.position="none"),nrow = 1),
                                       arrangeGrob(RavC_log_bin_plot + theme(legend.position = "none"),
                                                   V_RaCleg,nrow = 1),ncol = 1) 

ggsave("VbyCbyRa_plots.png",views_con_av_plot, units = "cm",width = 12, height = 12)






### temporal analysis

#make a new df
time_df = full_df[,c("unix_first_post","view","author","posts","con_len","run_av")] #,"vpd"

## get year, month, day and hour from timestamp
time_df$year = year(time_df$unix_first_post)
time_df$month = month(time_df$unix_first_post, label = T)
# time_df$month_num = month(time_df$unix_first_post)
time_df$wday = wday(time_df$unix_first_post, label = T)
# time_df$wday_num = wday(time_df$unix_first_post)
time_df$hour = hour(time_df$unix_first_post)
time_df$date = date(time_df$unix_first_post)

#trim to 2010
start.date.all = as.POSIXct("2010-01-01 00:00", tz = "GMT")
end.date.all = as.POSIXct("2018-12-31 23:59", tz = "GMT")
time_df_all = subset(time_df, time_df$date >= start.date.all & time_df$date <= end.date.all)
# time_df_all = time_df

# look at counts of topics created
topic.created =  stats::aggregate(time_df_all$date,
                                  by = list(as.character(time_df_all$date),
                                            as.character(time_df_all$year),
                                            as.character(time_df_all$month),
                                            as.character(time_df_all$wday),
                                            as.character(time_df_all$hour)), #
                                  FUN = length)
colnames(topic.created) = c("date","year","month","weekday","hour","num_topics")


#hourly
topic.hour =  stats::aggregate(topic.created$num_topics,
                               by = list(topic.created$hour,topic.created$year), #
                               FUN = sum)
colnames(topic.hour) = c("hour","year","num_topics")
topic.hour$hour = as.numeric(topic.hour$hour)

#to normalise
topic.hour2 <- transform(topic.hour, year.total = ave(num_topics, year, FUN = sum))
topic.hour2 <- transform(topic.hour2, proportion = num_topics/year.total)

topic_hour_plot = ggplot(topic.hour2,
       aes(x = hour,
           y = proportion,
           group = year,
           color = factor(year))) +
  geom_line(size = 0.75) +
  scale_color_viridis_d(name = "Year",option = "A",begin = 0.2,end = 1) +
  scale_x_continuous("Hour",breaks = seq(0,24,1)) +
  scale_y_continuous("Proportion of topics created",breaks = seq(0,0.08,0.01),limits = c(0,0.08)) +
  ggtitle("Topics created per hour") +
  theme(text = element_text(size=8),
        plot.title = element_text(size=10),
        axis.title = element_text(size = 8),
        axis.text = element_text(size=6),
        # axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgrey",size=0.05),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="right")
topic_hour_plot
# ggplot(topic.hour,
#        aes(x = hour,
#            y = num_topics,
#            group = year,
#            color = factor(year))) +
#   geom_line(size = 0.5)

#weekly
topic.week =  stats::aggregate(topic.created$num_topics,
                               by = list(topic.created$weekday,topic.created$year), #
                               FUN = sum)
colnames(topic.week) = c("wday","year","num_topics")
topic.week$wday = as.factor(topic.week$wday)
topic.week$wday = factor(topic.week$wday,levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

#to normalise
topic.week2 <- transform(topic.week, year.total = ave(num_topics, year, FUN = sum))
topic.week2 <- transform(topic.week2, proportion = num_topics/year.total)

topic.week.plot = ggplot(topic.week2,
       aes(x = wday,
           y = proportion,
           group = year,
           color = factor(year))) +
  geom_line(size = 0.75) +
  scale_color_viridis_d(name = "Year",option = "A",begin = 0.2,end = 1) +
  scale_x_discrete(name = "Weekday") + #,breaks = seq(0,24,1)
  scale_y_continuous("Proportion of topics created",breaks = seq(0,0.16,0.01)) + #,limits = c(0.04,0.08)
  ggtitle("Topics created per day") +
  theme(text = element_text(size=8),
        plot.title = element_text(size=10),
        axis.title = element_text(size = 8),
        axis.text = element_text(size=6),
        # axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgrey",size=0.05),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="bottom")
topic.week.plot

# year_legend = get_legend(topic.week.plot)

topic_created_plot = grid.arrange(topic_hour_plot + theme(legend.position="none"),
                                  topic.week.plot,heights = c(12,14), ncol = 1)
ggsave("time_plots.png",topic_created_plot, units = "cm",width = 12, height = 16)




