#-------------------------------------------------------------------------------
original_data_file  <- "D:/StatsProject R-codes/Data/AdjustedOriginal.csv"
collected_data_file <- "D:/StatsProject R-codes/Data/AdjustedCollected.csv"

#folder where the boxplots are saved
original_data_folder  <- "D:/StatsProject R-codes/Plots/Original/"
collected_data_folder <- "D:/StatsProject R-codes/Plots/Collected/"
#-------------------------------------------------------------------------------


library(reshape2)    #For converting named list to a data frame
library(ggplot2)     #For plotting the data

#This function is used to plot data. It takes in the indices of two groups and 
# names of the groups, then returns a ggplot plot which contains 5 boxplots in
# groups of two, 1 for each trust construct


plotdata <- function(X, Y, n1="g1", n2 = "g2"){
  
  #z is a data frame with 3 columns, col 1 is mean trust values, col 2 shows the
  #group X or Y, and col 3 shows which trust construct is that mean value from
  z <- melt(list(Benevolence    = list(X = benevolence[X]   , Y = benevolence[Y]),
                 Integrity      = list(X = integrity[X]     , Y = integrity[Y]), 
                 Competence     = list(X = competence[X]    , Y = competence[Y]), 
                 Identification = list(X = identification[X], Y = identification[Y]),
                 Concern        = list(X = concern[X]       , Y = concern[Y])))
  
  #p is the ggplot boxplot that is going to be returned
  p <- ggplot(z, aes(x=value, y=L1, group = interaction(L2, L1))) +    #initializing p with coupling the two groups boxplots
       geom_boxplot(alpha = 1, aes(fill = L2, color = L2), 
                    position = position_dodge(0.8)) +                   #making a boxplot with coupling according to col 2
       coord_flip()+labs(y="", x="") +                                  #making boxplot vertical and removing names from axes
       scale_color_manual(values = c("orange", "grey70"),               #colouring the boxplot lines and labeling legend acc to the 
                          name = " ", labels = c(n1,n2)) +              #  inputted group names
       scale_fill_manual(values = c("Orange", "Grey70"),                #Similar as above, but instead colouring the fill
                         name = " ", labels = c(n1,n2)) +               #  making the boxplot a solid colour
       scale_y_discrete(limits=c("Benevolence","Integrity","Competence",
                                 "Identification","Concern")) +         #Labelling x axis ticks acc to trust constructs
       theme_classic() +                                                #Setting theme for background and axis
       theme(legend.position="bottom",                                  #Positioning the Legend
             panel.background = element_rect(fill = "transparent"),     #The next few lines are to set the background
             plot.background = element_rect(fill = "transparent",       #  transparent so that the plots blend in 
                                            color = NA),                #  with the ppt background
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             legend.background = element_rect(fill = "transparent"),
             legend.box.background = element_rect(fill = "transparent"),
             legend.text = element_text(colour = "White"),              #Setting text and axis lines to white to make 
             axis.text=element_text(colour="White"),                    #  it easier to read on a black background.
             axis.line = element_line(colour = "White"),                #When the images will be used in the report, 
             axis.ticks = element_line(colour = "White")                #  the colour will be set to black
             ) +
       stat_summary(geom = "crossbar", width = 0.7, fatten=0.1,         #This part inserts the median lines for boxplots.
                    color="white", position = position_dodge(0.8),      #This part of code has been taken from online sources 
                    fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))})
  p
}

################################################################################
# plots for the original data
################################################################################

target_folder <- original_data_folder
ddt = read.csv(original_data_file)
attach(ddt)


#plots are made only for those groups which show significant difference
#ggsave function saves the generated plot to required folder, in the given 
#  dimensions, along with a transparent background

# By gender
X = which(Gender == 1)
Y = which(Gender == 2)
plotdata(X, Y, "Male", "Female")
ggsave("By_Gender.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")

#By age
X = which(Age < 2)
Y = which(Age > 1)
plotdata(X, Y, "Age >= 21", "Age =< 20")
ggsave("By_Age.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")

#By time used
X = which(Time < 5)
Y = which(Time > 4)
plotdata(X, Y, "more than \nfew times", "\nonce or less")
ggsave("By_use.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")

#LinkedIn usage
X = which(LinkedIn == 1)
Y = which(LinkedIn == 0)
plotdata(X, Y, "LinkedIn users", "LinkedIn non-users")
ggsave("LinkedIn.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")

#Instagram usage
X = which(Instagram == 1)
Y = which(Instagram == 0)
plotdata(X, Y, "Instagram users", "Instagram non-users")
ggsave("Instagram.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")


detach(ddt)

################################################################################
# plots for the data we collected
################################################################################

target_folder <- collected_data_folder
ddt = read.csv(collected_data_file)
attach(ddt)

#By age
X = which(Age > 19)
Y = which(Age < 20)
plotdata(X, Y, "Age >= 20", "Age =< 19")
ggsave("By_Age.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")

#By time used
X = which(Time < 5)
Y = which(Time > 4)
plotdata(X, Y, "more than \nfew times", "\nonce or less")
ggsave("By_use.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")

#LinkedIn usage
X = which(LinkedIn == 1)
Y = which(LinkedIn == 0)
plotdata(X, Y, "LinkedIn users", "LinkedIn non-users")
ggsave("LinkedIn.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")

#Instagram usage
X = which(Instagram == 1)
Y = which(Instagram == 0)
plotdata(X, Y, "Instagram users", "Instagram non-users")
ggsave("Instagram.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")

#Pinterest usage
X = which(Pinterest == 1)
Y = which(Pinterest == 0)
plotdata(X, Y, "Pinterest users", "Pinterest non-users")
ggsave("Pinterest.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")

#Amount of data shared
X = which(Shared_data < 4)
Y = which(Shared_data > 3)
plotdata(X, Y, "less data\nshared", "more data\nshared")
ggsave("By_shared.png", width = 5, height = 4, dpi = 300, units = "in",
       path = target_folder, bg = "transparent")


detach(ddt)
################################################################################
#comparing our data to theirs
#we basically combine those two to form a data frame and continue as above
################################################################################

ddt1 = read.csv(original_data_file)
ddt2 = read.csv(collected_data_file)

index<- append(rep(0,dim(ddt1)[1]), rep(1, dim(ddt2)[1]))

for (i in c("benevolence","integrity","competence","identification","concern"))
{
  assign(i, append(ddt1[,i], ddt2[,i]))
}

ddt = data.frame(index, benevolence, integrity, competence, identification, concern)
attach(ddt)

X <- which(index == 0)
Y <- which(index == 1)
plotdata(X, Y, "Original Data", "Our collected data")
ggsave("Original_vs_Collected.png", width = 5, height = 4, dpi = 300, units = "in",
       path = original_data_folder, bg = "transparent")

detach(ddt)

