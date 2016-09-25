# install "dplyr", "xlsx", "tcltk", and "ggplot2" if not already in installed
packages <- c("dplyr", "xlsx", "tcltk","ggplot2")
int <- packages %in% installed.packages()
if(sum(!int)>0) {
      install.packages(packages[!int])
}

# Loading data manipulation package "dplyr", excel package "xlsx", and interactive dialog box package "tcltk"
library(dplyr)
library(xlsx)
library(tcltk)
library(ggplot2)

# Interactive dialog box for user to select the appropriate files
ndc2rxmg_file <- tk_choose.files(default = "", caption = "select the NDC2RxMG file")
drugs_file <- tk_choose.files(default = "", caption = "select the Drugs file")
acgrxmgs_file <- tk_choose.files(default = "", caption = "select the ACGRxMGS file")
prescription_file <- tk_choose.files(default = "", caption = "select the Prescription file")

# load the data into R
ndc2rxmg <- read.delim(ndc2rxmg_file, header=FALSE, colClasses = c("character", "character"))
acgrxmgs <- read.delim(acgrxmgs_file, header=FALSE, colClasses = rep("character", 8))
drugs <- read.delim(drugs_file, header=FALSE, colClasses = rep("character", 10), quote = "")
prescription <- read.delim2(prescription_file, header=FALSE, colClasses = rep("character", 10))

# name the variables of the data 
colnames(drugs) <- c("drug", "ndc", "brand", "tcgpi", "cs", "ti", "ddid", "gpid", "hrm", "ras")
colnames(acgrxmgs) <- c("id", "rxmg", "rxmgd", "mrxmg", "mrxmgd", "rxmi", "msource", "psource")
colnames(ndc2rxmg) <- c("ndc", "rxmg")
colnames(prescription) <- c("cn", "id", "pid", "rxn", "nr", "ndc", "fd", "brand", "ds", "model")

# select the data to be used
acgrxmgs <- acgrxmgs[, c("id","rxmg", "rxmgd")]
prescription <- prescription[, c("id", "ndc")]
drugs <- drugs[, c("drug", "ndc", "tcgpi")]

# Since my computer can't merge all the files, I used a sample of the data: 
# acgrxmgs <- acgrxmgs[sample(nrow(acgrxmgs), 1000),]

# join the files together 
data <- acgrxmgs %>% inner_join(ndc2rxmg, by = "rxmg") %>% 
      inner_join(prescription, by = c("ndc", "id")) %>% inner_join(drugs, by = "ndc")

# create a variable name nscripts with value 1 
data$nscripts <- 1

# summarize the total scripts per rxmg and drug
nscripts <- data %>% group_by(rxmg, rxmgd, drug, tcgpi) %>% summarise(nscripts = sum(nscripts))

# summarize the total scripts per patient for each rxmg and drug
nsp <- data %>% group_by(rxmg, rxmgd, drug, tcgpi, id) %>% summarise(nsp = sum(nscripts))

# assign npatients variable to nsp (number of scripts per patient) data
nsp$npatients <- 1

# summarize the number of patients for each rxmg and drug
npatients <- nsp %>% group_by(rxmg, rxmgd, drug, tcgpi) %>% summarise(npatients = sum(npatients))

# rank the number of scripts and group them by rxmg
ranked_nscripts <- nscripts %>% arrange(rxmg, -nscripts) %>% group_by(rxmg)

# rank the number of patients and group them by rxmg
ranked_npatients <- npatients %>% arrange(rxmg, -npatients) %>% group_by(rxmg)

# combine the ranked data for nscripts and npatients
combined_ranked <- ranked_nscripts %>% inner_join(ranked_npatients, by = c("rxmg", "rxmgd", "drug", "tcgpi"))

# create a forloop to look at each unique rxmg and rank the top 15 drugs or maximum number of drugs per rxmg
unique_rxmg <- unique(ranked_nscripts$rxmg)
top <- list()
for(i in 1:length(unique_rxmg)) {
      # extract the data for one unique rxmg
      combined <- combined_ranked[combined_ranked$rxmg==unique_rxmg[i],]
      # if there are more than 15 drugs, set t to 15 else set to the number of drugs
      if(nrow(combined)>=15) {
            t <- 15
      } else {
            t <- nrow(combined)
      }
      # rank the data from 1 to number of drugs, up to 15
      combined$rank <- 1:nrow(combined)
      # store data as list
      top[[i]] <- combined[1:t, ]
}
# rbind all the data into a data frame
top <- do.call(rbind, top)

# select the appropriate variables
results <- top[, c("rxmg", "rxmgd", "rank", "drug", "tcgpi", "nscripts", "npatients")]

# plot showing relationship between number of scripts vs number of patients
ggplot(results, aes(nscripts, npatients))+geom_point(color = "darkblue")+geom_smooth(method = "lm", color = "black") + 
      xlab("Number of Scripts") + ylab("Number of Patients") + ggtitle("Number of Scripts vs Number of Patients") +
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))+
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12))

# histogram of rank
ggplot(results, aes(rank)) + geom_histogram(binwidth = 1)+xlab("Rank") +ylab("Frequency") +
      ggtitle("Histogram of Rank Variable") +
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))+
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12))

# boxplot of number of scripts by rank
ggplot(results, aes(as.factor(rank), nscripts)) + geom_boxplot(outlier.color = "darkred",fill = "lightblue") + 
      xlab("Rank") + ylab("Number of Scripts") + ggtitle("Number of Scripts by Rank") +
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))+
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12))

# boxplot of number of patients by rank
ggplot(results, aes(as.factor(rank), npatients)) + geom_boxplot(outlier.color = "darkred", fill = "orange") + 
      xlab("Rank") + ylab("Number of Patients") + ggtitle("Number of Patients by Rank") +
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14))+
      theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=12))

# rename the variables
colnames(results) <- c("RxMG", "description", "Rank", "Drug Name", "Therapeutic Class (MediSpan)", 
                       "# of scripts", "# of patients")
# set data as data frame
results <- as.data.frame(results)

# output the data in xlsx format and name according to the project type (ie commercial)
s <- strsplit(prescription_file, "/")
type <- s[[1]][length(s[[1]])-2]
write.xlsx2(results,paste(type, "results.xlsx", sep= "_"), row.names = F)
