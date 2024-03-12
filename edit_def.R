library(glue)

EditDef <- function(def_file, num, new_def_file) {
  # rewrite def file so we only print the desired generations
  # i.e., the last generation and the one num generations ago 
  # read.delim doesn't work well with files with many columns
  # so we find number of columns first
  command <- glue("awk '{print NF}' {{def_file} | sort -nr | head -n 1", .open="{{")
	ncols <- as.numeric(system(command, intern = T))
	def <- read.delim(def_file, header = F, sep = " ", col.names = paste0("V", 1:ncols))
	def$V2 <- 0
	def$V2[nrow(def)] <- 1
	def$V2[nrow(def) - num] <- 1
	write.table(def, new_def_file, sep = " ", na = "", col.names = F, quote = F, row.names = F)
}


