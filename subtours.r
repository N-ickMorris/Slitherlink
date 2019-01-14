# ---- create subtour function -----------------------------------------------

subtours = function(dat, S = 0)
{
	j = 1
	tours = list()
	done = FALSE
	
	while(done == FALSE)
	{
		i = 1
		rows = c(1)
		start.node = dat$V1[1]
		tour = FALSE
	
		while(tour == FALSE)
		{
			i = which(dat$V1 == dat$V2[i])
			
			if(start.node == dat$V1[i])
			{
				S = S + 1
				DF = dat[rows,]
				DF$V3 = paste0("(", DF$V1, ", ", DF$V2, ")")
				DF = data.frame(c(paste0("set S[", S, "] :="), DF[,3], ";"))
				colnames(DF) = rep(" ", NCOL(DF))
				tour = TRUE
			} else
			{
				rows = c(rows, i)
			}
		}

		dat = dat[-rows,]

		if(NROW(dat) == 0)
		{
			tours[[j]] = DF
			done = TRUE
		} else
		{
			tours[[j]] = DF
			j = j + 1
			rownames(dat) = 1:NROW(dat)
		}
	}

	if(length(tours) == 1)
	{
		print("Only 1 Tour Found")
	} else
	{
		for(i in 1:length(tours))
		{
			print(tours[[i]], right = FALSE, row.names = FALSE)
		}
	}
}

# ---- a script for returning subtours of the slither.mod solution --------------------------------------

dat = read.table(file.path("E:", "My Stuff", "ampl", "App", "slither.txt"))
subtours(dat = dat, S = 0)




















































