# -------------------------------------------------------------------------------------------------------------------------------------
# ---- required packages --------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

{

require(stringr)
require(data.table)
require(ggplot2)
require(gridExtra)

}

# -------------------------------------------------------------------------------------------------------------------------------------
# ---- import the demands of the puzzle -----------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

# these are two puzzles that will be solved using metahueristics
# this two examples are how a puzzle is required to be imported into R
	# the format is a matrix of the demands where NA corresponds to a blank

slither1 = cbind(c(NA, NA, 2, 2, NA, NA, 2, NA, NA, 3), 
				 c(3, NA, NA, 2, NA, NA, NA, 2, NA, 2), 
				 c(NA, 1, 2, 0, 2, 2, NA, 1, NA, 2), 
				 c(NA, 2, NA, NA, NA, 1, NA, NA, NA, 2), 
				 c(2, NA, 2, NA, 2, NA, 2, NA, NA, 2), 
				 c(NA, NA, 3, 3, NA, NA, NA, NA, 3, NA), 
				 c(NA, 1, NA, NA, 3, NA, 3, NA, 3, NA), 
				 c(NA, 2, 2, NA, NA, NA, 3, NA, NA, 2), 
				 c(3, 2, 2, 2, NA, NA, 3, 2, NA, NA), 
				 c(3, NA, NA, NA, NA, NA, NA, NA, 2, NA))

slither2 = cbind(c(3, NA, 2, 1, NA, 3, NA, 1, NA, 3), 
				 c(NA, NA, NA, 1, NA, NA, 0, 2, NA, NA), 
				 c(3, NA, NA, NA, 3, NA, NA, 3, 2, 3), 
				 c(NA, NA, 3, NA, NA, NA, 1, NA, NA, NA), 
				 c(3, NA, 3, NA, 2, NA, NA, NA, 2, NA), 
				 c(3, NA, 2, NA, NA, NA, 2, 3, NA, 2), 
				 c(NA, NA, 2, 1, NA, 2, NA, 2, NA, 1), 
				 c(3, NA, NA, 1, 3, 2, NA, NA, 2, 1), 
				 c(NA, 1, NA, NA, NA, 3, NA, NA, NA, NA), 
				 c(NA, 2, NA, 2, 3, NA, NA, 3, 1, 1))

# dat will be the puzzle that is currently being solved

dat = as.matrix(slither2)

rm(slither1, slither2)

# -------------------------------------------------------------------------------------------------------------------------------------
# ---- intialize data for the puzzle ------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

initialize = function(dat)
{
	require(stringr)
	require(data.table)
	
	# -------------------------------------------------------------------------------------------------------------------------------------
	# ---- compute all bidirectional arcs -------------------------------------------------------------------------------------------------
	# -------------------------------------------------------------------------------------------------------------------------------------

	# arcs initially computes all arcs in one particular direction

	arcs = c(as.vector(sapply(1:NROW(dat), function(j)				 
						sapply(1:(NROW(dat) + 1), function(i)
						paste(((NROW(dat) + 1) * (i - 1)) + j, ((NROW(dat) + 1) * (i - 1)) + j + 1, sep = ",")))),
					 
			 as.vector(sapply(1:(NROW(dat) + 1), function(j)				 
						sapply(1:NROW(dat), function(i)
						paste(((NROW(dat) + 1) * (i - 1)) + j, ((NROW(dat) + 1) * (i - 1)) + j + (NROW(dat) + 1), sep = ",")))))

	# id is a number to uniquely identify each arc
	# node1 is the starting position of an arc (ie. the "from" node)
	# node2 is the ending position of an arc (ie. the "to" node)

	arcs = str_split_fixed(arcs, ",", 2)
	arcs = data.frame("id" = 1:NROW(arcs), 
						"node1" = as.integer(arcs[,1]), 
						"node2" = as.integer(arcs[,2]))

	# -------------------------------------------------------------------------------------------------------------------------------------
	# ---- create decision vectors --------------------------------------------------------------------------------------------------------
	# -------------------------------------------------------------------------------------------------------------------------------------

	# x, the current soluition, represents whether or not an arc is used
		# 1 for used
		# 0 for not used

	x = rep(0, NROW(arcs))

	# v represent wheter or not an arc can vary

	v = rep(1, NROW(arcs))

	# f represents the fixed value of an arc
		# 1 for used
		# 0 for not used

	f = rep(0, NROW(arcs))

	# -------------------------------------------------------------------------------------------------------------------------------------
	# ---- extract arcs that wrap around demands ------------------------------------------------------------------------------------------
	# -------------------------------------------------------------------------------------------------------------------------------------

	# nums is a list of dataframes
		# each dataframe represents the coordinate position of all zeros, ones, twos, or threes in dat

	nums = sapply(0:3, function(k)
						matrix(paste(round(ifelse((which(dat == k) / NROW(dat)) - floor(which(dat == k) / NROW(dat)) == 0, 1, (which(dat == k) / NROW(dat)) - floor(which(dat == k) / NROW(dat))) * NROW(dat), 0),
										round(ceiling(which(dat == k) / NROW(dat)), 0), sep = ","),
								ncol = 1))

	nums = lapply(1:length(nums), function(i)
							data.frame(apply(X = str_split_fixed(nums[[i]], ",", 2),
												MARGIN = c(1,2),
												FUN = as.integer)))

	names(nums) = c("zeros", "ones", "twos", "threes")

	# ---- extract a set of 4 arcs that correspond to each coordinates in nums$zeros --------------------------------------------------------

	if(NROW(nums$zeros) > 0)
	{
		arcs.zeros = lapply(1:NROW(nums$zeros), function(i)
						matrix(paste(c(nums$zeros$X1[i] + ((nums$zeros$X2[i] - 1) * NROW(dat)) + (nums$zeros$X2[i] - 1),
									   nums$zeros$X1[i] + ((nums$zeros$X2[i] - 1) * NROW(dat)) + (nums$zeros$X2[i] - 1),
									   nums$zeros$X1[i] + ((nums$zeros$X2[i] - 1) * NROW(dat)) + (nums$zeros$X2[i] - 1) + 1,
									   nums$zeros$X1[i] + ((nums$zeros$X2[i] - 1) * NROW(dat)) + (nums$zeros$X2[i] - 1) + (NROW(dat) + 1)),
						 
									 c(nums$zeros$X1[i] + ((nums$zeros$X2[i] - 1) * NROW(dat)) + (nums$zeros$X2[i] - 1) + (NROW(dat) + 1),
									   nums$zeros$X1[i] + ((nums$zeros$X2[i] - 1) * NROW(dat)) + (nums$zeros$X2[i] - 1) + 1,
									   nums$zeros$X1[i] + ((nums$zeros$X2[i] - 1) * NROW(dat)) + (nums$zeros$X2[i] - 1) + (NROW(dat) + 2),
									   nums$zeros$X1[i] + ((nums$zeros$X2[i] - 1) * NROW(dat)) + (nums$zeros$X2[i] - 1) + (NROW(dat) + 2)),
									 sep = ",")))

		arcs.zeros = lapply(1:length(arcs.zeros), function(i)
							data.frame(apply(X = str_split_fixed(arcs.zeros[[i]], ",", 2),
												MARGIN = c(1,2),
												FUN = as.integer)))

		arcs.zeros = data.frame(rbindlist(arcs.zeros))

		arcs.zeros$X3 = sapply(1:NROW(arcs.zeros), function(i) 
								which(arcs$node1 == arcs.zeros$X1[i] & arcs$node2 == arcs.zeros$X2[i]))

		arcs.zeros = lapply(1:(NROW(arcs.zeros) / 4), function(i)
							data.frame(t(arcs.zeros$X3[((4 * (i - 1)) + 1):(((4 * (i - 1)) + 1) + 3)])))
		
		arcs.zeros = data.frame(rbindlist(arcs.zeros))
		colnames(arcs.zeros) = c("arc1", "arc2", "arc3", "arc4")
	}

	# ---- extract a set of 4 arcs that correspond to each coordinates in nums$ones -------------------------------------------------------------

	if(NROW(nums$ones) > 0)
	{
		arcs.ones = lapply(1:NROW(nums$ones), function(i)
						matrix(paste(c(nums$ones$X1[i] + ((nums$ones$X2[i] - 1) * NROW(dat)) + (nums$ones$X2[i] - 1),
									   nums$ones$X1[i] + ((nums$ones$X2[i] - 1) * NROW(dat)) + (nums$ones$X2[i] - 1),
									   nums$ones$X1[i] + ((nums$ones$X2[i] - 1) * NROW(dat)) + (nums$ones$X2[i] - 1) + 1,
									   nums$ones$X1[i] + ((nums$ones$X2[i] - 1) * NROW(dat)) + (nums$ones$X2[i] - 1) + (NROW(dat) + 1)),
						 
									 c(nums$ones$X1[i] + ((nums$ones$X2[i] - 1) * NROW(dat)) + (nums$ones$X2[i] - 1) + (NROW(dat) + 1),
									   nums$ones$X1[i] + ((nums$ones$X2[i] - 1) * NROW(dat)) + (nums$ones$X2[i] - 1) + 1,
									   nums$ones$X1[i] + ((nums$ones$X2[i] - 1) * NROW(dat)) + (nums$ones$X2[i] - 1) + (NROW(dat) + 2),
									   nums$ones$X1[i] + ((nums$ones$X2[i] - 1) * NROW(dat)) + (nums$ones$X2[i] - 1) + (NROW(dat) + 2)),
									 sep = ",")))

		arcs.ones = lapply(1:length(arcs.ones), function(i)
							data.frame(apply(X = str_split_fixed(arcs.ones[[i]], ",", 2),
												MARGIN = c(1,2),
												FUN = as.integer)))

		arcs.ones = data.frame(rbindlist(arcs.ones))

		arcs.ones$X3 = sapply(1:NROW(arcs.ones), function(i) 
								which(arcs$node1 == arcs.ones$X1[i] & arcs$node2 == arcs.ones$X2[i]))

		arcs.ones = lapply(1:(NROW(arcs.ones) / 4), function(i)
							data.frame(t(arcs.ones$X3[((4 * (i - 1)) + 1):(((4 * (i - 1)) + 1) + 3)])))

		arcs.ones = data.frame(rbindlist(arcs.ones))
		colnames(arcs.ones) = c("arc1", "arc2", "arc3", "arc4")
	}

	# ---- extract a set of 4 arcs that correspond to each coordinates in nums$twos --------------------------------------------------------------

	if(NROW(nums$twos) > 0)
	{
		arcs.twos = lapply(1:NROW(nums$twos), function(i)
						matrix(paste(c(nums$twos$X1[i] + ((nums$twos$X2[i] - 1) * NROW(dat)) + (nums$twos$X2[i] - 1),
									   nums$twos$X1[i] + ((nums$twos$X2[i] - 1) * NROW(dat)) + (nums$twos$X2[i] - 1),
									   nums$twos$X1[i] + ((nums$twos$X2[i] - 1) * NROW(dat)) + (nums$twos$X2[i] - 1) + 1,
									   nums$twos$X1[i] + ((nums$twos$X2[i] - 1) * NROW(dat)) + (nums$twos$X2[i] - 1) + (NROW(dat) + 1)),
						 
									 c(nums$twos$X1[i] + ((nums$twos$X2[i] - 1) * NROW(dat)) + (nums$twos$X2[i] - 1) + (NROW(dat) + 1),
									   nums$twos$X1[i] + ((nums$twos$X2[i] - 1) * NROW(dat)) + (nums$twos$X2[i] - 1) + 1,
									   nums$twos$X1[i] + ((nums$twos$X2[i] - 1) * NROW(dat)) + (nums$twos$X2[i] - 1) + (NROW(dat) + 2),
									   nums$twos$X1[i] + ((nums$twos$X2[i] - 1) * NROW(dat)) + (nums$twos$X2[i] - 1) + (NROW(dat) + 2)),
									 sep = ",")))

		arcs.twos = lapply(1:length(arcs.twos), function(i)
							data.frame(apply(X = str_split_fixed(arcs.twos[[i]], ",", 2),
												MARGIN = c(1,2),
												FUN = as.integer)))

		arcs.twos = data.frame(rbindlist(arcs.twos))

		arcs.twos$X3 = sapply(1:NROW(arcs.twos), function(i) 
								which(arcs$node1 == arcs.twos$X1[i] & arcs$node2 == arcs.twos$X2[i]))

		arcs.twos = lapply(1:(NROW(arcs.twos) / 4), function(i)
							data.frame(t(arcs.twos$X3[((4 * (i - 1)) + 1):(((4 * (i - 1)) + 1) + 3)])))

		arcs.twos = data.frame(rbindlist(arcs.twos))
		colnames(arcs.twos) = c("arc1", "arc2", "arc3", "arc4")
	}

	# ---- extract a set of 4 arcs that correspond to each coordinates in nums$threes -----------------------------------------------------------

	if(NROW(nums$threes) > 0)
	{
		arcs.threes = lapply(1:NROW(nums$threes), function(i)
						matrix(paste(c(nums$threes$X1[i] + ((nums$threes$X2[i] - 1) * NROW(dat)) + (nums$threes$X2[i] - 1),
									   nums$threes$X1[i] + ((nums$threes$X2[i] - 1) * NROW(dat)) + (nums$threes$X2[i] - 1),
									   nums$threes$X1[i] + ((nums$threes$X2[i] - 1) * NROW(dat)) + (nums$threes$X2[i] - 1) + 1,
									   nums$threes$X1[i] + ((nums$threes$X2[i] - 1) * NROW(dat)) + (nums$threes$X2[i] - 1) + (NROW(dat) + 1)),
						 
									 c(nums$threes$X1[i] + ((nums$threes$X2[i] - 1) * NROW(dat)) + (nums$threes$X2[i] - 1) + (NROW(dat) + 1),
									   nums$threes$X1[i] + ((nums$threes$X2[i] - 1) * NROW(dat)) + (nums$threes$X2[i] - 1) + 1,
									   nums$threes$X1[i] + ((nums$threes$X2[i] - 1) * NROW(dat)) + (nums$threes$X2[i] - 1) + (NROW(dat) + 2),
									   nums$threes$X1[i] + ((nums$threes$X2[i] - 1) * NROW(dat)) + (nums$threes$X2[i] - 1) + (NROW(dat) + 2)),
									 sep = ",")))

		arcs.threes = lapply(1:length(arcs.threes), function(i)
							data.frame(apply(X = str_split_fixed(arcs.threes[[i]], ",", 2),
												MARGIN = c(1,2),
												FUN = as.integer)))

		arcs.threes = data.frame(rbindlist(arcs.threes))

		arcs.threes$X3 = sapply(1:NROW(arcs.threes), function(i) 
								which(arcs$node1 == arcs.threes$X1[i] & arcs$node2 == arcs.threes$X2[i]))

		arcs.threes = lapply(1:(NROW(arcs.threes) / 4), function(i)
							data.frame(t(arcs.threes$X3[((4 * (i - 1)) + 1):(((4 * (i - 1)) + 1) + 3)])))

		arcs.threes = data.frame(rbindlist(arcs.threes))
		colnames(arcs.threes) = c("arc1", "arc2", "arc3", "arc4")
	}

	rm(nums)

	arcs.zeros$demand = 0
	arcs.ones$demand = 1
	arcs.twos$demand = 2
	arcs.threes$demand = 3

	demands = data.frame(rbindlist(list(arcs.zeros, arcs.ones, arcs.twos, arcs.threes)))

	rm(arcs.zeros, arcs.ones, arcs.twos, arcs.threes)

	# -------------------------------------------------------------------------------------------------------------------------------------
	# ---- initial rules ------------------------------------------------------------------------------------------------------------------
	# -------------------------------------------------------------------------------------------------------------------------------------

	# ---- fix all arcs around a zero to a value of 0 -------------------------------------------------------------------------------------

	zeros = as.vector(as.matrix(demands[which(demands$demand == 0),1:4]))

	v = sapply(1:length(v), function(i)
							ifelse(i %in% zeros, 0, v[i]))

	f = sapply(1:length(f), function(i)
							ifelse(i %in% zeros, 0, f[i]))

	demands = demands[-which(demands$demand == 0),]
	rownames(demands) = 1:NROW(demands)

	rm(zeros)

	# ---- numbers in a corner ----------------------------------------------------------------------------------------------------------

	# identify the demands in each corner

	corners = data.frame("row" = c(1, 1, NROW(dat), NROW(dat)),
						 "col" = c(1, NROW(dat), 1, NROW(dat)),
						 "demand" = c(dat[1, 1], dat[1, NROW(dat)], dat[NROW(dat), 1], dat[NROW(dat), NROW(dat)]))

	# identify the set of four arcs that are in each corner

	corner.arcs = lapply(1:NROW(corners), function(i)
							matrix(paste(c(corners$row[i] + ((corners$col[i] - 1) * NROW(dat)) + (corners$col[i] - 1),
										   corners$row[i] + ((corners$col[i] - 1) * NROW(dat)) + (corners$col[i] - 1),
										   corners$row[i] + ((corners$col[i] - 1) * NROW(dat)) + (corners$col[i] - 1) + 1,
										   corners$row[i] + ((corners$col[i] - 1) * NROW(dat)) + (corners$col[i] - 1) + (NROW(dat) + 1)),
										   
										 c(corners$row[i] + ((corners$col[i] - 1) * NROW(dat)) + (corners$col[i] - 1) + (NROW(dat) + 1),
										   corners$row[i] + ((corners$col[i] - 1) * NROW(dat)) + (corners$col[i] - 1) + 1,
										   corners$row[i] + ((corners$col[i] - 1) * NROW(dat)) + (corners$col[i] - 1) + (NROW(dat) + 2),
										   corners$row[i] + ((corners$col[i] - 1) * NROW(dat)) + (corners$col[i] - 1) + (NROW(dat) + 2)),
										   
										 sep = ","),
								   ncol = 1))

	corner.arcs = lapply(1:length(corner.arcs), function(i)
						data.frame(apply(X = str_split_fixed(corner.arcs[[i]], ",", 2),
											MARGIN = c(1,2),
											FUN = as.integer)))

	for(i in 1:4)
	{
		arc.id = c()
		
		for(j in 1:4)
		{
			arc.id[j] = which(arcs$node1 == corner.arcs[[i]]$X1[j] & arcs$node2 == corner.arcs[[i]]$X2[j])
		}
		
		corner.arcs[[i]] = data.frame(t(arc.id))
		colnames(corner.arcs[[i]]) = c("arc1", "arc2", "arc3", "arc4")
	}

	rm(i, j, arc.id)

	corner.arcs = data.frame(rbindlist(corner.arcs))
	corners = cbind(corner.arcs, "demand" = corners$demand)

	rm(corner.arcs)

	# fix arcs if there is a number in a corner

		# one in a corner

	obs = which(corners$demand == 1)

	if(length(obs) > 0)
	{
		for(i in 1:length(obs))
		{
			if(obs[i] == 1)
			{
				# fix v and f values for corner 1 bi-directional arcs
				# arc1 & arc2 fixed at 0
				
				zeros = c(corners$arc1[obs[i]], corners$arc2[obs[i]])

				v = sapply(1:length(v), function(j)
										ifelse(j %in% zeros, 0, v[j]))

				f = sapply(1:length(f), function(j)
										ifelse(j %in% zeros, 0, f[j]))
				
			} else if(obs[i] == 2)
			{
				# fix v and f values for corner 2 bi-directional arcs
				# arc1 & arc4 fixed at 0
				
				zeros = c(corners$arc1[obs[i]], corners$arc4[obs[i]])

				v = sapply(1:length(v), function(j)
										ifelse(j %in% zeros, 0, v[j]))

				f = sapply(1:length(f), function(j)
										ifelse(j %in% zeros, 0, f[j]))
				
			} else if(obs[i] == 3)
			{
				# fix v and f values for corner 3 bi-directional arcs
				# arc2 & arc3 fixed at 0
				
				zeros = c(corners$arc2[obs[i]], corners$arc3[obs[i]])

				v = sapply(1:length(v), function(j)
										ifelse(j %in% zeros, 0, v[j]))

				f = sapply(1:length(f), function(j)
										ifelse(j %in% zeros, 0, f[j]))
				
			} else
			{
				# fix v and f values for corner 4 bi-directional arcs
				# arc3 & arc4 fixed at 0
				
				zeros = c(corners$arc3[obs[i]], corners$arc4[obs[i]])

				v = sapply(1:length(v), function(j)
										ifelse(j %in% zeros, 0, v[j]))

				f = sapply(1:length(f), function(j)
										ifelse(j %in% zeros, 0, f[j]))
				
			}
		}
		
		rm(i, zeros)
	}

		# two in a corner

	obs = which(corners$demand == 2)

	if(length(obs) > 0)
	{
		for(i in 1:length(obs))
		{
			if(obs[i] == 1)
			{
				# fix v and f values for corner 1 bi-directional arcs
				# arc1 & arc2 fixed at 1
				# arc3 & arc4 fixed at 0
				
				zeros = c(corners$arc3[obs[i]], corners$arc4[obs[i]])
				ones = c(corners$arc1[obs[i]], corners$arc2[obs[i]])
												
				v = sapply(1:length(v), function(j)
										ifelse(j %in% zeros, 0, v[j]))
				
				v = sapply(1:length(v), function(j)
										ifelse(j %in% ones, 0, v[j]))
										
				f = sapply(1:length(f), function(j)
										ifelse(j %in% zeros, 0, f[j]))
				
				f = sapply(1:length(f), function(j)
										ifelse(j %in% ones, 1, f[j]))
				
			} else if(obs[i] == 2)
			{
				# fix v and f values for corner 2 bi-directional arcs
				# arc1 & arc4 fixed at 1
				# arc2 & arc3 fixed at 0
				
				zeros = c(corners$arc2[obs[i]], corners$arc3[obs[i]])
				ones = c(corners$arc1[obs[i]], corners$arc4[obs[i]])
												
				v = sapply(1:length(v), function(j)
										ifelse(j %in% zeros, 0, v[j]))
				
				v = sapply(1:length(v), function(j)
										ifelse(j %in% ones, 0, v[j]))
										
				f = sapply(1:length(f), function(j)
										ifelse(j %in% zeros, 0, f[j]))
				
				f = sapply(1:length(f), function(j)
										ifelse(j %in% ones, 1, f[j]))
				
			} else if(obs[i] == 3)
			{
				# fix v and f values for corner 3 bi-directional arcs
				# arc2 & arc3 fixed at 1
				# arc1 & arc4 fixed at 0
				
				zeros = c(corners$arc1[obs[i]], corners$arc4[obs[i]])
				ones = c(corners$arc2[obs[i]], corners$arc3[obs[i]])
												
				v = sapply(1:length(v), function(j)
										ifelse(j %in% zeros, 0, v[j]))
				
				v = sapply(1:length(v), function(j)
										ifelse(j %in% ones, 0, v[j]))
										
				f = sapply(1:length(f), function(j)
										ifelse(j %in% zeros, 0, f[j]))
				
				f = sapply(1:length(f), function(j)
										ifelse(j %in% ones, 1, f[j]))
				
			} else
			{
				# fix v and f values for corner 4 bi-directional arcs
				# arc3 & arc4 fixed at 1
				# arc1 & arc2 fixed at 0
				
				zeros = c(corners$arc1[obs[i]], corners$arc2[obs[i]])
				ones = c(corners$arc3[obs[i]], corners$arc4[obs[i]])
												
				v = sapply(1:length(v), function(j)
										ifelse(j %in% zeros, 0, v[j]))
				
				v = sapply(1:length(v), function(j)
										ifelse(j %in% ones, 0, v[j]))
										
				f = sapply(1:length(f), function(j)
										ifelse(j %in% zeros, 0, f[j]))
				
				f = sapply(1:length(f), function(j)
										ifelse(j %in% ones, 1, f[j]))
				
			}
		}
		
		rm(i, zeros, ones)
	}

		# three in a corner

	obs = which(corners$demand == 3)

	if(length(obs) > 0)
	{
		for(i in 1:length(obs))
		{
			if(obs[i] == 1)
			{
				# fix v and f values for corner 1 bi-directional arcs
				# arc1 & arc2 fixed at 1
				
				ones = c(corners$arc1[obs[i]], corners$arc2[obs[i]])

				v = sapply(1:length(v), function(j)
										ifelse(j %in% ones, 0, v[j]))

				f = sapply(1:length(f), function(j)
										ifelse(j %in% ones, 1, f[j]))
				
			} else if(obs[i] == 2)
			{
				# fix v and f values for corner 2 bi-directional arcs
				# arc1 & arc4 fixed at 1
				
				ones = c(corners$arc1[obs[i]], corners$arc4[obs[i]]) 

				v = sapply(1:length(v), function(j)
										ifelse(j %in% ones, 0, v[j]))

				f = sapply(1:length(f), function(j)
										ifelse(j %in% ones, 1, f[j]))
				
			} else if(obs[i] == 3)
			{
				# fix v and f values for corner 3 bi-directional arcs
				# arc2 & arc3 fixed at 1
				
				ones = c(corners$arc2[obs[i]], corners$arc3[obs[i]])

				v = sapply(1:length(v), function(j)
										ifelse(j %in% ones, 0, v[j]))

				f = sapply(1:length(f), function(j)
										ifelse(j %in% ones, 1, f[j]))
				
			} else
			{
				# fix v and f values for corner 4 bi-directional arcs
				# arc3 & arc4 fixed at 1
				
				ones = c(corners$arc3[obs[i]], corners$arc4[obs[i]])

				v = sapply(1:length(v), function(j)
										ifelse(j %in% ones, 0, v[j]))

				f = sapply(1:length(f), function(j)
										ifelse(j %in% ones, 1, f[j]))
				
			}
		}
		
		rm(i, ones)
	}

	rm(corners, obs)
	
	results = list("x" = x, "v" = v, "f" = f, "arcs" = arcs, "demands" = demands)
	return(results)
}

# -------------------------------------------------------------------------------------------------------------------------------------
# ---- tabu search to satisfy demands ------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

tabu = function(x, v, f, arcs, demands, tabu.list = list())
{
	require(ggplot2)
	
	# initialize parameters

	solved = FALSE
	it = 0
	adjust = 1:NROW(demands)
	intabu = FALSE
	it.time = NA

	# initialize dataframe to store parameter information thoughout the tabu search

	DF = data.frame("it" = it, "it.time" = it.time, "adjust" = length(adjust), "tabu" = length(tabu.list), "intabu" = as.numeric(intabu))

	# run tabu search

	while(solved == FALSE)
	{
		# start the iteration clock
		
		time2 = Sys.time()
		
		# decide which demands need to be satisfied
		
		satisfy = demands[adjust,]
		
		# randomly sample which arcs should be used
		
		ones = unlist(lapply(1:NROW(satisfy), function(i) 
						as.vector(as.matrix(satisfy[i,sample(1:4, satisfy$demand[i])]))))
		
		x = sapply(1:length(x), function(i)
								ifelse(i %in% ones, 1, x[i]))
		
		# ensure x meets the requirements in v and f
		
		x = (x * v) + f
		
		# check if x is in the tabu list

		if(length(tabu.list) > 0)
		{
			intabu = TRUE %in% sapply(1:length(tabu.list), function(i) 
										identical(x, tabu.list[[i]]))
		}
		
		# check out which demands have not been satisfied, and therefore need to be adjusted
		
		adjust = which(sapply(1:NROW(demands), function(i) 
					sum(x[as.vector(as.matrix(demands[i,1:4]))]) == demands$demand[i]) == FALSE)
		
		# if there are demands that need to be adjusted then reset the arc values for those demands back to 0, and they will be resampled next iteration
		# if no demands need to be adjusted then see if an intersection exists at any nodes
			# if an intersection exists at node(s) then reset the arcs, of the demand grids which uses those node(s), back to a value of 0, and they will be resampled next iteration
			# if an intersection doesn't exist then v and f will be updated and the tabu search will end
		
		if(length(adjust) > 0)
		{
			if(intabu == FALSE)
			{
				tabu.list[[length(tabu.list) + 1]] = x
			}
			
			zeros = unlist(lapply(adjust, function(i) 
							as.vector(as.matrix(demands[i,1:4]))))
							
			x = sapply(1:length(x), function(i)
							ifelse(i %in% zeros, 0, x[i]))
			
		} else 
		{
			# used.arcs is a vector of all used arcs in demands

			used.arcs = unique(as.vector(as.matrix(demands[,1:4]))[which(x[as.vector(as.matrix(demands[,1:4]))] == 1)])

			# used.nodes is a vector of nodes that make up each arc in used.arcs

			used.nodes = as.vector(t(as.matrix(arcs[used.arcs,2:3])))

			# dups counts how many times a node was seen in used.nodes
			# then dups becomes a vector of nodes which have been used more than two times
				# this means that an intersection exists at these nodes
				# an intersection at a node violates the closed loop requirement

			dups = data.frame("node" = unique(used.nodes),
							  "seen" = sapply(1:length(unique(used.nodes)), function(i)
												length(which(used.nodes == unique(used.nodes)[i]))))

			dups = dups$node[which(dups$seen > 2)]

			# any demand that can use a node in dups must be adjusted becuase that node was used incorrectly

			demand.nodes = lapply(1:NROW(demands), function(i)
									as.vector(as.matrix(arcs[as.vector(as.matrix(demands[i,1:4])),2:3])))

			adjust = which(sapply(1:NROW(demands), function(i)
							any(dups %in% demand.nodes[[i]] == TRUE)) == TRUE)
			
			if(length(adjust) > 0)
			{
				if(intabu == FALSE)
				{
					tabu.list[[length(tabu.list) + 1]] = x
				}
				
				zeros = unlist(lapply(adjust, function(i) 
								as.vector(as.matrix(demands[i,1:4]))))
								
				x = sapply(1:length(x), function(i)
								ifelse(i %in% zeros, 0, x[i]))
			} else
			{
				fix.arcs = unique(as.vector(as.matrix(demands[,1:4])))
				
				v = sapply(1:length(v), function(i)
							ifelse(i %in% fix.arcs, 0, v[i]))

				f = sapply(1:length(f), function(i)
							ifelse(i %in% fix.arcs, x[i], f[i]))
				
				solved = TRUE
			}
		}
		
		time1 = Sys.time()
		it.time = as.numeric(difftime(time1, time2, units = "secs"))
		it = it + 1
		DF = rbind(DF, c(it, it.time, length(adjust), length(tabu.list), as.numeric(intabu)))
	}
	
	tabu.plot.it = ggplot(data = DF, aes(x = it, y = adjust)) +
				geom_point(na.rm = TRUE) +
				geom_line(na.rm = TRUE) +
				geom_hline(yintercept = 0, color = "red") +
				labs(x = "Iteration", y = "Neighborhoods") +
				ggtitle("Slitherlink Tabu Search\nNeighborhoods to Visit") +
				theme_light(base_size = 25)
	
	mytime = c(0, cumsum(DF$it.time[-1]))
	
	tabu.plot.it.time = ggplot(data = DF, aes(x = mytime, y = adjust)) +
				geom_point(na.rm = TRUE) +
				geom_line(na.rm = TRUE) +
				geom_hline(yintercept = 0, color = "red") +
				labs(x = "Time (secs)", y = "Neighborhoods") +
				ggtitle("Slitherlink Tabu Search\nNeighborhoods to Visit Time Series") +
				theme_light(base_size = 25)
		
	results = list("x" = x, "v" = v, "f" = f, "DF" = DF, "tabu.list" = tabu.list, "tabu.plot.it" = tabu.plot.it, "tabu.plot.it.time" = tabu.plot.it.time)
	return(results)

}

# -------------------------------------------------------------------------------------------------------------------------------------
# ---- tours function ------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

# this funtion will compute how many tours a solution has, and the number of node violations
	# it will return a vector of length 2
		# element 1: the number of tours
			# this will take a value of infinity if a solution does not have closed loop tour(s)
		# element 2: the number of node violations
			# this will take a finite value indicating the total number of nodes used incorrectly

# test out the function
	# DF.loops has two closed loops
	# DF.break has one dead end
	# DF.int has one threeway intersection at a node
	
DF.loops = data.frame(rbind(c(1, 12), c(23, 12), c(23, 34), c(45, 34), c(45, 46), c(47, 46), c(48, 47), c(59, 48), c(59, 60), c(49, 60), c(38, 49), c(38, 27), c(27, 16), c(5, 16), c(5, 4), c(4, 3), c(2, 3), c(2, 1), c(121, 110), c(109, 110), c(109, 108), c(97, 108), c(97, 96), c(107, 96), c(118, 107), c(119, 118), c(119, 120), c(121, 120)))
DF.loops = DF.loops[sample(1:NROW(DF.loops), NROW(DF.loops)),]
colnames(DF.loops) = c("node1", "node2")
rownames(DF.loops) = 1:NROW(DF.loops)

DF.break = data.frame(rbind(c(23, 12), c(23, 34), c(45, 34), c(45, 46), c(47, 46), c(48, 47), c(59, 48), c(59, 60), c(49, 60), c(38, 49), c(38, 27), c(27, 16), c(5, 16), c(5, 4), c(4, 3), c(2, 3), c(2, 1)))
colnames(DF.break) = c("node1", "node2")

DF.int = data.frame(rbind(c(23, 34), c(100, 34), c(45, 34), c(45, 46), c(47, 46), c(48, 47), c(59, 48), c(59, 60), c(49, 60), c(38, 49), c(38, 27), c(27, 16), c(5, 16), c(5, 4), c(4, 3), c(2, 3), c(2, 1)))
colnames(DF.break) = c("node1", "node2")

tours = function(DF)
{	
	# compute how many times each unique node appears in the data frame
	
	used.nodes = as.vector(as.matrix(DF))
	
	used.nodes = data.frame("node" = unique(used.nodes),
							"seen" = sapply(1:length(unique(used.nodes)), function(i)
											length(which(used.nodes == unique(used.nodes)[i]))))
	
	# node.violations is the number of nodes that weren't used only twice
	
	node.violations = length(which(used.nodes$seen != 2))
	
	# if any of the nodes don't appear twice then this data frame is not made up of just closed loop tours
		# otherwise, the data frame is made up of only closed loop tours and we can find them
		
	if(any(used.nodes$seen != 2))
	{
		results = list("value" = c(Inf, node.violations))

	} else
	{
		# setup the data frame and save it
	
		colnames(DF) = c("node1", "node2")
		DF$index = 1:NROW(DF)
		DF.save = DF
		
		# create a list for storing tours
		
		tour.list = list()
		
		# initialize while loop parameter
		
		done = FALSE
		
		# search through the data frame until all closed loop tours have been found
		
		while(done == FALSE)
		{
			# start off the tour with the first arc in the data frame
				# id is the identifier of the first arc
				# node.start is the first node of the first arc
				# node.search is the second node of the first arc
				
			id = DF$index[1]
			node.start = DF$node1[1]
			node.search = DF$node2[1]

			# create a vector, tour, to store all the index values of arcs which make up a tour
				
			tour = vector()
			tour[1] = id
			
			# initialize while loop parameter
			
			loop = FALSE
			
			while(loop == FALSE)
			{			
				# determine which observation in column node1 matches with the value of node.search
	
				obs = DF$index[which(DF$node1 == node.search & DF$index != id)]
				
				# if no match was found
					# then determine which observation in column node2 matches with the value of node.search
				
				if(length(obs) == 0)
				{
					obs = DF$index[which(DF$node2 == node.search & DF$index != id)]
					
					# update tour, node.search, and id
					
					id = obs
					node.search = DF$node1[which(DF$index == id)]
					tour[length(tour) + 1] = id
					
				} else
				{
					# update tour, node.search, and id
					
					id = obs
					node.search = DF$node2[which(DF$index == id)]
					tour[length(tour) + 1] = id
				}
				
				# if node.search equals node.start then we found a closed loop tour
					# add the tour to tour.list
					# remove these observations from DF

				if(node.search == node.start)
				{
					tour.list[[length(tour.list) + 1]] = tour
					DF = DF[-which(DF$index %in% tour),]
					loop = TRUE
				}
				
				# if there are no more observations left in DF then the tours function will end
				
				if(NROW(DF) == 0)
				{
					value = c(length(tour.list), node.violations)
					done = TRUE
				}				
			}
		}
		
		results = list("value" = value, "DF" = DF.save, "tour.list" = tour.list)
		
	}
	
	return(results)
	
}

tours(DF.loops)$value

tours(DF.break)$value

tours(DF.int)$value

rm(DF.loops, DF.break, DF.int)

# -------------------------------------------------------------------------------------------------------------------------------------
# ---- solve for one closed loop with a genetic algorithm ------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

# ---- initialize ------------------------------------------------------------------------------------------

# lets create the x, v, and f vectors for each slither puzzle

slither1.used = data.frame(rbind(c(3, 14), c(4, 15), c(4, 5), c(7, 18), c(8, 19), c(10, 21), c(10, 11), c(11, 22), c(12, 13), c(13, 24), c(15, 26), c(16, 27), c(19, 30), c(20, 31), c(21, 32), c(22, 33), c(23, 24), c(25, 26), c(27, 28), c(28, 29), c(33, 44), c(35, 36), c(35, 46), c(38, 39), c(39, 40), c(41, 42), c(43, 44), c(45, 56), c(46, 57), c(47, 58), c(47, 48), c(49, 50), c(50, 61), c(51, 62), c(51, 52), c(54, 55), c(55, 66), c(58, 69), c(59, 60), c(59, 70), c(60, 71), c(64, 65), c(65, 76), c(69, 70), c(71, 72), c(72, 83), c(73, 74), c(73, 84), c(75, 86), c(75, 76), c(79, 80), c(80, 91), c(81, 92), c(82, 83), c(84, 85), c(85, 96), c(86, 87), c(87, 88), c(89, 90), c(90, 101), c(91, 102), c(92, 93), c(95, 96), c(95, 106), c(97, 108), c(98, 99), c(100, 111), c(100, 101), c(102, 103), c(103, 104), c(106, 107), c(107, 108), c(109, 120), c(111, 112), c(119, 120)))
slither1.not.used = data.frame(rbind(c(3, 4), c(5, 16), c(7, 8), c(12, 23), c(14, 15), c(15, 16), c(18, 19), c(19, 20), c(21, 22), c(24, 25), c(24, 35), c(25, 36), c(26, 27), c(26, 37), c(27, 38), c(28, 39), c(29, 40), c(30, 31), c(30, 41), c(31, 42), c(32, 43), c(32, 33), c(36, 47), c(37, 38), c(39, 50), c(40, 51), c(43, 54), c(44, 55), c(45, 46), c(46, 47), c(48, 59), c(49, 60), c(50, 51), c(52, 63), c(54, 65), c(56, 57), c(58, 59), c(60, 61), c(62, 63), c(64, 75), c(65, 66), c(68, 69), c(68, 79), c(69, 80), c(70, 71), c(71, 82), c(74, 85), c(76, 87), c(79, 90), c(80, 81), c(84, 95), c(87, 98), c(88, 99), c(89, 100), c(90, 91), c(91, 92), c(92, 103), c(93, 104), c(96, 107), c(96, 97), c(101, 112), c(101, 102), c(108, 119), c(108, 109)))
slither2.used = data.frame(rbind(c(1, 12), c(1, 2), c(2, 13), c(3, 14), c(3, 4), c(4, 5), c(6, 17), c(7, 18), c(8, 9), c(10, 11), c(11, 22), c(17, 18), c(20, 31), c(21, 22), c(23, 24), c(24, 35), c(26, 27), c(27, 38), c(28, 39), c(30, 31), c(30, 41), c(32, 43), c(32, 33), c(33, 44), c(34, 35), c(36, 37), c(37, 48), c(38, 39), c(40, 51), c(41, 42), c(42, 43), c(45, 46), c(46, 57), c(47, 48), c(47, 58), c(49, 60), c(49, 50), c(53, 54), c(56, 67), c(56, 57), c(58, 59), c(59, 70), c(62, 73), c(63, 74), c(64, 75), c(64, 65), c(65, 66), c(66, 77), c(67, 68), c(68, 79), c(70, 81), c(73, 84), c(74, 75), c(77, 88), c(78, 89), c(78, 79), c(80, 81), c(82, 93), c(82, 83), c(83, 84), c(85, 86), c(86, 97), c(88, 99), c(89, 90), c(90, 101), c(93, 94), c(94, 95), c(95, 106), c(97, 98), c(101, 112), c(103, 114), c(104, 115), c(104, 105), c(105, 106), c(107, 108), c(108, 119), c(109, 110), c(112, 113), c(115, 116), c(118, 119)))
slither2.not.used = data.frame(rbind(c(4, 15), c(5, 16), c(6, 7), c(8, 19), c(9, 20), c(10, 21), c(12, 13), c(14, 15), c(15, 26), c(15, 16), c(16, 27), c(18, 29), c(18, 19), c(19, 20), c(19, 30), c(23, 34), c(27, 28), c(29, 30), c(31, 32), c(31, 42), c(40, 41), c(41, 52), c(43, 44), c(45, 56), c(48, 59), c(50, 61), c(51, 52), c(53, 64), c(54, 65), c(57, 68), c(57, 68), c(58, 69), c(60, 61), c(62, 63), c(63, 64), c(65, 76), c(69, 70), c(69, 80), c(69, 70), c(70, 71), c(71, 82), c(72, 83), c(72, 73), c(73, 74), c(74, 85), c(75, 86), c(76, 77), c(76, 87), c(79, 90), c(81, 92), c(81, 82), c(83, 94), c(84, 95), c(86, 87), c(87, 98), c(87, 88), c(90, 91), c(91, 102), c(92, 93), c(94, 105), c(98, 99), c(101, 102), c(102, 113), c(103, 104), c(105, 116), c(107, 118), c(108, 109), c(109, 120), c(110, 121), c(114, 115), c(119, 120), c(120, 121)))

slither1.used = sapply(1:NROW(slither1.used), function(i) 
						which(phase1$arcs$node1 == slither1.used$X1[i] & phase1$arcs$node2 == slither1.used$X2[i]))

slither1.not.used = sapply(1:NROW(slither1.not.used), function(i) 
							which(phase1$arcs$node1 == slither1.not.used$X1[i] & phase1$arcs$node2 == slither1.not.used$X2[i]))
							
slither2.used = sapply(1:NROW(slither2.used), function(i) 
						which(phase1$arcs$node1 == slither2.used$X1[i] & phase1$arcs$node2 == slither2.used$X2[i]))
						
slither2.not.used = sapply(1:NROW(slither2.not.used), function(i) 
							which(phase1$arcs$node1 == slither2.not.used$X1[i] & phase1$arcs$node2 == slither2.not.used$X2[i]))

slither1.x = rep(0, 220)
slither1.v = rep(1, 220)
slither1.f = slither1.x
slither2.x = slither1.x
slither2.v = slither1.v
slither2.f = slither1.x

slither1.x = sapply(1:length(slither1.x), function(i)
					ifelse(i %in% slither1.used, 1, slither1.x[i])) 

slither1.v = sapply(1:length(slither1.v), function(i)
					ifelse(i %in% c(slither1.used, slither1.not.used), 0, slither1.v[i])) 

slither1.f = sapply(1:length(slither1.f), function(i)
					ifelse(i %in% slither1.used, 1, slither1.f[i])) 
							
slither2.x = sapply(1:length(slither2.x), function(i)
					ifelse(i %in% slither2.used, 1, slither2.x[i])) 

slither2.v = sapply(1:length(slither2.v), function(i)
					ifelse(i %in% c(slither2.used, slither2.not.used), 0, slither2.v[i])) 

slither2.f = sapply(1:length(slither2.f), function(i)
					ifelse(i %in% slither2.used, 1, slither2.f[i])) 

rm(slither1.used, slither2.used, slither1.not.used, slither2.not.used)

# create a function for creating an initial population with a node.violation reduction condition

ga.pop = function(N, x, v, arcs, reduce = 0.10)
{
	i = 1
	done = FALSE
	
	used.nodes = as.vector(as.matrix(arcs[which(x == 1),2:3]))
	
	used.nodes = data.frame("node" = unique(used.nodes),
							"seen" = sapply(1:length(unique(used.nodes)), function(i)
											length(which(used.nodes == unique(used.nodes)[i]))))
	
	node.violations = length(which(used.nodes$seen != 2))
	
	max.violations = floor((1 - reduce) * node.violations)
	
	while(done == FALSE)
	{
		pop = lapply(1:N, function(j)
			  sapply(1:length(x), function(i)
						ifelse(v[i] == 1, sample(0:1, 1), x[i])))
					
		used.nodes = lapply(1:N, function(i)
							as.vector(as.matrix(arcs[which(pop[[i]] == 1),2:3])))
		
		used.nodes = lapply(1:N, function(j) 
							data.frame("node" = unique(used.nodes[[j]]),
										"seen" = sapply(1:length(unique(used.nodes[[j]])), function(i)
														length(which(used.nodes[[j]] == unique(used.nodes[[j]])[i])))))
		
		node.violations = sapply(1:N, function(i)
									length(which(used.nodes[[i]]$seen != 2)))
		
		if(all(node.violations <= max.violations))
		{
			done = TRUE
			
			if(i == 1)
			{
				results = pop
			} else
			{
				results = append(results, pop)
			}
			
		} else
		{
			if(length(which(node.violations <= max.violations)) == 0)
			{
				pop = list()
			} else
			{
				pop = pop[which(node.violations <= max.violations)]
				N = N - length(pop)
			}
			
			if(i == 1)
			{
				results = pop
			} else
			{
				results = append(results, pop)
			}
		}
		
		i = i + 1
	}
	
	return(results)
}

time2 = Sys.time()
pop = ga.pop(N = 2000, x = slither1.x, v = slither1.v, arcs = phase1$arcs, reduce = 0.15)
time1 = Sys.time()
as.numeric(difftime(time1, time2, units = "secs"))

rm(time1, time2)

# check if any of the solutions are identical

comb = expand.grid("x1" = 1:length(pop), "x2" = 1:length(pop))
comb$same = comb$x1 == comb$x2
comb = comb[which(comb$same == FALSE),1:2]
comb = data.frame(t(apply(comb, 1, sort)))
comb = comb[!duplicated(comb), ]
colnames(comb) = c("run.i", "run.j")
rownames(comb) = 1:NROW(comb)
comb$same = sapply(1:NROW(comb), function(i) identical(pop[[comb$run.i[i]]], pop[[comb$run.j[i]]]))
length(which(comb$same == TRUE))

rm(comb)

# ---- the ga function --------------------------------------------------------------------------------------------------------------------------------------

# time.limit is the maximum time in seconds that a run of ga iterations is allowed to perform before it terminates
# pop is the population of initial solutions
# F is the number of solutions that must compete with eachother per competition
# PC is the proportion of the population that will go through cross section
# C is the number of cuts that will be performed in each cross section event
# PM is the proportion of the population that will go through mutation
# M is the number of mutations that will be made per mutation event

ga = function(pop, x, v, f, arcs, time.limit = 600, F = 2, PC = 0.5, C = 1, PM = 0.05, M = 3, wd = getwd())
{
	setwd(wd)
	
	# it is counter for ga iterations
	# done is the while loop parameter for the ga
	# x.best will be the best solution found by the ga at each iteration
	# runs is a data.frame of the parameters for each run
	# run.id is the current run that is being used
	# pop.save is a saved copy of the original pop
	
	it = 0
	done = FALSE
	x.best = vector()
	run.id = 1
	pop.save = pop
	
	runs = expand.grid("F" = F, "PC" = PC, "C" = C, "PM" = PM, "M" = M)
	F = runs$F[run.id]
	PC = runs$PC[run.id]
	C = runs$C[run.id]
	PM = runs$PM[run.id]
	M = runs$M[run.id]
	N = length(pop)
	
	# initialize dataframe to store parameter information thoughout the ga
	
	DF = data.frame(rbind(c("run.id","F","PC","C","PM","M","it","it.time","tours.used","node.violations")))
  
	write.table(DF, file = "DF.txt",
				append = TRUE,
				col.names = FALSE,
				row.names = FALSE,
				sep = ',')
	
	# run the ga 

	time.start = Sys.time()
	
	while(done == FALSE)
	{
		# start the iteration clock
		
		time2 = Sys.time()
		
		# ---- competition phase ------------------------------------------------------------------------------------------

		# comp is the competition details, where vector element i for every vector will compete against eachother 
			# the numbers in the vectors of comp correspond to the list element id in pop

		comp = lapply(1:F, function(i) sample(1:length(pop), length(pop)))

		# lets convert comp to a data fame

		comp = data.frame(t(sapply(1:length(pop), function(i)
							sapply(1:F, function(j)
									comp[[j]][i]))))

		# lets compute the tours value of each solution for every competition
		# comp.results is a list with N elements to represent the N competitions
			# each list element is a data frame with F rows to represent each competitor in a competition

		comp.results = lapply(1:length(pop), function(i)
								data.frame(t(sapply(1:F, function(j) 
										c(comp[i,j], tours(arcs[which(pop[[comp[i,j]]] == 1),2:3])$value)))))

		# lets extract the winners of each competition

		comp.winners = data.frame(rbindlist(lapply(1:length(pop), function(i)
										comp.results[[i]][ifelse(all(comp.results[[i]]$X2 == Inf), which.min(comp.results[[i]]$X3), which.min(comp.results[[i]]$X2)),])))

		colnames(comp.winners) = c("winner", "tours.used", "node.violations")

		# if any of the winners used just one tour then this is optimal and the ga is done

		if(any(comp.winners$tours.used == 1))
		{
			x.best = pop[[comp.winners$winner[which.min(comp.winners$tours.used)]]]
			node.violations = 0
			tours.used = 1
			done = TRUE
		}
		
		# if any of the winners used tours then this is promising so store the results
		
		if(any(comp.winners$tours.used < Inf))
		{
			x.best = pop[[comp.winners$winner[which.min(comp.winners$tours.used)]]]
			node.violations = 0
			tours.used = comp.winners$tours.used[which.min(comp.winners$tours.used)]
		}
		
		# update pop with the winners

		pop = pop[comp.winners$winner]

		# ---- cross section phase ------------------------------------------------------------------------------------------

		if(done == FALSE)
		{
			# cross.pop is the solutions of pop that were randomly chosen to go through cross section
			# rest.pop is the rest of the solutions in pop that won't go through cross section

			cross.pop = sample(1:length(pop), floor(length(pop) * PC))

			# make sure cross.pop consists of an even number of solutions to create pairs

			if(length(cross.pop) %% 2 != 0)
			{
				cross.pop = cross.pop[-1]
			}

			rest.pop = c(1:length(pop))[-cross.pop]

			# cross is pairing the first half of cross.pop with the second half of cross.pop

			cross = data.frame("sol1" = cross.pop[1:(length(cross.pop) / 2)],
								"sol2" = cross.pop[((length(cross.pop) / 2) + 1):length(cross.pop)])

			# cuts is a data frame where each column represents a cut, and each row corresponds to the cross section pair in cross
				# the values in each column represent the position in a solution where the cut will be made
					# ie. there are 220 elements in a solution so a cut can occur anywhere between, but not including, 1 and 220
				# the values in cuts were generated randomly

			cuts = data.frame(t(sapply(1:NROW(cross), function(j)
								sapply(1:C, function(i) sample(73:147, 1)))))
								
			if(C == 1)
			{
				cuts = data.frame(t(cuts))
				rownames(cuts) = 1:NROW(cuts)
				colnames(cuts) = "X1"
			}

			# cross.sols is using the solution ids from cross to extract the actual solution vectors from pop
				# cross.sols is a list and each element of the list is a dataframe with two columns
				# the two columns represent the pair of solutions that will be cross sectioned

			cross.sols = lapply(1:NROW(cross), function(i)
								data.frame(cbind(pop[[cross$sol1[i]]], pop[[cross$sol2[i]]])))

			# the following while loop performs the cross sectioning
				# if the cut number (ie. cut 1, cut 2, ... cut C) is an odd number then the first sections of the pair are swapped
				# otherwise, when the cut number is even, the last sections of the pair are swapped

			j = 1

			while(C >= j)
			{
				cross.sols = lapply(1:length(cross.sols), function(i)
									if(j %% 2 != 0)
									{
										data.frame("X1" = c(cross.sols[[i]]$X2[1:cuts[i,j]], 
															cross.sols[[i]]$X1[(cuts[i,j] + 1):length(x)]),

													"X2" = c(cross.sols[[i]]$X1[1:cuts[i,j]], 
															cross.sols[[i]]$X2[(cuts[i,j] + 1):length(x)]))
															
									} else
									{
										data.frame("X1" = c(cross.sols[[i]]$X1[1:cuts[i,j]], 
															cross.sols[[i]]$X2[(cuts[i,j] + 1):length(x)]),

													"X2" = c(cross.sols[[i]]$X2[1:cuts[i,j]], 
															cross.sols[[i]]$X1[(cuts[i,j] + 1):length(x)]))

									})

				j = j + 1
			}

			rm(j)

			# update pop with the cross sectioned solutions

			cross.sols = unlist(lapply(1:length(cross.sols), function(j) 
										lapply(1:2, function(i) 
												as.vector(cross.sols[[j]][,i]))), 
								recursive = FALSE)
								
			pop = append(pop[rest.pop], cross.sols)
		}

		# ---- mutation phase ------------------------------------------------------------------------------------------

		if(done == FALSE)
		{
			# mutate.pop is the solutions of pop that were randomly chosen to go through cross section
			# rest.pop is the rest of the solutions in pop that won't go through cross section

			mutate.pop = sample(1:length(pop), floor(length(pop) * PM))
			rest.pop = c(1:length(pop))[-mutate.pop]

			# mutate.sols is using the solution ids from mutate.pop to extract the actual solution vectors from pop

			mutate.sols = lapply(1:length(mutate.pop), function(i)
								pop[[mutate.pop[i]]])

			# mutate is a list of vectors that indicates the elements of each solution in mutate.sols that will be mutated

			mutate = lapply(1:length(mutate.sols), function(i)
								sample(which(v == 1), M))

			# the following for loop iterates through each solution in mutate.sols and switches a value of 1 to 0, or a value of 0 to 1, for each element idicated in mutate

			for(i in 1:length(mutate.sols))
			{
				mutate.sols[[i]][mutate[[i]]] = abs(mutate.sols[[i]][mutate[[i]]] - 1)
			}

			# update pop with the mutated solutions

			pop = append(pop[rest.pop], mutate.sols)

			# make sure the current solutions in pop respect the restrictions in v and f

			pop = lapply(1:length(pop), function(i)
							(pop[[i]] * v) + f)
		}

		# ---- evaluate end conditions and store iteration results ------------------------------------------------------------------------------------------

		it = it + 1
		time1 = Sys.time()
		it.time = as.numeric(difftime(time1, time2, units = "secs"))
		
		if(length(x.best) == 0)
		{
			vals = data.frame(rbindlist(lapply(1:length(pop), function(i)
								data.frame(t(tours(arcs[which(pop[[i]] == 1),2:3])$value)))))

			if(vals$X1[which.min(vals$X1)] < Inf)
			{
				x.best = pop[[which.min(vals$X1)]]
				DF = data.frame(rbind(c(run.id, F, PC, C, PM, M, it, it.time, vals$X1[which.min(vals$X1)], 0)))
				
			} else
			{
				x.best = pop[[which.min(vals$X2)]]
				DF = data.frame(rbind(c(run.id, F, PC, C, PM, M, it, it.time, NA, vals$X2[which.min(vals$X2)])))
				
			}
		} else
		{
			DF = data.frame(rbind(c(run.id, F, PC, C, PM, M, it, it.time, tours.used, node.violations)))
		}
		
		write.table(DF, file = "DF.txt",
					append = TRUE,
					col.names = FALSE,
					row.names = FALSE,
					sep = ',')
		
		x.best = data.frame(rbind(x.best))
		
		write.table(x.best, file = "x_history.txt",
					append = TRUE,
					col.names = FALSE,
					row.names = FALSE,
					sep = ',')

		x.best = vector()
		
		time.now = Sys.time()
		run.time = as.numeric(difftime(time.now, time.start, units = "secs"))
		
		if(run.time >= time.limit)
		{
			run.id = run.id + 1
			
			if(run.id > NROW(runs))
			{
				done = TRUE
			} else
			{
				pop = pop.save
				F = runs$F[run.id]
				PC = runs$PC[run.id]
				C = runs$C[run.id]
				PM = runs$PM[run.id]
				M = runs$M[run.id]
				it = 1
				time.start = Sys.time()
			}
		}
	}
}

# test run of 2 minutes

timeB = Sys.time()

ga(pop = pop, 
	x = slither1.x, 
	v = slither1.v, 
	f = slither1.f, 
	arcs = phase1$arcs, 
	time.limit = 10, 
	F = c(2, 5), 
	PC = c(1/3, 2/3), 
	C = c(1, 3), 
	PM = 0.05, 
	M = 3,
	wd = "\\\\labsvcsredirect/users$/njm2868/Desktop")
	
timeA = Sys.time()
as.numeric(difftime(timeA, timeB, units = "secs"))

rm(timeA, timeB)

# -------------------------------------------------------------------------------------------------------------------------------------
# ---- Solve Slitherlink Puzzle 1 ------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

# ---- phase 1 --------------------------------------------------------------------------------------------------------------------------

slither1 = cbind(c(NA, NA, 2, 2, NA, NA, 2, NA, NA, 3), 
				 c(3, NA, NA, 2, NA, NA, NA, 2, NA, 2), 
				 c(NA, 1, 2, 0, 2, 2, NA, 1, NA, 2), 
				 c(NA, 2, NA, NA, NA, 1, NA, NA, NA, 2), 
				 c(2, NA, 2, NA, 2, NA, 2, NA, NA, 2), 
				 c(NA, NA, 3, 3, NA, NA, NA, NA, 3, NA), 
				 c(NA, 1, NA, NA, 3, NA, 3, NA, 3, NA), 
				 c(NA, 2, 2, NA, NA, NA, 3, NA, NA, 2), 
				 c(3, 2, 2, 2, NA, NA, 3, 2, NA, NA), 
				 c(3, NA, NA, NA, NA, NA, NA, NA, 2, NA))

dat = as.matrix(slither1)

rm(slither1)

phase1 = initialize(dat)

# ---- phase 2 --------------------------------------------------------------------------------------------------------------------------

slither1.used = data.frame(rbind(c(3, 14), c(4, 15), c(4, 5), c(7, 18), c(8, 19), c(10, 21), c(10, 11), c(11, 22), c(12, 13), c(13, 24), c(15, 26), c(16, 27), c(19, 30), c(20, 31), c(21, 32), c(22, 33), c(23, 24), c(25, 26), c(27, 28), c(28, 29), c(33, 44), c(35, 36), c(35, 46), c(38, 39), c(39, 40), c(41, 42), c(43, 44), c(45, 56), c(46, 57), c(47, 58), c(47, 48), c(49, 50), c(50, 61), c(51, 62), c(51, 52), c(54, 55), c(55, 66), c(58, 69), c(59, 60), c(59, 70), c(60, 71), c(64, 65), c(65, 76), c(69, 70), c(71, 72), c(72, 83), c(73, 74), c(73, 84), c(75, 86), c(75, 76), c(79, 80), c(80, 91), c(81, 92), c(82, 83), c(84, 85), c(85, 96), c(86, 87), c(87, 88), c(89, 90), c(90, 101), c(91, 102), c(92, 93), c(95, 96), c(95, 106), c(97, 108), c(98, 99), c(100, 111), c(100, 101), c(102, 103), c(103, 104), c(106, 107), c(107, 108), c(109, 120), c(111, 112), c(119, 120)))
slither1.not.used = data.frame(rbind(c(3, 4), c(5, 16), c(7, 8), c(12, 23), c(14, 15), c(15, 16), c(18, 19), c(19, 20), c(21, 22), c(24, 25), c(24, 35), c(25, 36), c(26, 27), c(26, 37), c(27, 38), c(28, 39), c(29, 40), c(30, 31), c(30, 41), c(31, 42), c(32, 43), c(32, 33), c(36, 47), c(37, 38), c(39, 50), c(40, 51), c(43, 54), c(44, 55), c(45, 46), c(46, 47), c(48, 59), c(49, 60), c(50, 51), c(52, 63), c(54, 65), c(56, 57), c(58, 59), c(60, 61), c(62, 63), c(64, 75), c(65, 66), c(68, 69), c(68, 79), c(69, 80), c(70, 71), c(71, 82), c(74, 85), c(76, 87), c(79, 90), c(80, 81), c(84, 95), c(87, 98), c(88, 99), c(89, 100), c(90, 91), c(91, 92), c(92, 103), c(93, 104), c(96, 107), c(96, 97), c(101, 112), c(101, 102), c(108, 119), c(108, 109)))

slither1.used = sapply(1:NROW(slither1.used), function(i) 
						which(phase1$arcs$node1 == slither1.used$X1[i] & phase1$arcs$node2 == slither1.used$X2[i]))

slither1.not.used = sapply(1:NROW(slither1.not.used), function(i) 
							which(phase1$arcs$node1 == slither1.not.used$X1[i] & phase1$arcs$node2 == slither1.not.used$X2[i]))
							
slither1.x = rep(0, 220)
slither1.v = rep(1, 220)
slither1.f = slither1.x

slither1.x = sapply(1:length(slither1.x), function(i)
					ifelse(i %in% slither1.used, 1, slither1.x[i])) 

slither1.v = sapply(1:length(slither1.v), function(i)
					ifelse(i %in% c(slither1.used, slither1.not.used), 0, slither1.v[i])) 

slither1.f = sapply(1:length(slither1.f), function(i)
					ifelse(i %in% slither1.used, 1, slither1.f[i]))

rm(slither1.used, slither1.not.used)

pop = ga.pop(N = 2000, x = slither1.x, v = slither1.v, arcs = phase1$arcs, reduce = 0.15)

# ---- phase 3 --------------------------------------------------------------------------------------------------------------------------

timeB = Sys.time()

ga(pop = pop, 
	x = slither1.x, 
	v = slither1.v, 
	f = slither1.f, 
	arcs = phase1$arcs, 
	time.limit = 5400, 
	F = c(2, 5), 
	PC = c(1/3, 2/3), 
	C = c(1, 3), 
	PM = 0.05, 
	M = 3,
	wd = "\\\\labsvcsredirect/users$/njm2868/Desktop")
	
timeA = Sys.time()
as.numeric(difftime(timeA, timeB, units = "secs"))

rm(timeA, timeB)

# -------------------------------------------------------------------------------------------------------------------------------------
# ---- Solve Slitherlink Puzzle 2 ------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------

# ---- phase 1 --------------------------------------------------------------------------------------------------------------------------

slither2 = cbind(c(3, NA, 2, 1, NA, 3, NA, 1, NA, 3), 
				 c(NA, NA, NA, 1, NA, NA, 0, 2, NA, NA), 
				 c(3, NA, NA, NA, 3, NA, NA, 3, 2, 3), 
				 c(NA, NA, 3, NA, NA, NA, 1, NA, NA, NA), 
				 c(3, NA, 3, NA, 2, NA, NA, NA, 2, NA), 
				 c(3, NA, 2, NA, NA, NA, 2, 3, NA, 2), 
				 c(NA, NA, 2, 1, NA, 2, NA, 2, NA, 1), 
				 c(3, NA, NA, 1, 3, 2, NA, NA, 2, 1), 
				 c(NA, 1, NA, NA, NA, 3, NA, NA, NA, NA), 
				 c(NA, 2, NA, 2, 3, NA, NA, 3, 1, 1))

dat = as.matrix(slither2)

rm(slither2)

phase1 = initialize(dat)

# ---- phase 2 --------------------------------------------------------------------------------------------------------------------------

slither2.used = data.frame(rbind(c(1, 12), c(1, 2), c(2, 13), c(3, 14), c(3, 4), c(4, 5), c(6, 17), c(7, 18), c(8, 9), c(10, 11), c(11, 22), c(17, 18), c(20, 31), c(21, 22), c(23, 24), c(24, 35), c(26, 27), c(27, 38), c(28, 39), c(30, 31), c(30, 41), c(32, 43), c(32, 33), c(33, 44), c(34, 35), c(36, 37), c(37, 48), c(38, 39), c(40, 51), c(41, 42), c(42, 43), c(45, 46), c(46, 57), c(47, 48), c(47, 58), c(49, 60), c(49, 50), c(53, 54), c(56, 67), c(56, 57), c(58, 59), c(59, 70), c(62, 73), c(63, 74), c(64, 75), c(64, 65), c(65, 66), c(66, 77), c(67, 68), c(68, 79), c(70, 81), c(73, 84), c(74, 75), c(77, 88), c(78, 89), c(78, 79), c(80, 81), c(82, 93), c(82, 83), c(83, 84), c(85, 86), c(86, 97), c(88, 99), c(89, 90), c(90, 101), c(93, 94), c(94, 95), c(95, 106), c(97, 98), c(101, 112), c(103, 114), c(104, 115), c(104, 105), c(105, 106), c(107, 108), c(108, 119), c(109, 110), c(112, 113), c(115, 116), c(118, 119)))
slither2.not.used = data.frame(rbind(c(4, 15), c(5, 16), c(6, 7), c(8, 19), c(9, 20), c(10, 21), c(12, 13), c(14, 15), c(15, 26), c(15, 16), c(16, 27), c(18, 29), c(18, 19), c(19, 20), c(19, 30), c(23, 34), c(27, 28), c(29, 30), c(31, 32), c(31, 42), c(40, 41), c(41, 52), c(43, 44), c(45, 56), c(48, 59), c(50, 61), c(51, 52), c(53, 64), c(54, 65), c(57, 68), c(57, 68), c(58, 69), c(60, 61), c(62, 63), c(63, 64), c(65, 76), c(69, 70), c(69, 80), c(69, 70), c(70, 71), c(71, 82), c(72, 83), c(72, 73), c(73, 74), c(74, 85), c(75, 86), c(76, 77), c(76, 87), c(79, 90), c(81, 92), c(81, 82), c(83, 94), c(84, 95), c(86, 87), c(87, 98), c(87, 88), c(90, 91), c(91, 102), c(92, 93), c(94, 105), c(98, 99), c(101, 102), c(102, 113), c(103, 104), c(105, 116), c(107, 118), c(108, 109), c(109, 120), c(110, 121), c(114, 115), c(119, 120), c(120, 121)))

slither2.used = sapply(1:NROW(slither2.used), function(i) 
						which(phase1$arcs$node1 == slither2.used$X1[i] & phase1$arcs$node2 == slither2.used$X2[i]))
						
slither2.not.used = sapply(1:NROW(slither2.not.used), function(i) 
							which(phase1$arcs$node1 == slither2.not.used$X1[i] & phase1$arcs$node2 == slither2.not.used$X2[i]))

slither2.x = rep(0, 220)
slither2.v = rep(1, 220)
slither2.f = slither2.x

slither2.x = sapply(1:length(slither2.x), function(i)
					ifelse(i %in% slither2.used, 1, slither2.x[i])) 

slither2.v = sapply(1:length(slither2.v), function(i)
					ifelse(i %in% c(slither2.used, slither2.not.used), 0, slither2.v[i])) 

slither2.f = sapply(1:length(slither2.f), function(i)
					ifelse(i %in% slither2.used, 1, slither2.f[i])) 

rm(slither2.used, slither2.not.used)

pop = ga.pop(N = 2000, x = slither2.x, v = slither2.v, arcs = phase1$arcs, reduce = 0.15)

# ---- phase 3 --------------------------------------------------------------------------------------------------------------------------

timeB = Sys.time()

ga(pop = pop, 
	x = slither2.x, 
	v = slither2.v, 
	f = slither2.f, 
	arcs = phase1$arcs, 
	time.limit = 5400, 
	F = c(2, 5), 
	PC = c(1/3, 2/3), 
	C = c(1, 3), 
	PM = 0.05, 
	M = 3,
	wd = "\\\\labsvcsredirect/users$/njm2868/Desktop")
	
timeA = Sys.time()
as.numeric(difftime(timeA, timeB, units = "secs"))

rm(timeA, timeB)



# -------------------------------------------------------------------------------------------------------------------------------------
# ----  ------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------









