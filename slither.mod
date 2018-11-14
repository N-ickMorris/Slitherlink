param n := 11;		# number of nodes per row/column
param s := 11;		# number of subtours for slitherlink 1
# param s := 3;		# number of subtours for slitherlink 2

set N := 1..(n * n);		# nodes
set A within N cross N;		# arcs
set G within N cross N cross N cross N;		# grids
set K within N cross N;		# known arcs
set SF := 1..s;		# subtour family
set S{SF} within N cross N;		# subtour members

param d{(i, j, k, l) in G};		# demand of a grid
param b{i in N} default 0;		# net flow of at a node

var x{(i, j) in A} binary;		# do or don't use an arc

minimize Arcs: sum{(i, j) in A}(x[i, j]);		# minimize arcs used

s.t. Demand{(i, j, k, l) in G}: (x[i, j] + x[j, i]) + (x[l, k] + x[k, l]) + (x[i, l] + x[l, i]) + (x[j, k] + x[k, j]) = d[i, j, k, l];		# satisfy the demand of all grids

s.t. NetFlow{j in N}: sum{(i, j) in A}(x[i, j]) - sum{(j, k) in A}(x[j, k]) = b[j];		# satisfy the netflow at all nodes

s.t. OneWay{(i, j) in A}: x[i, j] + x[j, i] <= 1;		# at most, one direction of an arc can be used 

s.t. OneIn{j in N}: sum{(i, j) in A}(x[i, j]) <= 1;		# there can only be one inbound arc to a node

s.t. OneOut{j in N}: sum{(j, k) in A}(x[j, k]) <= 1;		# there can only be one outbound arc to a node

s.t. Cuts{(i, j) in K}: x[i, j] + x[j, i] = 1;		# satisfy all known arcs

s.t. Subtours{k in SF}: sum{(i, j) in S[k]}(x[i, j] + x[j, i]) <= card(S[k]) - 1;		# break known subtours









