### 
# We want a R script which is will accept a data frame, and get all the rows
# that have similar values.  The "similarity" can be controlled to lie within a
# certain percentage of each other.  

options( stringsAsFactors = F )

decide.n <- function( n, accept.perc = 50 ){
  # decide how many values should fit the criteria.  Ideally we want at least 50% + values should satisfy the condition.  We can change this to a different number later.
  my.denom <- 1 / ( accept.perc / 100 )
  print( my.denom )  # for debugging
  my.n <- ( n %/% my.denom ) + 1 # integer division
  print( my.n ) # for debugging
  return( as.integer( my.n ) )
}

accept.row <- function( v, range.perc = 10 ){
    # range.perc is the range in percentage that we want all the values to lie
    # in.  The default value is 10%, but it can be changed.
    # max.val <- min( v )
    max.val <- max( v )
#     print( max.val ) # for debugging
    accept.diff <- max.val * ( range.perc / 100 )
#     print( accept.diff ) # for debugging
    my.filt.vect <-  abs( v - max.val ) <= accept.diff 
#     print( my.filt.vect ) # for debugging
    # print( sum( my.filt.vect ) ) # for debugging
    if( sum( my.filt.vect ) >= length( v ) - 1 ){ return( TRUE ) }
    # if( sum( my.filt.vect ) == length( v ) ){ return( TRUE ) }
    else {return( FALSE ) }
}

filter.data.frame <- function( df, range.perc = 10, accept.perc = 50 ){
  # will accept a data-frame and then make the necessary transformations so that only rows that lie in specific range will be accepted.
  accept.n <- decide.n( ncol( df ), accept.perc = accept.perc )
  filter.df <- data.frame()
  for( i in 1:nrow( df ) ){
    my.e <- df[ i, ]
#     print( my.e ) # for debugging
    v    <- as.numeric( my.e )
#     print( v ) # for debugging
    if( accept.row( v, range.perc = range.perc ) ){ filter.df <- rbind( filter.df, my.e ) 
#     print( filter.df ) # for debugging
    }
  }
  return( filter.df )
}