# A script that will filter a dataframe by selecting all the rows
# that have similar values.  The "similarity" can be controlled to lie within a
# certain percentage of each other.

options( stringsAsFactors = F )

stderr.mean <- function( x ){
   # calculate standard error of the mean
   # see
   # http://stackoverflow.com/questions/2676554/in-r-how-to-find-the-standard-error-of-the-mean
   # also see
   # http://en.wikipedia.org/wiki/Standard_error
   sqrt( var( x, na.rm=T )/length( na.omit( x ) ) )
}

decide.n <- function( n, accept.perc = 50 ){
  # decide how many values should fit the criteria.  Ideally we want at least 50% + values should satisfy the condition.  We can change this to a different number later.
  my.denom <- 1 / ( accept.perc / 100 )
  my.n <- ( n %/% my.denom ) + 1 # integer division
  return( as.integer( my.n ) )
}

accept.row.stats <- function( v, accept.n ){
   # this function will accept the row if all the values are within one standard
   # error of the mean.  That way we get around the problems of the arbitrary
   # cut-offs.
   sem <- stderr.mean( v ) # calculate the standard error of the mean for the
                           # values in the row.
   my.mean <- mean( v )
   my.filt.vect <- abs( v - my.mean ) <= sem
   print( list( v=v, filtered=my.filt.vect ) ) # for debugging
   print(" ")
   if( sum( my.filt.vect ) >= accept.n + 1 ){ return( TRUE ) }
   return( FALSE )
}

accept.row <- function( v, accept.n, range.perc = 10  ){
    # range.perc is the range in percentage that we want all the values to lie
    # in.  The default value is 10%, but it can be changed.
    # max.val <- min( v )
    max.val <- max( v )
    accept.diff <- max.val * ( range.perc / 100 )
    my.filt.vect <-  abs( v - max.val ) <= accept.diff
    if( is.na( my.filt.vect ) ){return( FALSE ) }
    if( sum( my.filt.vect ) >= accept.n + 1){return( TRUE )}
    else {return( FALSE ) }
}

filter.data.frame <- function( df, range.perc = 10, accept.perc = 50 ){
  # will accept a data-frame and then make the necessary transformations so that only rows that lie in specific range will be accepted.
  accept.n <- decide.n( ncol( df ), accept.perc = accept.perc )
  filter.df <- data.frame()
  for( i in 1:nrow( df ) ){
    my.e <- df[ i, ]
    v    <- as.numeric( my.e )
    # if( accept.row( v, accept.n, range.perc = range.perc ) ){ filter.df <- rbind( filter.df, my.e )
    if( accept.row.stats( v, accept.n ) ){ filter.df <- rbind( filter.df, my.e )
    }
  }
  return( filter.df )
}

test.filter.df <- function( df, range.prec=10, accept.perc=50 ){
}