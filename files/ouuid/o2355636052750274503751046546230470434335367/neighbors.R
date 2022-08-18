( function () {

    .neighbor <- function ( x , X , IMAX , before = NA_integer_ , beyond = IMAX , adj = 0L ) {
        AT <- x == X
        if ( any ( AT ) ) {
            which ( AT )
        } else if ( x < X [[ 1 ]] ) {
            before
        } else if ( x > X [[ IMAX ]] ) {
            beyond
        } else {
            adj + sum ( X < x ) } }



    .neighborLeft <- .neighbor

    .neighborRight <- function ( x , X , IMAX ) .neighbor (
            x , X , IMAX , before = 1L , beyond = NA_integer_ , adj = 1L )



    .neighbors <- function ( X , x , FUN = .neighborLeft ) vapply (
            X = x , FUN.VALUE = 1L , USE.NAMES = FALSE , FUN =
                    function ( y ) FUN ( y , X , length ( X ) ) )



    .neighborsLeft <- function ( X , x ) .neighbors ( X , x )

    .neighborsRight <- function ( X , x )
            .neighbors ( X , x , FUN = .neighborRight )

    NEIGHBORS <- list (
        left = .neighborsLeft ,
        right = .neighborsRight )

    neighbors <- function ( X , x , direction = c ( "left" , "right" ) ) {
        stopifnot (
            is.numeric ( X ) ,
            is.numeric ( x ) )
        if ( ! length ( X ) )
                return ( rep ( NA_integer_ , length ( x ) ) )
        stopifnot (
            ! is.unsorted ( X ) ,
            ! any ( duplicated ( X ) ) )
        NEIGHBORS [[ match.arg ( direction ) ]] ( X , x ) }

    neighbors } ) ()
