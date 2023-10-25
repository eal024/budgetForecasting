

# Function for cleaning and prepering data

# formating date to text
fn_date_label <- function(d) { paste0( year(d) |> str_sub(start = 3, end = 4),"-",month(d, label = T) ) }


## Creating table
tbl <- tibble( this = c(3,6,9,12), last = this + 12 ) |> 
    mutate( `this year`       = map( this, function(x) df |> tail(x)     ),
            `last year`       = map( this, function(x) df |> tail(x+12) |> head(x)  ),
            `value this year` = map(`this year`, \(x) mean(x$value) ),
            `value last year` = map(`last year`, \(x) mean(x$value) ),
            ) |> 
    unnest( cols = c(`value this year`, `value last year`)
            ) |> 
    mutate( `percent development` = (`value this year`/`value last year`)-1,
            period  = pmap( list( this = `this year`, last = `last year`), function(this,last) {
        paste0( "from ",
                fn_date_label(min(last$date) ), "-", fn_date_label(max(last$date) ),
                " to ",
                fn_date_label(min(this$date) ), "-", fn_date_label(max(this$date) )
                    )
                } ) 
        ) |> 
    unnest( period)

# Organising the table
tbl |> 
    select( period, `value last year`, `value this year`, `percent development` )


# Function
fn_monthly_table <- function( tbl ){
    tibble( this = c(3,6,9,12), last = this + 12 ) |> 
        mutate( `this year`       = map( this, function(x) tbl |> tail(x)),
                `last year`       = map( this, function(x) tbl |> tail(x+12) |> head(x)  ),
                `value this year` = map(`this year`, \(x) mean(x$value) ),
                `value last year` = map(`last year`, \(x) mean(x$value) ),
        ) |> 
        unnest( cols = c(`value this year`, `value last year`)
        ) |> 
        mutate( `percent development` = (`value this year`/`value last year`)-1,
                period  = pmap( list( this = `this year`, last = `last year`), function(this,last) {
                    paste0( "from ",
                            fn_date_label(min(last$date) ), "-", fn_date_label(max(last$date) ),
                            " to ",
                            fn_date_label(min(this$date) ), "-", fn_date_label(max(this$date) )
                    )
                } ) 
        ) |> 
        unnest( period) |> 
        select( period, `value this year`,`value last year`,`percent development`)
}


