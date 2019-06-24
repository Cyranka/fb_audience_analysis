#' FB audience media markets breakdown
#'
#' Function that returns a data frame with the total audience for an interest in each of the top 50 media markets by population
#' @param id_vector: Vector of one or more facebook API interests
#' @param name_vector: Vector of one or more facebook API names
#' @param platform: "instagram", "facebook" or c("facebook", "instagram")
#' @keywords FB ads audience
#' @export
#' @examples
#' dma_ads_breakdown("6003123299417", "Computer science", "facebook")


dma_ads_breakdown <- function(id_vector, name_vector, platform){
  
  suppressMessages(library(dplyr))
  print('Fetching media markets data')
  
  dma <- tribble(
    ~dma_code, ~name,
    'DMA:501','New York',
    'DMA:803','Los Angeles',
    'DMA:602','Chicago',
    'DMA:504','Philadelphia',
    'DMA:807','San Francisco-Oak-San Jose',
    'DMA:623','Dallas-Ft. Worth',
    'DMA:511','Washington, DC (Hagrstwn)',
    'DMA:618','Houston',
    'DMA:524','Atlanta',
    'DMA:506','Boston (Manchester)',
    'DMA:753','Phoenix (Prescott)',
    'DMA:819','Seattle-Tacoma',
    'DMA:505','Detroit',
    'DMA:539','Tampa-St. Pete (Sarasota)',
    'DMA:528','Miami-Ft. Lauderdale',
    'DMA:613','Minneapolis-St. Paul',
    'DMA:751','Denver',
    'DMA:862','Sacramnto-Stkton-Modesto',
    'DMA:534','Orlando-Daytona Bch-Melbrn',
    'DMA:510','Cleveland-Akron (Canton)',
    'DMA:820','Portland, OR',
    'DMA:825','San Diego',
    'DMA:609','St. Louis',
    'DMA:517','Charlotte',
    'DMA:560','Raleigh-Durham (Fayetvlle)',
    'DMA:512','Baltimore',
    'DMA:527','Indianapolis',
    'DMA:770','Salt Lake City',
    'DMA:508','Pittsburgh',
    'DMA:659','Nashville',
    'DMA:533','Hartford & New Haven',
    'DMA:641','San Antonio',
    'DMA:522','Columbus, OH',
    'DMA:616','Kansas City',
    'DMA:515','Cincinnati',
    'DMA:617','Milwaukee',
    'DMA:567','Greenvll-Spart-Ashevll-And',
    'DMA:839','Las Vegas',
    'DMA:635','Austin',
    'DMA:548','West Palm Beach-Ft. Pierce',
    'DMA:563','Grand Rapids-Kalmzoo-B.Crk',
    'DMA:566','Harrisburg-Lncstr-Leb-York',
    'DMA:544','Norfolk-Portsmth-Newpt Nws',
    'DMA:866','Fresno-Visalia',
    'DMA:630','Birmingham (Ann And Tusc)',
    'DMA:790','Albuquerque-Santa Fe',
    'DMA:650','Oklahoma City',
    'DMA:561','Jacksonville',
    'DMA:518','Greensboro-H.Point-W.Salem',
    'DMA:640','Memphis'
  )
  
  
  dma_counts <- sapply(1:nrow(dma), function(i){
    Sys.sleep(5)
    fbad_reachestimate(targeting_spec = list(
      geo_locations = list(geo_markets =data.frame(key = as.character(dma$dma_code[i]))),
      publisher_platforms = platform,
      flexible_spec = list(
        list(interests = data.frame(
          id = as.character(id_vector),
          name = as.character(name_vector)
        ))
      )
    ))$users
  }
  )
  
  dma_data_frame <- tibble(dma = dma$name, count = as.numeric(dma_counts)) %>% arrange(count = desc(dma_counts))
  print('Media markets data fetched')
  return(dma_data_frame)

}
