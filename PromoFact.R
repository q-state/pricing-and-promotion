
PromoFact <- setRefClass("PromoFact",
  fields = list(
    id              = "character",
    displayName     = "character",
    acv             = "numeric",
    weeks           = "numeric",
    price           = "numeric",
    
    facts        = "list",
    elasticities = "data.frame",
    multipliers  = "data.frame"
  ),
  methods = list(
        
    pdiff = function() {
      (price / getSim('cprice')) - 1  
    },
    getSim = function(factId) {
      facts[factId][[1]]$sim
    },
    show = function() {
      df = data.frame(
        displayName = .self$displayName,
        acv = .self$acv,
        weeks = .self$weeks,
        price = .self$price)
      methods::show(df)
    },
    initialize = function(id = "(Unnamed PromoFact)", displayName = id, acv = 0, weeks = 0, price = 0) {
      .self$id = id
      .self$acv = acv
      .self$weeks = weeks
      .self$price = price
      .self$displayName = displayName
    }
  )
)

