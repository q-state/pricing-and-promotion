Sku <- setRefClass("Sku",
   fields = list(
     displayName     = "character",
     own             = "logical",
     facts           = "list",
     promoFacts      = "list",
       
     # values used in the aggregation at PricingModel level
     skuUnits              = "numeric",
     skuUnitsIncremental   = "numeric",
     skuRevenue            = "numeric",
     skuRevenueIncremental = "numeric",
     skuMargin             = "numeric",
     skuMarginIncremental  = "numeric",
     
     franchiseUnitsTotal             = "numeric",
     franchiseRevenueTotal           = "numeric",
     franchiseMarginTotal            = "numeric"
   ),
   methods = list(
     initialize = function(displayName = "(Unnamed SKU)") {
       .self$displayName = displayName
       
       .self$franchiseUnitsTotal = 0
       .self$franchiseRevenueTotal = 0
       .self$franchiseMarginTotal = 0
       
       ownRevenue = Fact$new('revenue', 'Revenue')
       ownMargin = Fact$new('margin', 'Margin')
       
       addFact(ownRevenue)
       addFact(ownMargin)
     },
     
     exportDataFrame = function() {
       el = list(sku.name = .self$displayName)
       el$sku.type = own
       cat('exporting facts: ')
       for(row in 1:length(facts)) {
         cat(facts[[row]]$id, ', ')
         el[paste0(facts[[row]]$id, ".orig")] = facts[[row]]$orig
         el[paste0(facts[[row]]$id, ".sim")] = facts[[row]]$sim
       }
      
      cat('\nexporting promofacts: ')
       
       for(row in 1:length(promoFacts)) {
         cat(promoFacts[[row]]$id, ', ')
         el[paste0(promoFacts[[row]]$id, '.acv')] = promoFacts[[row]]$acv
         el[paste0(promoFacts[[row]]$id, '.weeks')] = promoFacts[[row]]$weeks
         el[paste0(promoFacts[[row]]$id, '.price')] = promoFacts[[row]]$price
       }
       cat('\n')
       el$skuUnits = skuUnits
       el$skuUnitsIncremental = skuUnitsIncremental
       el$skuRevenue = skuRevenue
       el$skuRevenueIncremental = skuRevenueIncremental
       el$skuMargin = skuMargin
       el$skuMargin = skuMarginIncremental
       
       el$franchiseUnits = franchiseUnitsTotal
       el$franchiseRevenue = franchiseRevenueTotal
       el$franchiseMargin = franchiseMarginTotal
       
       data.frame(el)
     },
     
    calc = function(sourceSku, basePriceElasticities, tprElasticities, promoMultipliers) {
        #sourceSku is the actual sku class
        elasticity = basePriceElasticities[sourceSku$displayName, displayName]
        cpriceDiff = sourceSku$facts[['cprice']]$pdiff()
        
        baselineUnitsTemp              = (elasticity * cpriceDiff * facts[['units']]$orig)
        baselineRevenueTemp            = baselineUnitsTemp * facts[['wprice']]$sim
        baselineMarginPerUnitSimTemp = facts[['wprice']]$sim - facts[['cogs']]$sim
        baselineMarginTemp             = baselineUnitsTemp * baselineMarginPerUnitSimTemp
         
        # promo calcs
        promoUnitsTemp   = 0
        promoRevenueTemp = 0
        promoMarginTemp  = 0
        
        for(promoFact in promoFacts) {
          tprElasticity = tprElasticities[sourceSku$displayName, displayName]
          promoMultiplier = promoMultipliers[sourceSku$displayName, promoFact$id]
          
          promoUnitsTemp   = promoUnitsTemp + 
                    (tprElasticity * promoMultiplier * promoFact$acv / 100 * promoFact$pdiff() * facts[['units']]$sim * promoFact$weeks / 52)
          promoRevenueTemp = promoRevenueTemp + (promoUnitsTemp * (facts[['wprice']]$sim - facts[['cprice']]$sim + promoFact$price))
          promoMarginTemp  = promoMarginTemp + (promoUnitsTemp - (facts[['cprice']]$sim + promoFact$price - facts[['cogs']]$sim))
        }
        
        # set units, revenue, margin for own
        skuUnits            <<- baselineUnitsTemp + promoUnitsTemp + facts[['units']]$orig
        skuUnitsIncremental <<- baselineUnitsTemp + promoUnitsTemp
        
        skuRevenue            <<- ( facts[['units']]$orig * facts[['wprice']]$sim ) + baselineRevenueTemp + promoRevenueTemp 
        skuRevenueIncremental <<- baselineRevenueTemp + promoRevenueTemp
        
        skuMargin            <<- ( facts[['units']]$orig * baselineMarginPerUnitSimTemp)+ baselineMarginTemp + promoMarginTemp
        skuMarginIncremental <<- baselineMarginTemp + promoMarginTemp 
        
        if(sourceSku$displayName == displayName) {
          facts[['units']]$sim <<- skuUnits
          facts[['revenue']]$sim <<- skuRevenue
          facts[['margin']]$sim <<- skuMargin
        }
    },

    f = function(factId) {
      facts[[factId]]      
    },
    
    getUnits = function() {
      total = 0
      for(promoFact in .self$promoFacts) {
        total = total + promoFact$getUnits()
      }
      
      facts[['units']]$sim + total
    },
    
    getRevenue = function() {
      total = 0
      for(promoFact in .self$promoFacts) {
        total = total + promoFact$getRevenue()
      }
      
      facts[['baselineRevenue']]$sim + total  
    },

    getMargin = function() {
      total = 0
      for(promoFact in .self$promoFacts) {
        total = total + promoFact$getMargin()
      }
      
      facts[['baselineMargin']]$sim + total      
    },
    
    addFact = function(fact) {
       if(class(fact) != "Fact") {
         stop("Only Fact classes can be added to the list")
       }
       
        # get the fact names
        factNames = c(names(.self$facts), fact$id)
       
        # add the fact
        .self$facts = c(.self$facts, fact)
       
        # set the names
        .self$facts = setNames(.self$facts, factNames)
     },
    
    addPromoFact = function(promoFact) {
       if(class(promoFact) != "PromoFact") {
         stop("Only PromoFact classes can be added to the list")
       }
        
       promoFact$facts = .self$facts
       .self$promoFacts = c(.self$promoFacts, promoFact)
     }
   )
)
