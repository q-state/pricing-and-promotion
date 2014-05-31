
PricingModel <- setRefClass("PricingModel",
  fields = list(
    displayName           = "character",
    skus                  = "list",
    basePriceElasticities = "data.frame",
    tprElasticities       = "data.frame",
    promoMultipliers      = "data.frame",
    
    a = function(v) {
      if (missing(v)) return(x)
       assign('x',v,.self)                    
       cat ('a changed')
    }
  ),
  methods = list(
    initialize = function(displayName) {
      assign('x', numeric(), .self)
      .self$displayName = displayName
      .self$skus = list()
    },
    add = function(sku) {
      if(class(sku) != "Sku") {
        stop("Only SKU classes can be added to the list")
      }
      .self$skus = c(.self$skus, sku)
      cat("SKU added successfully\n")
    },
    
    calc = function(sourceSku) {
      baselineUnitsTotal = 0
      baselineRevenueTotal = 0
      baselineMarginTotal = 0
      
      promoUnitsTotal = 0
      promoRevenueTotal = 0
      promoMarginTotal = 0
      
      for(sku in skus) {
        cat("Updating sku ", sku$displayName, " as sku ", sourceSku$displayName, " changed. SKU$own is ", sku$own,"\n")
        sku$calc(sourceSku, basePriceElasticities, tprElasticities, promoMultipliers)
        
        if(sku$own == TRUE) {
          baselineUnitsTotal   = baselineUnitsTotal + sku$baselineUnitsTemp
          baselineRevenueTotal = baselineRevenueTotal + sku$baselineRevenueTemp
          baselineMarginTotal  = baselineMarginTotal + sku$baselineMarginTemp
          
          promoUnitsTotal = promoUnitsTotal + sku$promoUnitsTemp
          promoRevenueTotal = promoRevenueTotal + sku$promoRevenueTotal
          promoMarginTotal = promoMarginTotal + sku$promoMarginTotal
        }
      }

      # set the impact back on the source sku
      sourceSku$baselineUnitsTotal   = baselineUnitsTotal
      sourceSku$baselineRevenueTotal = baselineRevenueTotal
      sourceSku$baselineMarginTotal  = baselineMarginTotal
      
      sourceSku$unitsTotal = promoUnitsTotal + sourceSku$baselineUnitsTotal
      sourceSku$revenueTotal = promoRevenueTotal + sourceSku$baselineRevenueTotal
      sourceSku$marginTotal = promoMarginTotal + sourceSku$baselineMarginTotal
      
      list(baselineUnitsTotal, baselineRevenueTotal, baselineMarginTotal, promoUnitsTotal, promoRevenueTotal, promoMarginTotal, sourceSku$unitsTotal, sourceSku$revenueTotal, sourceSku$marginTotal)
    },
    
    importBasePriceElasticities = function(filename) {
      basePriceElasticities <<- read.csv(filename)
      rownames(basePriceElasticities) <<- basePriceElasticities$X
      basePriceElasticities$X <<- NULL
      cat("Loaded base price elasticities matrix successfully\n")
    },
    
    importTPRElasticities = function(filename) {
      tprElasticities <<- read.csv(filename)
      rownames(tprElasticities) <<- tprElasticities$sku.name
      cat("Loaded TPR elasticities matrix successfully\n")
    },
    
    importPromoMultipliers = function(filename) {
      promoMultipliers <<- read.csv(filename)
      rownames(promoMultipliers) <<- promoMultipliers$X
      promoMultipliers$X <<- NULL
      cat("Loaded promo multipliers successfully\n")
    },
    
    import = function(filename) {
      # warn user if elasticities have not been imported
      if(nrow(basePriceElasticities) == 0 || nrow(tprElasticities) == 0 || nrow(promoMultipliers) == 0)
        stop("Warning - elasticites and multipliers files might not have been loaded\n")
      
      # imports skus, facts and promofacts from a csv file
      import <- read.csv(filename);
      
      import$sku.name <- as.character(import$sku.name)
      import$fact.type <- as.character(import$fact.type)
      import$fact.name <- as.character(import$fact.name)
      import$fact.id <- as.character(import$fact.id)
      
      skuName = ""
      sku <- NULL
      for(num in 1:nrow(import)) {
        row <- import[num, ]
        
        if(skuName != row$sku.name) {
          cat("New SKU Name is ", row$sku.name, '\n')
          skuName = row$sku.name
          
          if(!is.null(sku)) {
            # add the last created sku to the pricing model
            cat("Adding sku: ", sku$displayName, '\n')
            .self$add(sku)
          }
          
          sku <- Sku$new(row$sku.name)
          if(row$sku.type == 1) sku$own = TRUE else sku$own = FALSE
        }
                   
        factType = row$fact.type
        factName = row$fact.name
        factId = row$fact.id
        orig = row$orig
        
        acv = row$acv
        weeks = row$weeks
        price = row$pprice
          
        if(factType == "fact") {
          fact = Fact$new(factId, factName, orig)
          
          cat("   * Adding fact: ", fact$displayName, '[', fact$id, ']\n')
          sku$addFact(fact)
        }
        
        if(factType == "promofact") {
          promoFact = PromoFact$new(factId, factName, acv, weeks, price)
          
          promoFact$elasticities = tprElasticities
          promoFact$multipliers = promoMultipliers
          
          cat("   - Adding PromoFact: ", promoFact$displayName, '[', promoFact$id, ']\n')
          sku$addPromoFact(promoFact)
        }       
      }
      
      if(!is.null(sku)) {
        # add the last created sku to the pricing model
        cat("Adding sku: ", sku$displayName, '\n')
        .self$add(sku)
      }
      
      cat("Import complete\n")
    },
    
    export = function(filename) {
      df = skus[[1]]$exportDataFrame()
      
      for(row in 2:length(skus)) {
        df <- rbind(df, skus[[row]]$exportDataFrame())
      }
      
      df
    }
  )
)

Sku <- setRefClass("Sku",
   fields = list(
     displayName     = "character",
     own             = "logical",
     facts           = "list",
     promoFacts      = "list",
     
     baselineUnitsTemp      = "numeric",
     baselineRevenueTemp    = "numeric",
     baselineMarginTemp     = "numeric",
     
     promoUnitsTemp         = "numeric",
     promoRevenueTemp       = "numeric",
     promoMarginTemp        = "numeric",
     
     baselineUnitsTotal     = "numeric",
     baselineRevenueTotal   = "numeric",
     baselineMarginTotal    = "numeric",
     
     promoUnitsTotal        = "numeric",
     promoRevenueTotal      = "numeric",
     promoMarginTotal       = "numeric",
     
     unitsTotal             = "numeric",
     revenueTotal           = "numeric",
     marginTotal            = "numeric"
   ),
   methods = list(
     initialize = function(displayName = "(Unnamed SKU)") {
       .self$displayName = displayName
       
        # add facts used for calc
        baselineRevenue         = Fact$new('baselineRevenue',       'Baseline Revenue')
        baselineMarginPerUnit   = Fact$new('baselineMarginPerUnit', 'Margin Per Unit')
        baselineMargin          = Fact$new('baselineMargin',        'Baseline Margin')
        
        promoRevenue            = Fact$new('promoRevenue',          'Promo Revenue')
        promoUnits              = Fact$new('promoUnits',            'Promo Units')
        promoMargin             = Fact$new('promoMargin',           'Promo Margin')
      
        addFact(baselineRevenue)
        addFact(baselineMarginPerUnit)
        addFact(baselineMargin)
       
        addFact(promoRevenue)
        addFact(promoUnits)
        addFact(promoMargin)
     },
     
     exportDataFrame = function() {
       el = list(sku.name = .self$displayName)
       for(row in 1:length(facts)) {
         cat('exporting fact:', facts[[row]]$id, '\n')
         el[paste0(facts[[row]]$id, ".orig")] = facts[[row]]$orig
         el[paste0(facts[[row]]$id, ".sim")] = facts[[row]]$sim
       }
      
       for(row in 1:length(promoFacts)) {
         cat('exporting promoFact:', promoFacts[[row]]$id, '\n')
         el[paste0(promoFacts[[row]]$id, '.acv')] = promoFacts[[row]]$acv
         el[paste0(promoFacts[[row]]$id, '.weeks')] = promoFacts[[row]]$weeks
         el[paste0(promoFacts[[row]]$id, '.price')] = promoFacts[[row]]$price
       }
       
       data.frame(el)
     },
     
     calc = function(sourceSku, basePriceElasticities, tprElasticities, promoMultipliers) {
        #sourceSku is the actual sku class
        elasticity = basePriceElasticities[sourceSku$displayName, displayName]
        cpriceDiff = sourceSku$facts[['cprice']]$pdiff()
        
      
        if(sourceSku$displayName == displayName)
        {
          facts[['units']]$sim <<- (elasticity * cpriceDiff * facts[['units']]$orig) + facts[['units']]$orig
          
          facts$baselineRevenue$sim        <<- facts[['units']]$sim *  facts[['wprice']]$sim
          facts$baselineMarginPerUnit$sim  <<- facts[['wprice']]$sim - facts[['cogs']]$sim
          facts$baselineMargin$sim         <<- facts[['units']]$sim *  facts$baselineMarginPerUnit$sim
        }
        
        baselineUnitsTemp              <<- (elasticity * cpriceDiff * facts[['units']]$orig)
        baselineRevenueTemp            <<- baselineUnitsTemp * facts[['wprice']]$sim
        baselineMarginPerUnitSimTemp = facts[['wprice']]$sim - facts[['cogs']]$sim
        baselineMarginTemp             <<- baselineUnitsTemp * baselineMarginPerUnitSimTemp
        
        
        # promo calcs
        promoUnitsTemp   <<- 0
        promoRevenueTemp <<- 0
        promoMarginTemp  <<- 0
        
        for(promoFact in promoFacts) {
          tprElasticity = tprElasticities[sourceSku$displayName, displayName]
          promoMultiplier = promoMultipliers[sourceSku$displayName, promoFact$id]
          
#           if(sourceSku$displayName == displayName)
#           {
#             facts$promoUnits$sim   <<- (tprElasticity * promoMultiplier * promoFact$acv / 100 * promoFact$pdiff() * facts[['units']]$sim * promoFact$weeks / 52)
#             facts$promoRevenue$sim <<- facts$promoUnits$sim * (facts[['wprice']]$sim - facts[['cprice']]$sim + promoFact$price)
#             facts$promoMargin$sim  <<- facts$promoUnits$sim * (facts[['wprice']]$sim - facts[['cprice']]$sim + promoFact$price - facts[['cogs']]$sim)
#           }
          
          promoUnitsTemp   <<- promoUnitsTemp + 
                (tprElasticity * promoMultiplier * promoFact$acv / 100 * promoFact$pdiff() * facts[['units']]$sim * promoFact$weeks / 52)
          promoRevenueTemp <<- promoRevenueTemp + (promoUnitsTemp * (facts[['wprice']]$sim - facts[['cprice']]$sim + promoFact$price))
          promoMarginTemp  <<- promoMarginTemp + (promoUnitsTemp - facts[['cprice']]$sim + promoFact$price - facts[['cogs']]$sim))
        }
        
        # set units, revenue, margin for own
        facts[['units']]$sim <<- facts[['units']]$sim + promoUnitsTemp
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

Fact <- setRefClass("Fact",
  fields = list(
    id            = "character",
    displayName   = "character",
    orig          = "numeric",
    sim           = "numeric"
  ),
  methods = list(
    pdiff = function() {
      (sim / orig) - 1
    },
    diff = function() {
      sim - orig
    },
    show = function() {
      df = data.frame(
        displayName = .self$displayName,
        orig = .self$orig,
        sim = .self$sim)
      methods::show(df)
    },
    initialize = function(id = "(Unnamed Fact)", displayName = id, orig = 0, sim = orig) {
      .self$id = id
      .self$orig = orig
      .self$sim = sim
      .self$displayName = displayName
    }
  )
)

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


pm = PricingModel$new("Test Model")
pm$importBasePriceElasticities("~/Documents/R Projects/matrix.csv")
pm$importTPRElasticities("~/Documents/R Projects/tpr-elasticities.csv")
pm$importPromoMultipliers("~/Documents/R Projects/promo-multipliers.csv")

pm$import("~/Documents/R Projects/test-import.csv")

pm$skus[[1]]$facts$cprice$sim = 3
pm$skus[[1]]$facts$wprice$sim = 2.50
  
pm$skus[[1]]$promoFacts[[1]]$acv = 50
pm$skus[[1]]$promoFacts[[1]]$weeks = 1
pm$skus[[1]]$promoFacts[[1]]$price = 1.99

pm$skus[[1]]$promoFacts[[2]]$acv = 50
pm$skus[[1]]$promoFacts[[2]]$weeks = 10
pm$skus[[1]]$promoFacts[[2]]$price = 1.15

out1 = pm$calc(pm$skus[[1]])

pm$skus[[2]]$facts$cprice$sim = 4
pm$skus[[2]]$facts$wprice$sim = 3.5

out2 = pm$calc(pm$skus[[2]])

df = pm$export()

