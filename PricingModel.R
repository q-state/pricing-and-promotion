
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
      
      unitsTotal = 0
      revenueTotal = 0
      marginTotal = 0
      
      for(sku in skus) {
        # cat("Updating sku ", sku$displayName, " as sku ", sourceSku$displayName, " changed. SKU$own is ", sku$own,"\n")
        sku$calc(sourceSku, basePriceElasticities, tprElasticities, promoMultipliers)
        
        if(sku$own == TRUE) {
          
          unitsTotal = unitsTotal + sku$skuUnitsIncremental
          revenueTotal = revenueTotal + sku$skuRevenueIncremental
          marginTotal = marginTotal + sku$skuMarginIncremental
        }
      }

      # set the impact back on the source sku
      sourceSku$franchiseUnitsTotal = unitsTotal
      sourceSku$franchiseRevenueTotal = revenueTotal
      sourceSku$franchiseMarginTotal = marginTotal
      
      list(baselineUnitsTotal, baselineRevenueTotal, baselineMarginTotal, promoUnitsTotal, promoRevenueTotal, promoMarginTotal, sourceSku$franchiseUnitsTotal, sourceSku$franchiseRevenueTotal, sourceSku$franchiseMarginTotal)
    },
    
    importBasePriceElasticities = function(filename) {
      basePriceElasticities <<- read.csv(filename)
      rownames(basePriceElasticities) <<- basePriceElasticities$X
      basePriceElasticities$X <<- NULL
      cat("Loaded base price elasticities matrix successfully\n")
    },
    
    importTPRElasticities = function(filename) {
      tprElasticities <<- read.csv(filename)
      rownames(tprElasticities) <<- tprElasticities$X
      tprElasticities$X <<- NULL
      cat("Loaded TPR elasticities matrix successfully\n")
    },
    
    importPromoMultipliers = function(filename) {
      promoMultipliers <<- read.csv(filename)
      rownames(promoMultipliers) <<- promoMultipliers$sku.name
      cat("Loaded promo multipliers successfully\n")
    },
    
    import = function(filename) {
      # warn user if elasticities have not been imported
      if(nrow(basePriceElasticities) == 0 || nrow(tprElasticities) == 0 || nrow(promoMultipliers) == 0)
        stop("Warning - elasticites and multipliers files might not have been loaded\n")
      
      # imports skus, facts and promofacts from a csv file
      import <- read.csv(filename, stringsAsFactors = FALSE);
      
#       import$sku.name <- as.character(import$sku.name)
#       import$fact.type <- as.character(import$fact.type)
#       import$fact.name <- as.character(import$fact.name)
#       import$fact.id <- as.character(import$fact.id)
      
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