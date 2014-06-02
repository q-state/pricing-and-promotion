pm = PricingModel$new("Test Model")
pm$importBasePriceElasticities("./Documents/R Projects/pricing/Support Files/baseprice-elasticities.csv")
pm$importTPRElasticities("./Documents/R Projects/pricing/Support Files/tpr-elasticities.csv")
pm$importPromoMultipliers("./Documents/R Projects/pricing/Support Files/promo-multipliers.csv")

pm$import("./Documents/R Projects/pricing/Support Files/test-import.csv")

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

t = system.time( 
    pm$calc(pm$skus[[2]]) 
)

df = pm$export()

View(df)
show(t)
