
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
