library(R6)

Animal <- R6Class(
  classname = "Animal",
  public = list(
    initialize = function(name = NA, age = NA, number_of_legs = NA) {
      private$name = name
      private$age = age
      private$number_of_legs = number_of_legs
    },
    make_sound = function(sound) {
      cat(private$name, " says ", sound, "\n", sep = "")
    }
  ),
  private = list(
    name = NULL,
    age = NULL,
    number_of_legs = NULL
  )
)

Dog <- R6Class(
  classname = "Dog",
  inherit = Animal,
  public = list(
    initialize = function(name = NA, age = NA, number_of_legs = NA, hair_color = NA) {
      super$initialize(name, age, number_of_legs)
      private$hair_color = hair_color
    }
  ),
  private = list(
    hair_color = NULL
  )
)

d <- Dog$new(name = "Milo", age = 4, number_of_legs = 4, hair_color = "black")
print(d)
d$make_sound(sound = "Woooof!")
