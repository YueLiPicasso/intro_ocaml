class point =
  object
    val mutable x = 0
    method get_x = x
    method move d = x <- x + d
  end;;
