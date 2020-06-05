
findOptState = function(mboObj, depth = 1L) {
  object_names = list(depth)

  for (i in 1:depth) {

    if (i == 1) {
      object_names[[i]] = names(mboObj)
    } else {
      object_names[[i]] = names(mboObj[[i]])
    }

  }
}
