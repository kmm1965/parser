verify = function(msg, value, expected){
  if(value.toString() != expected.toString())
    alert(msg + " failed\nactual=" + value + "\nexpected: " + expected);
}