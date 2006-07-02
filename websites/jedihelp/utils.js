// Utility script file

ns4 = (document.layers)? true:false
ie4 = (document.all)? true:false
ns6 = (!document.all && document.getElementById)? true:false

// Make an object visible
function showObject(obj) 
{
  if (ns4) obj.visibility = "show"
  else if (ie4 || ns6) obj.visibility = "visible"
}

// Hides an object
function hideObject(obj) 
{
  if (ns4) obj.visibility = "hide"
  else if (ie4 || ns6) obj.visibility = "hidden"
}

// Move a layer
function moveTo(obj,xL,yL) 
{
  obj.left = xL
  obj.top = yL
}