// Width of the popups in pixels
// 100-300 pixels is typical
  if (typeof width == 'undefined') { var width = "300";}
  
// How many pixels to the right/left of the cursor to show the popup
// Values between 3 and 12 are best
  if (typeof offsetx == 'undefined') { var offsetx = 10;}
  
// How many pixels to the below the cursor to show the popup
// Values between 3 and 12 are best
  if (typeof offsety == 'undefined') { var offsety = 10;}
  
////////////////////////////////////////////////////////////////////////////////////
// END CONFIGURATION
////////////////////////////////////////////////////////////////////////////////////

var x = 0;
var y = 0;
var snow = 0;
var sw = 0;
var cnt = 0;
var dir = 1;
var tr = 0;
var isFramed = 1;

if ( (ns4) || (ie4) || (ns6) )
	{
		if (ns4) 
      over = document.overDiv
		if (ie4 || ns6) 
    {
			var overDiv = document.getElementById("overDiv");
			over = overDiv.style;
		}
		document.onmousemove = mouseMove
		if (ns4) 
      document.captureEvents(Event.MOUSEMOVE)
	}
	
var bDisp = 0;

// Public functions to be used on pages.

// Clears popups if appropriate
function nd() 
{
  if ( cnt >= 1 ) { sw = 0 };
  if ( (ns4) || (ie4) || (ns6) ) 
  {
    if ( sw == 0 ) 
    {
      snow = 0;
      hideObject(over);
	    bDisp = 0;
    } 
    else 
    {
      cnt++;
    }
  }
}

function popup(text)
{
  var txt = 
    '<table boder="1" cellspacing="1" cellpadding="1" width="300">'+
    '  <tr>'+
    '    <td style="background-color: #EBFFFF;">'+
    '      <font size="-2">'+text+'</font>'+
    '    </td>'+
    '  </tr>'+
    '</table>';

	layerWrite(txt);
	dir = 1;
	disp();
}

// Non public functions. These are called by other functions etc.
// Common calls
function disp() 
{
	if (snow == 0) 
  {
		if (isFramed) 
    {
			if (dir == 2) 
        moveTo(over,x+offsetx-(width/2),y+offsety); // Center
			if (dir == 1) 
      { // Right
				if (x<document.width-width) moveTo(over,x+offsetx,y+offsety);
	    		if (x>document.width-width) moveTo(over,x-width,y+offsety);
			}
			if (dir == 0) 
        moveTo(over,x-offsetx-width,y+offsety); // Left
		}
		else 
      moveTo(over,x+offsetx-(width/2),y+offsety);
		
		snow = 1;
		showObject(over);
		bDisp = 1;
	}
}

// Moves the layer
function mouseMove(e) 
{
	if (ns4 || ns6) 
  {
    x=e.pageX; y=e.pageY
  }
	if (ie4) 
  {
    x=event.clientX+document.body.scrollLeft; y=event.clientY+document.body.scrollTop
  };
 
	if (snow && !bDisp) 
  {
		if (isFramed) 
    {
			if (x>=450 && x<=550) moveTo(over,x+offsetx-(width/2),y+offsety); // Center
			if (x<400) moveTo(over,x+offsetx,y+offsety); // Right
			if (x>400)  moveTo(over,x-width,y+offsety); // Left
		}
		else moveTo(over,x+offsetx-(width/2),y+offsety);
	}
}

// Writes to a layer
function layerWrite(txt) 
{
  if (ns4) 
  {
    var lyr = document.overDiv.document
    lyr.write(txt)
    lyr.close()
  }
  else if (ie4) 
    document.all["overDiv"].innerHTML = txt
  else if (ns6) 
    document.getElementById("overDiv").innerHTML = txt;	
}

