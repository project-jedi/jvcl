
// Globus SiteBuilder 1.921b generated scripts. Please do not edit.
// compatible with: ie 4.0, 5.1, 6.0; nn 4.7; opera 3.0, 5.0, 6.0

var IE = document.all;
var IEver = '';
var IE4 = 0;
var IE5 = 1;
var OP = 0;
var BodyAlign = "left";
var MenuLPadding = 0;
var MenuRPadding = 0;
var SelectedImage=0;
var TranslitMode = 0;


//----
function BorderOn (id) { id.className='menuitemhoverhover'  }
function SubBorderOn (id) { id.className='submenuitemhover'  }
//----
function BorderOff(id) { id.className='menuitem' }
function SubBorderOff(id) { id.className='submenuitem' }
//----
var occupied_header = new Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
var occupied_menu = new Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

//----
	SelectedImage=202;
var MenuItemsCount = 9;
var MenuItemsHPadding = 0;
var aW = new Array(0, 22, 134, 74, 73, 67, 61, 40, 44, 59, 63, 145);
var aSW = new Array(0, 22, 134, 74, 73, 67, 61, 40, 44, 59, 63, 145);


//----
function NN() { return document.layers; }
//----
function Turn(id, mode)
{
  var x;
  var y;
  var windowWidth, subMenuWidth;

  IE = document.all;
  OP = document.getElementById;
  
  if (IE) IE = document.body.filters;

  if(IE)
  {
    y = 19;
    if(!y) y = MAIN.offsetTop + MAINMENU.offsetTop + window.event.srcElement.offsetHeight + window.event.srcElement.offsetTop + window.event.srcElement.offsetParent.offsetTop;
  }
  else if(NN()) y = 25;
  else if (OP) y = 25;

  if(IE)
    windowWidth = document.body.clientWidth;
  else if(OP)
    windowWidth = window.innerWidth;
  else
    windowWidth = window.innerWidth-16;


  if(isRightAligned(id))
  {
    if(BodyAlign=="center") x = (windowWidth-640)/2 + 640;
    if(BodyAlign=="left") x = 640;
    if(BodyAlign=="right") x = windowWidth;
    for(i=MenuItemsCount;i>=id;i--) x -= aSW[i] + ((i==MenuItemsCount) ? 0 : MenuItemsHPadding);


    if(IE)
    {
      if(document.all["dmenu" + id])
        subMenuWidth = document.all["dmenu" + id].clientWidth
    }
    else if(NN())
    {
      if(eval("document.layers['lmenu" + id + "']"))
        subMenuWidth = aSW[id];
    }

    if(BodyAlign=="left" && x + subMenuWidth > 640)
      x = 640 - subMenuWidth;

    x -= MenuRPadding;
  }
  else
  {
    if(BodyAlign=="center") if (windowWidth > 640) x = (windowWidth-640)/2; else x = 0;
    if(BodyAlign=="left") x = 0;
    if(BodyAlign=="right") x = windowWidth - 640;
    for(i=1;i<id;i++) x += aW[i] + ((i==1) ? 0 : MenuItemsHPadding)
    x += MenuLPadding;
  }

  if(mode)
    eval("setTimeout(\"On(" + id + ", " + mode + ", " + x + ", " + y + ");\",100)");
  else
    eval("setTimeout(\"Off(" + id + ", " + mode + ");\",100)");
}
//----
function On(id, mode, x, y)
{
  if(occupied_header[id])
  {

   if(IE)
   {
     if(!document.all["dmenu" + id]) return;
     eval("dmenu" + id).style.pixelLeft = x;
     eval("dmenu" + id).style.pixelTop = y;
     eval("dmenu" + id).style.display="";

    if(eval("dmenu" + id).filters["revealTrans"])
    {
      eval("dmenu" + id).filters["revealTrans"].apply();
      eval("dmenu" + id).style.visibility = '';
      eval("dmenu" + id).filters["revealTrans"].play();
    }
    if(eval("dmenu" + id).filters["blendTrans"])
    {
      eval("dmenu" + id).filters["blendTrans"].apply();
      eval("dmenu" + id).style.visibility = '';
      eval("dmenu" + id).filters["blendTrans"].play();
    }
    else
      eval("dmenu" + id).style.visibility = '';
   }
   else
   if(NN())
   {
     if(!eval("document.layers['lmenu" + id + "']")) return;
     eval("document.layers['lmenu" + id + "']").visibility = "show";
     eval("document.layers['lmenu" + id + "']").left = x;
     eval("document.layers['lmenu" + id + "']").top = y;
     eval("document.layers['lmenu" + id + "']").display="";
   }
   else if(OP)
   {
     if(!document.getElementById("dmenu" + id)) return;
     document.getElementById("dmenu" + id).style.left=x;
     document.getElementById("dmenu" + id).style.top=y;
     document.getElementById("dmenu" + id).style.visibility='visible';
   }

  }
}
//----
function Off(id, mode)
{
  if(!occupied_menu[id] && !occupied_header[id])
  {
   if(IE)
   {
     if(!document.all["dmenu" + id]) return;
     eval("dmenu" + id).style.display="none";
   }
   else
   if(NN())
   {
     if(!eval("document.layers['lmenu" + id + "']")) return;
     eval("document.layers['lmenu" + id + "']").visibility="hide";
   }
   else if(OP)
   {
     if(!document.getElementById("dmenu" + id)) return;
     document.getElementById("dmenu" + id).style.left=1040;
     document.getElementById("dmenu" + id).style.visibility='hidden';
   }


   if(SelectedImage == id*100) return;
   if(document.images["mi"+id*100])
     document.images["mi"+id*100].src = "mi/mi" + id*100 + ".gif";
  }

}
//----
function isObject(id)
{
  if(IE)
  {
    return document.all[id];
  }
  else
  if(NN())
  {
    return eval("document.layers['" + id + "']");
  }
  else
  if (OP)
  {
    return document.getElementById(id);
  }
}

//----
function isimgact(imageID, act, objectID)
{
  if(objectID==null) objectID = imageID;

  if( (act==0) && (SelectedImage == objectID)) return;

  if( ! document.images["mi"+objectID] ) return;

  if(act==1)
    document.images["mi"+objectID].src = "mi/mi" + imageID + "_.gif";
  else if(act==0  && !isObject("dmenu" + imageID / 100) )
    document.images["mi"+objectID].src = "mi/mi" + imageID + ".gif";
}

function isRightAligned(id)
{
  return (0  || (id==7) || (id==8) || (id==9));
}

function openVMenu(ID)
{
  if (!IE) return false;
  if(!document.all[ID]) return false;

  var lowsrc;

  if(document.all[ID].style.display == "none")
  {
    document.all[ID].style.display = "";
    if((document.images)&& document.images["vi" + ID])
    {
      lowsrc = document.images["vi" + ID].lowsrc;
      document.images["vi" + ID].lowsrc = document.images["vi" + ID].src;
      document.images["vi" + ID].src = lowsrc;
    }
  }else
  {
    document.all[ID].style.display = "none";
    if((document.images)&& document.images["vi" + ID])
    {
      lowsrc = document.images["vi" + ID].lowsrc;
      document.images["vi" + ID].lowsrc = document.images["vi" + ID].src;
      document.images["vi" + ID].src = lowsrc;
    }
  }
  return false;
}


function activateMenu(id)
{
  occupied_menu[id] = 1;
}

function deactivateMenu(id)
{
  occupied_menu[id] = 0;
}

function activateItem(id)
{
  occupied_header[id] = 1;
}

function deactivateItem(id)
{
  occupied_header[id] = 0;
}

function switchDynamicImage(imageObject)
{
  var imageSrc;
  imageSrc = imageObject.src;
  imageObject.src = imageObject.lowsrc;
  imageObject.lowsrc = imageSrc;
}

submenudata = new Array();








 
 
 
