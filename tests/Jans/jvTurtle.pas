{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvTurtle.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit jvTurtle;

interface
uses Windows, Messages, math, SysUtils, Classes, Graphics, Controls;

type
  TRequestBackGroundEvent= procedure ( sender : Tobject; background:string) of object;
  TRequestFilterEvent= procedure ( sender : Tobject; filter:string) of object;
  TRequestImageSizeEvent= procedure (sender: Tobject; var Arect:Trect)of object;

type
  TjvTurtle=class (TComponent)
  private
    FPosition: Tpoint;
    FHeading: real;
    FCanvas: Tcanvas;
    FPenDown: boolean;
    FOnRepaintRequest: TNotifyEvent;
    FMark: Tpoint;
    FArea: trect;
    FOnRequestBackGround: TRequestBackGroundEvent;
    FOnRequestImageSize: TRequestImageSizeEvent;
    FOnRequestFilter: TRequestFilterEvent;
    function  gettoken(var token:string):boolean;
    function  getnum(var num:integer):boolean;
    function  inpot(token:string;var num:integer):boolean;
    function  gettex(var tex:string):boolean;
    function  getcol(var col:Tcolor):boolean;
    function  skipblock:boolean;
    function  push(num:integer):boolean;
    function  pop(var num:integer):boolean;
    function  Npush(var msg:string;num:integer):boolean;
    function  Npop(var msg:string;var num:integer):boolean;
    function  IsNum(tex:string):boolean;
    function  IsCol(tex:string):boolean;
    function  IsVar(tex:string):boolean;
    procedure SetPosition(const Value: Tpoint);
    procedure SetHeading(const Value: real);
    procedure SetCanvas(const Value: Tcanvas);
    procedure SetPenDown(const Value: boolean);
    procedure SetWidth(const Value: integer);
    function  GetWidth:integer;
    function  txUser(sym:string):string;
    function  txcomment:string;
    function  txin:string;
    function  txinadd:string;
    function  txinsub:string;
    function  txinmult:string;
    function  txindiv:string;
    function  txininc:string;
    function  txindec:string;
    function  txblock:string;
    function  txreturn:string;
    function  txpos:string;
    function  txdefault:string;
    function  txmove:string;
    function  txlineto:string;
    function  txangle:string;
    function  txdown:string;
    function  txup:string;
    function  txpensize:string;
    function  txpencolor:string;
    function  txaddpencolor:string;
    function  txaddbrushcolor:string;
    function  txturn:string;
    function  txleft:string;
    function  txright:string;
    function  txgo:string;
    function  txtext:string;
    function  txtextout:string;
    function  txtextfont:string;
    function  txtextsize:string;
    function  txtextcolor:string;
    function  txtextbold:string;
    function  txtextitalic:string;
    function  txtextunderline:string;
    function  txtextnormal:string;
    function  txbssolid:string;
    function  txbsclear:string;
    function  txbrushcolor:string;
    function  txrectangle:string;
    function  txroundrect:string;
    function  txellipse:string;
    function  txdiamond:string;
    function  txpolygon:string;
    function  txstar:string;
    function  txcurve:string;
    function  txmark:string;
    function  txgomark:string;
    function  txmarkangle:string;
    function  txgomarkangle:string;
    function  txArea:string;
    function  txCopy:string;
    function  txpenmode:string;
    function  txcopymode:string;
    function  txflood:string;
    function  txdo:string;
    function  txloop:string;
    function  txIleft:string;
    function  txItop:string;
    function  txIright:string;
    function  txIbottom:string;
    function  txIcenter:string;
    function  txsum:string;
    function  txsub:string;
    function  txmul:string;
    function  txdiv:string;
    function  txdup:string;
    function  txdrop:string;
    function  tx_posx:string;
    function  tx_posy:string;
    function  tx_pencolor:string;
    function  tx_brushcolor:string;
    function  tx_textcolor:string;
    function  tx_pensize:string;
    function  tx_textsize:string;
    function  tx_angle:string;
    function  tx_markx:string;
    function  tx_marky:string;
    function  tx_loop:string;
    function  tx_right:string;
    function  tx_left:string;
    function  tx_top:string;
    function  tx_bottom:string;
    function  txif:string;
    function  txgt:string;
    function  txge:string;
    function  txlt:string;
    function  txle:string;
    function  txeq:string;
    function  txne:string;
    function  txnot:string;
    function  txand:string;
    function  txor:string;
    function  txneg:string;
    function  txabs:string;
    function  txswap:string;
    function  txmax:string;
    function  txmin:string;
    function  txsqr:string;
    function  txsqrt:string;
    function  txinc:string;
    function  txdec:string;
    function  txbackground: string;
    function  txfilter:string;
    function  strtoPenMode(var pm:Tpenmode;s:string):boolean;
    function  strtoCopyMode(var cm:Tcopymode;s:string):boolean;

    procedure TextRotate(x, y, angle: integer; atext: string;
  afont: tfont);
    procedure SetOnRepaintRequest(const Value: TNotifyEvent);
    procedure SetMark(const Value: Tpoint);
    procedure SetArea(const Value: trect);
    procedure SetOnRequestBackGround(const Value: TRequestBackGroundEvent);
    procedure SetOnRequestImageSize(const Value: TRequestImageSizeEvent);
    procedure SetOnRequestFilter(const Value: TRequestFilterEvent);


  protected
    procedure DoRepaintRequest; virtual;
    procedure DoRequestBackground; virtual;
    procedure DoRequestFilter; virtual;
    function  DoRequestImageSize:boolean; virtual;
  public
    property Canvas:Tcanvas read FCanvas write SetCanvas;
    property Position:Tpoint read FPosition write SetPosition;
    property Mark:Tpoint read FMark write SetMark;
    property Area:trect read FArea write SetArea;
    property Heading:real read FHeading write SetHeading;
    property PenDown:boolean read FPenDown write SetPenDown;
    property Width:integer read GetWidth write SetWidth;
    function docom:string;
    procedure Turn(Aangle:real);
    procedure Right(AAngle:real);
    procedure MoveForward(Adistance:real);
    procedure MoveBackward(ADistance:real);
    function Interpret(var Epos:integer;s:string):string;
    constructor Create (AOwner:Tcomponent); override;
    destructor Destroy; override;
  published
    property OnRepaintRequest: TNotifyEvent read FOnRepaintRequest write SetOnRepaintRequest;
    property OnRequestBackGround: TRequestBackGroundEvent read FOnRequestBackGround write SetOnRequestBackGround;
    property OnRequestFilter:TRequestFilterEvent read FOnRequestFilter write SetOnRequestFilter;
    property OnRequestImageSize: TRequestImageSizeEvent read FOnRequestImageSize write SetOnRequestImageSize;
  end;


implementation

{ TjvTurtle }
const
  cr = chr(13)+chr(10);
  tab = chr(9);
  nspmax=255;

var
   scrip:string;
   ip,ipmax,sp,nsp:integer;
   stack:array [0..255] of integer;
   Nstack:array [0..nspmax] of integer;
   background:string;
   pot:tstringlist;
   filter:string;
   anglemark:integer;
   ImageRect:Trect;


function TjvTurtle.docom: string;
var
   com:string;
begin
  result:='ready';
  if not gettoken(com) then exit;
    if com='pos'then result:=txpos
    else if com='in' then result:=txin
    else if com='inadd' then result:=txinadd
    else if com='insub' then result:=txinsub
    else if com='inmul' then result:=txinmult
    else if com='indiv' then result:=txindiv
    else if com='ininc' then result:=txininc
    else if com='indec' then result:=txindec
    else if com='default' then result:=txdefault
    else if com='angle' then result:=txangle
    else if com='down' then result:=txdown
    else if com='up' then result:=txup
    else if com='pensize' then result:=txpensize
    else if com='pencolor' then result:=txpencolor
    else if com='turn' then result:=txturn
    else if com='right' then result:=txright
    else if com='left' then result:=txleft
    else if com='go' then result:=txgo
    else if com='move' then result:=txmove
    else if com='lineto' then result:=txlineto
    else if com='textfont' then result:=txtextfont
    else if com='textsize' then result:=txtextsize
    else if com='textcolor' then result:=txtextcolor
    else if com='addbrushcolor' then result:=txaddbrushcolor
    else if com='addpencolor' then result:=txaddpencolor
    else if com='text' then result:=txtext
    else if com='bold' then result:=txtextbold
    else if com='italic' then result:=txtextitalic
    else if com='underline' then result:=txtextunderline
    else if com='normal' then result:=txtextnormal
    else if com='textout' then result:=txtextout
    else if com='bssolid' then result:=txbssolid
    else if com='bsclear' then result:=txbsclear
    else if com='brushcolor' then result:=txbrushcolor
    else if com='rectangle' then result:=txrectangle
    else if com='roundrect' then result:=txroundrect
    else if com='ellipse' then result:=txellipse
    else if com='diamond' then result:=txdiamond
    else if com='polygon' then result:=txpolygon
    else if com='star' then result:=txstar
    else if com='curve' then result:=txcurve
    else if com='mark' then result:=txmark
    else if com='gomark' then result:=txgomark
    else if com='markangle' then result:=txmarkangle
    else if com='gomarkangle' then result:=txgomarkangle
    else if com='penmode' then result:=txpenmode
    else if com='copymode' then result:=txcopymode
    else if com='area' then result:=txArea
    else if com='copy' then result:=txcopy
    else if com='do' then result:=txdo
    else if com='loop' then result:=txloop
    else if com='flood' then result:=txflood
    else if com='background' then result:=txbackground
    else if com='filter' then result:=txfilter
    else if com='{' then result:=txcomment
    else if com='[' then result:=txblock
    else if com=']' then result:=txreturn
    else if com='goleft' then result:=txIleft
    else if com='gotop' then result:=txItop
    else if com='goright' then result:=txIright
    else if com='gobottom' then result:=txIbottom
    else if com='gocenter' then result:=txIcenter
    else if com='+' then result:=txsum
    else if com='-' then result:=txsub
    else if com='*' then result:=txmul
    else if com='/' then result:=txdiv
    else if com='.gt' then result:=txgt
    else if com='.ge' then result:=txge
    else if com='.lt' then result:=txlt
    else if com='.le' then result:=txle
    else if com='.eq' then result:=txeq
    else if com='.ne' then result:=txne
    else if com='.not' then result:=txnot
    else if com='.and' then result:=txand
    else if com='.or' then result:=txor
    else if com='neg' then result:=txneg
    else if com='abs' then result:=txabs
    else if com='swap' then result:=txswap
    else if com='max' then result:=txmax
    else if com='min' then result:=txmin
    else if com='sqr' then result:=txsqr
    else if com='sqrt' then result:=txsqrt
    else if com='inc' then result:=txinc
    else if com='dec' then result:=txdec
    else if com='if' then result:=txif
    else if com='drop' then result:=txdrop
    else if com='dup' then result:=txdup
    else if com='=posx' then result:=tx_posx
    else if com='=posy' then result:=tx_posy
    else if com='=pencolor' then result:=tx_pencolor
    else if com='=pensize' then result:=tx_pensize
    else if com='=brushcolor' then result:=tx_brushcolor
    else if com='=textcolor' then result:=tx_textcolor
    else if com='=textsize' then result:=tx_textsize
    else if com='=angle' then result:=tx_angle
    else if com='=markx' then result:=tx_markx
    else if com='=marky' then result:=tx_marky
    else if com='=loop' then result:=tx_loop
    else if com='=right' then result:=tx_right
    else if com='=left' then result:=tx_left
    else if com='=top' then result:= tx_top
    else if com='=bottom' then result:=tx_bottom
    else if IsNum(com) then result:=''
    else if IsCol(com) then result:=''
    else if IsVar(com) then result:=''
    else result:=txUser(com);

end;

constructor TjvTurtle.Create(AOwner: Tcomponent);
begin
 inherited create(Aowner);
 fPosition:=point(0,0);
 fMark:=fPosition;
 fHeading:=0;
 fPenDown:=false;
 fArea:=rect(0,0,0,0);
end;

destructor TjvTurtle.Destroy;
begin
inherited destroy;
end;

procedure TjvTurtle.DoRepaintRequest;
begin
  if assigned (FonRepaintRequest) then
    FonRepaintRequest(self);
end;

function TjvTurtle.getcol(var col: Tcolor): boolean;
var token,msg:string;
    num:integer;
begin
result:=false;
if gettoken(token) then begin
  if token='=' then begin
  result:=true;
  if Npop(msg,num) then
    col:=num
    else
    result:=false;
  end
  else
  try
   col:=stringtocolor(variant(token));
   result:=true
  except
   result:=false;
  end;
 end;
end;

function TjvTurtle.inpot(token:string;var num:integer):boolean;
var i:integer;
    s:string;
begin
result:=false;
s:=pot.Values [token];
if s='' then exit;
try
 num:=strtoint(s);
 result:=true;
 except
 result:=false;
 end;

end;

function TjvTurtle.getnum(var num: integer): boolean;
var token,msg:string;
    Anum:integer;
begin
  result:=false;
  if gettoken(token) then begin
    if token='=' then begin
    result:=npop(msg,num);
    end
    else if inpot(token,num) then begin
    result:=true;
    end
    else
    try
      num:=strtoint(token);
      result:=true;
    except
      result:=false;
    end;
  end;
end;

function TjvTurtle.gettex(var tex: string): boolean;
begin
tex:='';
result:=false;
while(ip<=ipmax) and (scrip[ip]<>'"') do inc(ip);
  if ip>ipmax then exit;
  inc(ip);
while(ip<=ipmax) and (scrip[ip]<>'"') do begin
  tex:=tex+scrip[ip];
  inc(ip);
  end;
if ip>ipmax then exit;
  inc(ip);
  result:=tex<>'';
end;

function TjvTurtle.gettoken(var token: string): boolean;
begin
token:='';
while(ip<=ipmax) and (scrip[ip]=' ') do inc(ip);
while(ip<=ipmax) and (scrip[ip]<>' ') do begin
  token:=token+scrip[ip];
  inc(ip)
  end;
result:= token<>'';
end;

function TjvTurtle.GetWidth: integer;
begin
if assigned(FCanvas) then
  result:=FCanvas.pen.Width
  else
  result:=1;
end;

function TjvTurtle.Interpret(var Epos:integer;s: string):string;
var msg:string;
begin
result:='#Error: Canvas not assigned';
  if not assigned(FCanvas) then exit;
s:=stringreplace(s,tab,' ',[rfreplaceall]);
s:=stringreplace(s,cr,'  ',[rfreplaceall]);
scrip:=s;
sp:=0;
ip:=1;
ipmax:=length(scrip);
if ipmax>0 then begin
   pot:=tstringlist.Create ;
   repeat
    msg:=docom;
   until msg<>'';
   result:=msg;
   Epos:=ip;
   pot.free;
  end
  else
   result:='empty script';
end;

procedure TjvTurtle.Turn(Aangle: real);
begin
fHeading:=fHeading+Aangle;
end;

procedure TjvTurtle.MoveBackward(ADistance: real);
var Rangle:real;
    dx,dy:real;
    newpoint:Tpoint;
begin
if not assigned(FCanvas) then exit;
Rangle:=fHeading*2*pi /360;
dx:=Adistance*cos(Rangle);
dy:=Adistance*sin(Rangle);
newpoint:=point(variant(fPosition.x-dx),variant(fPosition.y+dy));
FCanvas.MoveTo (fPosition.x,fPosition.y);
if FPenDown then
  FCanvas.LineTo (newpoint.x,newpoint.y)
  else
  FCanvas.MoveTo (newpoint.x,newpoint.y);
fPosition:=newpoint;
end;

procedure TjvTurtle.MoveForward(Adistance: real);
var Rangle:real;
    dx,dy:real;
    newpoint:Tpoint;
begin
if not assigned(FCanvas) then exit;
Rangle:=fHeading*2*pi /360;
dx:=Adistance*cos(Rangle);
dy:=Adistance*sin(Rangle);
newpoint:=point(variant(fPosition.x+dx),variant(fPosition.y-dy));
FCanvas.MoveTo (fPosition.x,fPosition.y);
if FPenDown then
  FCanvas.LineTo (newpoint.x,newpoint.y)
  else
  FCanvas.MoveTo (newpoint.x,newpoint.y);
fPosition:=newpoint;
end;

function TjvTurtle.pop(var num: integer): boolean;
begin
result:=false;
if sp>0 then begin
  dec(sp);
  num:=stack[sp];
  result:=true;
  end;
end;

function TjvTurtle.push(num: integer): boolean;
begin
result:=false;
if sp<255 then begin
  stack[sp]:=num;
  inc(sp);
  result:=true;
  end;
end;

procedure TjvTurtle.Right(AAngle: real);
begin
fHeading:=fHeading-Aangle;
end;

procedure TjvTurtle.SetArea(const Value: trect);
begin
  FArea := Value;
end;

procedure TjvTurtle.SetCanvas(const Value: Tcanvas);
begin
  FCanvas := Value;
end;




procedure TjvTurtle.SetHeading(const Value: real);
begin
  FHeading := Value;
end;


procedure TjvTurtle.SetMark(const Value: Tpoint);
begin
  FMark := Value;
end;

procedure TjvTurtle.SetOnRepaintRequest(const Value: TNotifyEvent);
begin
  FOnRepaintRequest := Value;
end;

procedure TjvTurtle.SetPenDown(const Value: boolean);
begin
  FPenDown := Value;
end;

procedure TjvTurtle.SetPosition(const Value: Tpoint);
begin
  FPosition := Value;
end;

procedure TjvTurtle.SetWidth(const Value: integer);
begin
  if assigned(FCanvas) then
    FCanvas.pen.width:= Value;
end;


function TjvTurtle.strtoCopyMode(var cm: Tcopymode; s: string): boolean;
begin
result:=true;
if s='cmblackness' then cm:=cmblackness
else if s='cmdstinvert' then cm:=cmDstInvert
else if s='cmmergecopy' then cm:=cmMergeCopy
else if s='cmmergepaint' then cm:=cmMergePaint
else if s='cmnotsrccopy' then cm:=cmNotSrcCopy
else if s='cmnotsrcerase' then cm:=cmNotSrcErase
else if s='cmpatcopy'then cm:=cmPatCopy
else if s='cmpatinvert' then cm:=cmPatInvert
else if s='cmpatpaint' then cm:=cmPatPaint
else if s='cmsrcand' then cm:=cmSrcAnd
else if s='cmsrccopy' then cm:=cmSrcCopy
else if s='cmsrcerase' then cm:=cmSrcErase
else if s='cmsrcinvert' then cm:=cmSrcInvert
else if s='cmscrpaint' then cm:=cmSrcPaint
else if s='cmwhiteness' then cm:=cmWhiteness
else result:=false;
end;

function TjvTurtle.strtoPenMode(var pm: Tpenmode; s: string): boolean;
begin
result:=true;
if s='pmBlack' then pm:=pmblack
else if s='pmwhite' then pm:=pmwhite
else if s='pmnop' then pm:=pmnop
else if s='pmnot' then pm:=pmnot
else if s='pmcopy' then pm:=pmcopy
else if s='pmnotcopy' then pm:=pmnotcopy
else if s='pmmergepennot' then pm:=pmmergepennot
else if s='pmmaskpennot' then pm:=pmmaskpennot
else if s='pmmergenotpen' then pm:=pmmergenotpen
else if s='pmmasknotpen' then pm:=pmmasknotpen
else if s='pmmerge' then pm:=pmmerge
else if s='pmnotmerge' then pm:=pmnotmerge
else if s='pmmask' then pm:=pmmask
else if s='pmnotmask' then pm:=pmnotmask
else if s='pmxor' then pm:=pmxor
else if s='pmnotxor' then pm:=pmnotxor
else result:=false;
end;

procedure TjvTurtle.TextRotate(x, y, angle: integer; atext: string;
  afont: tfont);
var dc:hdc;
    fnt:LogFont;
    plf:PLogFont;
    hfnt,hfntPrev:hfont;
    i:integer;
    fname,s:string;
begin
s:=atext;
fnt.lfEscapement :=angle*10;
fnt.lfOrientation :=angle*10;
if fsbold in afont.Style  then
  fnt.lfWeight :=FW_Bold
  else
  fnt.lfWeight :=FW_NORMAL;
if fsitalic in afont.style then
  fnt.lfItalic :=1
  else
  fnt.lfItalic :=0;
if fsunderline in afont.style then
  fnt.lfUnderline :=1
  else
  fnt.lfUnderline :=0;
fnt.lfStrikeOut :=0;
fnt.lfHeight :=abs(afont.Height);
fname:=afont.Name ;
for i:=1 to length(fname) do
  fnt.lffacename[i-1]:=fname[i];
fnt.lfFaceName [length(fname)]:=#0;
hfnt := CreateFontIndirect(fnt);
dc:=Fcanvas.handle;
SetBkMode(dc, TRANSPARENT);
SetTextColor(dc,afont.color);
hfntPrev := SelectObject(dc, hfnt);
//textout(dc,x,y,@atext[1],length(atext));
textout(dc,x,y,@s[1],length(s));
SelectObject(dc, hfntPrev);
DeleteObject(hfnt);
//mypaint.repaint;
end;


function TjvTurtle.txangle: string;
var x:integer;
begin
result:='invalid integer in angle';
 if getnum(x) then begin
   SetHeading(x);
   result:='';
   end;
end;

function TjvTurtle.txArea: string;
var x1,y1,x2,y2:integer;
begin
result:='invalid integer in area';
if getnum(x1) and getnum(y1) and getnum(x2) and getnum(y2) then begin
  FArea:=rect(x1,y1,x2,y2);
  result:='';
  end;
end;

function TjvTurtle.txbrushcolor: string;
var col:Tcolor;
begin
result:='invalid color in brushcolor';
if getcol(col) then begin
  Fcanvas.Brush.Color :=col;
  result:='';
  end;

end;

function TjvTurtle.txbsclear: string;
begin
FCanvas.brush.Style :=bsclear;
result:='';
end;

function TjvTurtle.txbssolid: string;
begin
FCanvas.brush.Style :=bssolid;
result:='';
end;

function TjvTurtle.txCopy: string;
var dest:trect;
    x,y:integer;
begin
x:=fPosition.x;
y:=fposition.y;
dest:=rect(x,y,x+FArea.right-FArea.left,y+FArea.bottom-Farea.top);
FCanvas.CopyRect (dest,FCanvas,Farea);
result:='';
end;

function TjvTurtle.txcopymode: string;
var s:string;
    Acopymode:TCopyMode;
begin
result:='invalid copy mode';
if gettoken(s) then begin
  s:='cm'+lowercase(s);
  if strtocopymode(Acopymode,s) then begin
  FCanvas.copymode :=Acopymode;
  result:='';
  end;
 end;
end;

function TjvTurtle.txdown: string;
begin
  FPenDown :=true;
  result:='';
end;

function TjvTurtle.txellipse: string;
var x1,y1,x2,y2:integer;
begin
result:='invalid integer in ellipse';
if getnum(x2) and getnum(y2) then begin
  x1:=Fposition.x;
  y1:=Fposition.y;
  x2:=x1+x2;
  y2:=y1+y2;
  FCanvas.ellipse(x1,y1,x2,y2);
  result:='';
  end;
end;

function TjvTurtle.txgo: string;
var x:integer;
begin
result:='invalid integer in go';
if getnum(x) then begin
  MoveForward(x);
  result:='';
  end;
end;



function TjvTurtle.txgomark: string;
begin
FCanvas.MoveTo (fPosition.x,fPosition.y);
if FPenDown then
  FCanvas.LineTo (fmark.x,fmark.y)
  else
  FCanvas.MoveTo (fmark.x,fmark.y);
fPosition:=fmark;
result:='';
end;


function TjvTurtle.txturn: string;
var x:integer;
begin
result:='invalid integer in turn';
if getnum(x) then begin
  turn(x);
  result:='';
  end;
end;

function TjvTurtle.txleft: string;
var x:integer;
begin
result:='invalid integer in left';
if getnum(x) then begin
  fHeading:=fHeading+x;
  result:='';
  end;
end;

function TjvTurtle.txright: string;
var x:integer;
begin
result:='invalid integer in right';
if getnum(x) then begin
  fHeading:=fHeading-x;
  result:='';
  end;
end;

function TjvTurtle.txmark: string;
begin
fmark:=fPosition;
result:='';

end;


function TjvTurtle.txpencolor: string;
var col:tcolor;
begin
result:='invalid color in pencolor';
if getcol(col) then begin
  FCanvas.pen.color :=col;
  result:='';
  end;
end;

function TjvTurtle.txpenmode: string;
var s:string;
    Apenmode:TPenMode;
begin
result:='invalid pen mode';
if gettoken(s) then begin
  s:='pm'+lowercase(s);
  if strtopenmode(Apenmode,s) then begin
    FCanvas.pen.Mode :=Apenmode;
    result:='';
  end;
 end;
end;

function TjvTurtle.txpensize: string;
var x:integer;
begin
result:='invalid integer in pensize';
  if getnum(x) then begin
  FCanvas.pen.Width :=x;
  result:='';
  end;
end;

function TjvTurtle.txpos: string;
var x,y:integer;
begin
result:='invalid integer in pos';
 if getnum(x) and getnum(y) then begin
 SetPosition(point(x,y));
 result:='';
 end;
end;

function TjvTurtle.txrectangle: string;
var x1,y1,x2,y2:integer;
begin
result:='invalid integer in rectangle';
if getnum(x2) and getnum(y2) then begin
  x1:=Fposition.x;
  y1:=Fposition.y;
  x2:=x1+x2;
  y2:=y1+y2;
  FCanvas.rectangle(x1,y1,x2,y2);
  result:='';
  end;

end;


function TjvTurtle.txtext: string;
var s:string;
    a:integer;
begin
result:='invalid text in text';
if gettex(s) then begin
  a:=variant(fHeading);
  Textrotate(Fposition.x,Fposition.y,a,s,Fcanvas.font);
  result:='';
  DoRepaintRequest;
  end;
end;

function TjvTurtle.txtextbold: string;
begin
FCanvas.Font.Style :=FCanvas.Font.style+[fsbold];
result:='';
end;

function TjvTurtle.txtextcolor: string;
var col:tcolor;
begin
result:='invalid color in textcolor';
if getcol(col) then begin
  Fcanvas.Font.Color :=col;
  result:='';
  end;
end;

function TjvTurtle.txtextfont: string;
var token:string;
begin
result:='missing fontname';
  if gettex(token) then begin
  FCanvas.font.name:=token;
  result:='';
  end;
end;

function TjvTurtle.txtextitalic: string;
begin
FCanvas.Font.Style :=FCanvas.Font.style+[fsitalic];
result:='';
end;

function TjvTurtle.txtextnormal: string;
begin
FCanvas.Font.Style :=[];
result:='';
end;

function TjvTurtle.txtextsize: string;
var x:integer;
begin
result:='invalid integer in fontsize';
if getnum(x) then begin
  FCanvas.font.size:=x;
  result:='';
  end;
end;

function TjvTurtle.txtextunderline: string;
begin
FCanvas.Font.Style :=FCanvas.Font.style+[fsunderline];
result:='';
end;

function TjvTurtle.txup: string;
begin
 FPenDown :=false;
 result:='';
end;

function TjvTurtle.txdo: string;
var num:integer;
begin
result:='number expected in do';
if not getnum(num) then exit;
result:='stack overflow';
if not push(ip) then exit;
if not push(num) then exit;
result:='';
end;

function TjvTurtle.txloop: string;
var reps,ret:integer;
begin
result:='stack underflow';
if not pop(reps) then exit;
if not pop(ret) then exit;
dec(reps);
if reps<>0 then begin
  ip:=ret;
  push(ret);
  push(reps);
  end;
result:='';
end;

function TjvTurtle.txflood: string;
var x,y,xf,yf:integer;
    floodcolor:Tcolor;
begin
result:='invalid integer in flood';
if getnum(x) and getnum(y) then begin
  xf:=Fposition.x+x;
  yf:=Fposition.y+y;
  floodcolor:=FCanvas.Pixels [xf,yf];
  FCanvas.FloodFill (xf,yf,floodcolor,fssurface);
  result:='';
  end;
end;

procedure TjvTurtle.SetOnRequestBackGround(
  const Value: TRequestBackGroundEvent);
begin
  FOnRequestBackGround := Value;
end;


procedure TjvTurtle.DoRequestBackground;
begin
  if assigned (FonRequestBackGround) then
    FonRequestBackground(self, background);

end;

function TjvTurtle.txbackground: string;
var aname:string;
begin
result:='invalid text in background';
if gettex(aname) then begin
  background:=aname;
  DoRequestBackground;
  result:='';
  end;
end;

function TjvTurtle.txtextout: string;
var s:string;
begin
result:='invalid text in text';
if gettex(s) then begin
  FCanvas.TextOut (Fposition.x,Fposition.y,s);
  result:='';
  end;
end;

function TjvTurtle.txaddbrushcolor: string;
var Acolor:tcolor;
begin
result:='invalid color in addbrushcolor';
if getcol(Acolor) then begin
   Fcanvas.brush.color:=Fcanvas.brush.color+Acolor;
   result:='';
   end;
end;

function TjvTurtle.txaddpencolor: string;
var Acolor:tcolor;
begin
result:='invalid color in addbrushcolor';
if getcol(Acolor) then begin
   FCanvas.pen.color:=FCanvas.pen.color+Acolor;
   result:='';
   end;
end;

function TjvTurtle.txgomarkangle: string;
begin
Fheading:=anglemark;
result:='';
end;

function TjvTurtle.txmarkangle: string;
begin
anglemark:=variant(FHeading);
result:='';
end;

function TjvTurtle.IsCol(tex: string): boolean;
var
    col:tcolor;
    msg:string;
begin
try
  col:=stringtocolor(variant(tex));
  result:=Npush(msg,col);
  except
  result:=false;
  end;
end;

function TjvTurtle.IsNum(tex: string): boolean;
var num:integer;
    msg:string;
begin
try
  num:=strtoint(tex);
  result:=Npush(msg,num);
  except
  result:=false;
  end;
end;

function TjvTurtle.Npop(var msg:string;var num:integer):boolean;
begin
msg:='number stack underflow';
result:=false;
if nsp=0 then exit;
dec(nsp);
num:=nstack[nsp];
msg:='';
result:=true;
end;

function TjvTurtle.Npush(var msg:string;num: integer):boolean;
begin
msg:='number stack overflow';
result:=false;
if nsp>=nspmax then exit;
nstack[nsp]:=num;
inc(nsp);
msg:='';
result:=true;
end;

function TjvTurtle.txcomment: string;
begin
result:='missing } after comment';
while (ip<=ipmax) and (scrip[ip]<>'}') do
  inc(ip);
if ip<=ipmax then begin
  inc(ip);
  result:='';
  end;
end;

function TjvTurtle.skipblock: boolean;
begin
result:=false;
while (ip<=ipmax) and (scrip[ip]<>'[') do
  inc(ip);
if ip>ipmax then exit;
inc(ip);
while (ip<=ipmax) and (scrip[ip]<>']') do
  inc(ip);
if ip>ipmax then exit;
inc(ip);
result:=true;  
end;

procedure TjvTurtle.SetOnRequestImageSize(const Value: TRequestImageSizeEvent);
begin
  FOnRequestImageSize := Value;
end;

function TjvTurtle.DoRequestImageSize:boolean;
begin
  if assigned (FonRequestImagesize) then begin
    FonRequestImageSize(self, Imagerect);
    result:=true;
    end
    else
    result:=false;
end;

function TjvTurtle.txIbottom: string;
var newpoint:tpoint;
begin
result:='error in gobottom';
if DorequestImageSize then begin
  newpoint:=point(Fposition.x,ImageRect.bottom);
  FCanvas.MoveTo (fPosition.x,fPosition.y);
  if FPenDown then
    FCanvas.LineTo (newpoint.x,newpoint.y)
    else
   FCanvas.MoveTo (newpoint.x,newpoint.y);
  fPosition:=newpoint;
  result:='';
  end;
end;

function TjvTurtle.txIleft: string;
var newpoint:tpoint;
begin
result:='error in goleft';
if DorequestImageSize then begin
  newpoint:=point(ImageRect.left,fPosition.y);
  FCanvas.MoveTo (fPosition.x,fPosition.y);
  if FPenDown then
    FCanvas.LineTo (newpoint.x,newpoint.y)
    else
   FCanvas.MoveTo (newpoint.x,newpoint.y);
  fPosition:=newpoint;
  result:='';
  end;
end;

function TjvTurtle.txIright: string;
var newpoint:tpoint;
begin
result:='error in goright';
if DorequestImageSize then begin
  newpoint:=point(ImageRect.right,fPosition.y);
  FCanvas.MoveTo (fPosition.x,fPosition.y);
  if FPenDown then
    FCanvas.LineTo (newpoint.x,newpoint.y)
    else
   FCanvas.MoveTo (newpoint.x,newpoint.y);
  fPosition:=newpoint;
  result:='';
  end;
end;

function TjvTurtle.txItop: string;
var newpoint:tpoint;
begin
result:='error in gotop';
if DorequestImageSize then begin
  newpoint:=point(Fposition.x,ImageRect.top);
  FCanvas.MoveTo (fPosition.x,fPosition.y);
  if FPenDown then
    FCanvas.LineTo (newpoint.x,newpoint.y)
    else
   FCanvas.MoveTo (newpoint.x,newpoint.y);
  fPosition:=newpoint;
  result:='';
  end;
end;

function TjvTurtle.txdiv: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
if b<>0 then
  Npush(result,a div b)
  else
  result:='division by zero';
end;

function TjvTurtle.txdrop: string;
var a:integer;
begin
Npop(result,a);
end;

function TjvTurtle.txdup: string;
var a:integer;
begin
if not npop(result,a)then exit;
Npush(result,a);
Npush(result,a);
end;

function TjvTurtle.txmul: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
Npush(result,a*b);
end;

function TjvTurtle.txsub: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
Npush(result,a-b);
end;

function TjvTurtle.txsum: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
Npush(result,a+b);
end;





function TjvTurtle.txIcenter: string;
var cx,cy:integer;
    newpoint:tpoint;
begin
result:='error in gocenter';
if DorequestImageSize then begin
 cx:=(Imagerect.Right -Imagerect.Left) div 2 ;
 cy:=(Imagerect.bottom-Imagerect.Top) div 2 ;
 newpoint:=point(cx,cy);
 FCanvas.MoveTo (fPosition.x,fPosition.y);
 if FPenDown then
    FCanvas.LineTo (newpoint.x,newpoint.y)
    else
 FCanvas.MoveTo (newpoint.x,newpoint.y);
 fPosition:=newpoint;
 result:='';
 end;
end;


function TjvTurtle.txdiamond: string;
var i,x:integer;tdown:boolean;
begin
result:='invalid integer in diamond';
 if getnum(x) then begin
   tdown:=Fpendown;
   Fpendown:=true;
   turn(45);
   for i:=1 to 4 do begin
     moveforward(x);
     turn(-90);
     end;
   turn(-45);
   Fpendown:=tdown;
   result:='';
   end;
end;

function TjvTurtle.txcurve: string;
var pts:array[0..3] of Tpoint;
    i,px,py:integer;
begin
result:='invalid parameter in curve';
if getnum(pts[1].x) and getnum(pts[1].y)
  and getnum(pts[2].x) and getnum (pts[2].y)
    and getnum(pts[3].x) and getnum (pts[3].y) then
  begin
  px:=Fposition.x;
  py:=Fposition.y;
  pts[0].x:=px;
  pts[0].y:=py;
  for i:=1 to 3 do begin
   pts[i].x:=px+pts[i].x;
   pts[i].y:=py+pts[i].y;
   end;
  FCanvas.PolyBezier (pts);
  FPosition:=pts[3];
  result:='';
  end;
end;

function TjvTurtle.txmove: string;
var x,y,x0,y0:integer;
begin
result:='invalid integer in move';
 if getnum(x) and getnum(y) then begin
 x0:=Fposition.x;
 y0:=Fposition.y;
 SetPosition(point(x0+x,y0+y));
 result:='';
 end;
end;

procedure TjvTurtle.SetOnRequestFilter(const Value: TRequestFilterEvent);
begin
  FOnRequestFilter := Value;
end;

procedure TjvTurtle.DoRequestFilter;
begin
  if assigned (FonRequestFilter) then
    FonRequestFilter(self, Filter);

end;

function TjvTurtle.txfilter: string;
var aname:string;
begin
result:='invalid text in filter';
if gettex(aname) then begin
  filter:=aname;
  DoRequestFilter;
  result:='';
  end;
end;



function TjvTurtle.txUser(sym:string): string;
var p:integer;
begin
result:='symbol '+sym+' is not defined';
p:=pos(sym,scrip);
if p=0 then exit;
result:='stack overflow';
if push(ip) then begin
  ip:=p+length(sym);
  result:='';
  end;
end;

function TjvTurtle.txblock: string;
begin
result:='missing ] after block';
while (ip<=ipmax) and (scrip[ip]<>']') do
  inc(ip);
if ip<=ipmax then begin
  inc(ip);
  result:='';
  end;
end;

function TjvTurtle.txreturn: string;
var num:integer;
begin
result:='stack underflow';
if pop(num) then begin
  ip:=num;
  result:='';
  end;
end;

function TjvTurtle.tx_angle: string;
var num:integer;
begin
num:=variant(Fheading);
npush(result,num);
end;

function TjvTurtle.tx_bottom: string;
begin
result:='error in =bottom';
if DorequestImageSize then begin
  npush(result,ImageRect.bottom);
  end;
end;

function TjvTurtle.tx_brushcolor: string;
begin
npush(result,FCanvas.brush.color);
end;

function TjvTurtle.tx_left: string;
begin
result:='error in =left';
if DorequestImageSize then begin
  npush(result,ImageRect.left);
  end;

end;

function TjvTurtle.tx_loop: string;
var num:integer;
begin
result:='stack underflow in =loop';
if not pop(num)then exit;
push(num);
npush(result,num);
end;

function TjvTurtle.tx_markx: string;
begin
npush(result,mark.x);
end;

function TjvTurtle.tx_marky: string;
begin
npush(result,mark.y);
end;

function TjvTurtle.tx_pencolor: string;
begin
npush(result,FCanvas.pen.color);
end;

function TjvTurtle.tx_posx: string;
begin
npush(result,Fposition.x);
end;

function TjvTurtle.tx_posy: string;
begin
npush(result,Fposition.y);
end;

function TjvTurtle.tx_right: string;
begin
result:='error in =right';
if DorequestImageSize then
  npush(result,ImageRect.right);
end;

function TjvTurtle.tx_top: string;
begin
result:='error in =top';
if DorequestImageSize then
  npush(result,ImageRect.top);
end;

function TjvTurtle.tx_pensize: string;
begin
npush(result,FCanvas.pen.Width);
end;

function TjvTurtle.tx_textcolor: string;
begin
npush(result,FCanvas.font.color);
end;

function TjvTurtle.tx_textsize: string;
begin
npush(result,FCanvas.font.size);
end;

function TjvTurtle.txif: string;
var num:integer;
    token:string;
begin
if not npop(result,num) then exit;
if num<>0 then exit;
if gettoken(token) then
  result:=''
  else
  result:='symbol expected after if';
end;

function TjvTurtle.txand: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
if (a<>0) and (b<>0) then
 Npush(result,1)
 else
 Npush(result,0)
end;

function TjvTurtle.txeq: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
if a=b then
 Npush(result,1)
 else
 Npush(result,0)
end;

function TjvTurtle.txge: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
if a>=b then
 Npush(result,1)
 else
 Npush(result,0)
end;

function TjvTurtle.txgt: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
if a>b then
 Npush(result,1)
 else
 Npush(result,0)
end;

function TjvTurtle.txle: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
if a<=b then
 Npush(result,1)
 else
 Npush(result,0)
end;

function TjvTurtle.txlt: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
if a<b then
 Npush(result,1)
 else
 Npush(result,0)
end;

function TjvTurtle.txne: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
if a<>b then
 Npush(result,1)
 else
 Npush(result,0)
end;

function TjvTurtle.txnot: string;
var a:integer;
begin
if not npop(result,a) then exit;
if a=0 then
 Npush(result,1)
 else
 Npush(result,0)
end;

function TjvTurtle.txor: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
if (a<>0) or (b<>0) then
 Npush(result,1)
 else
 Npush(result,0)
end;


function TjvTurtle.txabs: string;
var a:integer;
begin
if not npop(result,a) then exit;
 Npush(result,-a)
end;

function TjvTurtle.txneg: string;
var a:integer;
begin
if not npop(result,a) then exit;
 Npush(result,abs(a));
end;

function TjvTurtle.txswap: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
Npush(result,b);
Npush(result,a);
end;

function TjvTurtle.txmax: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
Npush(result,max(a,b));
end;

function TjvTurtle.txmin: string;
var b,a:integer;
begin
if not (npop(result,b) and npop(result,a)) then exit;
Npush(result,min(a,b));
end;

function TjvTurtle.txsqr: string;
var a:integer;
begin
if not npop(result,a) then exit;
 Npush(result,variant(sqr(a)));
end;

function TjvTurtle.txsqrt: string;
var a:integer;
begin
if not npop(result,a) then exit;
if a<>0 then
 Npush(result,variant(sqrt(a)))
 else
 result:='can not take sqrt of 0';
end;

function TjvTurtle.txdec: string;
var a:integer;
begin
if not npop(result,a) then exit;
 dec(a);
 Npush(result,a);
end;

function TjvTurtle.txinc: string;
var a:integer;
begin
if not npop(result,a) then exit;
 inc(a);
 Npush(result,a);
end;

function TjvTurtle.txpolygon: string;
var i,s,n:integer;tdown:boolean;
    ta,a:real;
    pt:tpoint;
begin
result:='invalid integer in polygon';
if not (getnum(n) and getnum(s)) then exit;
result:='0 not allowed in polygon';
if (n=0) or (s=0) then exit;
result:='need minimum of 3 sides in polygon';
if (n<3) then exit;
   ta:=Fheading;
   pt:=Fposition;
   tdown:=Fpendown;
   Fpendown:=true;
   a:=360 / n;
   for i:=1 to n-1 do begin
     moveforward(s);
     turn(a);
     end;
   Fcanvas.LineTo (pt.x,pt.y);
   Fpendown:=tdown;
   FHeading:=ta;
   Fposition:=pt;
   result:='';
end;

function TjvTurtle.txstar: string;
var i,s,am,n:integer;tdown:boolean;
    a,ta:real;
    pt:tpoint;
begin
result:='invalid integer in star';
if not (getnum(n) and getnum(s)) then exit;
result:='0 not allowed in star';
if (n=0) or (s=0) then exit;
result:='need minimum of 3 sides in star';
if (n<3) then exit;
result:='maximum 12 sides exceeded in star';
if (n>12) then exit;
   am:=1;
   case n of
   5:am:=2;
   7:am:=3;
   9:am:=4;
   11:am:=5;
   end;
   ta:=Fheading;
   pt:=Fposition;
   tdown:=Fpendown;
   Fpendown:=true;
   a:=am * 360 / n;
   for i:=1 to n-1 do begin
     moveforward(s);
     turn(a);
     end;
   Fcanvas.LineTo (pt.x,pt.y);
   Fpendown:=tdown;
   FHeading:=ta;
   Fposition:=pt;
   result:='';
end;

function TjvTurtle.txlineto: string;
var x,y,x0,y0:integer;
begin
result:='invalid integer in lineto';
 if getnum(x) and getnum(y) then begin
 x0:=Fposition.x;
 y0:=Fposition.y;
 SetPosition(point(x0+x,y0+y));
 Fcanvas.MoveTo (x0,y0);
 Fcanvas.LineTo (x0+x,y0+y);
 result:='';
 end;
end;

function TjvTurtle.txroundrect: string;
var x1,y1,x2,y2,rx,ry:integer;
begin
result:='invalid integer in roundrect';
if getnum(x2) and getnum(y2) and getnum(rx) and getnum(ry) then begin
  x1:=Fposition.x;
  y1:=Fposition.y;
  x2:=x1+x2;
  y2:=y1+y2;
  FCanvas.roundrect(x1,y1,x2,y2,rx,ry);
  result:='';
  end;

end;

function TjvTurtle.txdefault: string;
begin
result:='';
Fheading:=0;
Fposition:=point(0,0);
Fpendown:=false;
Fcanvas.Pen.color:=clblack;
Fcanvas.Brush.color:=clwhite;
Fcanvas.CopyMode :=cmsrccopy;
Fmark:=Fposition;
Farea:=rect(0,0,0,0);
end;

function TjvTurtle.txin: string;
var token:string;
    index,num:integer;
begin
if not npop(result,num) then exit;
  if gettoken(token) then begin
    index:=pot.IndexOfName (token);
    if index<0 then
     index:=pot.Add (token+'='+inttostr(num))
     else pot.Values [token]:=inttostr(num);
    result:='';
    end
    else
    result:='token expected';
end;

function TjvTurtle.IsVar(tex: string): boolean;
var num:integer;
    msg,s:string;
begin
  result:=false;
  s:=pot.values[tex];
  if s='' then exit;
  try
    num:=strtoint(s);
    result:=Npush(msg,num);
    except
    result:=false;
    end;
end;

function TjvTurtle.txinadd: string;
var token:string;
    index,num:integer;
begin
if not npop(result,num) then exit;
  if gettoken(token) then begin
    index:=pot.IndexOfName (token);
    result:='';
    if index>=0 then
     pot.values[token]:=inttostr(strtoint(pot.values[token])+num)
     else
     result:=token+ ' does not exist';
  end
  else
    result:='token expected';
end;

function TjvTurtle.txindiv: string;
var token:string;
    index,num:integer;
begin
if not npop(result,num) then exit;
  if gettoken(token) then begin
    index:=pot.IndexOfName (token);
    result:='';
    if index>=0 then
     pot.values[token]:=inttostr(strtoint(pot.values[token])-num)
     else
     result:=token+ ' does not exist';
  end
  else
    result:='token expected';
end;

function TjvTurtle.txinmult: string;
var token:string;
    index,num:integer;
begin
if not npop(result,num) then exit;
  if gettoken(token) then begin
    index:=pot.IndexOfName (token);
    result:='';
    if index>=0 then
     pot.values[token]:=inttostr(strtoint(pot.values[token])*num)
     else
     result:=token+ ' does not exist';
  end
  else
    result:='token expected';
end;

function TjvTurtle.txinsub: string;
var token:string;
    index,num:integer;
begin
if not npop(result,num) then exit;
if num=0 then begin
  result:='division by zero not allowed in in-';
  exit;
  end;
  if gettoken(token) then begin
    index:=pot.IndexOfName (token);
    result:='';
    if index>=0 then
     pot.values[token]:=inttostr(strtoint(pot.values[token]) div num)
     else
     result:=token+ ' does not exist';
  end
  else
    result:='token expected';
end;

function TjvTurtle.txindec: string;
var token:string;
    index:integer;
begin
  if gettoken(token) then begin
    index:=pot.IndexOfName (token);
    result:='';
    if index>=0 then
     pot.values[token]:=inttostr(strtoint(pot.values[token])-1)
     else
     result:=token+ ' does not exist';
  end
  else
    result:='token expected';
end;

function TjvTurtle.txininc: string;
var token:string;
    index:integer;
begin
  if gettoken(token) then begin
    index:=pot.IndexOfName (token);
    result:='';
    if index>=0 then
     pot.values[token]:=inttostr(strtoint(pot.values[token])+1)
     else
     result:=token+ ' does not exist';
  end
  else
    result:='token expected';
end;

end.
