{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTurtle.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}
unit JvTurtle;

interface
uses Windows, Messages, math, SysUtils, Classes, Graphics, Controls;

type
  TRequestBackGroundEvent = procedure(sender: Tobject; background: string) of object;
  TRequestFilterEvent = procedure(sender: Tobject; filter: string) of object;
  TRequestImageSizeEvent = procedure(sender: Tobject; var Arect: Trect) of object;

type
  TJvTurtle = class(TComponent)
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
    function gettoken(var token: string): boolean;
    function getnum(var num: integer): boolean;
    function inpot(token: string; var num: integer): boolean;
    function gettex(var tex: string): boolean;
    function getcol(var col: Tcolor): boolean;
//    function skipblock: boolean;
    function push(num: integer): boolean;
    function pop(var num: integer): boolean;
    function Npush(var msg: string; num: integer): boolean;
    function Npop(var msg: string; var num: integer): boolean;
    function IsNum(tex: string): boolean;
    function IsCol(tex: string): boolean;
    function IsVar(tex: string): boolean;
    procedure SetPosition(const Value: Tpoint);
    procedure SetHeading(const Value: real);
    procedure SetCanvas(const Value: Tcanvas);
    procedure SetPenDown(const Value: boolean);
    procedure SetWidth(const Value: integer);
    function GetWidth: integer;
    function txUser(sym: string): string;
    function txcomment: string;
    function txin: string;
    function txinadd: string;
    function txinsub: string;
    function txinmult: string;
    function txindiv: string;
    function txininc: string;
    function txindec: string;
    function txblock: string;
    function txreturn: string;
    function txpos: string;
    function txdefault: string;
    function txmove: string;
    function txlineto: string;
    function txangle: string;
    function txdown: string;
    function txup: string;
    function txpensize: string;
    function txpencolor: string;
    function txaddpencolor: string;
    function txaddbrushcolor: string;
    function txturn: string;
    function txleft: string;
    function txright: string;
    function txgo: string;
    function txtext: string;
    function txtextout: string;
    function txtextfont: string;
    function txtextsize: string;
    function txtextcolor: string;
    function txtextbold: string;
    function txtextitalic: string;
    function txtextunderline: string;
    function txtextnormal: string;
    function txbssolid: string;
    function txbsclear: string;
    function txbrushcolor: string;
    function txrectangle: string;
    function txroundrect: string;
    function txellipse: string;
    function txdiamond: string;
    function txpolygon: string;
    function txstar: string;
    function txcurve: string;
    function txmark: string;
    function txgomark: string;
    function txmarkangle: string;
    function txgomarkangle: string;
    function txArea: string;
    function txCopy: string;
    function txpenmode: string;
    function txcopymode: string;
    function txflood: string;
    function txdo: string;
    function txloop: string;
    function txIleft: string;
    function txItop: string;
    function txIright: string;
    function txIbottom: string;
    function txIcenter: string;
    function txsum: string;
    function txsub: string;
    function txmul: string;
    function txdiv: string;
    function txdup: string;
    function txdrop: string;
    function tx_posx: string;
    function tx_posy: string;
    function tx_pencolor: string;
    function tx_brushcolor: string;
    function tx_textcolor: string;
    function tx_pensize: string;
    function tx_textsize: string;
    function tx_angle: string;
    function tx_markx: string;
    function tx_marky: string;
    function tx_loop: string;
    function tx_right: string;
    function tx_left: string;
    function tx_top: string;
    function tx_bottom: string;
    function txif: string;
    function txgt: string;
    function txge: string;
    function txlt: string;
    function txle: string;
    function txeq: string;
    function txne: string;
    function txnot: string;
    function txand: string;
    function txor: string;
    function txneg: string;
    function txabs: string;
    function txswap: string;
    function txmax: string;
    function txmin: string;
    function txsqr: string;
    function txsqrt: string;
    function txinc: string;
    function txdec: string;
    function txbackground: string;
    function txfilter: string;
    function strtoPenMode(var pm: Tpenmode; s: string): boolean;
    function strtoCopyMode(var cm: Tcopymode; s: string): boolean;

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
    function DoRequestImageSize: boolean; virtual;
  public
    property Canvas: Tcanvas read FCanvas write SetCanvas;
    property Position: Tpoint read FPosition write SetPosition;
    property Mark: Tpoint read FMark write SetMark;
    property Area: trect read FArea write SetArea;
    property Heading: real read FHeading write SetHeading;
    property PenDown: boolean read FPenDown write SetPenDown;
    property Width: integer read GetWidth write SetWidth;
    function DoCom: string;
    procedure Turn(Aangle: real);
    procedure Right(AAngle: real);
    procedure MoveForward(Adistance: real);
    procedure MoveBackward(ADistance: real);
    function Interpret(var Epos: integer; s: string): string;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
    property OnRepaintRequest: TNotifyEvent read FOnRepaintRequest write SetOnRepaintRequest;
    property OnRequestBackGround: TRequestBackGroundEvent read FOnRequestBackGround write SetOnRequestBackGround;
    property OnRequestFilter: TRequestFilterEvent read FOnRequestFilter write SetOnRequestFilter;
    property OnRequestImageSize: TRequestImageSizeEvent read FOnRequestImageSize write SetOnRequestImageSize;
  end;


resourcestring
  sErrorCanvasNotAssigned = '#Error: Canvas not assigned';
  sEmptyScript = 'empty script';
  sInvalidIntegerIns = 'invalid integer in %s';
  sInvalidColorIns = 'invalid color in %s';
  sInvalidCopyMode = 'invalid copy mode';
  sInvalidPenMode = 'invalid pen mode';
  sInvalidTextIns = 'invalid text in %s';
  sMissingFontname = 'missing fontname';
  sNumberExpectedIns = 'number expected in %s';
  sStackOverflow = 'stack overflow';
  sStackUnderflow = 'stack underflow';
  sNumberStackUnderflow = 'number stack underflow';
  sNumberStackOverflow = 'number stack overflow';
  sMissingAfterComment = 'missing } after comment';
  sErrorIns = 'error in %s';
  sDivisionByZero = 'division by zero';
  sInvalidParameterIns = 'invalid parameter in %s';
  sSymbolsIsNotDefined = 'symbol %s is not defined';
  sMissingAfterBlock = 'missing ] after block';
  sStackUnderflowIns = 'stack underflow in %s';
  sSymbolExpectedAfterIf = 'symbol expected after if';
  sCanNotTakeSqrtOf = 'can not take sqrt of 0';
  sNotAllowedIns = '0 not allowed in %s';
  sNeedMinimumOfSidesIns = 'need minimum of 3 sides in %s';

implementation

uses
  JvConsts, JvTypes;

{ TJvTurtle }
const
  tab = chr(9);
  nspmax = 255;

var
  scrip: string;
  ip, ipmax, sp, nsp: integer;
  stack: array[0..255] of integer;
  Nstack: array[0..nspmax] of integer;
  background: string;
  pot: tstringlist;
  filter: string;
  anglemark: integer;
  ImageRect: Trect;

function TJvTurtle.docom: string;
var
  com: string;
begin
  Result := 'ready';
  if not gettoken(com) then Exit;
  if com = 'pos' then
    Result := txpos
  else if com = 'in' then
    Result := txin
  else if com = 'inadd' then
    Result := txinadd
  else if com = 'insub' then
    Result := txinsub
  else if com = 'inmul' then
    Result := txinmult
  else if com = 'indiv' then
    Result := txindiv
  else if com = 'ininc' then
    Result := txininc
  else if com = 'indec' then
    Result := txindec
  else if com = 'default' then
    Result := txdefault
  else if com = 'angle' then
    Result := txangle
  else if com = 'down' then
    Result := txdown
  else if com = 'up' then
    Result := txup
  else if com = 'pensize' then
    Result := txpensize
  else if com = 'pencolor' then
    Result := txpencolor
  else if com = 'turn' then
    Result := txturn
  else if com = 'right' then
    Result := txright
  else if com = 'left' then
    Result := txleft
  else if com = 'go' then
    Result := txgo
  else if com = 'move' then
    Result := txmove
  else if com = 'lineto' then
    Result := txlineto
  else if com = 'textfont' then
    Result := txtextfont
  else if com = 'textsize' then
    Result := txtextsize
  else if com = 'textcolor' then
    Result := txtextcolor
  else if com = 'addbrushcolor' then
    Result := txaddbrushcolor
  else if com = 'addpencolor' then
    Result := txaddpencolor
  else if com = 'text' then
    Result := txtext
  else if com = 'bold' then
    Result := txtextbold
  else if com = 'italic' then
    Result := txtextitalic
  else if com = 'underline' then
    Result := txtextunderline
  else if com = 'normal' then
    Result := txtextnormal
  else if com = 'textout' then
    Result := txtextout
  else if com = 'bssolid' then
    Result := txbssolid
  else if com = 'bsclear' then
    Result := txbsclear
  else if com = 'brushcolor' then
    Result := txbrushcolor
  else if com = 'rectangle' then
    Result := txrectangle
  else if com = 'roundrect' then
    Result := txroundrect
  else if com = 'ellipse' then
    Result := txellipse
  else if com = 'diamond' then
    Result := txdiamond
  else if com = 'polygon' then
    Result := txpolygon
  else if com = 'star' then
    Result := txstar
  else if com = 'curve' then
    Result := txcurve
  else if com = 'mark' then
    Result := txmark
  else if com = 'gomark' then
    Result := txgomark
  else if com = 'markangle' then
    Result := txmarkangle
  else if com = 'gomarkangle' then
    Result := txgomarkangle
  else if com = 'penmode' then
    Result := txpenmode
  else if com = 'copymode' then
    Result := txcopymode
  else if com = 'area' then
    Result := txArea
  else if com = 'copy' then
    Result := txcopy
  else if com = 'do' then
    Result := txdo
  else if com = 'loop' then
    Result := txloop
  else if com = 'flood' then
    Result := txflood
  else if com = 'background' then
    Result := txbackground
  else if com = 'filter' then
    Result := txfilter
  else if com = '{' then
    Result := txcomment
  else if com = '[' then
    Result := txblock
  else if com = ']' then
    Result := txreturn
  else if com = 'goleft' then
    Result := txIleft
  else if com = 'gotop' then
    Result := txItop
  else if com = 'goright' then
    Result := txIright
  else if com = 'gobottom' then
    Result := txIbottom
  else if com = 'gocenter' then
    Result := txIcenter
  else if com = '+' then
    Result := txsum
  else if com = '-' then
    Result := txsub
  else if com = '*' then
    Result := txmul
  else if com = '/' then
    Result := txdiv
  else if com = '.gt' then
    Result := txgt
  else if com = '.ge' then
    Result := txge
  else if com = '.lt' then
    Result := txlt
  else if com = '.le' then
    Result := txle
  else if com = '.eq' then
    Result := txeq
  else if com = '.ne' then
    Result := txne
  else if com = '.not' then
    Result := txnot
  else if com = '.and' then
    Result := txand
  else if com = '.or' then
    Result := txor
  else if com = 'neg' then
    Result := txneg
  else if com = 'abs' then
    Result := txabs
  else if com = 'swap' then
    Result := txswap
  else if com = 'max' then
    Result := txmax
  else if com = 'min' then
    Result := txmin
  else if com = 'sqr' then
    Result := txsqr
  else if com = 'sqrt' then
    Result := txsqrt
  else if com = 'inc' then
    Result := txinc
  else if com = 'dec' then
    Result := txdec
  else if com = 'if' then
    Result := txif
  else if com = 'drop' then
    Result := txdrop
  else if com = 'dup' then
    Result := txdup
  else if com = '=posx' then
    Result := tx_posx
  else if com = '=posy' then
    Result := tx_posy
  else if com = '=pencolor' then
    Result := tx_pencolor
  else if com = '=pensize' then
    Result := tx_pensize
  else if com = '=brushcolor' then
    Result := tx_brushcolor
  else if com = '=textcolor' then
    Result := tx_textcolor
  else if com = '=textsize' then
    Result := tx_textsize
  else if com = '=angle' then
    Result := tx_angle
  else if com = '=markx' then
    Result := tx_markx
  else if com = '=marky' then
    Result := tx_marky
  else if com = '=loop' then
    Result := tx_loop
  else if com = '=right' then
    Result := tx_right
  else if com = '=left' then
    Result := tx_left
  else if com = '=top' then
    Result := tx_top
  else if com = '=bottom' then
    Result := tx_bottom
  else if IsNum(com) then
    Result := ''
  else if IsCol(com) then
    Result := ''
  else if IsVar(com) then
    Result := ''
  else
    Result := txUser(com);

end;

constructor TJvTurtle.Create(AOwner: Tcomponent);
begin
  inherited create(Aowner);
  fPosition := point(0, 0);
  fMark := fPosition;
  fHeading := 0;
  fPenDown := false;
  fArea := rect(0, 0, 0, 0);
end;

destructor TJvTurtle.Destroy;
begin
  inherited destroy;
end;

procedure TJvTurtle.DoRepaintRequest;
begin
  if assigned(FonRepaintRequest) then
    FonRepaintRequest(self);
end;

function TJvTurtle.getcol(var col: Tcolor): boolean;
var
  token, msg: string;
  num: integer;
begin
  Result := false;
  if gettoken(token) then
  begin
    if token = '=' then
    begin
      Result := true;
      if Npop(msg, num) then
        col := num
      else
        Result := false;
    end
    else
    try
      col := stringtocolor(variant(token));
      Result := true
    except
      Result := false;
    end;
  end;
end;

function TJvTurtle.inpot(token: string; var num: integer): boolean;
var
//  i: integer;
  s: string;
begin
  Result := false;
  s := pot.Values[token];
  if s = '' then Exit;
  try
    num := strtoint(s);
    Result := true;
  except
    Result := false;
  end;

end;

function TJvTurtle.getnum(var num: integer): boolean;
var
  token, msg: string;
//  Anum: integer;
begin
  Result := false;
  if gettoken(token) then
  begin
    if token = '=' then
    begin
      Result := npop(msg, num);
    end
    else if inpot(token, num) then
    begin
      Result := true;
    end
    else
    try
      num := strtoint(token);
      Result := true;
    except
      Result := false;
    end;
  end;
end;

function TJvTurtle.gettex(var tex: string): boolean;
begin
  tex := '';
  Result := false;
  while (ip <= ipmax) and (scrip[ip] <> '"') do
    inc(ip);
  if ip > ipmax then Exit;
  inc(ip);
  while (ip <= ipmax) and (scrip[ip] <> '"') do
  begin
    tex := tex + scrip[ip];
    inc(ip);
  end;
  if ip > ipmax then Exit;
  inc(ip);
  Result := tex <> '';
end;

function TJvTurtle.gettoken(var token: string): boolean;
begin
  token := '';
  while (ip <= ipmax) and (scrip[ip] = ' ') do
    inc(ip);
  while (ip <= ipmax) and (scrip[ip] <> ' ') do
  begin
    token := token + scrip[ip];
    inc(ip)
  end;
  Result := token <> '';
end;

function TJvTurtle.GetWidth: integer;
begin
  if assigned(FCanvas) then
    Result := FCanvas.pen.Width
  else
    Result := 1;
end;

function TJvTurtle.Interpret(var Epos: integer; s: string): string;
var
  msg: string;
begin
  Result := sErrorCanvasNotAssigned;
  if not assigned(FCanvas) then Exit;
  s := stringreplace(s, tab, ' ', [rfreplaceall]);
  s := stringreplace(s, cr, '  ', [rfreplaceall]);
  scrip := s;
  sp := 0;
  ip := 1;
  ipmax := length(scrip);
  if ipmax > 0 then
  begin
    pot := tstringlist.Create;
    repeat
      msg := docom;
    until msg <> '';
    Result := msg;
    Epos := ip;
    pot.free;
  end
  else
    Result := sEmptyScript;
end;

procedure TJvTurtle.Turn(Aangle: real);
begin
  fHeading := fHeading + Aangle;
end;

procedure TJvTurtle.MoveBackward(ADistance: real);
var
  Rangle: real;
  dx, dy: real;
  newpoint: Tpoint;
begin
  if not assigned(FCanvas) then Exit;
  Rangle := fHeading * 2 * pi / 360;
  dx := Adistance * cos(Rangle);
  dy := Adistance * sin(Rangle);
  newpoint := point(variant(fPosition.x - dx), variant(fPosition.y + dy));
  FCanvas.MoveTo(fPosition.x, fPosition.y);
  if FPenDown then
    FCanvas.LineTo(newpoint.x, newpoint.y)
  else
    FCanvas.MoveTo(newpoint.x, newpoint.y);
  fPosition := newpoint;
end;

procedure TJvTurtle.MoveForward(Adistance: real);
var
  Rangle: real;
  dx, dy: real;
  newpoint: Tpoint;
begin
  if not assigned(FCanvas) then Exit;
  Rangle := fHeading * 2 * pi / 360;
  dx := Adistance * cos(Rangle);
  dy := Adistance * sin(Rangle);
  newpoint := point(variant(fPosition.x + dx), variant(fPosition.y - dy));
  FCanvas.MoveTo(fPosition.x, fPosition.y);
  if FPenDown then
    FCanvas.LineTo(newpoint.x, newpoint.y)
  else
    FCanvas.MoveTo(newpoint.x, newpoint.y);
  fPosition := newpoint;
end;

function TJvTurtle.pop(var num: integer): boolean;
begin
  Result := false;
  if sp > 0 then
  begin
    dec(sp);
    num := stack[sp];
    Result := true;
  end;
end;

function TJvTurtle.push(num: integer): boolean;
begin
  Result := false;
  if sp < 255 then
  begin
    stack[sp] := num;
    inc(sp);
    Result := true;
  end;
end;

procedure TJvTurtle.Right(AAngle: real);
begin
  fHeading := fHeading - Aangle;
end;

procedure TJvTurtle.SetArea(const Value: trect);
begin
  FArea := Value;
end;

procedure TJvTurtle.SetCanvas(const Value: Tcanvas);
begin
  FCanvas := Value;
end;

procedure TJvTurtle.SetHeading(const Value: real);
begin
  FHeading := Value;
end;

procedure TJvTurtle.SetMark(const Value: Tpoint);
begin
  FMark := Value;
end;

procedure TJvTurtle.SetOnRepaintRequest(const Value: TNotifyEvent);
begin
  FOnRepaintRequest := Value;
end;

procedure TJvTurtle.SetPenDown(const Value: boolean);
begin
  FPenDown := Value;
end;

procedure TJvTurtle.SetPosition(const Value: Tpoint);
begin
  FPosition := Value;
end;

procedure TJvTurtle.SetWidth(const Value: integer);
begin
  if assigned(FCanvas) then
    FCanvas.pen.width := Value;
end;

function TJvTurtle.strtoCopyMode(var cm: Tcopymode; s: string): boolean;
begin
  Result := true;
  if s = 'cmblackness' then
    cm := cmblackness
  else if s = 'cmdstinvert' then
    cm := cmDstInvert
  else if s = 'cmmergecopy' then
    cm := cmMergeCopy
  else if s = 'cmmergepaint' then
    cm := cmMergePaint
  else if s = 'cmnotsrccopy' then
    cm := cmNotSrcCopy
  else if s = 'cmnotsrcerase' then
    cm := cmNotSrcErase
  else if s = 'cmpatcopy' then
    cm := cmPatCopy
  else if s = 'cmpatinvert' then
    cm := cmPatInvert
  else if s = 'cmpatpaint' then
    cm := cmPatPaint
  else if s = 'cmsrcand' then
    cm := cmSrcAnd
  else if s = 'cmsrccopy' then
    cm := cmSrcCopy
  else if s = 'cmsrcerase' then
    cm := cmSrcErase
  else if s = 'cmsrcinvert' then
    cm := cmSrcInvert
  else if s = 'cmscrpaint' then
    cm := cmSrcPaint
  else if s = 'cmwhiteness' then
    cm := cmWhiteness
  else
    Result := false;
end;

function TJvTurtle.strtoPenMode(var pm: Tpenmode; s: string): boolean;
begin
  Result := true;
  if s = 'pmBlack' then
    pm := pmblack
  else if s = 'pmwhite' then
    pm := pmwhite
  else if s = 'pmnop' then
    pm := pmnop
  else if s = 'pmnot' then
    pm := pmnot
  else if s = 'pmcopy' then
    pm := pmcopy
  else if s = 'pmnotcopy' then
    pm := pmnotcopy
  else if s = 'pmmergepennot' then
    pm := pmmergepennot
  else if s = 'pmmaskpennot' then
    pm := pmmaskpennot
  else if s = 'pmmergenotpen' then
    pm := pmmergenotpen
  else if s = 'pmmasknotpen' then
    pm := pmmasknotpen
  else if s = 'pmmerge' then
    pm := pmmerge
  else if s = 'pmnotmerge' then
    pm := pmnotmerge
  else if s = 'pmmask' then
    pm := pmmask
  else if s = 'pmnotmask' then
    pm := pmnotmask
  else if s = 'pmxor' then
    pm := pmxor
  else if s = 'pmnotxor' then
    pm := pmnotxor
  else
    Result := false;
end;

procedure TJvTurtle.TextRotate(x, y, angle: integer; atext: string;
  afont: tfont);
var
  dc: hdc;
  fnt: LogFont;
//  plf: PLogFont;
  hfnt, hfntPrev: hfont;
  i: integer;
  fname, s: string;
begin
  s := atext;
  fnt.lfEscapement := angle * 10;
  fnt.lfOrientation := angle * 10;
  if fsbold in afont.Style then
    fnt.lfWeight := FW_Bold
  else
    fnt.lfWeight := FW_NORMAL;
  if fsitalic in afont.style then
    fnt.lfItalic := 1
  else
    fnt.lfItalic := 0;
  if fsunderline in afont.style then
    fnt.lfUnderline := 1
  else
    fnt.lfUnderline := 0;
  fnt.lfStrikeOut := 0;
  fnt.lfHeight := abs(afont.Height);
  fname := afont.Name;
  for i := 1 to length(fname) do
    fnt.lffacename[i - 1] := fname[i];
  fnt.lfFaceName[length(fname)] := #0;
  hfnt := CreateFontIndirect(fnt);
  dc := Fcanvas.handle;
  SetBkMode(dc, TRANSPARENT);
  SetTextColor(dc, afont.color);
  hfntPrev := SelectObject(dc, hfnt);
  //textout(dc,x,y,@atext[1],length(atext));
  textout(dc, x, y, @s[1], length(s));
  SelectObject(dc, hfntPrev);
  DeleteObject(hfnt);
  //mypaint.repaint;
end;

function TJvTurtle.txangle: string;
var
  x: integer;
begin
  Result := Format(sInvalidIntegerIns, ['angle']);
  if getnum(x) then
  begin
    SetHeading(x);
    Result := '';
  end;
end;

function TJvTurtle.txArea: string;
var
  x1, y1, x2, y2: integer;
begin
  Result := Format(sInvalidIntegerIns, ['area']);
  if getnum(x1) and getnum(y1) and getnum(x2) and getnum(y2) then
  begin
    FArea := rect(x1, y1, x2, y2);
    Result := '';
  end;
end;

function TJvTurtle.txbrushcolor: string;
var
  col: Tcolor;
begin
  Result := Format(sInvalidColorIns, ['brushcolor']);
  if getcol(col) then
  begin
    Fcanvas.Brush.Color := col;
    Result := '';
  end;

end;

function TJvTurtle.txbsclear: string;
begin
  FCanvas.brush.Style := bsclear;
  Result := '';
end;

function TJvTurtle.txbssolid: string;
begin
  FCanvas.brush.Style := bssolid;
  Result := '';
end;

function TJvTurtle.txCopy: string;
var
  dest: trect;
  x, y: integer;
begin
  x := fPosition.x;
  y := fposition.y;
  dest := rect(x, y, x + FArea.right - FArea.left, y + FArea.bottom - Farea.top);
  FCanvas.CopyRect(dest, FCanvas, Farea);
  Result := '';
end;

function TJvTurtle.txcopymode: string;
var
  s: string;
  Acopymode: TCopyMode;
begin
  Result := sInvalidCopyMode;
  if gettoken(s) then
  begin
    s := 'cm' + lowercase(s);
    if strtocopymode(Acopymode, s) then
    begin
      FCanvas.copymode := Acopymode;
      Result := '';
    end;
  end;
end;

function TJvTurtle.txdown: string;
begin
  FPenDown := true;
  Result := '';
end;

function TJvTurtle.txellipse: string;
var
  x1, y1, x2, y2: integer;
begin
  Result := Format(sInvalidIntegerIns, ['ellipse']);
  if getnum(x2) and getnum(y2) then
  begin
    x1 := Fposition.x;
    y1 := Fposition.y;
    x2 := x1 + x2;
    y2 := y1 + y2;
    FCanvas.ellipse(x1, y1, x2, y2);
    Result := '';
  end;
end;

function TJvTurtle.txgo: string;
var
  x: integer;
begin
  Result := Format(sInvalidIntegerIns, ['go']);
  if getnum(x) then
  begin
    MoveForward(x);
    Result := '';
  end;
end;

function TJvTurtle.txgomark: string;
begin
  FCanvas.MoveTo(fPosition.x, fPosition.y);
  if FPenDown then
    FCanvas.LineTo(fmark.x, fmark.y)
  else
    FCanvas.MoveTo(fmark.x, fmark.y);
  fPosition := fmark;
  Result := '';
end;

function TJvTurtle.txturn: string;
var
  x: integer;
begin
  Result := Format(sInvalidIntegerIns, ['turn']);
  if getnum(x) then
  begin
    turn(x);
    Result := '';
  end;
end;

function TJvTurtle.txleft: string;
var
  x: integer;
begin
  Result := Format(sInvalidIntegerIns, ['left']);
  if getnum(x) then
  begin
    fHeading := fHeading + x;
    Result := '';
  end;
end;

function TJvTurtle.txright: string;
var
  x: integer;
begin
  Result := Format(sInvalidIntegerIns, ['right']);
  if getnum(x) then
  begin
    fHeading := fHeading - x;
    Result := '';
  end;
end;

function TJvTurtle.txmark: string;
begin
  fmark := fPosition;
  Result := '';

end;

function TJvTurtle.txpencolor: string;
var
  col: tcolor;
begin
  Result := Format(sInvalidColorIns, ['pencolor']);
  if getcol(col) then
  begin
    FCanvas.pen.color := col;
    Result := '';
  end;
end;

function TJvTurtle.txpenmode: string;
var
  s: string;
  Apenmode: TPenMode;
begin
  Result := sInvalidPenMode;
  if gettoken(s) then
  begin
    s := 'pm' + lowercase(s);
    if strtopenmode(Apenmode, s) then
    begin
      FCanvas.pen.Mode := Apenmode;
      Result := '';
    end;
  end;
end;

function TJvTurtle.txpensize: string;
var
  x: integer;
begin
  Result := Format(sInvalidIntegerIns, ['pensize']);
  if getnum(x) then
  begin
    FCanvas.pen.Width := x;
    Result := '';
  end;
end;

function TJvTurtle.txpos: string;
var
  x, y: integer;
begin
  Result := Format(sInvalidIntegerIns, ['pos']);
  if getnum(x) and getnum(y) then
  begin
    SetPosition(point(x, y));
    Result := '';
  end;
end;

function TJvTurtle.txrectangle: string;
var
  x1, y1, x2, y2: integer;
begin
  Result := Format(sInvalidIntegerIns, ['rectangle']);
  if getnum(x2) and getnum(y2) then
  begin
    x1 := Fposition.x;
    y1 := Fposition.y;
    x2 := x1 + x2;
    y2 := y1 + y2;
    FCanvas.rectangle(x1, y1, x2, y2);
    Result := '';
  end;

end;

function TJvTurtle.txtext: string;
var
  s: string;
  a: integer;
begin
  Result := Format(sInvalidTextIns, ['text']);
  if gettex(s) then
  begin
    a := variant(fHeading);
    Textrotate(Fposition.x, Fposition.y, a, s, Fcanvas.font);
    Result := '';
    DoRepaintRequest;
  end;
end;

function TJvTurtle.txtextbold: string;
begin
  FCanvas.Font.Style := FCanvas.Font.style + [fsbold];
  Result := '';
end;

function TJvTurtle.txtextcolor: string;
var
  col: tcolor;
begin
  Result := Format(sInvalidColorIns, ['textcolor']);
  if getcol(col) then
  begin
    Fcanvas.Font.Color := col;
    Result := '';
  end;
end;

function TJvTurtle.txtextfont: string;
var
  token: string;
begin
  Result := sMissingFontname;
  if gettex(token) then
  begin
    FCanvas.font.name := token;
    Result := '';
  end;
end;

function TJvTurtle.txtextitalic: string;
begin
  FCanvas.Font.Style := FCanvas.Font.style + [fsitalic];
  Result := '';
end;

function TJvTurtle.txtextnormal: string;
begin
  FCanvas.Font.Style := [];
  Result := '';
end;

function TJvTurtle.txtextsize: string;
var
  x: integer;
begin
  Result := Format(sInvalidIntegerIns, ['fontsize']);
  if getnum(x) then
  begin
    FCanvas.font.size := x;
    Result := '';
  end;
end;

function TJvTurtle.txtextunderline: string;
begin
  FCanvas.Font.Style := FCanvas.Font.style + [fsunderline];
  Result := '';
end;

function TJvTurtle.txup: string;
begin
  FPenDown := false;
  Result := '';
end;

function TJvTurtle.txdo: string;
var
  num: integer;
begin
  Result := Format(sNumberExpectedIns, ['do']);
  if not getnum(num) then Exit;
  Result := sStackOverflow;
  if not push(ip) then Exit;
  if not push(num) then Exit;
  Result := '';
end;

function TJvTurtle.txloop: string;
var
  reps, ret: integer;
begin
  Result := sStackUnderflow;
  if not pop(reps) then Exit;
  if not pop(ret) then Exit;
  dec(reps);
  if reps <> 0 then
  begin
    ip := ret;
    push(ret);
    push(reps);
  end;
  Result := '';
end;

function TJvTurtle.txflood: string;
var
  x, y, xf, yf: integer;
  floodcolor: Tcolor;
begin
  Result := Format(sInvalidIntegerIns, ['flood']);
  if getnum(x) and getnum(y) then
  begin
    xf := Fposition.x + x;
    yf := Fposition.y + y;
    floodcolor := FCanvas.Pixels[xf, yf];
    FCanvas.FloodFill(xf, yf, floodcolor, fssurface);
    Result := '';
  end;
end;

procedure TJvTurtle.SetOnRequestBackGround(
  const Value: TRequestBackGroundEvent);
begin
  FOnRequestBackGround := Value;
end;

procedure TJvTurtle.DoRequestBackground;
begin
  if assigned(FonRequestBackGround) then
    FonRequestBackground(self, background);

end;

function TJvTurtle.txbackground: string;
var
  aname: string;
begin
  Result := Format(sInvalidTextIns, ['background']);
  if gettex(aname) then
  begin
    background := aname;
    DoRequestBackground;
    Result := '';
  end;
end;

function TJvTurtle.txtextout: string;
var
  s: string;
begin
  Result := Format(sInvalidTextIns, ['text']);
  if gettex(s) then
  begin
    FCanvas.TextOut(Fposition.x, Fposition.y, s);
    Result := '';
  end;
end;

function TJvTurtle.txaddbrushcolor: string;
var
  Acolor: tcolor;
begin
  Result := Format(sInvalidColorIns, ['addbrushcolor']);
  if getcol(Acolor) then
  begin
    Fcanvas.brush.color := Fcanvas.brush.color + Acolor;
    Result := '';
  end;
end;

function TJvTurtle.txaddpencolor: string;
var
  Acolor: tcolor;
begin
  Result := Format(sInvalidColorIns, ['addbrushcolor']);
  if getcol(Acolor) then
  begin
    FCanvas.pen.color := FCanvas.pen.color + Acolor;
    Result := '';
  end;
end;

function TJvTurtle.txgomarkangle: string;
begin
  Fheading := anglemark;
  Result := '';
end;

function TJvTurtle.txmarkangle: string;
begin
  anglemark := variant(FHeading);
  Result := '';
end;

function TJvTurtle.IsCol(tex: string): boolean;
var
  col: tcolor;
  msg: string;
begin
  try
    col := stringtocolor(variant(tex));
    Result := Npush(msg, col);
  except
    Result := false;
  end;
end;

function TJvTurtle.IsNum(tex: string): boolean;
var
  num: integer;
  msg: string;
begin
  try
    num := strtoint(tex);
    Result := Npush(msg, num);
  except
    Result := false;
  end;
end;

function TJvTurtle.Npop(var msg: string; var num: integer): boolean;
begin
  msg := sNumberStackUnderflow;
  Result := false;
  if nsp = 0 then Exit;
  dec(nsp);
  num := nstack[nsp];
  msg := '';
  Result := true;
end;

function TJvTurtle.Npush(var msg: string; num: integer): boolean;
begin
  msg := sNumberStackOverflow;
  Result := false;
  if nsp >= nspmax then Exit;
  nstack[nsp] := num;
  inc(nsp);
  msg := '';
  Result := true;
end;

function TJvTurtle.txcomment: string;
begin
  Result := sMissingAfterComment;
  while (ip <= ipmax) and (scrip[ip] <> '}') do
    inc(ip);
  if ip <= ipmax then
  begin
    inc(ip);
    Result := '';
  end;
end;
(*)
function TJvTurtle.skipblock: boolean;
begin
  Result := false;
  while (ip <= ipmax) and (scrip[ip] <> '[') do
    inc(ip);
  if ip > ipmax then Exit;
  inc(ip);
  while (ip <= ipmax) and (scrip[ip] <> ']') do
    inc(ip);
  if ip > ipmax then Exit;
  inc(ip);
  Result := true;
end;
(*)
procedure TJvTurtle.SetOnRequestImageSize(const Value: TRequestImageSizeEvent);
begin
  FOnRequestImageSize := Value;
end;

function TJvTurtle.DoRequestImageSize: boolean;
begin
  if assigned(FonRequestImagesize) then
  begin
    FonRequestImageSize(self, Imagerect);
    Result := true;
  end
  else
    Result := false;
end;

function TJvTurtle.txIbottom: string;
var
  newpoint: tpoint;
begin
  Result := Format(sErrorIns, ['gobottom']);
  if DorequestImageSize then
  begin
    newpoint := point(Fposition.x, ImageRect.bottom);
    FCanvas.MoveTo(fPosition.x, fPosition.y);
    if FPenDown then
      FCanvas.LineTo(newpoint.x, newpoint.y)
    else
      FCanvas.MoveTo(newpoint.x, newpoint.y);
    fPosition := newpoint;
    Result := '';
  end;
end;

function TJvTurtle.txIleft: string;
var
  newpoint: tpoint;
begin
  Result := Format(sErrorIns, ['goleft']);
  if DorequestImageSize then
  begin
    newpoint := point(ImageRect.left, fPosition.y);
    FCanvas.MoveTo(fPosition.x, fPosition.y);
    if FPenDown then
      FCanvas.LineTo(newpoint.x, newpoint.y)
    else
      FCanvas.MoveTo(newpoint.x, newpoint.y);
    fPosition := newpoint;
    Result := '';
  end;
end;

function TJvTurtle.txIright: string;
var
  newpoint: tpoint;
begin
  Result := Format(sErrorIns, ['goright']);
  if DorequestImageSize then
  begin
    newpoint := point(ImageRect.right, fPosition.y);
    FCanvas.MoveTo(fPosition.x, fPosition.y);
    if FPenDown then
      FCanvas.LineTo(newpoint.x, newpoint.y)
    else
      FCanvas.MoveTo(newpoint.x, newpoint.y);
    fPosition := newpoint;
    Result := '';
  end;
end;

function TJvTurtle.txItop: string;
var
  newpoint: tpoint;
begin
  Result := Format(sErrorIns, ['gotop']);
  if DorequestImageSize then
  begin
    newpoint := point(Fposition.x, ImageRect.top);
    FCanvas.MoveTo(fPosition.x, fPosition.y);
    if FPenDown then
      FCanvas.LineTo(newpoint.x, newpoint.y)
    else
      FCanvas.MoveTo(newpoint.x, newpoint.y);
    fPosition := newpoint;
    Result := '';
  end;
end;

function TJvTurtle.txdiv: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  if b <> 0 then
    Npush(Result, a div b)
  else
    Result := sDivisionByZero;
end;

function TJvTurtle.txdrop: string;
var
  a: integer;
begin
  Npop(Result, a);
end;

function TJvTurtle.txdup: string;
var
  a: integer;
begin
  if not npop(Result, a) then Exit;
  Npush(Result, a);
  Npush(Result, a);
end;

function TJvTurtle.txmul: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  Npush(Result, a * b);
end;

function TJvTurtle.txsub: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  Npush(Result, a - b);
end;

function TJvTurtle.txsum: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  Npush(Result, a + b);
end;

function TJvTurtle.txIcenter: string;
var
  cx, cy: integer;
  newpoint: tpoint;
begin
  Result := Format(sErrorIns, ['gocenter']);
  if DorequestImageSize then
  begin
    cx := (Imagerect.Right - Imagerect.Left) div 2;
    cy := (Imagerect.bottom - Imagerect.Top) div 2;
    newpoint := point(cx, cy);
    FCanvas.MoveTo(fPosition.x, fPosition.y);
    if FPenDown then
      FCanvas.LineTo(newpoint.x, newpoint.y)
    else
      FCanvas.MoveTo(newpoint.x, newpoint.y);
    fPosition := newpoint;
    Result := '';
  end;
end;

function TJvTurtle.txdiamond: string;
var
  i, x: integer;
  tdown: boolean;
begin
  Result := Format(sInvalidIntegerIns, ['diamond']);
  if getnum(x) then
  begin
    tdown := Fpendown;
    Fpendown := true;
    turn(45);
    for i := 1 to 4 do
    begin
      moveforward(x);
      turn(-90);
    end;
    turn(-45);
    Fpendown := tdown;
    Result := '';
  end;
end;

function TJvTurtle.txcurve: string;
var
  pts: array[0..3] of Tpoint;
  i, px, py: integer;
begin
  Result := Format(sInvalidParameterIns, ['curve']);
  if getnum(pts[1].x) and getnum(pts[1].y)
    and getnum(pts[2].x) and getnum(pts[2].y)
    and getnum(pts[3].x) and getnum(pts[3].y) then
  begin
    px := Fposition.x;
    py := Fposition.y;
    pts[0].x := px;
    pts[0].y := py;
    for i := 1 to 3 do
    begin
      pts[i].x := px + pts[i].x;
      pts[i].y := py + pts[i].y;
    end;
    FCanvas.PolyBezier(pts);
    FPosition := pts[3];
    Result := '';
  end;
end;

function TJvTurtle.txmove: string;
var
  x, y, x0, y0: integer;
begin
  Result := Format(sInvalidIntegerIns, ['move']);
  if getnum(x) and getnum(y) then
  begin
    x0 := Fposition.x;
    y0 := Fposition.y;
    SetPosition(point(x0 + x, y0 + y));
    Result := '';
  end;
end;

procedure TJvTurtle.SetOnRequestFilter(const Value: TRequestFilterEvent);
begin
  FOnRequestFilter := Value;
end;

procedure TJvTurtle.DoRequestFilter;
begin
  if assigned(FonRequestFilter) then
    FonRequestFilter(self, Filter);

end;

function TJvTurtle.txfilter: string;
var
  aname: string;
begin
  Result := Format(sInvalidTextIns, ['filter']);
  if gettex(aname) then
  begin
    filter := aname;
    DoRequestFilter;
    Result := '';
  end;
end;

function TJvTurtle.txUser(sym: string): string;
var
  p: integer;
begin
  Result := Format(sSymbolsIsNotDefined, [sym]);
  p := pos(sym, scrip);
  if p = 0 then Exit;
  Result := sStackOverflow;
  if push(ip) then
  begin
    ip := p + length(sym);
    Result := '';
  end;
end;

function TJvTurtle.txblock: string;
begin
  Result := sMissingAfterBlock;
  while (ip <= ipmax) and (scrip[ip] <> ']') do
    inc(ip);
  if ip <= ipmax then
  begin
    inc(ip);
    Result := '';
  end;
end;

function TJvTurtle.txreturn: string;
var
  num: integer;
begin
  Result := sStackUnderflow;
  if pop(num) then
  begin
    ip := num;
    Result := '';
  end;
end;

function TJvTurtle.tx_angle: string;
var
  num: integer;
begin
  num := variant(Fheading);
  npush(Result, num);
end;

function TJvTurtle.tx_bottom: string;
begin
  Result := Format(sErrorIns, ['=bottom']);
  if DorequestImageSize then
  begin
    npush(Result, ImageRect.bottom);
  end;
end;

function TJvTurtle.tx_brushcolor: string;
begin
  npush(Result, FCanvas.brush.color);
end;

function TJvTurtle.tx_left: string;
begin
  Result := Format(sErrorIns, ['=left']);
  if DorequestImageSize then
  begin
    npush(Result, ImageRect.left);
  end;

end;

function TJvTurtle.tx_loop: string;
var
  num: integer;
begin
  Result := Format(sStackUnderflowIns, ['=loop']);
  if not pop(num) then Exit;
  push(num);
  npush(Result, num);
end;

function TJvTurtle.tx_markx: string;
begin
  npush(Result, mark.x);
end;

function TJvTurtle.tx_marky: string;
begin
  npush(Result, mark.y);
end;

function TJvTurtle.tx_pencolor: string;
begin
  npush(Result, FCanvas.pen.color);
end;

function TJvTurtle.tx_posx: string;
begin
  npush(Result, Fposition.x);
end;

function TJvTurtle.tx_posy: string;
begin
  npush(Result, Fposition.y);
end;

function TJvTurtle.tx_right: string;
begin
  Result := Format(sErrorIns, ['=right']);
  if DorequestImageSize then
    npush(Result, ImageRect.right);
end;

function TJvTurtle.tx_top: string;
begin
  Result := Format(sErrorIns, ['=top']);
  if DorequestImageSize then
    npush(Result, ImageRect.top);
end;

function TJvTurtle.tx_pensize: string;
begin
  npush(Result, FCanvas.pen.Width);
end;

function TJvTurtle.tx_textcolor: string;
begin
  npush(Result, FCanvas.font.color);
end;

function TJvTurtle.tx_textsize: string;
begin
  npush(Result, FCanvas.font.size);
end;

function TJvTurtle.txif: string;
var
  num: integer;
  token: string;
begin
  if not npop(Result, num) then Exit;
  if num <> 0 then Exit;
  if gettoken(token) then
    Result := ''
  else
    Result := sSymbolExpectedAfterIf;
end;

function TJvTurtle.txand: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  if (a <> 0) and (b <> 0) then
    Npush(Result, 1)
  else
    Npush(Result, 0)
end;

function TJvTurtle.txeq: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  if a = b then
    Npush(Result, 1)
  else
    Npush(Result, 0)
end;

function TJvTurtle.txge: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  if a >= b then
    Npush(Result, 1)
  else
    Npush(Result, 0)
end;

function TJvTurtle.txgt: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  if a > b then
    Npush(Result, 1)
  else
    Npush(Result, 0)
end;

function TJvTurtle.txle: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  if a <= b then
    Npush(Result, 1)
  else
    Npush(Result, 0)
end;

function TJvTurtle.txlt: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  if a < b then
    Npush(Result, 1)
  else
    Npush(Result, 0)
end;

function TJvTurtle.txne: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  if a <> b then
    Npush(Result, 1)
  else
    Npush(Result, 0)
end;

function TJvTurtle.txnot: string;
var
  a: integer;
begin
  if not npop(Result, a) then Exit;
  if a = 0 then
    Npush(Result, 1)
  else
    Npush(Result, 0)
end;

function TJvTurtle.txor: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  if (a <> 0) or (b <> 0) then
    Npush(Result, 1)
  else
    Npush(Result, 0)
end;

function TJvTurtle.txabs: string;
var
  a: integer;
begin
  if not npop(Result, a) then Exit;
  Npush(Result, -a)
end;

function TJvTurtle.txneg: string;
var
  a: integer;
begin
  if not npop(Result, a) then Exit;
  Npush(Result, abs(a));
end;

function TJvTurtle.txswap: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  Npush(Result, b);
  Npush(Result, a);
end;

function TJvTurtle.txmax: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  Npush(Result, max(a, b));
end;

function TJvTurtle.txmin: string;
var
  b, a: integer;
begin
  if not (npop(Result, b) and npop(Result, a)) then Exit;
  Npush(Result, min(a, b));
end;

function TJvTurtle.txsqr: string;
var
  a: integer;
begin
  if not npop(Result, a) then Exit;
  Npush(Result, variant(sqr(a)));
end;

function TJvTurtle.txsqrt: string;
var
  a: integer;
begin
  if not npop(Result, a) then Exit;
  if a <> 0 then
    Npush(Result, variant(sqrt(a)))
  else
    Result := sCanNotTakeSqrtOf;
end;

function TJvTurtle.txdec: string;
var
  a: integer;
begin
  if not npop(Result, a) then Exit;
  dec(a);
  Npush(Result, a);
end;

function TJvTurtle.txinc: string;
var
  a: integer;
begin
  if not npop(Result, a) then Exit;
  inc(a);
  Npush(Result, a);
end;

function TJvTurtle.txpolygon: string;
var
  i, s, n: integer;
  tdown: boolean;
  ta, a: real;
  pt: tpoint;
begin
  Result := Format(sInvalidIntegerIns, ['polygon']);
  if not (getnum(n) and getnum(s)) then Exit;
  Result := Format(sNotAllowedIns, ['polygon']);
  if (n = 0) or (s = 0) then Exit;
  Result := Format(sNeedMinimumOfSidesIns, ['polygon']);
  if (n < 3) then Exit;
  ta := Fheading;
  pt := Fposition;
  tdown := Fpendown;
  Fpendown := true;
  a := 360 / n;
  for i := 1 to n - 1 do
  begin
    moveforward(s);
    turn(a);
  end;
  Fcanvas.LineTo(pt.x, pt.y);
  Fpendown := tdown;
  FHeading := ta;
  Fposition := pt;
  Result := '';
end;

function TJvTurtle.txstar: string;
var
  i, s, am, n: integer;
  tdown: boolean;
  a, ta: real;
  pt: tpoint;
begin
  Result := _('invalid integer in %s', ['star']);
  if not (getnum(n) and getnum(s)) then Exit;
  Result := _('0 not allowed in %s', ['star']);
  if (n = 0) or (s = 0) then Exit;
  Result := _('need minimum of 3 sides in %s', ['star']);
  if (n < 3) then Exit;
  Result := _('maximum 12 sides exceeded in %s', ['star']);
  if (n > 12) then Exit;
  am := 1;
  case n of
    5: am := 2;
    7: am := 3;
    9: am := 4;
    11: am := 5;
  end;
  ta := Fheading;
  pt := Fposition;
  tdown := Fpendown;
  Fpendown := true;
  a := am * 360 / n;
  for i := 1 to n - 1 do
  begin
    moveforward(s);
    turn(a);
  end;
  Fcanvas.LineTo(pt.x, pt.y);
  Fpendown := tdown;
  FHeading := ta;
  Fposition := pt;
  Result := '';
end;

function TJvTurtle.txlineto: string;
var
  x, y, x0, y0: integer;
begin
  Result := _('invalid integer in %s', ['lineto']);
  if getnum(x) and getnum(y) then
  begin
    x0 := Fposition.x;
    y0 := Fposition.y;
    SetPosition(point(x0 + x, y0 + y));
    Fcanvas.MoveTo(x0, y0);
    Fcanvas.LineTo(x0 + x, y0 + y);
    Result := '';
  end;
end;

function TJvTurtle.txroundrect: string;
var
  x1, y1, x2, y2, rx, ry: integer;
begin
  Result := _('invalid integer in %s', ['roundrect']);
  if getnum(x2) and getnum(y2) and getnum(rx) and getnum(ry) then
  begin
    x1 := Fposition.x;
    y1 := Fposition.y;
    x2 := x1 + x2;
    y2 := y1 + y2;
    FCanvas.roundrect(x1, y1, x2, y2, rx, ry);
    Result := '';
  end;

end;

function TJvTurtle.txdefault: string;
begin
  Result := '';
  Fheading := 0;
  Fposition := point(0, 0);
  Fpendown := false;
  Fcanvas.Pen.color := clblack;
  Fcanvas.Brush.color := clwhite;
  Fcanvas.CopyMode := cmsrccopy;
  Fmark := Fposition;
  Farea := rect(0, 0, 0, 0);
end;

function TJvTurtle.txin: string;
var
  token: string;
  index, num: integer;
begin
  if not npop(Result, num) then Exit;
  if gettoken(token) then
  begin
    index := pot.IndexOfName(token);
    if index < 0 then
      {index := }pot.Add(token + '=' + inttostr(num)) { TODO -oJVCL -cPOSSIBLEBUG : (p3) index is never used... }
    else
      pot.Values[token] := inttostr(num);
    Result := '';
  end
  else
    Result := _('token expected');
end;

function TJvTurtle.IsVar(tex: string): boolean;
var
  num: integer;
  msg, s: string;
begin
  Result := false;
  s := pot.values[tex];
  if s = '' then Exit;
  try
    num := strtoint(s);
    Result := Npush(msg, num);
  except
    Result := false;
  end;
end;

function TJvTurtle.txinadd: string;
var
  token: string;
  index, num: integer;
begin
  if not npop(Result, num) then Exit;
  if gettoken(token) then
  begin
    index := pot.IndexOfName(token);
    Result := '';
    if index >= 0 then
      pot.values[token] := inttostr(strtoint(pot.values[token]) + num)
    else
      Result := _('%s does not exist', [token]);
  end
  else
    Result := _('token expected');
end;

function TJvTurtle.txindiv: string;
var
  token: string;
  index, num: integer;
begin
  if not npop(Result, num) then Exit;
  if gettoken(token) then
  begin
    index := pot.IndexOfName(token);
    Result := '';
    if index >= 0 then
      pot.values[token] := inttostr(strtoint(pot.values[token]) - num)
    else
      Result := _('%s does not exist', [token]);
  end
  else
    Result := _('token expected');
end;

function TJvTurtle.txinmult: string;
var
  token: string;
  index, num: integer;
begin
  if not npop(Result, num) then Exit;
  if gettoken(token) then
  begin
    index := pot.IndexOfName(token);
    Result := '';
    if index >= 0 then
      pot.values[token] := inttostr(strtoint(pot.values[token]) * num)
    else
      Result := _('%s does not exist', [token]);
  end
  else
    Result := _('token expected');
end;

function TJvTurtle.txinsub: string;
var
  token: string;
  index, num: integer;
begin
  if not npop(Result, num) then Exit;
  if num = 0 then
  begin
    Result := _('division by zero not allowed in in-');
    Exit;
  end;
  if gettoken(token) then
  begin
    index := pot.IndexOfName(token);
    Result := '';
    if index >= 0 then
      pot.values[token] := inttostr(strtoint(pot.values[token]) div num)
    else
      Result := _('%s does not exist', [token]);
  end
  else
    Result := _('token expected');
end;

function TJvTurtle.txindec: string;
var
  token: string;
  index: integer;
begin
  if gettoken(token) then
  begin
    index := pot.IndexOfName(token);
    Result := '';
    if index >= 0 then
      pot.values[token] := inttostr(strtoint(pot.values[token]) - 1)
    else
      Result := _('%s does not exist', [token]);
  end
  else
    Result := _('token expected');
end;

function TJvTurtle.txininc: string;
var
  token: string;
  index: integer;
begin
  if gettoken(token) then
  begin
    index := pot.IndexOfName(token);
    Result := '';
    if index >= 0 then
      pot.values[token] := inttostr(strtoint(pot.values[token]) + 1)
    else
      Result := _('%s does not exist', [token]);
  end
  else
    Result := _('token expected');
end;

end.
