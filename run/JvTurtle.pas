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

uses
  Windows, Messages, Math, SysUtils, Classes, Graphics, Controls;

type
  TRequestBackGroundEvent = procedure(Sender: TObject; Background: string) of object;
  TRequestFilterEvent = procedure(Sender: TObject; Filter: string) of object;
  TRequestImageSizeEvent = procedure(Sender: TObject; var ARect: TRect) of object;

type
  TJvTurtle = class(TComponent)
  private
    FPosition: TPoint;
    FHeading: Real;
    FCanvas: TCanvas;
    FPenDown: Boolean;
    FMark: TPoint;
    FArea: TRect;
    FPot: TStringList;
    FOnRepaintRequest: TNotifyEvent;
    FOnRequestBackGround: TRequestBackGroundEvent;
    FOnRequestImageSize: TRequestImageSizeEvent;
    FOnRequestFilter: TRequestFilterEvent;
    function GetToken(var Token: string): Boolean;
    function GetNum(var Num: Integer): Boolean;
    function InPot(Token: string; var Num: Integer): Boolean;
    function GetTex(var Tex: string): Boolean;
    function GetCol(var Col: TColor): Boolean;
    // function SkipBlock: Boolean;
    function Push(Num: Integer): Boolean;
    function Pop(var Num: Integer): Boolean;
    function NPush(var Msg: string; Num: Integer): Boolean;
    function NPop(var Msg: string; var Num: Integer): Boolean;
    function IsNum(Tex: string): Boolean;
    function IsCol(Tex: string): Boolean;
    function IsVar(Tex: string): Boolean;
    procedure SetPosition(const Value: TPoint);
    procedure SetHeading(const Value: Real);
    procedure SetCanvas(const Value: TCanvas);
    procedure SetPenDown(const Value: Boolean);
    procedure SetPenWidth(const Value: Integer);
    function GetWidth: Integer;
    procedure DoGo(Dest: TPoint);
    function txUser(Sym: string): string;
    function txComment: string;
    function txIn: string;
    function txInAdd: string;
    function txInSub: string;
    function txInMult: string;
    function txInDiv: string;
    function txInInc: string;
    function txInDec: string;
    function txBlock: string;
    function txReturn: string;
    function txPos: string;
    function txDefault: string;
    function txMove: string;
    function txLineTo: string;
    function txAngle: string;
    function txDown: string;
    function txUp: string;
    function txPenSize: string;
    function txPenColor: string;
    function txAddPenColor: string;
    function txAddBrushColor: string;
    function txTurn: string;
    function txLeft: string;
    function txRight: string;
    function txGo: string;
    function txText: string;
    function txTextOut: string;
    function txTextFont: string;
    function txTextSize: string;
    function txTextColor: string;
    function txTextBold: string;
    function txTextItalic: string;
    function txTextUnderline: string;
    function txTextNormal: string;
    function txBsSolid: string;
    function txBsClear: string;
    function txBrushColor: string;
    function txRectangle: string;
    function txRoundRect: string;
    function txEllipse: string;
    function txDiamond: string;
    function txPolygon: string;
    function txStar: string;
    function txCurve: string;
    function txMark: string;
    function txGoMark: string;
    function txMarkAngle: string;
    function txGoMarkAngle: string;
    function txArea: string;
    function txCopy: string;
    function txPenMode: string;
    function txCopyMode: string;
    function txFlood: string;
    function txDo: string;
    function txLoop: string;
    function txGoLeft: string;
    function txGoTop: string;
    function txGoRight: string;
    function txGoBottom: string;
    function txGoCenter: string;
    function txAdd: string;
    function txSub: string;
    function txMul: string;
    function txDiv: string;
    function txDup: string;
    function txDrop: string;
    function tx_PosX: string;
    function tx_PosY: string;
    function tx_PenColor: string;
    function tx_BrushColor: string;
    function tx_TextColor: string;
    function tx_PenSize: string;
    function tx_TextSize: string;
    function tx_Angle: string;
    function tx_MarkX: string;
    function tx_MarkY: string;
    function tx_Loop: string;
    function tx_Right: string;
    function tx_Left: string;
    function tx_Top: string;
    function tx_Bottom: string;
    function txIf: string;
    function txGt: string;
    function txGe: string;
    function txLt: string;
    function txLe: string;
    function txEq: string;
    function txNe: string;
    function txNot: string;
    function txAnd: string;
    function txOr: string;
    function txNeg: string;
    function txAbs: string;
    function txSwap: string;
    function txMax: string;
    function txMin: string;
    function txSqr: string;
    function txSqrt: string;
    function txInc: string;
    function txDec: string;
    function txBackground: string;
    function txFilter: string;
    function StrToPenMode(var Pm: TPenMode; S: string): Boolean;
    function StrToCopyMode(var Cm: TCopyMode; S: string): Boolean;

    procedure TextRotate(X, Y, Angle: Integer; AText: string;
      AFont: TFont);
    procedure SetOnRepaintRequest(const Value: TNotifyEvent);
    procedure SetMark(const Value: TPoint);
    procedure SetArea(const Value: TRect);
    procedure SetOnRequestBackGround(const Value: TRequestBackGroundEvent);
    procedure SetOnRequestImageSize(const Value: TRequestImageSizeEvent);
    procedure SetOnRequestFilter(const Value: TRequestFilterEvent);
  protected
    procedure DoRepaintRequest; virtual;
    procedure DoRequestBackground; virtual;
    procedure DoRequestFilter; virtual;
    function DoRequestImageSize: Boolean; virtual;
  public
    property Canvas: TCanvas read FCanvas write SetCanvas;
    property Position: TPoint read FPosition write SetPosition;
    property Mark: TPoint read FMark write SetMark;
    property Area: TRect read FArea write SetArea;
    property Heading: Real read FHeading write SetHeading;
    property PenDown: Boolean read FPenDown write SetPenDown;
    property PenWidth: Integer read GetWidth write SetPenWidth;
    function DoCom: string;
    procedure Turn(AAngle: Real);
    procedure Right(AAngle: Real);
    procedure MoveForward(ADistance: Real);
    procedure MoveBackward(ADistance: Real);
    function Interpret(out APos: Integer; S: string): string;
    constructor Create(AOwner: TComponent); override;
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
  sMaximumSidesExceededIns = 'maximum 12 sides exceeded in %s';
  sTokenExpected = 'token expected';
  ssDoesNotExist = '%s does not exist';
  sDivisionByZeroNotAllowedInIn = 'division by zero not allowed in in-';

implementation

uses
  JvConsts, JvTypes;

const
  cStackMax = 255;
  cNStackMax = 255;

var
  Script: string;
  IP, IPMax, SP, NSP: Integer;
  Stack: array [0..cStackMax] of Integer;
  NStack: array [0..cNStackMax] of Integer;
  Background: string;
  Filter: string;
  AngleMark: Integer;
  ImageRect: TRect;

constructor TJvTurtle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPot := TStringList.Create;
  txDefault;
end;

destructor TJvTurtle.Destroy;
begin
  FPot.Free;
  inherited Destroy;
end;

function TJvTurtle.DoCom: string;
var
  Com: string;
begin
  Result := 'ready';
  if not GetToken(Com) then
    Exit;
  if Com = 'pos' then
    Result := txPos
  else
  if Com = 'in' then
    Result := txIn
  else
  if Com = 'inadd' then
    Result := txInAdd
  else
  if Com = 'insub' then
    Result := txInSub
  else
  if Com = 'inmul' then
    Result := txInMult
  else
  if Com = 'indiv' then
    Result := txInDiv
  else
  if Com = 'ininc' then
    Result := txInInc
  else
  if Com = 'indec' then
    Result := txInDec
  else
  if Com = 'default' then
    Result := txDefault
  else
  if Com = 'angle' then
    Result := txAngle
  else
  if Com = 'down' then
    Result := txDown
  else
  if Com = 'up' then
    Result := txUp
  else
  if Com = 'pensize' then
    Result := txPenSize
  else
  if Com = 'pencolor' then
    Result := txPenColor
  else
  if Com = 'turn' then
    Result := txTurn
  else
  if Com = 'right' then
    Result := txRight
  else
  if Com = 'left' then
    Result := txLeft
  else
  if Com = 'go' then
    Result := txGo
  else
  if Com = 'move' then
    Result := txMove
  else
  if Com = 'lineto' then
    Result := txLineTo
  else
  if Com = 'textfont' then
    Result := txTextFont
  else
  if Com = 'textsize' then
    Result := txTextSize
  else
  if Com = 'textcolor' then
    Result := txTextColor
  else
  if Com = 'addbrushcolor' then
    Result := txAddBrushColor
  else
  if Com = 'addpencolor' then
    Result := txAddPenColor
  else
  if Com = 'text' then
    Result := txText
  else
  if Com = 'bold' then
    Result := txTextBold
  else
  if Com = 'italic' then
    Result := txTextItalic
  else
  if Com = 'underline' then
    Result := txTextUnderline
  else
  if Com = 'normal' then
    Result := txTextNormal
  else
  if Com = 'textout' then
    Result := txTextOut
  else
  if Com = 'bssolid' then
    Result := txBsSolid
  else
  if Com = 'bsclear' then
    Result := txBsClear
  else
  if Com = 'brushcolor' then
    Result := txBrushColor
  else
  if Com = 'rectangle' then
    Result := txRectangle
  else
  if Com = 'roundrect' then
    Result := txRoundRect
  else
  if Com = 'ellipse' then
    Result := txEllipse
  else
  if Com = 'diamond' then
    Result := txDiamond
  else
  if Com = 'polygon' then
    Result := txPolygon
  else
  if Com = 'star' then
    Result := txStar
  else
  if Com = 'curve' then
    Result := txCurve
  else
  if Com = 'mark' then
    Result := txMark
  else
  if Com = 'gomark' then
    Result := txGoMark
  else
  if Com = 'markangle' then
    Result := txMarkAngle
  else
  if Com = 'gomarkangle' then
    Result := txGoMarkAngle
  else
  if Com = 'penmode' then
    Result := txPenMode
  else
  if Com = 'copymode' then
    Result := txCopyMode
  else
  if Com = 'area' then
    Result := txArea
  else
  if Com = 'copy' then
    Result := txCopy
  else
  if Com = 'do' then
    Result := txDo
  else
  if Com = 'loop' then
    Result := txLoop
  else
  if Com = 'flood' then
    Result := txFlood
  else
  if Com = 'background' then
    Result := txBackground
  else
  if Com = 'filter' then
    Result := txFilter
  else
  if Com = '{' then
    Result := txComment
  else
  if Com = '[' then
    Result := txBlock
  else
  if Com = ']' then
    Result := txReturn
  else
  if Com = 'goleft' then
    Result := txGoLeft
  else
  if Com = 'gotop' then
    Result := txGoTop
  else
  if Com = 'goright' then
    Result := txGoRight
  else
  if Com = 'gobottom' then
    Result := txGoBottom
  else
  if Com = 'gocenter' then
    Result := txGoCenter
  else
  if Com = '+' then
    Result := txAdd
  else
  if Com = '-' then
    Result := txSub
  else
  if Com = '*' then
    Result := txMul
  else
  if Com = '/' then
    Result := txDiv
  else
  if Com = '.gt' then
    Result := txGt
  else
  if Com = '.ge' then
    Result := txGe
  else
  if Com = '.lt' then
    Result := txLt
  else
  if Com = '.le' then
    Result := txLe
  else
  if Com = '.eq' then
    Result := txEq
  else
  if Com = '.ne' then
    Result := txNe
  else
  if Com = '.not' then
    Result := txNot
  else
  if Com = '.and' then
    Result := txAnd
  else
  if Com = '.or' then
    Result := txOr
  else
  if Com = 'neg' then
    Result := txNeg
  else
  if Com = 'abs' then
    Result := txAbs
  else
  if Com = 'swap' then
    Result := txSwap
  else
  if Com = 'max' then
    Result := txMax
  else
  if Com = 'min' then
    Result := txMin
  else
  if Com = 'sqr' then
    Result := txSqr
  else
  if Com = 'sqrt' then
    Result := txSqrt
  else
  if Com = 'inc' then
    Result := txInc
  else
  if Com = 'dec' then
    Result := txDec
  else
  if Com = 'if' then
    Result := txIf
  else
  if Com = 'drop' then
    Result := txDrop
  else
  if Com = 'dup' then
    Result := txDup
  else
  if Com = '=posx' then
    Result := tx_PosX
  else
  if Com = '=posy' then
    Result := tx_PosY
  else
  if Com = '=pencolor' then
    Result := tx_PenColor
  else
  if Com = '=pensize' then
    Result := tx_PenSize
  else
  if Com = '=brushcolor' then
    Result := tx_BrushColor
  else
  if Com = '=textcolor' then
    Result := tx_TextColor
  else
  if Com = '=textsize' then
    Result := tx_TextSize
  else
  if Com = '=angle' then
    Result := tx_Angle
  else
  if Com = '=markx' then
    Result := tx_MarkX
  else
  if Com = '=marky' then
    Result := tx_MarkY
  else
  if Com = '=loop' then
    Result := tx_Loop
  else
  if Com = '=right' then
    Result := tx_Right
  else
  if Com = '=left' then
    Result := tx_Left
  else
  if Com = '=top' then
    Result := tx_Top
  else
  if Com = '=bottom' then
    Result := tx_Bottom
  else
  if IsNum(Com) then
    Result := ''
  else
  if IsCol(Com) then
    Result := ''
  else
  if IsVar(Com) then
    Result := ''
  else
    Result := txUser(Com);

end;

procedure TJvTurtle.DoRepaintRequest;
begin
  if Assigned(FOnRepaintRequest) then
    FOnRepaintRequest(Self);
end;

function TJvTurtle.GetCol(var Col: TColor): Boolean;
var
  Token, Msg: string;
  Num: Integer;
begin
  Result := False;
  if GetToken(Token) then
    if Token = '=' then
    begin
      Result := True;
      if NPop(Msg, Num) then
        Col := Num
      else
        Result := False;
    end
    else
      try
        Col := StringToColor(Variant(Token));
        Result := True;
      except
        Result := False;
      end;
end;

function TJvTurtle.InPot(Token: string; var Num: Integer): Boolean;
var
  S: string;
begin
  Result := False;
  S := FPot.Values[Token];
  if S <> '' then
    try
      Num := StrToInt(S);
      Result := True;
    except
      Result := False;
    end;
end;

function TJvTurtle.GetNum(var Num: Integer): Boolean;
var
  Token, Msg: string;
begin
  Result := False;
  if GetToken(Token) then
    if Token = '=' then
      Result := NPop(Msg, Num)
    else
    if InPot(Token, Num) then
      Result := True
    else
      try
        Num := StrToInt(Token);
        Result := True;
      except
        Result := False;
      end;
end;

function TJvTurtle.GetTex(var Tex: string): Boolean;
begin
  Tex := '';
  Result := False;
  while (IP <= IPMax) and (Script[IP] <> '"') do
    Inc(IP);
  if IP > IPMax then
    Exit;
  Inc(IP);
  while (IP <= IPMax) and (Script[IP] <> '"') do
  begin
    Tex := Tex + Script[IP];
    Inc(IP);
  end;
  if IP > IPMax then
    Exit;
  Inc(IP);
  Result := Tex <> '';
end;

function TJvTurtle.GetToken(var Token: string): Boolean;
begin
  Token := '';
  while (IP <= IPMax) and (Script[IP] = ' ') do
    Inc(IP);
  while (IP <= IPMax) and (Script[IP] <> ' ') do
  begin
    Token := Token + Script[IP];
    Inc(IP);
  end;
  Result := Token <> '';
end;

function TJvTurtle.GetWidth: Integer;
begin
  if Assigned(FCanvas) then
    Result := FCanvas.Pen.Width
  else
    Result := 1;
end;

function TJvTurtle.Interpret(out APos: Integer; S: string): string;
var
  Msg: string;
begin
  Result := sErrorCanvasNotAssigned;
  if not Assigned(FCanvas) then
    Exit;
  S := StringReplace(S, Tab, ' ', [rfReplaceAll]);
  S := StringReplace(S, Cr,  ' ', [rfReplaceAll]);
  S := StringReplace(S, Lf,  ' ', [rfReplaceAll]);
  Script := S;
  SP := 0;
  IP := 1;
  IPMax := Length(Script);
  if IPMax > 0 then
  begin
    FPot.Clear;
    repeat
      Msg := DoCom;
    until Msg <> '';
    Result := Msg;
    APos := IP;
  end
  else
    Result := sEmptyScript;
end;

procedure TJvTurtle.DoGo(Dest: TPoint);
begin
  Canvas.MoveTo(Position.X, Position.Y);
  if PenDown then
    Canvas.LineTo(Dest.X, Dest.Y)
  else
    Canvas.MoveTo(Dest.X, Dest.Y);
  Position := Dest;
end;

procedure TJvTurtle.Turn(AAngle: Real);
begin
  Heading := Heading + AAngle;
end;

procedure TJvTurtle.MoveBackward(ADistance: Real);
var
  RAngle: Real;
  dX, dY: Real;
  NewPoint: TPoint;
begin
  if not Assigned(FCanvas) then
    Exit;
  RAngle := Heading * 2 * Pi / 360;
  dX := ADistance * Cos(RAngle);
  dY := ADistance * Sin(RAngle);
  NewPoint := Point(Variant(Position.X - dX), Variant(Position.Y + dY));
  DoGo(NewPoint);
end;

procedure TJvTurtle.MoveForward(ADistance: Real);
var
  RAngle: Real;
  dX, dY: Real;
  NewPoint: TPoint;
begin
  if not Assigned(FCanvas) then
    Exit;
  RAngle := Heading * 2 * pi / 360;
  dX := ADistance * Cos(RAngle);
  dY := ADistance * Sin(RAngle);
  NewPoint := Point(Variant(Position.X + dX), Variant(Position.Y - dY));
  DoGo(NewPoint);
end;

function TJvTurtle.Pop(var Num: Integer): Boolean;
begin
  Result := False;
  if SP > 0 then
  begin
    Dec(SP);
    Num := Stack[SP];
    Result := True;
  end;
end;

function TJvTurtle.Push(Num: Integer): Boolean;
begin
  Result := False;
  if SP < cStackMax then
  begin
    Stack[SP] := Num;
    Inc(SP);
    Result := True;
  end;
end;

procedure TJvTurtle.Right(AAngle: Real);
begin
  Heading := Heading - AAngle;
end;

procedure TJvTurtle.SetArea(const Value: TRect);
begin
  FArea := Value;
end;

procedure TJvTurtle.SetCanvas(const Value: TCanvas);
begin
  FCanvas := Value;
end;

procedure TJvTurtle.SetHeading(const Value: Real);
begin
  FHeading := Value;
end;

procedure TJvTurtle.SetMark(const Value: TPoint);
begin
  FMark := Value;
end;

procedure TJvTurtle.SetOnRepaintRequest(const Value: TNotifyEvent);
begin
  FOnRepaintRequest := Value;
end;

procedure TJvTurtle.SetPenDown(const Value: Boolean);
begin
  FPenDown := Value;
end;

procedure TJvTurtle.SetPosition(const Value: TPoint);
begin
  FPosition := Value;
end;

procedure TJvTurtle.SetPenWidth(const Value: Integer);
begin
  if Assigned(FCanvas) then
    FCanvas.Pen.Width := Value;
end;

function TJvTurtle.StrToCopyMode(var Cm: TCopyMode; S: string): Boolean;
begin
  Result := True;
  if S = 'cmblackness' then
    Cm := cmBlackness
  else
  if S = 'cmdstinvert' then
    Cm := cmDstInvert
  else
  if S = 'cmmergecopy' then
    Cm := cmMergeCopy
  else
  if S = 'cmmergepaint' then
    Cm := cmMergePaint
  else
  if S = 'cmnotsrccopy' then
    Cm := cmNotSrcCopy
  else
  if S = 'cmnotsrcerase' then
    Cm := cmNotSrcErase
  else
  if S = 'cmpatcopy' then
    Cm := cmPatCopy
  else
  if S = 'cmpatinvert' then
    Cm := cmPatInvert
  else
  if S = 'cmpatpaint' then
    Cm := cmPatPaint
  else
  if S = 'cmsrcand' then
    Cm := cmSrcAnd
  else
  if S = 'cmsrccopy' then
    Cm := cmSrcCopy
  else
  if S = 'cmsrcerase' then
    Cm := cmSrcErase
  else
  if S = 'cmsrcinvert' then
    Cm := cmSrcInvert
  else
  if S = 'cmscrpaint' then
    Cm := cmSrcPaint
  else
  if S = 'cmwhiteness' then
    Cm := cmWhiteness
  else
    Result := False;
end;

function TJvTurtle.StrToPenMode(var Pm: TPenMode; S: string): Boolean;
begin
  Result := True;
  if S = 'pmBlack' then
    Pm := pmBlack
  else
  if S = 'pmwhite' then
    Pm := pmWhite
  else
  if S = 'pmnop' then
    Pm := pmNop
  else
  if S = 'pmnot' then
    Pm := pmNot
  else
  if S = 'pmcopy' then
    Pm := pmCopy
  else
  if S = 'pmnotcopy' then
    Pm := pmNotCopy
  else
  if S = 'pmmergepennot' then
    Pm := pmMergePenNot
  else
  if S = 'pmmaskpennot' then
    Pm := pmMaskPenNot
  else
  if S = 'pmmergenotpen' then
    Pm := pmMergeNotPen
  else
  if S = 'pmmasknotpen' then
    Pm := pmMaskNotPen
  else
  if S = 'pmmerge' then
    Pm := pmMerge
  else
  if S = 'pmnotmerge' then
    Pm := pmNotMerge
  else
  if S = 'pmmask' then
    Pm := pmMask
  else
  if S = 'pmnotmask' then
    Pm := pmNotMask
  else
  if S = 'pmxor' then
    Pm := pmXor
  else
  if S = 'pmnotxor' then
    Pm := pmNotXor
  else
    Result := False;
end;

procedure TJvTurtle.TextRotate(X, Y, Angle: Integer; AText: string;
  AFont: TFont);
var
  DC: HDC;
  Fnt: LOGFONT;
  HFnt, HFntPrev: HFONT;
  I: Integer;
  FontName: string;
begin
  if AText = '' then
    Exit;
  Fnt.lfEscapement := Angle * 10;
  Fnt.lfOrientation := Angle * 10;
  if fsBold in AFont.Style then
    Fnt.lfWeight := FW_BOLD
  else
    Fnt.lfWeight := FW_NORMAL;
  if fsItalic in AFont.Style then
    Fnt.lfItalic := 1
  else
    Fnt.lfItalic := 0;
  if fsUnderline in AFont.Style then
    Fnt.lfUnderline := 1
  else
    Fnt.lfUnderline := 0;
  Fnt.lfStrikeOut := 0;
  Fnt.lfHeight := Abs(AFont.Height);
  FontName := AFont.Name;
  for I := 1 to Length(FontName) do
    Fnt.lfFaceName[I - 1] := FontName[I];
  Fnt.lfFaceName[Length(FontName)] := #0;
  HFnt := CreateFontIndirect(Fnt);
  DC := Canvas.Handle;
  SetBkMode(DC, Transparent);
  SetTextColor(DC, AFont.Color);
  HFntPrev := SelectObject(DC, HFnt);
  TextOut(DC, X, Y, PChar(AText), Length(AText));
  SelectObject(DC, HFntPrev);
  DeleteObject(HFnt);
end;

function TJvTurtle.txAngle: string;
var
  X: Integer;
begin
  if GetNum(X) then
  begin
    SetHeading(X);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['angle']);
end;

function TJvTurtle.txArea: string;
var
  X1, Y1, X2, Y2: Integer;
begin
  if GetNum(X1) and GetNum(Y1) and GetNum(X2) and GetNum(Y2) then
  begin
    Area := Rect(X1, Y1, X2, Y2);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['area']);
end;

function TJvTurtle.txBrushColor: string;
var
  Col: TColor;
begin
  if GetCol(Col) then
  begin
    Canvas.Brush.Color := Col;
    Result := '';
  end
  else
    Result := Format(sInvalidColorIns, ['brushcolor']);
end;

function TJvTurtle.txBsClear: string;
begin
  Canvas.Brush.Style := bsClear;
  Result := '';
end;

function TJvTurtle.txBsSolid: string;
begin
  Canvas.Brush.Style := bsSolid;
  Result := '';
end;

function TJvTurtle.txCopy: string;
var
  X, Y: Integer;
begin
  X := Position.X;
  Y := Position.Y;
  with Area do
    Canvas.CopyRect(Rect(X, Y, X + Right - Left, Y + Bottom - Top), Canvas, Area);
  Result := '';
end;

function TJvTurtle.txCopyMode: string;
var
  S: string;
  CopyMode: TCopyMode;
begin
  Result := sInvalidCopyMode;
  if GetToken(S) then
  begin
    S := 'cm' + LowerCase(S);
    if StrToCopyMode(CopyMode, S) then
    begin
      Canvas.CopyMode := CopyMode;
      Result := '';
    end;
  end;
end;

function TJvTurtle.txDown: string;
begin
  PenDown := True;
  Result := '';
end;

function TJvTurtle.txEllipse: string;
var
  X2, Y2: Integer;
begin
  if GetNum(X2) and GetNum(Y2) then
  begin
    X2 := Position.X + X2;
    Y2 := Position.Y + Y2;
    Canvas.Ellipse(Position.X, Position.Y, X2, Y2);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['ellipse']);
end;

function TJvTurtle.txGo: string;
var
  X: Integer;
begin
  if GetNum(X) then
  begin
    MoveForward(X);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['go']);
end;

function TJvTurtle.txGoMark: string;
begin
  DoGo(Mark);
  Result := '';
end;

function TJvTurtle.txTurn: string;
var
  X: Integer;
begin
  if GetNum(X) then
  begin
    Turn(X);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['turn']);
end;

function TJvTurtle.txLeft: string;
var
  X: Integer;
begin
  if GetNum(X) then
  begin
    Heading := Heading + X;
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['left']);
end;

function TJvTurtle.txRight: string;
var
  X: Integer;
begin
  if GetNum(X) then
  begin
    Heading := Heading - X;
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['right']);
end;

function TJvTurtle.txMark: string;
begin
  Mark := Position;
  Result := '';
end;

function TJvTurtle.txPenColor: string;
var
  Col: TColor;
begin
  if GetCol(Col) then
  begin
    Canvas.Pen.Color := Col;
    Result := '';
  end
  else
    Result := Format(sInvalidColorIns, ['pencolor']);
end;

function TJvTurtle.txPenMode: string;
var
  S: string;
  PenMode: TPenMode;
begin
  Result := sInvalidPenMode;
  if GetToken(S) then
  begin
    S := 'pm' + LowerCase(S);
    if StrToPenMode(PenMode, S) then
    begin
      Canvas.Pen.Mode := PenMode;
      Result := '';
    end;
  end;
end;

function TJvTurtle.txPenSize: string;
var
  Width: Integer;
begin
  if GetNum(Width) then
  begin
    Canvas.Pen.Width := Width;
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['pensize']);
end;

function TJvTurtle.txPos: string;
var
  X, Y: Integer;
begin
  if GetNum(X) and GetNum(Y) then
  begin
    Position := Point(X, Y);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['pos']);
end;

function TJvTurtle.txRectangle: string;
var
  X2, Y2: Integer;
begin
  if GetNum(X2) and GetNum(Y2) then
  begin
    X2 := Position.X + X2;
    Y2 := Position.Y + Y2;
    Canvas.Rectangle(Position.X, Position.Y, X2, Y2);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['rectangle']);
end;

function TJvTurtle.txText: string;
var
  S: string;
  A: Integer;
begin
  if GetTex(S) then
  begin
    A := Variant(Heading);
    TextRotate(Position.X, Position.Y, A, S, Canvas.Font);
    Result := '';
    DoRepaintRequest;
  end
  else
    Result := Format(sInvalidTextIns, ['text']);
end;

function TJvTurtle.txTextBold: string;
begin
  Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  Result := '';
end;

function TJvTurtle.txTextColor: string;
var
  Col: TColor;
begin
  if GetCol(Col) then
  begin
    Canvas.Font.Color := Col;
    Result := '';
  end
  else
    Result := Format(sInvalidColorIns, ['textcolor']);
end;

function TJvTurtle.txTextFont: string;
var
  FontName: string;
begin
  if GetTex(FontName) then
  begin
    Canvas.Font.Name := FontName;
    Result := '';
  end
  else
    Result := sMissingFontname;
end;

function TJvTurtle.txTextItalic: string;
begin
  Canvas.Font.Style := Canvas.Font.Style + [fsItalic];
  Result := '';
end;

function TJvTurtle.txTextNormal: string;
begin
  Canvas.Font.Style := [];
  Result := '';
end;

function TJvTurtle.txTextSize: string;
var
  FontSize: Integer;
begin
  if GetNum(FontSize) then
  begin
    Canvas.Font.Size := FontSize;
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['fontsize']);
end;

function TJvTurtle.txTextUnderline: string;
begin
  Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
  Result := '';
end;

function TJvTurtle.txUp: string;
begin
  PenDown := False;
  Result := '';
end;

function TJvTurtle.txDo: string;
var
  Num: Integer;
begin
  Result := Format(sNumberExpectedIns, ['do']);
  if not GetNum(Num) then
    Exit;
  Result := sStackOverflow;
  if not Push(IP) then
    Exit;
  if not Push(Num) then
    Exit;
  Result := '';
end;

function TJvTurtle.txLoop: string;
var
  Reps, Ret: Integer;
begin
  Result := sStackUnderflow;
  if not Pop(Reps) then
    Exit;
  if not Pop(Ret) then
    Exit;
  Dec(Reps);
  if Reps <> 0 then
  begin
    IP := Ret;
    Push(Ret);
    Push(Reps);
  end;
  Result := '';
end;

function TJvTurtle.txFlood: string;
var
  X, Y, XF, YF: Integer;
begin
  if GetNum(X) and GetNum(Y) then
  begin
    XF := Position.X + X;
    YF := Position.Y + Y;
    Canvas.FloodFill(XF, YF, Canvas.Pixels[XF, YF], fsSurface);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['flood']);
end;

procedure TJvTurtle.SetOnRequestBackGround(const Value: TRequestBackGroundEvent);
begin
  FOnRequestBackGround := Value;
end;

procedure TJvTurtle.DoRequestBackground;
begin
  if Assigned(FOnRequestBackGround) then
    FOnRequestBackground(Self, Background);
end;

function TJvTurtle.txBackground: string;
var
  Name: string;
begin
  if GetTex(Name) then
  begin
    Background := Name;
    DoRequestBackground;
    Result := '';
  end
  else
    Result := Format(sInvalidTextIns, ['background']);
end;

function TJvTurtle.txTextOut: string;
var
  Text: string;
begin
  if GetTex(Text) then
  begin
    Canvas.TextOut(Position.X, Position.Y, Text);
    Result := '';
  end
  else
    Result := Format(sInvalidTextIns, ['text']);
end;

function TJvTurtle.txAddBrushColor: string;
var
  Color: TColor;
begin
  if GetCol(Color) then
  begin
    Canvas.Brush.Color := Canvas.Brush.Color + Color;
    Result := '';
  end
  else
    Result := Format(sInvalidColorIns, ['addbrushcolor']);
end;

function TJvTurtle.txAddPenColor: string;
var
  Color: TColor;
begin
  if GetCol(Color) then
  begin
    Canvas.Pen.Color := Canvas.Pen.Color + Color;
    Result := '';
  end
  else
    Result := Format(sInvalidColorIns, ['addbrushcolor']);
end;

function TJvTurtle.txGoMarkAngle: string;
begin
  Heading := AngleMark;
  Result := '';
end;

function TJvTurtle.txMarkAngle: string;
begin
  AngleMark := Variant(Heading);
  Result := '';
end;

function TJvTurtle.IsCol(Tex: string): Boolean;
var
  Msg: string;
begin
  try
    Result := NPush(Msg, StringToColor(Tex));
  except
    Result := False;
  end;
end;

function TJvTurtle.IsNum(Tex: string): Boolean;
var
  Msg: string;
begin
  try
    Result := NPush(Msg, StrToInt(Tex));
  except
    Result := False;
  end;
end;

function TJvTurtle.NPop(var Msg: string; var Num: Integer): Boolean;
begin
  Result := NSP > Low(NStack);
  if Result then
  begin
    Dec(NSP);
    Num := NStack[NSP];
    Msg := '';
  end
  else
    Msg := sNumberStackUnderflow;
end;

function TJvTurtle.NPush(var Msg: string; Num: Integer): Boolean;
begin
  Result := NSP < High(NStack);
  if Result then
  begin
    NStack[NSP] := Num;
    Inc(NSP);
    Msg := '';
  end
  else
    Msg := sNumberStackOverflow;
end;

function TJvTurtle.txComment: string;
begin
  while (IP <= IPMax) and (Script[IP] <> '}') do
    Inc(IP);
  if IP <= IPMax then
  begin
    Inc(IP);
    Result := '';
  end
  else
    Result := sMissingAfterComment;
end;
(*)

function TJvTurtle.SkipBlock: Boolean;
begin
  Result := False;
  while (IP <= IPMax) and (Script[IP] <> '[') do
    Inc(IP);
  if IP > IPMax then
    Exit;
  Inc(IP);
  while (IP <= IPMax) and (Script[IP] <> ']') do
    Inc(IP);
  if IP > IPMax then
    Exit;
  Inc(IP);
  Result := True;
end;
(*)

procedure TJvTurtle.SetOnRequestImageSize(const Value: TRequestImageSizeEvent);
begin
  FOnRequestImageSize := Value;
end;

function TJvTurtle.DoRequestImageSize: Boolean;
begin
  Result := Assigned(FOnRequestImageSize);
  if Result then
    FOnRequestImageSize(Self, ImageRect);
end;

function TJvTurtle.txGoBottom: string;
var
  NewPoint: TPoint;
begin
  if DoRequestImageSize then
  begin
    NewPoint := Point(Position.X, ImageRect.Bottom);
    DoGo(NewPoint);
    Result := '';
  end
  else
    Result := Format(sErrorIns, ['gobottom']);
end;

function TJvTurtle.txGoLeft: string;
var
  NewPoint: TPoint;
begin
  if DoRequestImageSize then
  begin
    NewPoint := Point(ImageRect.Left, Position.Y);
    DoGo(NewPoint);
    Result := '';
  end
  else
    Result := Format(sErrorIns, ['goleft']);
end;

function TJvTurtle.txGoRight: string;
var
  NewPoint: TPoint;
begin
  if DoRequestImageSize then
  begin
    NewPoint := Point(ImageRect.Right, Position.Y);
    DoGo(NewPoint);
    Result := '';
  end
  else
    Result := Format(sErrorIns, ['goright']);
end;

function TJvTurtle.txGoTop: string;
var
  NewPoint: TPoint;
begin
  if DoRequestImageSize then
  begin
    NewPoint := Point(Position.X, ImageRect.Top);
    DoGo(NewPoint);
    Result := '';
  end
  else
    Result := Format(sErrorIns, ['gotop']);
end;

function TJvTurtle.txDiv: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    if B <> 0 then
      NPush(Result, A div B)
    else
      Result := sDivisionByZero;
end;

function TJvTurtle.txDrop: string;
var
  A: Integer;
begin
  NPop(Result, A);
end;

function TJvTurtle.txDup: string;
var
  A: Integer;
begin
  if NPop(Result, A) then
  begin
    NPush(Result, A);
    NPush(Result, A);
  end;
end;

function TJvTurtle.txMul: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, A * B);
end;

function TJvTurtle.txSub: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, A - B);
end;

function TJvTurtle.txAdd: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, A + B);
end;

function TJvTurtle.txGoCenter: string;
var
  CX, CY: Integer;
begin
  if DoRequestImageSize then
  begin
    CX := (ImageRect.Right - ImageRect.Left) div 2;
    CY := (ImageRect.Bottom - ImageRect.Top) div 2;
    DoGo(Point(CX, CY));
    Result := '';
  end
  else
    Result := Format(sErrorIns, ['gocenter']);
end;

function TJvTurtle.txDiamond: string;
var
  I, X: Integer;
  OldDown: Boolean;
begin
  Result := Format(sInvalidIntegerIns, ['diamond']);
  if GetNum(X) then
  begin
    OldDown := PenDown;
    PenDown := True;
    Turn(45);
    for I := 1 to 4 do
    begin
      MoveForward(X);
      Turn(-90);
    end;
    Turn(-45);
    PenDown := OldDown;
    Result := '';
  end;
end;

function TJvTurtle.txCurve: string;
var
  Pts: array [0..3] of TPoint;
  I: Integer;
begin
  if GetNum(Pts[1].X) and GetNum(Pts[1].Y) and
    GetNum(Pts[2].X) and GetNum(Pts[2].Y) and
    GetNum(Pts[3].X) and GetNum(Pts[3].Y) then
  begin
    Pts[0].X := Position.X;
    Pts[0].Y := Position.Y;
    for I := 1 to 3 do
    begin
      Pts[I].X := Position.X + Pts[I].X;
      Pts[I].Y := Position.Y + Pts[I].Y;
    end;
    Canvas.PolyBezier(Pts);
    Position := Pts[3];
    Result := '';
  end
  else
    Result := Format(sInvalidParameterIns, ['curve']);
end;

function TJvTurtle.txMove: string;
var
  X, Y: Integer;
begin
  if GetNum(X) and GetNum(Y) then
  begin
    Position := Point(Position.X + X, Position.Y + Y);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['move']);
end;

procedure TJvTurtle.SetOnRequestFilter(const Value: TRequestFilterEvent);
begin
  FOnRequestFilter := Value;
end;

procedure TJvTurtle.DoRequestFilter;
begin
  if Assigned(FOnRequestFilter) then
    FOnRequestFilter(Self, Filter);
end;

function TJvTurtle.txFilter: string;
var
  AName: string;
begin
  if GetTex(AName) then
  begin
    Filter := AName;
    DoRequestFilter;
    Result := '';
  end
  else
    Result := Format(sInvalidTextIns, ['filter']);
end;

function TJvTurtle.txUser(Sym: string): string;
var
  P: Integer;
begin
  P := Pos(Sym, Script);
  if P <> 0 then
  begin
    if Push(IP) then
    begin
      IP := P + Length(Sym);
      Result := '';
    end
    else
      Result := sStackOverflow;
  end
  else
    Result := Format(sSymbolsIsNotDefined, [Sym]);
end;

function TJvTurtle.txBlock: string;
begin
  while (IP <= IPMax) and (Script[IP] <> ']') do
    Inc(IP);
  if IP <= IPMax then
  begin
    Inc(IP);
    Result := '';
  end
  else
    Result := sMissingAfterBlock;
end;

function TJvTurtle.txReturn: string;
var
  Num: Integer;
begin
  if Pop(Num) then
  begin
    IP := Num;
    Result := '';
  end
  else
    Result := sStackUnderflow;
end;

function TJvTurtle.tx_Angle: string;
var
  Num: Integer;
begin
  Num := Variant(Heading);
  NPush(Result, Num);
end;

function TJvTurtle.tx_Bottom: string;
begin
  if DoRequestImageSize then
    NPush(Result, ImageRect.Bottom)
  else
    Result := Format(sErrorIns, ['=bottom']);
end;

function TJvTurtle.tx_BrushColor: string;
begin
  NPush(Result, Canvas.Brush.Color);
end;

function TJvTurtle.tx_Left: string;
begin
  if DoRequestImageSize then
    NPush(Result, ImageRect.Left)
  else
    Result := Format(sErrorIns, ['=left']);
end;

function TJvTurtle.tx_Loop: string;
var
  Num: Integer;
begin
  if Pop(Num) then
  begin
    Push(Num);
    NPush(Result, Num);
  end
  else
    Result := Format(sStackUnderflowIns, ['=loop']);
end;

function TJvTurtle.tx_MarkX: string;
begin
  NPush(Result, Mark.X);
end;

function TJvTurtle.tx_MarkY: string;
begin
  NPush(Result, Mark.Y);
end;

function TJvTurtle.tx_PenColor: string;
begin
  NPush(Result, Canvas.Pen.Color);
end;

function TJvTurtle.tx_PosX: string;
begin
  NPush(Result, Position.X);
end;

function TJvTurtle.tx_PosY: string;
begin
  NPush(Result, Position.Y);
end;

function TJvTurtle.tx_Right: string;
begin
  if DoRequestImageSize then
    NPush(Result, ImageRect.Right)
  else
    Result := Format(sErrorIns, ['=right']);
end;

function TJvTurtle.tx_Top: string;
begin
  if DoRequestImageSize then
    NPush(Result, ImageRect.Top)
  else
    Result := Format(sErrorIns, ['=top']);
end;

function TJvTurtle.tx_PenSize: string;
begin
  NPush(Result, Canvas.Pen.Width);
end;

function TJvTurtle.tx_TextColor: string;
begin
  NPush(Result, Canvas.Font.Color);
end;

function TJvTurtle.tx_TextSize: string;
begin
  NPush(Result, Canvas.Font.Size);
end;

function TJvTurtle.txIf: string;
var
  Num: Integer;
  Token: string;
begin
  if NPop(Result, Num) then
    if Num = 0 then
      if GetToken(Token) then
        Result := ''
      else
        Result := sSymbolExpectedAfterIf;
end;

function TJvTurtle.txAnd: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Ord((A <> 0) and (B <> 0)));
end;

function TJvTurtle.txEq: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Ord(A = B));
end;

function TJvTurtle.txGe: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Ord(A >= B));
end;

function TJvTurtle.txGt: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Ord(A > B));
end;

function TJvTurtle.txLe: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Ord(A <= B));
end;

function TJvTurtle.txLt: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Ord(A < B));
end;

function TJvTurtle.txNe: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Ord(A <> B));
end;

function TJvTurtle.txNot: string;
var
  A: Integer;
begin
  if NPop(Result, A) then
    NPush(Result, Ord(A = 0))
end;

function TJvTurtle.txOr: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Ord((A <> 0) or (B <> 0)));
end;

function TJvTurtle.txAbs: string;
var
  A: Integer;
begin
  if NPop(Result, A) then
    NPush(Result, Abs(A))
end;

function TJvTurtle.txNeg: string;
var
  A: Integer;
begin
  if NPop(Result, A) then
    NPush(Result, -A);
end;

function TJvTurtle.txSwap: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
  begin
    NPush(Result, B);
    NPush(Result, A);
  end;
end;

function TJvTurtle.txMax: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Max(A, B));
end;

function TJvTurtle.txMin: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    NPush(Result, Min(A, B));
end;

function TJvTurtle.txSqr: string;
var
  A: Integer;
begin
  if NPop(Result, A) then
    NPush(Result, Variant(Sqr(A)));
end;

function TJvTurtle.txSqrt: string;
var
  A: Integer;
begin
  if NPop(Result, A) then
    if A <> 0 then
      NPush(Result, Variant(Sqrt(A)))
    else
      Result := sCanNotTakeSqrtOf;
end;

function TJvTurtle.txDec: string;
var
  A: Integer;
begin
  if NPop(Result, A) then
    NPush(Result, A-1);
end;

function TJvTurtle.txInc: string;
var
  A: Integer;
begin
  if NPop(Result, A) then
    NPush(Result, A+1);
end;

function TJvTurtle.txPolygon: string;
var
  I, S, N: Integer;
  OldDown: Boolean;
  OldHeading, A: Real;
  Pt: TPoint;
begin
  Result := Format(sInvalidIntegerIns, ['polygon']);
  if not (GetNum(N) and GetNum(S)) then
    Exit;
  Result := Format(sNotAllowedIns, ['polygon']);
  if (N = 0) or (S = 0) then
    Exit;
  Result := Format(sNeedMinimumOfSidesIns, ['polygon']);
  if N < 3 then
    Exit;
  OldHeading := Heading;
  Pt := Position;
  OldDown := PenDown;
  PenDown := True;
  A := 360 / N;
  for I := 1 to N - 1 do
  begin
    MoveForward(S);
    Turn(A);
  end;
  Canvas.LineTo(Pt.X, Pt.Y);
  PenDown := OldDown;
  Heading := OldHeading;
  Position := Pt;
  Result := '';
end;

function TJvTurtle.txStar: string;
var
  I, S, am, N: Integer;
  OldDown: Boolean;
  A, OldHeading: Real;
  Pt: TPoint;
begin
  Result := Format(sInvalidIntegerIns, ['star']);
  if not (GetNum(N) and GetNum(S)) then
    Exit;
  Result := Format(sNotAllowedIns, ['star']);
  if (N = 0) or (S = 0) then
    Exit;
  Result := Format(sNeedMinimumOfSidesIns, ['star']);
  if N < 3 then
    Exit;
  Result := Format(sMaximumSidesExceededIns, ['star']);
  if N > 12 then
    Exit;
  case N of
    5:
      am := 2;
    7:
      am := 3;
    9:
      am := 4;
    11:
      am := 5;
  else
    am := 1;
  end;
  OldHeading := Heading;
  Pt := Position;
  OldDown := PenDown;
  PenDown := True;
  A := am * 360 / N;
  for I := 1 to N - 1 do
  begin
    MoveForward(S);
    Turn(A);
  end;
  Canvas.LineTo(Pt.X, Pt.Y);
  PenDown := OldDown;
  Heading := OldHeading;
  Position := Pt;
  Result := '';
end;

function TJvTurtle.txLineTo: string;
var
  X, Y: Integer;
begin
  if GetNum(X) and GetNum(Y) then
  begin
    Canvas.MoveTo(Position.X, Position.Y);
    Canvas.LineTo(Position.X + X, Position.Y + Y);
    Position := Point(Position.X + X, Position.Y + Y);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['lineto']);
end;

function TJvTurtle.txRoundRect: string;
var
  X2, Y2, RX, RY: Integer;
begin
  if GetNum(X2) and GetNum(Y2) and GetNum(RX) and GetNum(RY) then
  begin
    X2 := Position.X + X2;
    Y2 := Position.Y + Y2;
    Canvas.RoundRect(Position.X, Position.Y, X2, Y2, RX, RY);
    Result := '';
  end
  else
    Result := Format(sInvalidIntegerIns, ['roundrect']);
end;

function TJvTurtle.txDefault: string;
begin
  Result := '';
  Heading := 0;
  Position := Point(0, 0);
  PenDown := False;
  Canvas.Pen.Color := clWindowText;  // (rom) from clBlack
  Canvas.Brush.Color := clWindow;    // (rom) from clWhite
  Canvas.Font.Color := clWindowText; // (rom) added
  Canvas.CopyMode := cmSrcCopy;
  Mark := Position;
  Area := Rect(0, 0, 0, 0);
end;

function TJvTurtle.txIn: string;
var
  Token: string;
  Num: Integer;
begin
  if NPop(Result, Num) then
    if GetToken(Token) then
    begin
      if FPot.IndexOfName(Token) < 0 then
        FPot.Add(Token + '=' + IntToStr(Num))
      else
        FPot.Values[Token] := IntToStr(Num);
      Result := '';
    end
    else
      Result := sTokenExpected;
end;

function TJvTurtle.IsVar(Tex: string): Boolean;
var
  Num: Integer;
  Msg, S: string;
begin
  S := FPot.Values[Tex];
  if S <> '' then
    try
      Num := StrToInt(S);
      Result := NPush(Msg, Num);
    except
      Result := False;
    end
  else
    Result := False;
end;

function TJvTurtle.txInAdd: string;
var
  Token: string;
  Index, Num: Integer;
begin
  if NPop(Result, Num) then
    if GetToken(Token) then
    begin
      Index := FPot.IndexOfName(Token);
      if Index >= 0 then
      begin
        FPot.Values[Token] := IntToStr(StrToInt(FPot.Values[Token]) + Num);
        Result := '';
      end
      else
        Result := Format(ssDoesNotExist, [Token]);
    end
    else
      Result := sTokenExpected;
end;

function TJvTurtle.txInDiv: string;
var
  Token: string;
  Num: Integer;
begin
  if NPop(Result, Num) then
    if GetToken(Token) then
    begin
      if FPot.IndexOfName(Token) >= 0 then
      begin
        FPot.Values[Token] := IntToStr(StrToInt(FPot.Values[Token]) - Num);
        Result := '';
      end
      else
        Result := Format(ssDoesNotExist, [Token]);
    end
    else
      Result := sTokenExpected;
end;

function TJvTurtle.txInMult: string;
var
  Token: string;
  Num: Integer;
begin
  if NPop(Result, Num) then
    if GetToken(Token) then
    begin
      if FPot.IndexOfName(Token) >= 0 then
      begin
        FPot.Values[Token] := IntToStr(StrToInt(FPot.Values[Token]) * Num);
        Result := '';
      end
      else
        Result := Format(ssDoesNotExist, [Token]);
    end
    else
      Result := sTokenExpected;
end;

function TJvTurtle.txInSub: string;
var
  Token: string;
  Num: Integer;
begin
  if NPop(Result, Num) then
  begin
    if Num = 0 then
      Result := sDivisionByZeroNotAllowedInIn
    else
    if GetToken(Token) then
    begin
      if FPot.IndexOfName(Token) >= 0 then
      begin
        FPot.Values[Token] := IntToStr(StrToInt(FPot.Values[Token]) div Num);
        Result := '';
      end
      else
        Result := Format(ssDoesNotExist, [Token]);
    end
    else
      Result := sTokenExpected;
  end;
end;

function TJvTurtle.txInDec: string;
var
  Token: string;
begin
  if GetToken(Token) then
  begin
    if FPot.IndexOfName(Token) >= 0 then
    begin
      FPot.Values[Token] := IntToStr(StrToInt(FPot.Values[Token]) - 1);
      Result := '';
    end
    else
      Result := Format(ssDoesNotExist, [Token]);
  end
  else
    Result := sTokenExpected;
end;

function TJvTurtle.txInInc: string;
var
  Token: string;
begin
  if GetToken(Token) then
  begin
    if FPot.IndexOfName(Token) >= 0 then
    begin
      FPot.Values[Token] := IntToStr(StrToInt(FPot.Values[Token]) + 1);
      Result := '';
    end
    else
      Result := Format(ssDoesNotExist, [Token]);
  end
  else
    Result := sTokenExpected;
end;

end.

