{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTurtle.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQTurtle;

interface

uses
  SysUtils, Classes,  
  QGraphics, QControls, Types, QWindows, 
  Math;


type
  TRequestBackgroundEvent = procedure(Sender: TObject; Background: string) of object;
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
    FBackground: string;
    FFilter: string;
    FScript: string;
    FIP: Integer;
    FIPMax: Integer;
    FSP: Integer;
    FNSP: Integer;
    FStack: array of Integer;
    FNStack: array of Integer;
    FVariables: TStringList;
    FAngleMark: Integer;
    FImageRect: TRect;
    FOnRepaintRequest: TNotifyEvent;
    FOnRequestBackground: TRequestBackgroundEvent;
    FOnRequestImageSize: TRequestImageSizeEvent;
    FOnRequestFilter: TRequestFilterEvent;
    function GetToken(var Token: string): Boolean;
    function GetNum(var Num: Integer): Boolean;
    function InVariables(Token: string; var Num: Integer): Boolean;
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
    procedure TextRotate(X, Y, Angle: Integer; AText: string; AFont: TFont);
    procedure SetOnRepaintRequest(const Value: TNotifyEvent);
    procedure SetMark(const Value: TPoint);
    procedure SetArea(const Value: TRect);
    procedure SetOnRequestBackground(const Value: TRequestBackgroundEvent);
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
    function Interpret(var ALine, ACol: Integer; const S: TStrings): string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnRepaintRequest: TNotifyEvent read FOnRepaintRequest write SetOnRepaintRequest;
    property OnRequestBackground: TRequestBackgroundEvent read FOnRequestBackground write SetOnRequestBackground;
    property OnRequestFilter: TRequestFilterEvent read FOnRequestFilter write SetOnRequestFilter;
    property OnRequestImageSize: TRequestImageSizeEvent read FOnRequestImageSize write SetOnRequestImageSize;
  end;

implementation

uses
  JvQConsts, JvQTypes, JvQResources;

constructor TJvTurtle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVariables := TStringList.Create;
  FVariables.Sorted := True;
  SetLength(FStack, 256);
  SetLength(FNStack, 256);
  txDefault;
end;

destructor TJvTurtle.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

function TJvTurtle.DoCom: string;
const
  // sorted for binary search
  Mapper: array [0..101] of PChar =
   (
    '-',
    '*',
    '.and',
    '.eq',
    '.ge',
    '.gt',
    '.le',
    '.lt',
    '.ne',
    '.not',
    '.or',
    '/',
    '[',
    ']',
    '{',
    '+',
    '=angle',
    '=bottom',
    '=brushcolor',
    '=left',
    '=loop',
    '=markx',
    '=marky',
    '=pencolor',
    '=pensize',
    '=posx',
    '=posy',
    '=right',
    '=textcolor',
    '=textsize',
    '=top',
    'abs',
    'addbrushcolor',
    'addpencolor',
    'angle',
    'area',
    'background',
    'bold',
    'brushcolor',
    'bsclear',
    'bssolid',
    'copy',
    'copymode',
    'curve',
    'dec',
    'default',
    'diamond',
    'do',
    'down',
    'drop',
    'dup',
    'ellipse',
    'filter',
    'flood',
    'go',
    'gobottom',
    'gocenter',
    'goleft',
    'gomark',
    'gomarkangle',
    'goright',
    'gotop',
    'if',
    'in',
    'inadd',
    'inc',
    'indec',
    'indiv',
    'ininc',
    'inmul',
    'insub',
    'italic',
    'left',
    'lineto',
    'loop',
    'mark',
    'markangle',
    'max',
    'min',
    'move',
    'neg',
    'normal',
    'pencolor',
    'penmode',
    'pensize',
    'polygon',
    'pos',
    'rectangle',
    'right',
    'roundrect',
    'sqr',
    'sqrt',
    'star',
    'swap',
    'text',
    'textcolor',
    'textfont',
    'textout',
    'textsize',
    'turn',
    'underline',
    'up'
   );
var
  Com: string;
  Lo, Mid, Hi: Integer;
begin
  Result := 'ready';
  if not GetToken(Com) then
    Exit;

  Lo := Low(Mapper);
  Hi := High(Mapper)+1;
  repeat
    Mid := Lo + (Hi - Lo) div 2;
    if Com > Mapper[Mid] then
      Lo := Mid+1
    else
      Hi := Mid;
  until Lo >= Hi;
  if (Hi > High(Mapper)) or (Com <> Mapper[Hi]) then
    Hi := -1;

  case Hi of
    0:
      Result := txSub;
    1:
      Result := txMul;
    2:
      Result := txAnd;
    3:
      Result := txEq;
    4:
      Result := txGe;
    5:
      Result := txGt;
    6:
      Result := txLe;
    7:
      Result := txLt;
    8:
      Result := txNe;
    9:
      Result := txNot;
    10:
      Result := txOr;
    11:
      Result := txDiv;
    12:
      Result := txBlock;
    13:
      Result := txReturn;
    14:
      Result := txComment;
    15:
      Result := txAdd;
    16:
      Result := tx_Angle;
    17:
      Result := tx_Bottom;
    18:
      Result := tx_BrushColor;
    19:
      Result := tx_Left;
    20:
      Result := tx_Loop;
    21:
      Result := tx_MarkX;
    22:
      Result := tx_MarkY;
    23:
      Result := tx_PenColor;
    24:
      Result := tx_PenSize;
    25:
      Result := tx_PosX;
    26:
      Result := tx_PosY;
    27:
      Result := tx_Right;
    28:
      Result := tx_TextColor;
    29:
      Result := tx_TextSize;
    30:
      Result := tx_Top;
    31:
      Result := txAbs;
    32:
      Result := txAddBrushColor;
    33:
      Result := txAddPenColor;
    34:
      Result := txAngle;
    35:
      Result := txArea;
    36:
      Result := txBackground;
    37:
      Result := txTextBold;
    38:
      Result := txBrushColor;
    39:
      Result := txBsClear;
    40:
      Result := txBsSolid;
    41:
      Result := txCopy;
    42:
      Result := txCopyMode;
    43:
      Result := txCurve;
    44:
      Result := txDec;
    45:
      Result := txDefault;
    46:
      Result := txDiamond;
    47:
      Result := txDo;
    48:
      Result := txDown;
    49:
      Result := txDrop;
    50:
      Result := txDup;
    51:
      Result := txEllipse;
    52:
      Result := txFilter;   
    54:
      Result := txGo;
    55:
      Result := txGoBottom;
    56:
      Result := txGoCenter;
    57:
      Result := txGoLeft;
    58:
      Result := txGoMark;
    59:
      Result := txGoMarkAngle;
    60:
      Result := txGoRight;
    61:
      Result := txGoTop;
    62:
      Result := txIf;
    63:
      Result := txIn;
    64:
      Result := txInAdd;
    65:
      Result := txInc;
    66:
      Result := txInDec;
    67:
      Result := txInDiv;
    68:
      Result := txInInc;
    69:
      Result := txInMult;
    70:
      Result := txInSub;
    71:
      Result := txTextItalic;
    72:
      Result := txLeft;
    73:
      Result := txLineTo;
    74:
      Result := txLoop;
    75:
      Result := txMark;
    76:
      Result := txMarkAngle;
    77:
      Result := txMax;
    78:
      Result := txMin;
    79:
      Result := txMove;
    80:
      Result := txNeg;
    81:
      Result := txTextNormal;
    82:
      Result := txPenColor;
    83:
      Result := txPenMode;
    84:
      Result := txPenSize;
    85:
      Result := txPolygon;
    86:
      Result := txPos;
    87:
      Result := txRectangle;
    88:
      Result := txRight;
    89:
      Result := txRoundRect;
    90:
      Result := txSqr;
    91:
      Result := txSqrt;
    92:
      Result := txStar;
    93:
      Result := txSwap;
    94:
      Result := txText;
    95:
      Result := txTextColor;
    96:
      Result := txTextFont;
    97:
      Result := txTextOut;
    98:
      Result := txTextSize;
    99:
      Result := txTurn;
    100:
      Result := txTextUnderline;
    101:
      Result := txUp;
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

function TJvTurtle.InVariables(Token: string; var Num: Integer): Boolean;
var
  N: Integer;
begin
  Result := FVariables.Find(Token, N);
  if Result then
    Num := Integer(FVariables.Objects[N]);
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
    if InVariables(Token, Num) then
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
  while (FIP <= FIPMax) and (FScript[FIP] <> '"') do
    Inc(FIP);
  if FIP > FIPMax then
    Exit;
  Inc(FIP);
  while (FIP <= FIPMax) and (FScript[FIP] <> '"') do
  begin
    Tex := Tex + FScript[FIP];
    Inc(FIP);
  end;
  if FIP > FIPMax then
    Exit;
  Inc(FIP);
  Result := Tex <> '';
end;

function TJvTurtle.GetToken(var Token: string): Boolean;
const
  Delimiters = [' ', Tab, Cr, Lf];
begin
  Token := '';
  while (FIP <= FIPMax) and (FScript[FIP] in Delimiters) do
    Inc(FIP);
  while (FIP <= FIPMax) and not (FScript[FIP] in Delimiters) do
  begin
    Token := Token + FScript[FIP];
    Inc(FIP);
  end;
  Token := LowerCase(Token);
  Result := Token <> '';
end;

function TJvTurtle.GetWidth: Integer;
begin
  if Assigned(FCanvas) then
    Result := FCanvas.Pen.Width
  else
    Result := 1;
end;

function TJvTurtle.Interpret(var ALine, ACol: Integer; const S: TStrings): string;
var
  I: Integer;
  Msg: string;
begin
  ALine := 0;
  ACol := 0;
  Result := RsErrorCanvasNotAssigned;
  if not Assigned(FCanvas) then
    Exit;
  txDefault;
  FScript := S.Text;
  FSP := 0;
  FIP := 1;
  FIPMax := Length(FScript);
  if FIPMax > 0 then
  begin
    FVariables.Clear;
    repeat
      Msg := DoCom;
    until Msg <> '';
    Result := Msg;
    ALine := 0;
    ACol := 0;
    for I := 1 to FIP-1 do
    begin
      Inc(ACol);
      if (FScript[I] = Cr) or (FScript[I] = Lf) then
      begin
        Inc(ALine);
        ACol := 0;
      end;
      if (I > 1) and (FScript[I] = Lf) and (FScript[I-1] = Cr) then
      begin
        Dec(ALine);
        Dec(ACol);
      end;
    end;
    if ACol < 0 then
      ACol := 0;
  end
  else
    Result := RsEmptyScript;
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
  RAngle := Heading * 2 * Pi / 360;
  dX := ADistance * Cos(RAngle);
  dY := ADistance * Sin(RAngle);
  NewPoint := Point(Variant(Position.X + dX), Variant(Position.Y - dY));
  DoGo(NewPoint);
end;

function TJvTurtle.Pop(var Num: Integer): Boolean;
begin
  Result := FSP > 0;
  if Result then
  begin
    Dec(FSP);
    Num := FStack[FSP];
  end;
end;

function TJvTurtle.Push(Num: Integer): Boolean;
begin
  try
    if FSP >= Length(FStack) then
      SetLength(FStack, Length(FStack) + 256);
    FStack[FSP] := Num;
    Inc(FSP);
    Result := True;
  except
    Result := False;
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
type
  TMapper = record
    Name: PChar;
    Val: TCopyMode;
  end;
const
  // sorted for binary search
  Mapper: array [0..14] of TMapper =
   (
    (Name: 'cmblackness';   Val: cmBlackness),
    (Name: 'cmdstinvert';   Val: cmDstInvert),
    (Name: 'cmmergecopy';   Val: cmMergeCopy),
    (Name: 'cmmergepaint';  Val: cmMergePaint),
    (Name: 'cmnotsrccopy';  Val: cmNotSrcCopy),
    (Name: 'cmnotsrcerase'; Val: cmNotSrcErase),
    (Name: 'cmpatcopy';     Val: cmPatCopy),
    (Name: 'cmpatinvert';   Val: cmPatInvert),
    (Name: 'cmpatpaint';    Val: cmPatPaint),
    (Name: 'cmscrpaint';    Val: cmSrcPaint),
    (Name: 'cmsrcand';      Val: cmSrcAnd),
    (Name: 'cmsrccopy';     Val: cmSrcCopy),
    (Name: 'cmsrcerase';    Val: cmSrcErase),
    (Name: 'cmsrcinvert';   Val: cmSrcInvert),
    (Name: 'cmwhiteness';   Val: cmWhiteness)
   );
var
  Lo, Mid, Hi: Integer;
begin
  Lo := Low(Mapper);
  Hi := High(Mapper)+1;
  repeat
    Mid := Lo + (Hi - Lo) div 2;
    if S > Mapper[Mid].Name then
      Lo := Mid+1
    else
      Hi := Mid;
  until Lo >= Hi;
  Result := (Hi <= High(Mapper)) and (S = Mapper[Hi].Name);
  if Result then
    Cm := Mapper[Mid].Val;
end;

function TJvTurtle.StrToPenMode(var Pm: TPenMode; S: string): Boolean;
type
  TMapper = record
    Name: PChar;
    Val: TPenMode;
  end;
const
  // sorted for binary search
  Mapper: array [0..15] of TMapper =
   (
    (Name: 'pmblack';       Val: pmBlack),
    (Name: 'pmcopy';        Val: pmCopy),
    (Name: 'pmmask';        Val: pmMask),
    (Name: 'pmmasknotpen';  Val: pmMaskNotPen),
    (Name: 'pmmaskpennot';  Val: pmMaskPenNot),
    (Name: 'pmmerge';       Val: pmMerge),
    (Name: 'pmmergenotpen'; Val: pmMergeNotPen),
    (Name: 'pmmergepennot'; Val: pmMergePenNot),
    (Name: 'pmnop';         Val: pmNop),
    (Name: 'pmnot';         Val: pmNot),
    (Name: 'pmnotcopy';     Val: pmNotCopy),
    (Name: 'pmnotmask';     Val: pmNotMask),
    (Name: 'pmnotmerge';    Val: pmNotMerge),
    (Name: 'pmnotxor';      Val: pmNotXor),
    (Name: 'pmwhite';       Val: pmWhite),
    (Name: 'pmxor';         Val: pmXor)
   );
var
  Lo, Mid, Hi: Integer;
begin
  Lo := Low(Mapper);
  Hi := High(Mapper)+1;
  repeat
    Mid := Lo + (Hi - Lo) div 2;
    if S > Mapper[Mid].Name then
      Lo := Mid+1
    else
      Hi := Mid;
  until Lo >= Hi;
  Result := (Hi <= High(Mapper)) and (S = Mapper[Hi].Name);
  if Result then
    Pm := Mapper[Mid].Val;
end;

procedure TJvTurtle.TextRotate(X, Y, Angle: Integer; AText: string;
  AFont: TFont);

begin
  if AText = '' then
    Exit; 
  TextOutAngle(Canvas, Angle, X, Y, AText);  
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
    Result := Format(RsInvalidIntegerIns, ['angle']);
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
    Result := Format(RsInvalidIntegerIns, ['area']);
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
    Result := Format(RsInvalidColorIns, ['brushcolor']);
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
  Result := RsInvalidCopyMode;
  if GetToken(S) then
  begin
    S := 'cm' + S;
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
    Result := Format(RsInvalidIntegerIns, ['ellipse']);
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
    Result := Format(RsInvalidIntegerIns, ['go']);
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
    Result := Format(RsInvalidIntegerIns, ['turn']);
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
    Result := Format(RsInvalidIntegerIns, ['left']);
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
    Result := Format(RsInvalidIntegerIns, ['right']);
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
    Result := Format(RsInvalidColorIns, ['pencolor']);
end;

function TJvTurtle.txPenMode: string;
var
  S: string;
  PenMode: TPenMode;
begin
  Result := RsInvalidPenMode;
  if GetToken(S) then
  begin
    S := 'pm' + S;
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
    Result := Format(RsInvalidIntegerIns, ['pensize']);
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
    Result := Format(RsInvalidIntegerIns, ['pos']);
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
    Result := Format(RsInvalidIntegerIns, ['rectangle']);
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
    Result := Format(RsInvalidTextIns, ['text']);
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
    Result := Format(RsInvalidColorIns, ['textcolor']);
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
    Result := RsMissingFontname;
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
    Result := Format(RsInvalidIntegerIns, ['fontsize']);
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
  if GetNum(Num) then
  begin
    Result := RsStackOverflow;
    if Push(FIP) then
      if not Push(Num) then
        Result := '';
  end
  else
    Result := Format(RsNumberExpectedIns, ['do']);
end;

function TJvTurtle.txLoop: string;
var
  Reps, Ret: Integer;
begin
  if Pop(Reps) and Pop(Ret) then
  begin
    Dec(Reps);
    if Reps <> 0 then
    begin
      FIP := Ret;
      Push(Ret);
      Push(Reps);
    end;
    Result := '';
  end
  else
    Result := RsStackUnderflow;
end;



procedure TJvTurtle.SetOnRequestBackground(const Value: TRequestBackgroundEvent);
begin
  FOnRequestBackground := Value;
end;

procedure TJvTurtle.DoRequestBackground;
begin
  if Assigned(FOnRequestBackground) then
    FOnRequestBackground(Self, FBackground);
end;

function TJvTurtle.txBackground: string;
var
  Name: string;
begin
  if GetTex(Name) then
  begin
    FBackground := Name;
    DoRequestBackground;
    Result := '';
  end
  else
    Result := Format(RsInvalidTextIns, ['background']);
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
    Result := Format(RsInvalidTextIns, ['text']);
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
    Result := Format(RsInvalidColorIns, ['addbrushcolor']);
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
    Result := Format(RsInvalidColorIns, ['addbrushcolor']);
end;

function TJvTurtle.txGoMarkAngle: string;
begin
  Heading := FAngleMark;
  Result := '';
end;

function TJvTurtle.txMarkAngle: string;
begin
  FAngleMark := Variant(Heading);
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
  Result := FNSP > 0;
  if Result then
  begin
    Dec(FNSP);
    Num := FNStack[FNSP];
    Msg := '';
  end
  else
    Msg := RsNumberStackUnderflow;
end;

function TJvTurtle.NPush(var Msg: string; Num: Integer): Boolean;
begin
  try
    if FNSP >= Length(FNStack) then
      SetLength(FNStack, Length(FNStack) + 256);
    FNStack[FNSP] := Num;
    Inc(FNSP);
    Msg := '';
    Result := True;
  except
    Msg := RsNumberStackOverflow;
    Result := False;
  end;
end;

function TJvTurtle.txComment: string;
begin
  while (FIP <= FIPMax) and (FScript[FIP] <> '}') do
    Inc(FIP);
  if FIP <= FIPMax then
  begin
    Inc(FIP);
    Result := '';
  end
  else
    Result := RsMissingAfterComment;
end;
(*)

function TJvTurtle.SkipBlock: Boolean;
begin
  Result := False;
  while (FIP <= FIPMax) and (FScript[FIP] <> '[') do
    Inc(FIP);
  if FIP > FIPMax then
    Exit;
  Inc(FIP);
  while (FIP <= FIPMax) and (FScript[FIP] <> ']') do
    Inc(FIP);
  if FIP > FIPMax then
    Exit;
  Inc(FIP);
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
    FOnRequestImageSize(Self, FImageRect);
end;

function TJvTurtle.txGoBottom: string;
var
  NewPoint: TPoint;
begin
  if DoRequestImageSize then
  begin
    NewPoint := Point(Position.X, FImageRect.Bottom);
    DoGo(NewPoint);
    Result := '';
  end
  else
    Result := Format(RsErrorIns, ['gobottom']);
end;

function TJvTurtle.txGoLeft: string;
var
  NewPoint: TPoint;
begin
  if DoRequestImageSize then
  begin
    NewPoint := Point(FImageRect.Left, Position.Y);
    DoGo(NewPoint);
    Result := '';
  end
  else
    Result := Format(RsErrorIns, ['goleft']);
end;

function TJvTurtle.txGoRight: string;
var
  NewPoint: TPoint;
begin
  if DoRequestImageSize then
  begin
    NewPoint := Point(FImageRect.Right, Position.Y);
    DoGo(NewPoint);
    Result := '';
  end
  else
    Result := Format(RsErrorIns, ['goright']);
end;

function TJvTurtle.txGoTop: string;
var
  NewPoint: TPoint;
begin
  if DoRequestImageSize then
  begin
    NewPoint := Point(Position.X, FImageRect.Top);
    DoGo(NewPoint);
    Result := '';
  end
  else
    Result := Format(RsErrorIns, ['gotop']);
end;

function TJvTurtle.txDiv: string;
var
  A, B: Integer;
begin
  if NPop(Result, B) and NPop(Result, A) then
    if B <> 0 then
      NPush(Result, A div B)
    else
      Result := RsDivisionByZero;
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
    CX := (FImageRect.Right - FImageRect.Left) div 2;
    CY := (FImageRect.Bottom - FImageRect.Top) div 2;
    DoGo(Point(CX, CY));
    Result := '';
  end
  else
    Result := Format(RsErrorIns, ['gocenter']);
end;

function TJvTurtle.txDiamond: string;
var
  I, X: Integer;
  OldDown: Boolean;
begin
  Result := Format(RsInvalidIntegerIns, ['diamond']);
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
    Result := Format(RsInvalidParameterIns, ['curve']);
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
    Result := Format(RsInvalidIntegerIns, ['move']);
end;

procedure TJvTurtle.SetOnRequestFilter(const Value: TRequestFilterEvent);
begin
  FOnRequestFilter := Value;
end;

procedure TJvTurtle.DoRequestFilter;
begin
  if Assigned(FOnRequestFilter) then
    FOnRequestFilter(Self, FFilter);
end;

function TJvTurtle.txFilter: string;
var
  AName: string;
begin
  if GetTex(AName) then
  begin
    FFilter := AName;
    DoRequestFilter;
    Result := '';
  end
  else
    Result := Format(RsInvalidTextIns, ['filter']);
end;

function TJvTurtle.txUser(Sym: string): string;
var
  P: Integer;
begin
  P := Pos(Sym, FScript);
  if P <> 0 then
  begin
    if Push(FIP) then
    begin
      FIP := P + Length(Sym);
      Result := '';
    end
    else
      Result := RsStackOverflow;
  end
  else
    Result := Format(RsSymbolsIsNotDefined, [Sym]);
end;

function TJvTurtle.txBlock: string;
begin
  while (FIP <= FIPMax) and (FScript[FIP] <> ']') do
    Inc(FIP);
  if FIP <= FIPMax then
  begin
    Inc(FIP);
    Result := '';
  end
  else
    Result := RsMissingAfterBlock;
end;

function TJvTurtle.txReturn: string;
var
  Num: Integer;
begin
  if Pop(Num) then
  begin
    FIP := Num;
    Result := '';
  end
  else
    Result := RsStackUnderflow;
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
    NPush(Result, FImageRect.Bottom)
  else
    Result := Format(RsErrorIns, ['=bottom']);
end;

function TJvTurtle.tx_BrushColor: string;
begin
  NPush(Result, Canvas.Brush.Color);
end;

function TJvTurtle.tx_Left: string;
begin
  if DoRequestImageSize then
    NPush(Result, FImageRect.Left)
  else
    Result := Format(RsErrorIns, ['=left']);
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
    Result := Format(RsStackUnderflowIns, ['=loop']);
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
    NPush(Result, FImageRect.Right)
  else
    Result := Format(RsErrorIns, ['=right']);
end;

function TJvTurtle.tx_Top: string;
begin
  if DoRequestImageSize then
    NPush(Result, FImageRect.Top)
  else
    Result := Format(RsErrorIns, ['=top']);
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
        Result := RsSymbolExpectedAfterIf;
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
      Result := RsCanNotTakeSqrtOf;
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
  Result := Format(RsInvalidIntegerIns, ['polygon']);
  if not (GetNum(N) and GetNum(S)) then
    Exit;
  Result := Format(RsNotAllowedIns, ['polygon']);
  if (N = 0) or (S = 0) then
    Exit;
  Result := Format(RsNeedMinimumOfSidesIns, ['polygon']);
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
  I, S, N: Integer;
  OldDown: Boolean;
  A, OldHeading: Real;
  Pt: TPoint;
begin
  Result := Format(RsInvalidIntegerIns, ['star']);
  if not (GetNum(N) and GetNum(S)) then
    Exit;
  Result := Format(RsNotAllowedIns, ['star']);
  if (N = 0) or (S = 0) then
    Exit;
  Result := Format(RsNeedMinimumOfSidesIns, ['star']);
  if N < 3 then
    Exit;
  Result := Format(RsMaximumSidesExceededIns, ['star']);
  if N > 12 then
    Exit;
  OldHeading := Heading;
  Pt := Position;
  OldDown := PenDown;
  PenDown := True;
  A := (N div 2) * 360 / N;
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
    Result := Format(RsInvalidIntegerIns, ['lineto']);
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
    Result := Format(RsInvalidIntegerIns, ['roundrect']);
end;

function TJvTurtle.txDefault: string;
begin
  Result := '';
  Heading := 0;
  Position := Point(0, 0);
  PenDown := False;
  if Assigned(Canvas) then
  begin
    Canvas.Pen.Color := clWindowText;  // (rom) from clBlack
    Canvas.Brush.Color := clWindow;    // (rom) from clWhite
    Canvas.Font.Color := clWindowText; // (rom) added
    Canvas.CopyMode := cmSrcCopy;
  end;
  Mark := Position;
  Area := Rect(0, 0, 0, 0);
end;

function TJvTurtle.txIn: string;
var
  Token: string;
  Num: Integer;
  N: Integer;
begin
  if NPop(Result, Num) then
    if GetToken(Token) then
    begin
      if not FVariables.Find(Token, N) then
        N := FVariables.Add(Token);
      FVariables.Objects[N] := TObject(Num);
      Result := '';
    end
    else
      Result := RsTokenExpected;
end;

function TJvTurtle.IsVar(Tex: string): Boolean;
var
  N: Integer;
  Msg: string;
begin
  Result := FVariables.Find(Tex, N);
  if Result then
    Result := NPush(Msg, Integer(FVariables.Objects[N]));
end;

function TJvTurtle.txInAdd: string;
var
  Token: string;
  N, Num: Integer;
begin
  if NPop(Result, Num) then
    if GetToken(Token) then
    begin
      if FVariables.Find(Token, N) then
      begin
        FVariables.Objects[N] := TObject(Integer(FVariables.Objects[N]) + Num);
        Result := '';
      end
      else
        Result := Format(RssDoesNotExist, [Token]);
    end
    else
      Result := RsTokenExpected;
end;

function TJvTurtle.txInSub: string;
var
  Token: string;
  N, Num: Integer;
begin
  if NPop(Result, Num) then
    if GetToken(Token) then
    begin
      if FVariables.Find(Token, N) then
      begin
        FVariables.Objects[N] := TObject(Integer(FVariables.Objects[N]) - Num);
        Result := '';
      end
      else
        Result := Format(RssDoesNotExist, [Token]);
    end
    else
      Result := RsTokenExpected;
end;

function TJvTurtle.txInMult: string;
var
  Token: string;
  N, Num: Integer;
begin
  if NPop(Result, Num) then
    if GetToken(Token) then
    begin
      if FVariables.Find(Token, N) then
      begin
        FVariables.Objects[N] := TObject(Integer(FVariables.Objects[N]) * Num);
        Result := '';
      end
      else
        Result := Format(RssDoesNotExist, [Token]);
    end
    else
      Result := RsTokenExpected;
end;

function TJvTurtle.txInDiv: string;
var
  Token: string;
  N, Num: Integer;
begin
  if NPop(Result, Num) then
    if Num = 0 then
      Result := RsDivisionByZeroNotAllowedInIn
    else
    if GetToken(Token) then
    begin
      if FVariables.Find(Token, N) then
      begin
        FVariables.Objects[N] := TObject(Integer(FVariables.Objects[N]) div Num);
        Result := '';
      end
      else
        Result := Format(RssDoesNotExist, [Token]);
    end
    else
      Result := RsTokenExpected;
end;

function TJvTurtle.txInDec: string;
var
  Token: string;
  N: Integer;
begin
  if GetToken(Token) then
  begin
    if FVariables.Find(Token, N) then
    begin
      FVariables.Objects[N] := TObject(Integer(FVariables.Objects[N]) - 1);
      Result := '';
    end
    else
      Result := Format(RssDoesNotExist, [Token]);
  end
  else
    Result := RsTokenExpected;
end;

function TJvTurtle.txInInc: string;
var
  Token: string;
  N: Integer;
begin
  if GetToken(Token) then
  begin
    if FVariables.Find(Token, N) then
    begin
      FVariables.Objects[N] := TObject(Integer(FVariables.Objects[N]) + 1);
      Result := '';
    end
    else
      Result := Format(RssDoesNotExist, [Token]);
  end
  else
    Result := RsTokenExpected;
end;

end.

