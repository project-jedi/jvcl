{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvArrayButton.PAS, released on 2002-06-15.

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

unit JvArrayButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons;

type
  TArrayButtonClicked = procedure(ACol, ARow: Integer) of object;

  TJvArrayButton = class(TGraphicControl)
  private
    FPtDown: TPoint;
    FPushDown: boolean;
    FColor: TColor;
    FRows: Integer;
    FCols: Integer;
    FOnArrayButtonClicked: TArrayButtonClicked;
    FCaptions: TStringList;
    FColors: TStringList;
    FHints: TStringList;
  {$IFDEF JVCLThemesEnabled}
    FThemed: Boolean;
    procedure SetThemed(Value: Boolean);
  {$ENDIF}
    procedure SetCols(const Value: Integer);
    procedure SetRows(const Value: Integer);
    procedure SetOnArrayButtonClicked(const Value: TArrayButtonClicked);
    procedure SetCaptions(const Value: TStringList);
    procedure SetColors(const Value: TStringList);
    procedure MouseToCell(const x, y: Integer; var ACol, ARow: Integer);
    function CellRect(ACol, ARow: Integer): TRect;
    procedure SetHints(const Value: TStringList);
    { Private declarations }
  protected
    { Protected declarations }
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  {$IFDEF JVCLThemesEnabled}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    {Creates and initializes an instance of TJvArrayButton. }
    destructor Destroy; override;
    {Destroys an instance of TJvArrayButton.}
    procedure Paint; override;
    {Renders the image of the button.}
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    {this procedure can be used in response to a application.onshowhint event
     button hints are stored in the hints property from arry top-left to array bottom right
     in your application create a seperate onshowhint event Handler
     within that Handler test hintinfo.hintcontrol is this object. If it is dispatch to this objects doShowHint.
     In the formcreate eventHandler include:
       application.OnShowHint:=drawhint;

     procedure TDrawF.DrawHint(var HintStr: string; var CanShow: Boolean;
     var HintInfo: THintInfo);
     begin
       if HintInfo.HintControl=janArrayButton1 then
          janArrayButton1.DoShowHint(Hintstr,canshow,hintinfo);
     end;

     I could have set the application.onshowhint Handler directly in this component,
     but if you have more components that do this then only the last one would work
     }
  published
    { Published declarations }
    property Align;
    property Rows: Integer read FRows write SetRows;
    property Cols: Integer read FCols write SetCols;
    property Font;
    property Captions: TStringList read FCaptions write SetCaptions;
    {A List of button captions from the top-left to the bottom-right button}
    property Hints: TStringList read FHints write SetHints;
    {A List of button hints from the top-left to the bottom-right button}
    property Colors: TStringList read FColors write SetColors;
    {A List of button Colors from the top-left to the bottom-right button
     values must standard delphi Color names like clred, clblue or hex Color strings like $0000ff for red.
     please note the hex order in Delphi is BGR i.s.o. the RGB order you may know from HTML hex Color triplets}
    property Hint;
    property ShowHint;
  {$IFDEF JVCLThemesEnabled}
    property Themed: Boolean read FThemed write SetThemed default False;
  {$ENDIF}
    property OnArrayButtonClicked: TArrayButtonClicked read FOnArrayButtonClicked write SetOnArrayButtonClicked;
    {provides you with the Column and Row of the clicked button
    the topleft button has Column=0 and Row=0}
  end;

implementation

uses
  JvThemes;

{ TJvArrayButton }

constructor TJvArrayButton.Create(AOwner: TComponent);
begin
  inherited;
  width := 35;
  height := 35;
  FColor := clSilver;
  FPushDown := false;
  FCols := 1;
  FRows := 1;
  showhint := true;
  FCaptions := TStringList.create;
  FHints := TStringList.create;
  FColors := TStringList.create;
{$IFDEF JVCLThemesEnabled}
  FThemed := False;
{$ENDIF}
end;

procedure TJvArrayButton.MouseToCell(const x, y: Integer; var ACol, ARow: Integer);
var
  dh, dw: Integer;
begin
  dh := (height - 2) div FRows;
  dw := (width - 2) div FCols;
  ACol := x div dw;
  ARow := y div dh;
end;

procedure TJvArrayButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  if button = mbleft then
  begin
    FPushDown := true;
    {  dh:=(height-2) div FRows;
      dw:=(width-2) div FCols;
      Col:=x div dw;
      Row:=y div dh;}
    MouseToCell(x, y, Col, Row);
    FptDown := point(Col, Row);
    invalidate;
  end;
end;

procedure TJvArrayButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if button = mbleft then
  begin
    FPushDown := false;
    invalidate;
    if assigned(FonArraybuttonClicked) then
      onarraybuttonclicked(FptDown.x, FptDown.y);
  end
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvArrayButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Paint;
end;

procedure TJvArrayButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  Paint;
end;

procedure TJvArrayButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  Paint;
end;
{$ENDIF}

procedure TJvArrayButton.Paint;
var
  R: TRect;
  Col, Row: Integer;
  dh, dw: Integer;
  x0, y0: Integer;
  cap: string;
  backColor: TColor;
  index: Integer;

  procedure DrawBackground(AColor: TColor);
  begin
    Canvas.Brush.Color := AColor;
    DrawThemedBackground(Self, Canvas, R);
  end;

  procedure drawup;
  begin
{$IFDEF JVCLThemesEnabled}
    if FThemed and ThemeServices.ThemesEnabled then
    begin
      R := DrawThemedButtonFace(Self, Canvas, R, 0, bsAutoDetect, False, False, False,
        PtInRect(R, ScreenToClient(Mouse.CursorPos)));
      SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    end
    else
    begin
{$ENDIF}
    DrawBackground(BackColor);
    Frame3D(Self.Canvas, R, clBtnHighlight, clBlack, 1);
{$IFDEF JVCLThemesEnabled}
    end;
{$ENDIF}
    if cap <> '' then
      DrawText(Canvas.Handle, PChar(cap), -1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

  procedure drawdown;
  begin
{$IFDEF JVCLThemesEnabled}
    if FThemed and ThemeServices.ThemesEnabled then
    begin
      R := DrawThemedButtonFace(Self, Canvas, R, 0, bsAutoDetect, False, True, False,
        PtInRect(R, ScreenToClient(Mouse.CursorPos)));
      SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    end
    else
    begin
{$ENDIF}
    DrawBackground(BackColor);
    Frame3D(Self.Canvas, R, clblack, clBtnHighlight, 1);
{$IFDEF JVCLThemesEnabled}
    end;
{$ENDIF}
    if cap <> '' then
      DrawText(Canvas.Handle, PChar(cap), -1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

begin
  dh := (height - 2) div FRows;
  dw := (width - 2) div FCols;
  for Row := 0 to FRows - 1 do
  begin
    y0 := 1 + Row * dh;
    for Col := 0 to FCols - 1 do
    begin
      x0 := 1 + Col * dw;
      R := rect(x0, y0, x0 + dw, y0 + dh);
      index := Row * FCols + Col;
      if index < FCaptions.count then
        cap := FCaptions[index]
      else
        cap := '';
      if index < FColors.count then
      try
        backColor := stringtoColor(FColors[index]);
      except
        backColor := clsilver;
      end
      else
        backColor := clsilver;
      if (csDesigning in ComponentState) then
        drawup
      else if (FptDown.x = Col) and (FptDown.y = Row) then
      begin
        if FPushDown then
          drawdown
        else
          drawup;
      end
      else
        drawup;
    end;
  end;
end;

destructor TJvArrayButton.Destroy;
begin
  FCaptions.free;
  FHints.Free;
  FColors.free;
  inherited;
end;

procedure TJvArrayButton.SetCols(const Value: Integer);
begin
  if FCols <> Value then
  begin
    if (value >= 1) and (value <= 10) then
    begin
      FCols := value;
      invalidate;
    end
  end;
end;

procedure TJvArrayButton.SetRows(const Value: Integer);
begin
  if FRows <> Value then
  begin
    if (value >= 1) and (value <= 10) then
    begin
      FRows := value;
      invalidate;
    end
  end;
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvArrayButton.SetThemed(Value: Boolean);
begin
  if Value <> FThemed then
  begin
    FThemed := Value;
    if FThemed then
      IncludeThemeStyle(Self, [csParentBackground])
    else
      ExcludeThemeStyle(Self, [csParentBackground]);
    Invalidate;
  end;
end;
{$ENDIF}

procedure TJvArrayButton.SetonArrayButtonClicked(
  const Value: TArrayButtonClicked);
begin
  FonArrayButtonClicked := Value;
end;

procedure TJvArrayButton.SetCaptions(const Value: TStringList);
begin
  FCaptions.assign(Value);
  invalidate;
end;

procedure TJvArrayButton.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font.Assign(font);
  invalidate;
end;

procedure TJvArrayButton.SetColors(const Value: TStringList);
begin
  FColors.assign(Value);
  invalidate;
end;

function TJvArrayButton.CellRect(ACol, ARow: Integer): TRect;
var
  dh, dw, x0, y0: Integer;
begin
  dh := (height - 2) div FRows;
  dw := (width - 2) div FCols;
  y0 := 1 + ARow * dh;
  x0 := 1 + ACol * dw;
  //  pt1:=clienttoscreen(point(x0,y0));
  //  pt2:=clienttoscreen(point(x0+dw,y0+dh));
  //  result:=rect(pt1.x,pt1.y,pt2.x,pt2.y);
  result := rect(x0, y0, x0 + dw, y0 + dh);
end;

procedure TJvArrayButton.DoShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
var
  ACol, ARow, x, y: Integer;
  index: Integer;
begin
  if HintInfo.HintControl = self then
  begin
    x := HintInfo.CursorPos.x;
    y := HintInfo.cursorPos.y;
    MouseToCell(x, y, ACol, ARow);
    if ((ACol < 0) or (ARow < 0)) then exit;
    index := ARow * FCols + ACol;
    if index < FHints.count then
      Hintstr := FHints[index]
    else
      Hintstr := Hint;
    Hintinfo.CursorRect := CellRect(ACol, ARow);
    canshow := true;
  end;
end;

procedure TJvArrayButton.SetHints(const Value: TStringList);
begin
  FHints.assign(Value);
end;

end.
