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

Last Modified: 2003-10-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}

unit JvArrayButton;

interface

uses
  {$IFDEF COMPLIB_VCL}
  Windows, Messages, Graphics, Controls, Forms, ExtCtrls, Buttons,
  {$ENDIF}
  {$IFDEF COMPLIB_CLX}
  Types, QGraphics, QControls, QForms, QExtCtrls, QButtons,
  {$ENDIF}
  SysUtils, Classes,
  JvClxUtils;

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
    FMouseOverBtn: TPoint;
    FThemed: Boolean;
    procedure SetThemed(Value: Boolean);
  {$ENDIF}
    procedure SetCols(const Value: Integer);
    procedure SetRows(const Value: Integer);
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
    property OnArrayButtonClicked: TArrayButtonClicked read FOnArrayButtonClicked write FOnArrayButtonClicked;
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
  Width := 35;
  Height := 35;
  FColor := clSilver;
  FPushDown := false;
  FCols := 1;
  FRows := 1;
  ShowHint := true;
  FCaptions := TStringList.create;
  FHints := TStringList.create;
  FColors := TStringList.create;
{$IFDEF JVCLThemesEnabled}
  FThemed := False;
  FMouseOverBtn := Point(-1, -1);  
{$ENDIF}
end;

procedure TJvArrayButton.MouseToCell(const x, y: Integer; var ACol, ARow: Integer);
var
  dh, dw: Integer;
begin
  dh := (Height - 2) div FRows;
  dw := (Width - 2) div FCols;
  ACol := x div dw;
  ARow := y div dh;
end;

procedure TJvArrayButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  if Button = mbLeft then
  begin
    FPushDown := True;
    MouseToCell(x, y, Col, Row);
    FptDown := Point(Col, Row);
    Invalidate;
  end;
end;

procedure TJvArrayButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FPushDown := False;
    Invalidate;
    if Assigned(FOnArraybuttonClicked) then
      OnArrayButtonClicked(FptDown.x, FptDown.y);
  end
end;

{$IFDEF JVCLThemesEnabled}
procedure TJvArrayButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var Pt: TPoint;
begin
  inherited;
  MouseToCell(X, Y, Pt.X, Pt.Y);
  if (not FPushDown) and
     ((Pt.X <> FMouseOverBtn.X) or (Pt.Y <> FMouseOverBtn.Y)) then
  begin
    FMouseOverBtn := Pt;
    Invalidate;
  end;
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
  BackColor: TColor;
  Index: Integer;

  procedure DrawBackground(AColor: TColor);
  begin
    Canvas.Brush.Color := AColor;
    DrawThemedBackground(Self, Canvas, R);
  end;

  procedure DrawUp;
  begin
{$IFDEF JVCLThemesEnabled}
    if FThemed and ThemeServices.ThemesEnabled then
    begin
      R := DrawThemedButtonFace(Self, Canvas, R, 0, bsAutoDetect, False, False, False,
        PtInRect(R, ScreenToClient(Mouse.CursorPos)));
      SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    end
    else
{$ENDIF}
    begin
      DrawBackground(BackColor);
      Frame3D(Self.Canvas, R, clBtnHighlight, clBlack, 1);
    end;
    if cap <> '' then
      ClxDrawText(Canvas, cap, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

  procedure DrawDown;
  begin
{$IFDEF JVCLThemesEnabled}
    if FThemed and ThemeServices.ThemesEnabled then
    begin
      R := DrawThemedButtonFace(Self, Canvas, R, 0, bsAutoDetect, False, True, False,
        PtInRect(R, ScreenToClient(Mouse.CursorPos)));
      SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    end
    else
{$ENDIF}
    begin
      DrawBackground(BackColor);
      Frame3D(Self.Canvas, R, clblack, clBtnHighlight, 1);
    end;
    if cap <> '' then
      ClxDrawText(Canvas, cap, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

begin
  dh := (Height - 2) div FRows;
  dw := (Width - 2) div FCols;
  for Row := 0 to FRows - 1 do
  begin
    y0 := 1 + Row * dh;
    for Col := 0 to FCols - 1 do
    begin
      x0 := 1 + Col * dw;
      R := Rect(x0, y0, x0 + dw, y0 + dh);
      Index := Row * FCols + Col;
      if Index < FCaptions.Count then
        cap := FCaptions[Index]
      else
        cap := '';
      if Index < FColors.Count then
      try
        BackColor := StringToColor(FColors[Index]);
      except
        BackColor := clSilver;
      end
      else
        BackColor := clSilver;
      if (csDesigning in ComponentState) then
        DrawUp
      else if (FptDown.x = Col) and (FptDown.y = Row) then
      begin
        if FPushDown then
          DrawDown
        else
          DrawUp;
      end
      else
        DrawUp;
    end;
  end;
end;

destructor TJvArrayButton.Destroy;
begin
  FCaptions.Free;
  FHints.Free;
  FColors.Free;
  inherited;
end;

procedure TJvArrayButton.SetCols(const Value: Integer);
begin
  if FCols <> Value then
  begin
    if (Value >= 1) and (Value <= 10) then
    begin
      FCols := Value;
      Invalidate;
    end
  end;
end;

procedure TJvArrayButton.SetRows(const Value: Integer);
begin
  if FRows <> Value then
  begin
    if (Value >= 1) and (Value <= 10) then
    begin
      FRows := Value;
      Invalidate;
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

procedure TJvArrayButton.SetCaptions(const Value: TStringList);
begin
  FCaptions.Assign(Value);
  Invalidate;
end;

procedure TJvArrayButton.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font.Assign(font);
  Invalidate;
end;

procedure TJvArrayButton.SetColors(const Value: TStringList);
begin
  FColors.Assign(Value);
  Invalidate;
end;

function TJvArrayButton.CellRect(ACol, ARow: Integer): TRect;
var
  dh, dw, x0, y0: Integer;
begin
  dh := (Height - 2) div FRows;
  dw := (Width - 2) div FCols;
  y0 := 1 + ARow * dh;
  x0 := 1 + ACol * dw;
  //  pt1:=clienttoscreen(point(x0,y0));
  //  pt2:=clienttoscreen(point(x0+dw,y0+dh));
  //  result:=rect(pt1.x,pt1.y,pt2.x,pt2.y);
  Result := Rect(x0, y0, x0 + dw, y0 + dh);
end;

procedure TJvArrayButton.DoShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
var
  ACol, ARow, x, y: Integer;
  Index: Integer;
begin
  if HintInfo.HintControl = Self then
  begin
    x := HintInfo.CursorPos.x;
    y := HintInfo.CursorPos.y;
    MouseToCell(x, y, ACol, ARow);
    if ((ACol < 0) or (ARow < 0)) then
      Exit;
    Index := ARow * FCols + ACol;
    if Index < FHints.count then
      HintStr := FHints[Index]
    else
      HintStr := Hint;
    Hintinfo.CursorRect := CellRect(ACol, ARow);
    CanShow := True;
  end;
end;

procedure TJvArrayButton.SetHints(const Value: TStringList);
begin
  FHints.Assign(Value);
end;

end.
