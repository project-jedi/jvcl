{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvArrayButton.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvArrayButton;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls, Forms, Types,
  JvComponent, JvTypes;

type
  TArrayButtonClicked = procedure(ACol, ARow: Integer) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvArrayButton = class(TJvGraphicControl)
  private
    FPtDown: TPoint;
    FPushDown: Boolean;
    FDown: Boolean;
    FRows: Integer;
    FCols: Integer;
    FOnArrayButtonClicked: TArrayButtonClicked;
    FCaptions: TStringList;
    FColors: TStringList;
    FHints: THintStringList;
    FEnableds: array of Boolean;
    FDowns: array of Boolean;
    FMouseOverBtn: TPoint;
    FThemed: Boolean;
    procedure SetThemed(Value: Boolean);
    procedure SetMouseOverBtn(Col, Row: Integer);
    function GetCaptions: TStrings;
    function GetColors: TStrings;
    procedure SetCols(Value: Integer);
    procedure SetRows(Value: Integer);
    procedure SetCaptions(const Value: TStrings);
    procedure SetColors(const Value: TStrings);
    function CellRect(ACol, ARow: Integer): TRect;
    procedure SetHints(const Value: THintStringList);
    function GetEnableds(Index: Integer): Boolean;
    procedure SetEnableds(Index: Integer; const Value: Boolean);
    function GetDowns(Index: Integer): Boolean;
    procedure SetDowns(Index: Integer; const Value: Boolean);
    procedure InvalidateCell(Col, Row: Integer);
  protected
    procedure FontChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure Paint; override;
    procedure SizeChanged; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MouseToCell(const X, Y: Integer; var ACol, ARow: Integer): Boolean;
    function GetCellRect(ACol, ARow: Integer; var R: TRect): Boolean;

    {this procedure can be used in response to a Application.OnShowHint event
     button hints are stored in the hints property from array top-left to array bottom right
     in your application create a separate OnShowHint event Handler
     within that Handler test HintInfo.HintControl is this object. If it is dispatch to this objects doShowHint.
     In the FormCreate event handler include:
       Application.OnShowHint := DrawHint;

     procedure TDrawF.DrawHint(var HintStr: string; var CanShow: Boolean;
       var HintInfo: THintInfo);
     begin
       if HintInfo.HintControl = JvArrayButton1 then
          JvArrayButton1.DoShowHint(HintStr, CanShow, HintInfo);
     end;

     I could have set the Application.OnShowHint handler directly in this component,
     but if you have more components that do this then only the last one would work
     }
    procedure DoShowHint(var HintStr: THintString; var CanShow: Boolean; var HintInfo: THintInfo);

    // A list of individual button Enabled state, from the top-left to the bottom-right button
    property Enableds[Index: Integer]: Boolean read GetEnableds write SetEnableds;
    property Downs[Index: Integer]: Boolean read GetDowns write SetDowns;
  published
    property Align;
    property Anchors;
    property Rows: Integer read FRows write SetRows;
    property Cols: Integer read FCols write SetCols;
    {A List of button captions from the top-left to the bottom-right button}
    property Captions: TStrings read GetCaptions write SetCaptions;
    property Enabled;
    property Font;
    property Height default 35;
    {A List of button hints from the top-left to the bottom-right button}
    property Hints: THintStringList read FHints write SetHints;
    {A List of button Colors from the top-left to the bottom-right button
     values must be standard Delphi Color names like clRed, clBlue or hex Color strings like $0000ff for red.
     please note the hex order in Delphi is BGR i.s.o. the RGB order you may know from HTML hex Color triplets}
    property Colors: TStrings read GetColors write SetColors;
    property Color default clSilver;
    {$IFDEF RTL240_UP}
    [Default(False)]  // for proper XE3 IDE work
    {$ENDIF RTL240_UP}
    property CanDown: Boolean read FDown write FDown default False;	
    property Hint;
    property ShowHint default True;
    property Themed: Boolean read FThemed write SetThemed default False;
    property Visible;
    property Width default 35;
    {provides you with the Column and Row of the clicked button
    the topleft button has Column=0 and Row=0}
    property OnArrayButtonClicked: TArrayButtonClicked read FOnArrayButtonClicked write FOnArrayButtonClicked;
    property OnCanResize;
    property OnMouseDown;
    {$IFDEF COMPILER9_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF COMPILER9_UP}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  ExtCtrls, Buttons,
  JvJCLUtils, JvThemes;

constructor TJvArrayButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 35;
  Height := 35;
  Color := clSilver;
  FPushDown := False;
  FCols := 1;
  FRows := 1;
  ShowHint := True;
  FCaptions := TStringList.Create;
  FHints := THintStringList.Create;
  FColors := TStringList.Create;
  ControlStyle := ControlStyle + [csOpaque]; // reduce flicker, we paint everthing
  FMouseOverBtn := Point(-1, -1);
  FThemed := False;
end;

destructor TJvArrayButton.Destroy;
begin
  FCaptions.Free;
  FHints.Free;
  FColors.Free;
  SetLength(FEnableds, 0);
  inherited Destroy;
end;

function TJvArrayButton.MouseToCell(const X, Y: Integer; var ACol, ARow: Integer): Boolean;
var
  DH, DW: Integer;
begin
  DH := (Height - 2) div Rows;
  DW := (Width - 2) div Cols;

  ACol := -1;
  ARow := -1;
  Result := False;
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    ACol := (X - 1) div DW;
    ARow := (Y - 1) div DH;
    if (ACol < 0) or (ARow < 0) or (ACol > Cols) or (ARow > Rows) then
    begin
      ACol := -1;
      ARow := -1;
    end
    else
      Result := True;
  end;
end;

function TJvArrayButton.GetCellRect(ACol, ARow: Integer; var R: TRect): Boolean;
begin
  if (ACol >= 0) and (ACol < Cols) and (ARow >= 0) and (ARow < Rows) then
  begin
    R := CellRect(ACol, ARow);
    Result := True;
  end
  else
    Result := False;
end;

procedure TJvArrayButton.InvalidateCell(Col, Row: Integer);
var
  R: TRect;
begin
  if Visible and (Parent <> nil) and Parent.HandleAllocated then
    if GetCellRect(Col, Row, R) then
      InvalidateRect(Parent.Handle, R, False);
end;

procedure TJvArrayButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  Index: Integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Button = mbLeft then
    begin
      if MouseToCell(X, Y, Col, Row) then
      begin
        Index := Row * Cols + Col;
        if FEnableds[Index] then
        begin
          FPushDown := True;
          FPtDown := Point(Col, Row);
          InvalidateCell(Col, Row);
        end;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TJvArrayButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  Index: Integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    if (Button = mbLeft) and FPushDown then
    begin
      FPushDown := False;
      if MouseToCell(X, Y, Col, Row) and (Col = FPtDown.X) and (Row = FPtDown.Y) then
      begin
        Index := FPtDown.Y * Cols + FPtDown.X;
        if FEnableds[Index] then
        begin
          if FDown then
            FDowns[Index] := not FDowns[Index];
          InvalidateCell(FPtDown.X, FPtDown.Y);
          if Assigned(FOnArrayButtonClicked) then
            OnArrayButtonClicked(FPtDown.X, FPtDown.Y);
        end;
      end;
    end;
  end;
  inherited MouseUp(BUtton, Shift, X, Y);
end;

procedure TJvArrayButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
begin
  if not (csDesigning in ComponentState) then
  begin
    MouseToCell(X, Y, Pt.X, Pt.Y);
    SetMouseOverBtn(Pt.X, Pt.Y);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TJvArrayButton.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  if not (csDesigning in ComponentState) then
    SetMouseOverBtn(-1, -1);
end;

procedure TJvArrayButton.SetMouseOverBtn(Col, Row: Integer);
begin
  if (Col <> FMouseOverBtn.X) or (Row <> FMouseOverBtn.Y) then
  begin
    if (FMouseOverBtn.X <> -1) or (FMouseOverBtn.Y <> -1) then
      if Themed or (FPushDown and (FMouseOverBtn.X = FPtDown.X) and (FMouseOverBtn.Y = FPtDown.Y)) then
        InvalidateCell(FMouseOverBtn.X, FMouseOverBtn.Y);
    FMouseOverBtn := Point(Col, Row);
    if (FMouseOverBtn.X <> -1) or (FMouseOverBtn.Y <> -1) then
      if Themed or (FPushDown and (FMouseOverBtn.X = FPtDown.X) and (FMouseOverBtn.Y = FPtDown.Y)) then
        InvalidateCell(FMouseOverBtn.X, FMouseOverBtn.Y);
  end;
end;

procedure TJvArrayButton.Paint;
var
  R: TRect;
  BackColor: TColor;
  Cap: string;

  procedure DrawThemedBkgrnd(ACanvas: TCanvas; ARect: TRect);
  begin
    ACanvas.Brush.Color := Color;
    InflateRect(ARect, 1, 1);
    SetBkMode(ACanvas.Handle, Windows.TRANSPARENT);
    ACanvas.FillRect(ARect);
  end;

  procedure DrawBackground(ACanvas: TCanvas; AColor: TColor);
  begin
    ACanvas.Brush.Color := AColor;
    DrawThemedBackground(Self, ACanvas, R);
  end;

  procedure DrawUp(ACanvas: TCanvas; AMouseOver: Boolean);
  begin
    {$IFDEF JVCLThemesEnabled}
    if Themed and StyleServices.Enabled then
    begin
      DrawThemedBkgrnd(ACanvas, R);
      R := DrawThemedButtonFace(Self, ACanvas, R, 0, bsAutoDetect, False, False, False, AMouseOver);
      SetBkMode(ACanvas.Handle, Windows.TRANSPARENT);
    end
    else
    {$ENDIF JVCLThemesEnabled}
    begin
      DrawBackground(ACanvas, BackColor);
      Frame3D(ACanvas, R, clBtnHighlight, clBlack, 1);
    end;
    if Cap <> '' then
      DrawText(ACanvas, Cap, -1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

  procedure DrawDown(ACanvas: TCanvas; AMouseOver: Boolean);
  begin
    {$IFDEF JVCLThemesEnabled}
    if Themed and StyleServices.Enabled then
    begin
      DrawThemedBkgrnd(ACanvas, R);
      R := DrawThemedButtonFace(Self, ACanvas, R, 0, bsAutoDetect, False, True, False, AMouseOver);
      SetBkMode(ACanvas.Handle, Windows.TRANSPARENT);
    end
    else
    {$ENDIF JVCLThemesEnabled}
    begin
      DrawBackground(ACanvas, BackColor);
      Frame3D(ACanvas, R, clBlack, clBtnHighlight, 1);
    end;
    if Cap <> '' then
      DrawText(ACanvas, Cap, -1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

var
  Col, Row: Integer;
  DH, DW: Integer;
  X0, Y0: Integer;
  Index: Integer;
  l: Integer;
  OldBmp, Bmp: HBITMAP;
  BmpDC: HDC;
  BmpCanvas: TCanvas;
begin
  DH := (Height - 2) div Rows;
  DW := (Width - 2) div Cols;

  // WinAPI is much faster than TBitmap that fills the rect and copies the image what we don't need.
  OldBmp := 0;
  R := Canvas.ClipRect;
  Bmp := CreateCompatibleBitmap(Canvas.Handle, Width, Height);
  BmpDC := CreateCompatibleDC(Canvas.Handle);
  BmpCanvas := TCanvas.Create;
  try
    OldBmp := SelectObject(BmpDC, Bmp);
    BmpCanvas.Handle := BmpDC;
    IntersectClipRect(BmpDC, R.Left, R.Top, R.Right, R.Bottom);

    // Fill the border areas
    BmpCanvas.Brush.Color := Color;
    BmpCanvas.FillRect(Rect(0, 0, Width, 1));
    BmpCanvas.FillRect(Rect(0, 0, 1, Height));
    BmpCanvas.FillRect(Rect(DW * Cols, 1, Width, Height));
    BmpCanvas.FillRect(Rect(0, DH * Rows, Width, Height));

    BmpCanvas.Font := Font;
    // Draw the button cells
    for Row := 0 to Rows - 1 do
    begin
      Y0 := 1 + Row * DH;
      for Col := 0 to Cols - 1 do
      begin
        X0 := 1 + Col * DW;
        R := Rect(X0, Y0, X0 + DW, Y0 + DH);
        if RectVisible(BmpDC, R) then
        begin
          Index := Row * Cols + Col;
          if Index < Captions.Count then
            Cap := Captions[Index]
          else
            Cap := '';
          if Index < Colors.Count then
            try
              BackColor := StringToColor(Colors[Index]);
            except
              BackColor := Color;
            end
          else
            BackColor := Color;

          MouseOver := (Col = FMouseOverBtn.X) and (Row = FMouseOverBtn.Y);
          if (csDesigning in ComponentState) then
            DrawUp(BmpCanvas, MouseOver)
          else
          if (FPtDown.X = Col) and (FPtDown.Y = Row) then
          begin
            if (FPushDown and MouseOver) or FDowns[Index] then
              DrawDown(BmpCanvas, MouseOver)
            else
              DrawUp(BmpCanvas, MouseOver);
          end
          else
          if FDowns[Index]  then
            DrawDown(BmpCanvas, MouseOver)
          else
            DrawUp(BmpCanvas, MouseOver);

          if Col = Cols -1 then
          begin
            l := Width - X0;
            if l <> 0 then
              DrawThemedBkgrnd(BmpCanvas, Rect(X0 + DW + 2, 0, R.Right + l, Height));
          end;
        end;
      end;
    end;
    BitBlt(Canvas, 0, 0, Width, Height, BmpCanvas, 0, 0, SRCCOPY);
  finally
    BmpCanvas.Handle := 0;
    BmpCanvas.Free;
    SelectObject(BmpDC, OldBmp);
    DeleteDC(BmpDC);
    DeleteObject(Bmp);
  end;
end;

procedure TJvArrayButton.SetCols(Value: Integer);
begin
  if FCols <> Value then
  begin
    if Value > 10 then
      Value := 10;
    if Value >= 1 then
    begin
      FCols := Value;
      Invalidate;
      SizeChanged;
    end;
  end;
end;

procedure TJvArrayButton.SetEnableds(Index: Integer; const Value: Boolean);
begin
  if FEnableds[Index] <> Value then
  begin
    FEnableds[Index] := Value;
    Invalidate;
  end;
end;

procedure TJvArrayButton.SetDowns(Index: Integer; const Value: Boolean);
begin
  if FDowns[Index] <> Value then
  begin
    FDowns[Index] := Value;
    Invalidate;
  end;
end;

procedure TJvArrayButton.SetRows(Value: Integer);
begin
  if FRows <> Value then
  begin
    if Value > 10 then
      Value := 10;
    if Value >= 1 then
    begin
      FRows := Value;
      Invalidate;
      SizeChanged;
    end;
  end;
end;

procedure TJvArrayButton.SizeChanged;
var
  OriginalEnableds: array of Boolean;
  OriginalDowns: array of Boolean;
  I: Integer;
  MinLength: Integer;
begin
  SetLength(OriginalEnableds, Length(FEnableds));
  for I := 0 to Length(FEnableds) - 1 do
    OriginalEnableds[I] := FEnableds[I];

  SetLength(FEnableds, Rows * Cols);

  MinLength := Length(OriginalEnableds);
  if MinLength > Length(FEnableds) then
    MinLength := Length(FEnableds);

  for I := 0 to MinLength - 1 do
    FEnableds[I] := OriginalEnableds[I];
  for I := MinLength to Length(FEnableds) - 1 do
    FEnableds[I] := True;

//-----------------------------------------------

  SetLength(OriginalDowns, Length(FDowns));

  for I := 0 to Length(FDowns) - 1 do
    OriginalDowns[I] := FDowns[I];

  SetLength(FDowns, Rows * Cols);

  MinLength := Length(OriginalDowns);
  if MinLength > Length(FDowns) then
    MinLength := Length(FDowns);

  for I := 0 to MinLength - 1 do
    FDowns[I] := OriginalDowns[I];
  for I := MinLength to Length(FDowns) - 1 do
    FDowns[I] := False;
end;

procedure TJvArrayButton.SetThemed(Value: Boolean);
begin
  if Value <> FThemed then
  begin
    FThemed := Value;
    {$IFDEF JVCLThemesEnabled}
//    if FThemed then
//      IncludeThemeStyle(Self, [csParentBackground])
//    else
//      ExcludeThemeStyle(Self, [csParentBackground]);
    Invalidate;
    {$ENDIF JVCLThemesEnabled}
  end;
end;

function TJvArrayButton.GetCaptions: TStrings;
begin
  Result := FCaptions;
end;

procedure TJvArrayButton.SetCaptions(const Value: TStrings);
begin
  FCaptions.Assign(Value);
  Invalidate;
end;

procedure TJvArrayButton.FontChanged;
begin
  inherited FontChanged;
  Canvas.Font.Assign(Font);
  Invalidate;
end;

function TJvArrayButton.GetColors: TStrings;
begin
  Result := FColors;
end;

function TJvArrayButton.GetEnableds(Index: Integer): Boolean;
begin
  Result := FEnableds[Index];
end;

function TJvArrayButton.GetDowns(Index: Integer): Boolean;
begin
  Result := FDowns[Index];
end;

procedure TJvArrayButton.SetColors(const Value: TStrings);
begin
  FColors.Assign(Value);
  Invalidate;
end;

function TJvArrayButton.CellRect(ACol, ARow: Integer): TRect;
var
  DH, DW, X0, Y0: Integer;
begin
  DH := (Height - 2) div Rows;
  DW := (Width - 2) div Cols;
  Y0 := 1 + ARow * DH;
  X0 := 1 + ACol * DW;
  Result := Rect(X0, Y0, X0 + DW, Y0 + DH);
end;

procedure TJvArrayButton.DoShowHint(var HintStr: THintString; var CanShow: Boolean; var HintInfo: THintInfo);
var
  ACol, ARow, X, Y: Integer;
  Index: Integer;
begin
  if HintInfo.HintControl = Self then
  begin
    X := HintInfo.CursorPos.X;
    Y := HintInfo.CursorPos.Y;
    if MouseToCell(X, Y, ACol, ARow) then
    begin
      Index := ARow * Cols + ACol;
      if Index < Hints.Count then
        HintStr := Hints[Index]
      else
        HintStr := Hint;
      HintInfo.CursorRect := CellRect(ACol, ARow);
      CanShow := True;
    end;
  end;
end;

procedure TJvArrayButton.SetHints(const Value: THintStringList);
begin
  FHints.Assign(Value);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
