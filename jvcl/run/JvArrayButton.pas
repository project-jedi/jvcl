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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvArrayButton;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  JvComponent, JvTypes;

type
  TArrayButtonClicked = procedure(ACol, ARow: Integer) of object;

  TJvArrayButton = class(TJvGraphicControl)
  private
    FPtDown: TPoint;
    FPushDown: Boolean;
    FColor: TColor;
    FRows: Integer;
    FCols: Integer;
    FOnArrayButtonClicked: TArrayButtonClicked;
    FCaptions: TStringList;
    FColors: TStringList;
    FHints: THintStringList;
    {$IFDEF JVCLThemesEnabled}
    FMouseOverBtn: TPoint;
    FThemed: Boolean;
    procedure SetThemed(Value: Boolean);
    {$ENDIF JVCLThemesEnabled}
    function GetCaptions: TStrings;
    function GetColors: TStrings;
    procedure SetCols(const Value: Integer);
    procedure SetRows(const Value: Integer);
    procedure SetCaptions(const Value: TStrings);
    procedure SetColors(const Value: TStrings);
    procedure MouseToCell(const X, Y: Integer; var ACol, ARow: Integer);
    function CellRect(ACol, ARow: Integer): TRect;
    procedure SetHints(const Value: THintStringList);
  protected
    procedure FontChanged; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$IFDEF JVCLThemesEnabled}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    {$ENDIF JVCLThemesEnabled}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure DoShowHint(var HintStr: THintString;
      var CanShow: Boolean; var HintInfo: THintInfo);

    {this procedure can be used in response to a Application.OnShowHint event
     button hints are stored in the hints property from array top-left to array bottom right
     in your application create a seperate OnShowHint event Handler
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
  published
    property Align;
    property Rows: Integer read FRows write SetRows;
    property Cols: Integer read FCols write SetCols;
    property Font;
    property Captions: TStrings read GetCaptions write SetCaptions;
    property Height default 35;
    {A List of button captions from the top-left to the bottom-right button}
    property Hints: THintStringList read FHints write SetHints;
    {A List of button hints from the top-left to the bottom-right button}
    property Colors: TStrings read GetColors write SetColors;
    {A List of button Colors from the top-left to the bottom-right button
     values must standard Delphi Color names like clRed, clBlue or hex Color strings like $0000ff for red.
     please note the hex order in Delphi is BGR i.s.o. the RGB order you may know from HTML hex Color triplets}
    property Hint;
    property ShowHint default True;
    {$IFDEF JVCLThemesEnabled}
    property Themed: Boolean read FThemed write SetThemed default False;
    {$ENDIF JVCLThemesEnabled}
    property Width default 35;
    property OnArrayButtonClicked: TArrayButtonClicked read FOnArrayButtonClicked write FOnArrayButtonClicked;
    {provides you with the Column and Row of the clicked button
    the topleft button has Column=0 and Row=0}
  end;

implementation

uses
  ExtCtrls, Buttons,
  JvJCLUtils, JvThemes;

constructor TJvArrayButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 35;
  Height := 35;
  FColor := clSilver;
  FPushDown := False;
  FCols := 1;
  FRows := 1;
  ShowHint := True;
  FCaptions := TStringList.Create;
  FHints := THintStringList.Create;
  FColors := TStringList.Create;
  {$IFDEF JVCLThemesEnabled}
  FThemed := False;
  FMouseOverBtn := Point(-1, -1);
  {$ENDIF JVCLThemesEnabled}
end;

destructor TJvArrayButton.Destroy;
begin
  FCaptions.Free;
  FHints.Free;
  FColors.Free;
  inherited Destroy;
end;

procedure TJvArrayButton.MouseToCell(const X, Y: Integer; var ACol, ARow: Integer);
var
  DH, DW: Integer;
begin
  DH := (Height - 2) div Rows;
  DW := (Width - 2) div Cols;
  ACol := X div DW;
  ARow := Y div DH;
end;

procedure TJvArrayButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
begin
  if Button = mbLeft then
  begin
    FPushDown := True;
    MouseToCell(X, Y, Col, Row);
    FPtDown := Point(Col, Row);
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
    if Assigned(FOnArrayButtonClicked) then
      OnArrayButtonClicked(FPtDown.X, FPtDown.Y);
  end
end;

{$IFDEF JVCLThemesEnabled}

procedure TJvArrayButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  MouseToCell(X, Y, Pt.X, Pt.Y);
  if (not FPushDown) and
    ((Pt.X <> FMouseOverBtn.X) or (Pt.Y <> FMouseOverBtn.Y)) then
  begin
    FMouseOverBtn := Pt;
    Invalidate;
  end;
end;

procedure TJvArrayButton.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  Repaint;
end;

procedure TJvArrayButton.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  Repaint;
end;

{$ENDIF JVCLThemesEnabled}

procedure TJvArrayButton.Paint;
var
  R: TRect;
  Col, Row: Integer;
  DH, DW: Integer;
  X0, Y0: Integer;
  Cap: string;
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
    if Themed and ThemeServices.ThemesEnabled then
    begin
      R := DrawThemedButtonFace(Self, Canvas, R, 0, bsAutoDetect, False, False, False,
        PtInRect(R, ScreenToClient(Mouse.CursorPos)));
      SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    end
    else
    {$ENDIF JVCLThemesEnabled}
    begin
      DrawBackground(BackColor);
      Frame3D(Self.Canvas, R, clBtnHighlight, clBlack, 1);
    end;
    if Cap <> '' then
      DrawText(Canvas, Cap, -1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

  procedure DrawDown;
  begin
    {$IFDEF JVCLThemesEnabled}
    if Themed and ThemeServices.ThemesEnabled then
    begin
      R := DrawThemedButtonFace(Self, Canvas, R, 0, bsAutoDetect, False, True, False,
        PtInRect(R, ScreenToClient(Mouse.CursorPos)));
      {$IFDEF VCL}
      SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
      {$ENDIF VCL}
    end
    else
    {$ENDIF JVCLThemesEnabled}
    begin
      DrawBackground(BackColor);
      Frame3D(Self.Canvas, R, clBlack, clBtnHighlight, 1);
    end;
    if Cap <> '' then
      DrawText(Canvas, Cap, -1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;

begin
  DH := (Height - 2) div Rows;
  DW := (Width - 2) div Cols;
  for Row := 0 to Rows - 1 do
  begin
    Y0 := 1 + Row * DH;
    for Col := 0 to Cols - 1 do
    begin
      X0 := 1 + Col * DW;
      R := Rect(X0, Y0, X0 + DW, Y0 + DH);
      Index := Row * Cols + Col;
      if Index < Captions.Count then
        Cap := Captions[Index]
      else
        Cap := '';
      if Index < Colors.Count then
        try
          BackColor := StringToColor(Colors[Index]);
        except
          BackColor := clSilver;
        end
      else
        BackColor := clSilver;
      if (csDesigning in ComponentState) then
        DrawUp
      else
      if (FPtDown.X = Col) and (FPtDown.Y = Row) then
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

procedure TJvArrayButton.SetCols(const Value: Integer);
begin
  if FCols <> Value then
    if (Value >= 1) and (Value <= 10) then
    begin
      FCols := Value;
      Invalidate;
    end;
end;

procedure TJvArrayButton.SetRows(const Value: Integer);
begin
  if FRows <> Value then
    if (Value >= 1) and (Value <= 10) then
    begin
      FRows := Value;
      Invalidate;
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
{$ENDIF JVCLThemesEnabled}

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
  //  pt1:=clienttoscreen(point(X0,Y0));
  //  pt2:=clienttoscreen(point(X0+DW,Y0+DH));
  //  result:=rect(pt1.X,pt1.Y,pt2.X,pt2.Y);
  Result := Rect(X0, Y0, X0 + DW, Y0 + DH);
end;

procedure TJvArrayButton.DoShowHint(var HintStr: THintString;
  var CanShow: Boolean; var HintInfo: THintInfo);
var
  ACol, ARow, X, Y: Integer;
  Index: Integer;
begin
  if HintInfo.HintControl = Self then
  begin
    X := HintInfo.CursorPos.X;
    Y := HintInfo.CursorPos.Y;
    MouseToCell(X, Y, ACol, ARow);
    if (ACol < 0) or (ARow < 0) then
      Exit;
    Index := ARow * Cols + ACol;
    if Index < Hints.Count then
      HintStr := Hints[Index]
    else
      HintStr := Hint;
    HintInfo.CursorRect := CellRect(ACol, ARow);
    CanShow := True;
  end;
end;

procedure TJvArrayButton.SetHints(const Value: THintStringList);
begin
  FHints.Assign(Value);
end;

end.

