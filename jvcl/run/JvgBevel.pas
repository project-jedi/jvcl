{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgBevel.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  This unit implements the TJvgBevel component which is an extended
  TBevel Delphi component with gradient filling and advanced borders
  drawing.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgBevel;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, ExtCtrls, Forms,
  JvComponent,
  JvgTypes, JvgCommClasses, JvgUtils;

type
  TJvgBevel = class(TJvGraphicControl)
  private
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelSides: TglSides;
    FBevelBold: Boolean;
    FBevelPenStyle: TPenStyle;
    FBevelPenWidth: Word;
    FInteriorOffset: Word;
    FGradient: TJvgGradient;
    FVertLines: TJvgBevelLines;
    FHorLines: TJvgBevelLines;
    FExternalCanvas: TCanvas;
    procedure SomethingChanged(Sender: TObject);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelSides(Value: TglSides);
    procedure SetBevelBold(Value: Boolean);
    procedure SetBevelPenStyle(Value: TPenStyle);
    procedure SetBevelPenWidth(Value: Word);
    procedure SetInteriorOffset(Value: Word);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property ExternalCanvas: TCanvas read FExternalCanvas write FExternalCanvas stored False;
    procedure Loaded; override;
  published
    property Anchors;
    property Align;
    property Height default 50;
    property ParentShowHint;
    property Width default 50;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property DragCursor;
    property DragMode;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvLowered;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvNone;
    property BevelSides: TglSides read FBevelSides write SetBevelSides
      default [fsdLeft, fsdTop, fsdRight, fsdBottom];
    property BevelBold: Boolean read FBevelBold write SetBevelBold default False;
    property BevelPenStyle: TPenStyle read FBevelPenStyle write SetBevelPenStyle default psSolid;
    property BevelPenWidth: Word read FBevelPenWidth write SetBevelPenWidth default 1;
    property InteriorOffset: Word read FInteriorOffset write SetInteriorOffset default 0;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property HintColor;
    property VertLines: TJvgBevelLines read FVertLines write FVertLines;
    property HorLines: TJvgBevelLines read FHorLines write FHorLines;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

implementation

constructor TJvgBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGradient := TJvgGradient.Create;
  FVertLines := TJvgBevelLines.Create;
  FHorLines := TJvgBevelLines.Create;
  //..defaults
  Width := 50;
  Height := 50;
  FBevelInner := bvLowered;
  FBevelOuter := bvNone;
  FBevelSides := [fsdLeft, fsdTop, fsdRight, fsdBottom];
  FBevelPenStyle := psSolid;
  FBevelPenWidth := 1;
  FGradient.OnChanged := SomethingChanged;
  FVertLines.OnChanged := SomethingChanged;
  FHorLines.OnChanged := SomethingChanged;
end;

destructor TJvgBevel.Destroy;
begin
  FGradient.Free;
  FVertLines.Free;
  FHorLines.Free;
  inherited Destroy;
end;

procedure TJvgBevel.Loaded;
begin
  inherited Loaded;
  if FGradient.Active then
    ControlStyle := ControlStyle + [csOpaque];
end;

procedure TJvgBevel.Paint;
var
  R, R_: TRect;
  BoxSides: TglSides;
  TargetCanvas: TCanvas;

  procedure DrawLines(R_: TRect; Direction: TglLinesDir; Lines:
    TJvgBevelLines);
  var
    I: Integer;
  begin
    if Direction = fldVertical then
    begin
      BoxSides := [fsdLeft, fsdRight];
      if Lines.IgnoreBorder then
      begin
        R_.Top := R.Top;
        R_.Bottom := R.Bottom;
      end;
    end
    else
    begin
      BoxSides := [fsdTop, fsdBottom];
      if Lines.IgnoreBorder then
      begin
        R_.Left := R.Left;
        R_.Right := R.Right;
      end;
    end;

    for I := 1 to Lines.Count do
    begin
      case Direction of
        fldVertical:
          begin
            R_.Left := MulDiv(I, Width, Lines.Count + 1);
            R_.Right := R_.Left + Lines.Thickness + Ord(Lines.Bold);
          end;
      else {fldHorizontal:}
        begin
          R_.Top := MulDiv(I, Height, Lines.Count + 1);
          // if I = 1 then Dec( R_.Top, Lines.Thickness );
          R_.Bottom := R_.Top + Lines.Thickness + Ord(Lines.Bold);
        end;
      end;
      if Lines.Style = bvSpace then
        BoxSides := [fsdLeft, fsdTop];

      DrawBoxEx(TargetCanvas.Handle, R_, BoxSides, Lines.Style, bvNone,
        Lines.Bold, 0, True);
    end;
  end;

begin
  if Assigned(ExternalCanvas) then
    TargetCanvas := ExternalCanvas
  else
    TargetCanvas := Canvas;
  R := ClientRect;
  InflateRect(R, -FInteriorOffset, -FInteriorOffset);
  GradientBox(TargetCanvas.Handle, R, Gradient,
    Ord(FBevelPenStyle), FBevelPenWidth);

  R := ClientRect;
  Dec(R.Right);
  Dec(R.Bottom);
  TargetCanvas.Pen.Width := FBevelPenWidth;
  TargetCanvas.Pen.Style := FBevelPenStyle;
  R_ := DrawBoxEx(TargetCanvas.Handle, R, BevelSides, BevelInner, BevelOuter,
    FBevelBold, 0, True);

  DrawLines(R_, fldHorizontal, HorLines);
  DrawLines(R_, fldVertical, VertLines);
end;

procedure TJvgBevel.SomethingChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TJvgBevel.SetBevelOuter(Value: TPanelBevel);
begin
  if FBevelOuter <> Value then
  begin
    FBevelOuter := Value;
    Invalidate;
  end;
end;

procedure TJvgBevel.SetBevelInner(Value: TPanelBevel);
begin
  if FBevelInner <> Value then
  begin
    FBevelInner := Value;
    Invalidate;
  end;
end;

procedure TJvgBevel.SetBevelSides(Value: TglSides);
begin
  if FBevelSides <> Value then
  begin
    FBevelSides := Value;
    Invalidate;
  end;
end;

procedure TJvgBevel.SetBevelBold(Value: Boolean);
begin
  if FBevelBold <> Value then
  begin
    FBevelBold := Value;
    Invalidate;
  end;
end;

procedure TJvgBevel.SetBevelPenStyle(Value: TPenStyle);
begin
  if FBevelPenStyle <> Value then
  begin
    FBevelPenStyle := Value;
    Invalidate;
  end;
end;

procedure TJvgBevel.SetBevelPenWidth(Value: Word);
begin
  if FBevelPenWidth <> Value then
  begin
    FBevelPenWidth := Value;
    Invalidate;
  end;
end;

procedure TJvgBevel.SetInteriorOffset(Value: Word);
begin
  if FInteriorOffset <> Value then
  begin
    FInteriorOffset := Value;
    Invalidate;
  end;
end;

end.

