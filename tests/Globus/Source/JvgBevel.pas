{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgBevel.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgBevel;

{ This unit implements the TJvgBevel component which is an extended
 TBevel Delphi component with gradient filling and advanced borders
 drawing.}

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
    FHintColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOver: Boolean;
    procedure SomethingChanged(Sender: TObject);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelSides(Value: TglSides);
    procedure SetBevelBold(Value: Boolean);
    procedure SetBevelPenStyle(Value: TPenStyle);
    procedure SetBevelPenWidth(Value: Word);
    procedure SetInteriorOffset(Value: Word);
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property ExternalCanvas: TCanvas read FExternalCanvas write FExternalCanvas stored False;
    procedure Loaded; override;
  published
    {$IFDEF COMPILER5_UP}
    property Anchors;
    {$ENDIF}
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
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property VertLines: TJvgBevelLines read FVertLines write FVertLines;
    property HorLines: TJvgBevelLines read FHorLines write FHorLines;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
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
  FHintColor := clInfoBk;
  FBevelOuter := bvNone;
  FBevelSides := [fsdLeft, fsdTop, fsdRight, fsdBottom];
  FBevelPenStyle := psSolid;
  FBevelPenWidth := 1;
  FGradient.OnChanged := SomethingChanged;
  FVertLines.OnChanged := SomethingChanged;
  FHorLines.OnChanged := SomethingChanged;
  FOver := False;
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
            R_.Right := R_.Left + Lines.Thickness + Integer(Lines.Bold);
          end;
      else {fldHorizontal:}
        begin
          R_.Top := MulDiv(I, Height, Lines.Count + 1);
          //	  if I = 1 then Dec( R_.Top, Lines.Thickness );
          R_.Bottom := R_.Top + Lines.Thickness + Integer(Lines.Bold);
        end;
      end;
      {$IFDEF COMPILER5_UP}
      if Lines.Style = bvSpace then
        BoxSides := [fsdLeft, fsdTop];
      {$ENDIF}

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
    Integer(FBevelPenStyle), FBevelPenWidth);

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

procedure TJvgBevel.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvgBevel.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvgBevel.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
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

