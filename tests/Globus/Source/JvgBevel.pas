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

{ This unit implements the TJvgBevel component which is an  extended
 TBevel Delphi component with gradient filling and advanced  borders
 drawing.}

unit JvgBevel;

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  JVComponent,
  Graphics,
  JvgTypes,
  JvgCommClasses,
  JvgUtils,
  ExtCtrls;
type

  TJvgBevel = class(TJvGraphicControl)
  private
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelSides: TglSides;
    FBevelBold: boolean;
    FBevelPenStyle: TPenStyle;
    FBevelPenWidth: word;
    FInteriorOffset: word;
    FGradient: TJvgGradient;
    FVertLines: TJvgBevelLines;
    FHorLines: TJvgBevelLines;
    //    FMouseSentencive	  : boolean;
    FExternalCanvas: TCanvas;
    procedure OnSmthChanged(Sender: TObject);

    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelSides(Value: TglSides);
    procedure SetBevelBold(Value: boolean);
    procedure SetBevelPenStyle(Value: TPenStyle);
    procedure SetBevelPenWidth(Value: word);
    procedure SetInteriorOffset(Value: word);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
  public
    Ctrl3D: boolean;
    procedure Paint; override;
    property ExternalCanvas: TCanvas read FExternalCanvas write
      FExternalCanvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;

  published
    {$IFDEF COMPILER5_UP}
    property Anchors;
    {$ENDIF}
    property Align;
    property ParentShowHint;
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

    property Canvas;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner
      default bvLowered;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter
      default bvNone;
    property BevelSides: TglSides read FBevelSides write SetBevelSides
      default [fsdLeft, fsdTop, fsdRight, fsdBottom];
    property BevelBold: boolean read FBevelBold write SetBevelBold
      default false;
    property BevelPenStyle: TPenStyle read FBevelPenStyle write SetBevelPenStyle
      default psSolid;
    property BevelPenWidth: word read FBevelPenWidth write SetBevelPenWidth
      default 1;
    property InteriorOffset: word read FInteriorOffset write SetInteriorOffset
      default 0;
    property Gradient: TJvgGradient read FGradient write FGradient;
    property VertLines: TJvgBevelLines read FVertLines write FVertLines;
    property HorLines: TJvgBevelLines read FHorLines write FHorLines;
  end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure Register;
begin

end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}
//________________________________________________________ Methods _

constructor TJvgBevel.Create(AOwner: TComponent);
begin
  inherited;
  FGradient := TJvgGradient.Create;
  FVertLines := TJvgBevelLines.Create;
  FHorLines := TJvgBevelLines.Create;
  //..defaults
  Width := 50;
  Height := 50;
  FBevelInner := bvLowered;
  //  FBevelOuter := bvNone;
  FBevelSides := [fsdLeft, fsdTop, fsdRight, fsdBottom];
  FBevelPenStyle := psSolid;
  FBevelPenWidth := 1;
  Ctrl3D := true;
  FGradient.OnChanged := OnSmthChanged;
  FVertLines.OnChanged := OnSmthChanged;
  FHorLines.OnChanged := OnSmthChanged;
end;

destructor TJvgBevel.Destroy;
begin
  Gradient.Free;
  FVertLines.Free;
  FHorLines.Free;
  inherited;
end;

procedure TJvgBevel.Paint;
var
  r, r_: TRect;
  BoxSides: TglSides;
  TargetCanvas: TCanvas;

  procedure DrawLines(r_: TRect; Direction: TglLinesDir; Lines:
    TJvgBevelLines);
  var
    i: integer;
  begin
    if Direction = fldVertical then
    begin
      if Ctrl3D then
        BoxSides := [fsdLeft, fsdRight]
      else
        BoxSides := [fsdLeft];
      if Lines.IgnoreBorder then
      begin
        r_.top := r.top;
        r_.bottom := r.bottom;
      end;
    end
    else
    begin
      if Ctrl3D then
        BoxSides := [fsdTop, fsdBottom]
      else
        BoxSides := [fsdTop];
      if Lines.IgnoreBorder then
      begin
        r_.left := r.left;
        r_.right := r.right;
      end;
    end;

    for i := 1 to Lines.Count do
    begin
      case Direction of
        fldVertical:
          begin
            r_.left := MulDiv(i, Width, Lines.Count + 1);
            r_.Right := r_.Left + Lines.Thickness + integer(Lines.Bold);
          end;
      else {fldHorizontal:}
        begin
          r_.Top := MulDiv(i, Height, Lines.Count + 1);
          //	  if i = 1 then dec( r_.Top, Lines.Thickness );
          r_.Bottom := r_.Top + Lines.Thickness + integer(Lines.Bold);
        end;
      end;
      {$IFDEF COMPILER5_UP}
      if Lines.Style = bvSpace then
        BoxSides := [fsdLeft, fsdTop];
      {$ENDIF}

      DrawBoxEx(TargetCanvas.Handle, r_, BoxSides, Lines.Style, bvNone,
        Lines.Bold, 0, true);
    end;
  end;
begin
  if Assigned(ExternalCanvas) then
    TargetCanvas := ExternalCanvas
  else
    TargetCanvas := Canvas;
  r := ClientRect;
  InflateRect(r, -FInteriorOffset, -FInteriorOffset);
  GradientBox(TargetCanvas.handle, r, Gradient,
    integer(FBevelPenStyle), FBevelPenWidth);

  r := ClientRect;
  dec(r.right);
  dec(r.bottom);
  TargetCanvas.Pen.Width := FBevelPenWidth;
  TargetCanvas.Pen.Style := FBevelPenStyle;
  r_ := DrawBoxEx(TargetCanvas.Handle, r, BevelSides, BevelInner, BevelOuter,
    FBevelBold, 0, true);

  DrawLines(r_, fldHorizontal, HorLines);
  DrawLines(r_, fldVertical, VertLines);
end;

procedure TJvgBevel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TJvgBevel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

procedure TJvgBevel.OnSmthChanged(Sender: TObject);
begin
  Repaint;
end;
//...______________________________________________PROPERTIES METHODS

procedure TJvgBevel.SetBevelOuter(Value: TPanelBevel);
//var r: TRect;
begin
  if FBevelOuter = Value then
    exit;
  //  r:=ClientRect; InflateRect( r, -5, -5 );
  FBevelOuter := Value;
  Invalidate; // ValidateRect( canvas.handle, @r );
end;

procedure TJvgBevel.SetBevelInner(Value: TPanelBevel);
//var r: TRect;
begin
  if FBevelInner = Value then
    exit;
  //  r:=ClientRect; InflateRect( r, -5, -5 );
  FBevelInner := Value;
  Invalidate; //ValidateRect( canvas.handle, @r );
end;

procedure TJvgBevel.SetBevelSides(Value: TglSides);
begin
  if FBevelSides = Value then
    exit;
  FBevelSides := Value;
  Invalidate;
end;

procedure TJvgBevel.SetBevelBold(Value: boolean);
begin
  if FBevelBold = Value then
    exit;
  FBevelBold := Value;
  Invalidate;
end;

procedure TJvgBevel.SetBevelPenStyle(Value: TPenStyle);
begin
  if FBevelPenStyle = Value then
    exit;
  FBevelPenStyle := Value;
  Invalidate;
end;

procedure TJvgBevel.SetBevelPenWidth(Value: word);
begin
  if FBevelPenWidth = Value then
    exit;
  FBevelPenWidth := Value;
  Invalidate;
end;

procedure TJvgBevel.SetInteriorOffset(Value: word);
begin
  if FInteriorOffset = Value then
    exit;
  FInteriorOffset := Value;
  Invalidate;
end;

procedure TJvgBevel.Loaded;
begin
  inherited;
  if FGradient.Active then
    ControlStyle := ControlStyle + [csOpaque];
end;

end.
