{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExExtCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExExtCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  Graphics, Controls, Forms, ExtCtrls,
  {$IFDEF VisualCLX}
  Qt, QTypes, Types, QWindows,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

{$IFDEF VCL}
 {$DEFINE NeedMouseEnterLeave}
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
  {$DEFINE NeedMouseEnterLeave}
 {$IFEND}
{$ENDIF VisualCLX}

type
  JV_CONTROL_EVENTS(Shape)
  JV_CONTROL_EVENTS(PaintBox)
  JV_CONTROL_EVENTS(Image)
  JV_CONTROL_EVENTS(Bevel)
  JV_CUSTOMCONTROL_EVENTS(CustomPanel)
  JV_WINCONTROL_EVENTS(CustomRadioGroup)

  JV_CONTROL_EVENTS_BEGIN(Splitter)
  JV_CONSTRUCTOR
  {$IFDEF VisualCLX}
  private
    FText: string;
  protected
    function GetText: TCaption; override;
    procedure SetText(const Value: TCaption); override;
  {$ENDIF VisualCLX}
  JV_CONTROL_EVENTS_END(Splitter)

  JV_CUSTOMCONTROL_EVENTS_BEGIN(CustomControlBar)
  JV_CONSTRUCTOR
  {$IFDEF VisualCLX}
  protected
    function HitTest(X, Y: Integer): Boolean; overload; dynamic;
  {$ENDIF VisualCLX}
  JV_CUSTOMCONTROL_EVENTS_END(CustomControlBar)

  {$IFDEF VCL}
  JV_CUSTOMCONTROL_EVENTS(ControlBar)
  JV_CUSTOMCONTROL_EVENTS(Panel)
  JV_WINCONTROL_EVENTS(RadioGroup)
  JV_CUSTOMCONTROL_EVENTS(Page)
  JV_CUSTOMCONTROL_EVENTS(Notebook)
  JV_CUSTOMCONTROL_EVENTS(Header)
  {$IFDEF COMPILER6_UP}
  JV_CONTROL_EVENTS(BoundLabel)
  JV_WINCONTROL_EVENTS(CustomLabeledEdit)
  JV_WINCONTROL_EVENTS(LabeledEdit)
  JV_WINCONTROL_EVENTS(CustomColorBox)
  JV_WINCONTROL_EVENTS(ColorBox)
  {$ENDIF COMPILER6_UP}
  {$ENDIF VCL}

  {$IFDEF VisualCLX}
  TJvExControlBar = class(TJvExCustomControlBar)
  public
    property Canvas;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Color;
    property Constraints;
    property DragMode;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property Enabled;
    property GrabCursor;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Picture;
    property PopupMenu;
    property RowSize;
    property RowSnap;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBandDrag;
    property OnBandInfo;
    property OnBandMove;
    property OnBandPaint;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDrag;
  end;

  TJvExPanel = class(TJvExCustomPanel)
  published
    property Align default alNone;
    property Alignment;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property Bitmap;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color default clBackground;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default False;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

  TJvExRadioGroup = class(TJvExCustomRadioGroup)
  published
    property Items;
    property Align default alNone;
    property Alignment default taLeftJustify;
    property Bitmap;
    property Anchors;
    property Caption;
    property Color;
    property ColumnLayout;
    property Columns;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ItemIndex;
    property Masked default False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint default False;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
  end;
{$ENDIF VisualCLX}

implementation

JV_CONTROL_EVENTS_IMPL(Shape)
JV_CONTROL_EVENTS_IMPL(PaintBox)
JV_CONTROL_EVENTS_IMPL(Image)
JV_CONTROL_EVENTS_IMPL(Bevel)
JV_CUSTOMCONTROL_EVENTS_IMPL(CustomPanel)
JV_WINCONTROL_EVENTS_IMPL(CustomRadioGroup)

{$UNDEF CONSTRUCTORCODE}
{$DEFINE CONSTRUCTORCODE ControlStyle := ControlStyle + [csSetCaption];}
JV_CONTROL_EVENTS_IMPL_BEGIN(Splitter)
{$IFDEF VisualCLX}
function TJvExSplitter.GetText: TCaption;
begin
  Result := FText;
end;

procedure TJvExSplitter.SetText(const Value: TCaption);
begin
  if Value <> FText then
  begin
    FText := Value;
    TextChanged;
  end;
end;
{$ENDIF VisualCLX}
JV_CONTROL_EVENTS_IMPL_END(Splitter)
{$UNDEF CONSTRUCTORCODE}
{$DEFINE CONSTRUCTORCODE}

JV_CUSTOMCONTROL_EVENTS_IMPL_BEGIN(CustomControlBar)
{$IFDEF VisualCLX}
function TJvExCustomControlBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
{$ENDIF VisualCLX}
JV_CUSTOMCONTROL_EVENTS_IMPL_END(CustomControlBar)


{$IFDEF VCL}
JV_CUSTOMCONTROL_EVENTS_IMPL(ControlBar)
JV_CUSTOMCONTROL_EVENTS_IMPL(Panel)
JV_WINCONTROL_EVENTS_IMPL(RadioGroup)
JV_CUSTOMCONTROL_EVENTS_IMPL(Page)
JV_CUSTOMCONTROL_EVENTS_IMPL(Notebook)
JV_CUSTOMCONTROL_EVENTS_IMPL(Header)
{$IFDEF COMPILER6_UP}
JV_CONTROL_EVENTS_IMPL(BoundLabel)
JV_WINCONTROL_EVENTS_IMPL(CustomLabeledEdit)
JV_WINCONTROL_EVENTS_IMPL(LabeledEdit)
JV_WINCONTROL_EVENTS_IMPL(CustomColorBox)
JV_WINCONTROL_EVENTS_IMPL(ColorBox)
{$ENDIF COMPILER6_UP}
{$ENDIF VCL}

end.
