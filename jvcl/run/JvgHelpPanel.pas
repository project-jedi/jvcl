{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHelpPanel.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgHelpPanel;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, ExtCtrls, ComCtrls,
  JvComponent;

type
  TJvgHelpPanel = class(TJvCustomPanel)
  private
    FRich: TRichEdit;
    FStrings: TStringList;
    FButtonRect: TRect;
    FHighlightButton: Boolean;
    FExpanded: Boolean;
    FExpandedHeight: Integer;
    FInitializing: Boolean;
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure SetHighlightButton(const Value: Boolean);
    procedure SetExpanded(const Value: Boolean);
    procedure SetExpandedHeight(const Value: Integer);
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure InitRichText;
    property HighlightButton: Boolean read FHighlightButton write SetHighlightButton;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner stored True;
    property BevelOuter stored True;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnUnDock;
    property OnStartDrag;
    property Expanded: Boolean read FExpanded write SetExpanded default False;
    property Strings: TStrings read GetStrings write SetStrings;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight;
  end;

implementation

uses
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  JvConsts;

{$IFNDEF USEJVCL}
resourcestring
  RsHelp = ' help ';
  RsOpenContextMenuToLoadRTFTextControl = 'Open context menu to load RTF text. Control shows text at runtime only.';
{$ENDIF USEJVCL}

constructor TJvgHelpPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitializing := True;

  BevelInner := bvNone;
  BevelOuter := bvNone;

  FStrings := TStringList.Create;
  Height := 70;
  Caption := RsHelp;

  //if csDesigning in ComponentState then Align := alBottom;
  Expanded := False;
  FInitializing := False;

  if not (csDesigning in ComponentState) then
  begin
    FRich := TRichEdit.Create(Self);
    FRich.Parent := Self;
    FRich.ReadOnly := True;
  end;
end;

destructor TJvgHelpPanel.Destroy;
begin
  FStrings.Free;
  FRich.Free;
  inherited Destroy;
end;

procedure TJvgHelpPanel.CMMouseLeave(var Msg: TMessage);
begin
  HighlightButton := False;
end;

procedure TJvgHelpPanel.Loaded;
begin
  inherited Loaded;
  InitRichText;
end;

procedure TJvgHelpPanel.InitRichText;
begin
  if Assigned(FRich) then
  begin
    FRich.BorderStyle := bsNone;
    FRich.SetBounds(12, 16, Width - 24, ExpandedHeight - 22);
    FRich.Lines.Assign(FStrings);
  end;
end;

procedure TJvgHelpPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if PtInRect(FButtonRect, Point(X, Y)) then
  begin
    Expanded := not Expanded;
    if Assigned(OnClick) then
      OnClick(Self);
  end;
end;

procedure TJvgHelpPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if PtInRect(FButtonRect, Point(X, Y)) then
  begin
    if not HighlightButton then
      HighlightButton := not HighlightButton;
  end
  else
  if HighlightButton then
    HighlightButton := not HighlightButton;
end;

procedure TJvgHelpPanel.Paint;
var
  Warning: string;
  R: TRect;
begin
  Warning := RsOpenContextMenuToLoadRTFTextControl;
  //inherited;

  Canvas.Brush.Style := bsSolid;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  Canvas.Brush.Color := clBtnShadow;
  Canvas.FillRect(Bounds(5, 7, Width - 10, 2));

  Canvas.Brush.Color := clWindow;
  Canvas.Pen.Color := clBlack;
  if Expanded then
    Canvas.Rectangle(5, 15, Width - 5, Height - 5);

  FButtonRect := Bounds(Width - 80, 0, 80, 20);

  // (rom) the whole rest of the function seems clumsy
  Canvas.Font.Style := [fsBold];
  if FHighlightButton then
  begin
    SetBkColor(Canvas.Handle, ColorToRGB(clBtnShadow));
    SetTextColor(Canvas.Handle, clWhite);
  end
  else
  begin
    SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
    SetTextColor(Canvas.Handle, clBlack);
  end;
  SetBkMode(Canvas.Handle, OPAQUE);
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), FButtonRect,
    DT_SINGLELINE or DT_RIGHT);

  if csDesigning in ComponentState then
  begin
    R := ClientRect;
    inc(R.Top, 20);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas.Handle, PChar(Warning), Length(Warning), R, DT_SINGLELINE or
      DT_CENTER or DT_VCENTER);
  end;
end;

procedure TJvgHelpPanel.SetExpanded(const Value: Boolean);
begin
  FExpanded := Value;
  if FExpanded then
    Height := ExpandedHeight
  else
  begin
    FExpandedHeight := Height;
    Height := 16;
  end;

  if not FInitializing then
    if Parent is TForm then
      with Parent as TForm do
        if FExpanded then
          Height := Height + ExpandedHeight - 16
        else
          Height := Height - ExpandedHeight + 16;
end;

procedure TJvgHelpPanel.SetExpandedHeight(const Value: Integer);
begin
  FExpandedHeight := Value;
end;

procedure TJvgHelpPanel.SetHighlightButton(const Value: Boolean);
begin
  FHighlightButton := Value;
  if FHighlightButton then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  Repaint;
end;

function TJvgHelpPanel.GetStrings: TStrings;
begin
  Result := FStrings;
end;

procedure TJvgHelpPanel.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
  InitRichText;
end;

end.

