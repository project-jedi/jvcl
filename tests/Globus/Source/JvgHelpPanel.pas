{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHelpPanel.PAS, released on 2003-01-15.

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

unit JvgHelpPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, comctrls;

type
  TJvgHelpPanel = class(TCustomPanel)
  private
    Rich: TRichEdit;
    FStrings: TStrings;
    ButtonRect: TRect;
    FHighlightButton: boolean;
    FExpanded: boolean;
    FExpandedHeight: integer;
    fInitializing: boolean;
    procedure SetStrings(const Value: TStrings);
    procedure SetHighlightButton(const Value: boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetExpanded(const Value: boolean);
    procedure SetExpandedHeight(const Value: integer);

  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure InitRichText;
    property HighlightButton: boolean read FHighlightButton write SetHighlightButton;
  published
    property Align;
    property Alignment;
    {$IFDEF GLVER_D4}
    property Anchors;
    {$ENDIF}
    property AutoSize;
    property BevelInner stored true;
    property BevelOuter stored true;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    {$IFDEF GLVER_D4}
    property UseDockManager default True;
    property DockSite;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
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
    {$IFDEF GLVER_D5}
    property OnContextPopup;
    {$ENDIF}
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF GLVER_D4}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF GLVER_D4}
    property OnGetSiteInfo;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF GLVER_D4}
    property OnStartDock;
    property OnUnDock;
    {$ENDIF}
    property OnStartDrag;

    property Expanded: boolean read FExpanded write SetExpanded default false;
    property Strings: TStrings read FStrings write SetStrings;
    property ExpandedHeight: integer read FExpandedHeight write SetExpandedHeight;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gl Controls', [TJvgHelpPanel]);
end;

{ TJvgHelpPanel }

procedure TJvgHelpPanel.CMMouseLeave(var Message: TMessage);
begin
  HighlightButton := false;
end;

constructor TJvgHelpPanel.Create(AOwner: TComponent);
begin
  inherited;
  fInitializing := true;

  BevelInner := bvNone;
  BevelOuter := bvNone;

  FStrings := TStringList.Create;
  Height := 70;
  Caption := ' help ';

  //if csDesigning in ComponentState then Align := alBottom;
  Expanded := false;
  fInitializing := false;

  if csDesigning in ComponentState then exit;
  Rich := TRichEdit.Create(self);
  Rich.Parent := self;
  Rich.ReadOnly := true;

end;

destructor TJvgHelpPanel.Destroy;
begin
  FStrings.Free;
  if Assigned(Rich) then Rich.Free;
  inherited;
end;

procedure TJvgHelpPanel.Loaded;
begin
  inherited;
  InitRichText;
end;

procedure TJvgHelpPanel.InitRichText;
var
  ms: TMemoryStream;
begin
  if not Assigned(Rich) then exit;
  Rich.BorderStyle := bsNone;
  Rich.SetBounds(12, 16, Width - 24, ExpandedHeight - 22);
  ms := TMemoryStream.Create;
  try
    FStrings.SaveToStream(ms);
    ms.Position := 0;
    Rich.Lines.LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TJvgHelpPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if PtInRect(ButtonRect, Point(X, Y)) then
  begin
    Expanded := not Expanded;
    if Assigned(onClick) then onClick(self);
  end;
end;

procedure TJvgHelpPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if PtInRect(ButtonRect, Point(X, Y)) then
  begin
    if not HighlightButton then HighlightButton := not HighlightButton;
  end
  else if HighlightButton then
    HighlightButton := not HighlightButton;
end;

procedure TJvgHelpPanel.Paint;
const
  WARNING = 'Open context menu to load RTF text. Control shows text at runtime only.';
var
  R: TRect;
begin
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

  ButtonRect := Bounds(Width - 80, 0, 80, 20);

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
  DrawText(Canvas.Handle, PChar(Caption), length(Caption), ButtonRect, DT_SINGLELINE or DT_RIGHT);

  if csDesigning in ComponentState then
  begin
    R := ClientRect;
    inc(R.Top, 20);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas.Handle, WARNING, length(WARNING), R, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;
end;

procedure TJvgHelpPanel.SetExpanded(const Value: boolean);
begin
  FExpanded := Value;
  if FExpanded then
    Height := ExpandedHeight
  else
  begin
    FExpandedHeight := Height;
    Height := 16;
  end;

  if not fInitializing then
    if Parent is TForm then
      with (Parent as TForm) do
        if FExpanded then
          Height := Height + ExpandedHeight - 16
        else
          Height := Height - ExpandedHeight + 16;
end;

procedure TJvgHelpPanel.SetExpandedHeight(const Value: integer);
begin
  FExpandedHeight := Value;
end;

procedure TJvgHelpPanel.SetHighlightButton(const Value: boolean);
begin
  FHighlightButton := Value;
  if FHighlightButton then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
  Repaint;
end;

procedure TJvgHelpPanel.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
  InitRichText;
end;

end.
