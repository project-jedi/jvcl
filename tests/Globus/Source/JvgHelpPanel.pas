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

UNIT JvgHelpPanel;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   JvComponent,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   ExtCtrls,
   comctrls;

TYPE
   TJvgHelpPanel = CLASS(TJvCustomPanel)
   PRIVATE
      Rich: TRichEdit;
      FStrings: TStrings;
      ButtonRect: TRect;
      FHighlightButton: boolean;
      FExpanded: boolean;
      FExpandedHeight: integer;
      fInitializing: boolean;
      PROCEDURE SetStrings(CONST Value: TStrings);
      PROCEDURE SetHighlightButton(CONST Value: boolean);
      PROCEDURE CMMouseLeave(VAR Message: TMessage); MESSAGE CM_MOUSELEAVE;
      PROCEDURE SetExpanded(CONST Value: boolean);
      PROCEDURE SetExpandedHeight(CONST Value: integer);

   PROTECTED
      PROCEDURE Paint; OVERRIDE;
      PROCEDURE MouseMove(Shift: TShiftState; X, Y: Integer); OVERRIDE;
      PROCEDURE MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
         Integer); OVERRIDE;
   PUBLIC
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;
      PROCEDURE Loaded; OVERRIDE;
      PROCEDURE InitRichText;
      PROPERTY HighlightButton: boolean READ FHighlightButton WRITE
         SetHighlightButton;
   PUBLISHED
      PROPERTY Align;
      PROPERTY Alignment;
      {$IFDEF COMPILER4_UP}
      PROPERTY Anchors;
      {$ENDIF}
      PROPERTY AutoSize;
      PROPERTY BevelInner STORED true;
      PROPERTY BevelOuter STORED true;
      PROPERTY BevelWidth;
      PROPERTY BiDiMode;
      PROPERTY BorderWidth;
      PROPERTY BorderStyle;
      PROPERTY Caption;
      PROPERTY Color;
      PROPERTY Constraints;
      PROPERTY Ctl3D;
      {$IFDEF COMPILER4_UP}
      PROPERTY UseDockManager DEFAULT True;
      PROPERTY DockSite;
      {$ENDIF}
      PROPERTY DragCursor;
      PROPERTY DragKind;
      PROPERTY DragMode;
      PROPERTY Enabled;
      PROPERTY FullRepaint;
      PROPERTY Font;
      PROPERTY Locked;
      PROPERTY ParentBiDiMode;
      PROPERTY ParentColor;
      PROPERTY ParentCtl3D;
      PROPERTY ParentFont;
      PROPERTY ParentShowHint;
      PROPERTY PopupMenu;
      PROPERTY ShowHint;
      PROPERTY TabOrder;
      PROPERTY TabStop;
      PROPERTY Visible;
      PROPERTY OnCanResize;
      PROPERTY OnClick;
      PROPERTY OnConstrainedResize;
      {$IFDEF COMPILER5_UP}
      PROPERTY OnContextPopup;
      {$ENDIF}
      PROPERTY OnDockDrop;
      PROPERTY OnDockOver;
      PROPERTY OnDblClick;
      PROPERTY OnDragDrop;
      PROPERTY OnDragOver;
      {$IFDEF COMPILER4_UP}
      PROPERTY OnEndDock;
      {$ENDIF}
      PROPERTY OnEndDrag;
      PROPERTY OnEnter;
      PROPERTY OnExit;
      {$IFDEF COMPILER4_UP}
      PROPERTY OnGetSiteInfo;
      {$ENDIF}
      PROPERTY OnMouseDown;
      PROPERTY OnMouseMove;
      PROPERTY OnMouseUp;
      PROPERTY OnResize;
      {$IFDEF COMPILER4_UP}
      PROPERTY OnStartDock;
      PROPERTY OnUnDock;
      {$ENDIF}
      PROPERTY OnStartDrag;

      PROPERTY Expanded: boolean READ FExpanded WRITE SetExpanded DEFAULT false;
      PROPERTY Strings: TStrings READ FStrings WRITE SetStrings;
      PROPERTY ExpandedHeight: integer READ FExpandedHeight WRITE
         SetExpandedHeight;
   END;

PROCEDURE Register;

IMPLEMENTATION

PROCEDURE Register;
BEGIN
   //  RegisterComponents('Gl Controls', [TJvgHelpPanel]);
END;

{ TJvgHelpPanel }

PROCEDURE TJvgHelpPanel.CMMouseLeave(VAR Message: TMessage);
BEGIN
   HighlightButton := false;
END;

CONSTRUCTOR TJvgHelpPanel.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   fInitializing := true;

   BevelInner := bvNone;
   BevelOuter := bvNone;

   FStrings := TStringList.Create;
   Height := 70;
   Caption := ' help ';

   //if csDesigning in ComponentState then Align := alBottom;
   Expanded := false;
   fInitializing := false;

   IF csDesigning IN ComponentState THEN
      exit;
   Rich := TRichEdit.Create(self);
   Rich.Parent := self;
   Rich.ReadOnly := true;

END;

DESTRUCTOR TJvgHelpPanel.Destroy;
BEGIN
   FStrings.Free;
   IF Assigned(Rich) THEN
      Rich.Free;
   INHERITED;
END;

PROCEDURE TJvgHelpPanel.Loaded;
BEGIN
   INHERITED;
   InitRichText;
END;

PROCEDURE TJvgHelpPanel.InitRichText;
VAR
   ms                         : TMemoryStream;
BEGIN
   IF NOT Assigned(Rich) THEN
      exit;
   Rich.BorderStyle := bsNone;
   Rich.SetBounds(12, 16, Width - 24, ExpandedHeight - 22);
   ms := TMemoryStream.Create;
   TRY
      FStrings.SaveToStream(ms);
      ms.Position := 0;
      Rich.Lines.LoadFromStream(ms);
   FINALLY
      ms.Free;
   END;
END;

PROCEDURE TJvgHelpPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
   Y: Integer);
BEGIN
   INHERITED;
   IF PtInRect(ButtonRect, Point(X, Y)) THEN
   BEGIN
      Expanded := NOT Expanded;
      IF Assigned(onClick) THEN
         onClick(self);
   END;
END;

PROCEDURE TJvgHelpPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
BEGIN
   INHERITED;
   IF PtInRect(ButtonRect, Point(X, Y)) THEN
   BEGIN
      IF NOT HighlightButton THEN
         HighlightButton := NOT HighlightButton;
   END
   ELSE IF HighlightButton THEN
      HighlightButton := NOT HighlightButton;
END;

PROCEDURE TJvgHelpPanel.Paint;
CONST
   WARNING                       =
      'Open context menu to load RTF text. Control shows text at runtime only.';
VAR
   R                          : TRect;
BEGIN
   //inherited;

   Canvas.Brush.Style := bsSolid;

   Canvas.Brush.Color := Color;
   Canvas.FillRect(ClientRect);

   Canvas.Brush.Color := clBtnShadow;
   Canvas.FillRect(Bounds(5, 7, Width - 10, 2));

   Canvas.Brush.Color := clWindow;
   Canvas.Pen.Color := clBlack;
   IF Expanded THEN
      Canvas.Rectangle(5, 15, Width - 5, Height - 5);

   ButtonRect := Bounds(Width - 80, 0, 80, 20);

   Canvas.Font.Style := [fsBold];
   IF FHighlightButton THEN
   BEGIN
      SetBkColor(Canvas.Handle, ColorToRGB(clBtnShadow));
      SetTextColor(Canvas.Handle, clWhite);
   END
   ELSE
   BEGIN
      SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
      SetTextColor(Canvas.Handle, clBlack);
   END;
   SetBkMode(Canvas.Handle, OPAQUE);
   DrawText(Canvas.Handle, PChar(Caption), length(Caption), ButtonRect,
      DT_SINGLELINE OR DT_RIGHT);

   IF csDesigning IN ComponentState THEN
   BEGIN
      R := ClientRect;
      inc(R.Top, 20);
      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawText(Canvas.Handle, WARNING, length(WARNING), R, DT_SINGLELINE OR
         DT_CENTER OR DT_VCENTER);
   END;
END;

PROCEDURE TJvgHelpPanel.SetExpanded(CONST Value: boolean);
BEGIN
   FExpanded := Value;
   IF FExpanded THEN
      Height := ExpandedHeight
   ELSE
   BEGIN
      FExpandedHeight := Height;
      Height := 16;
   END;

   IF NOT fInitializing THEN
      IF Parent IS TForm THEN
         WITH (Parent AS TForm) DO
            IF FExpanded THEN
               Height := Height + ExpandedHeight - 16
            ELSE
               Height := Height - ExpandedHeight + 16;
END;

PROCEDURE TJvgHelpPanel.SetExpandedHeight(CONST Value: integer);
BEGIN
   FExpandedHeight := Value;
END;

PROCEDURE TJvgHelpPanel.SetHighlightButton(CONST Value: boolean);
BEGIN
   FHighlightButton := Value;
   IF FHighlightButton THEN
      Cursor := crHandPoint
   ELSE
      Cursor := crDefault;
   Repaint;
END;

PROCEDURE TJvgHelpPanel.SetStrings(CONST Value: TStrings);
BEGIN
   FStrings.Assign(Value);
   InitRichText;
END;

END.

