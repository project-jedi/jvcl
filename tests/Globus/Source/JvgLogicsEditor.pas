{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLogicsEditor.PAS, released on 2003-01-15.

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

UNIT JvgLogicsEditor;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   JvgLogics,
   JvgScrollBox,
   StdCtrls,
   JvgGroupBox,
   ExtCtrls,
   ComCtrls,
   {$IFDEF COMPILER6_UP}
   DesignIntf,
   DesignEditors,
   PropertyCategories,
   {$ELSE}
   DsgnIntf,
   {$ENDIF COMPILER6_UP}

   ToolWin,
   ImgList,
   richEdit,
   JvgPage,
   Tabs;

TYPE
   TJvgGroupBoxPlus = CLASS;

   TJvgLogicsComponentEditor = CLASS(TComponentEditor)
      PROCEDURE ExecuteVerb(Index: Integer); OVERRIDE;
      FUNCTION GetVerb(Index: Integer): STRING; OVERRIDE;
      FUNCTION GetVerbCount: Integer; OVERRIDE;
   PRIVATE
      PROCEDURE ShowEditor(LogicProducer: TJvgLogicProducer);
   END;

   TJvgLogicsEditor = CLASS(TForm)
      SB: TJvgScrollBox;
      Panel1: TPanel;
      iPKey: TImage;
      iFKey: TImage;
      Label2: TLabel;
      cbMode: TComboBox;
      SBar: TStatusBar;
      iLink: TImage;
      cbNext: TComboBox;
      Label4: TLabel;
      eStepName: TEdit;
      Label1: TLabel;
      ImageList1: TImageList;
      ToolBar1: TToolBar;
      ToolButton1: TToolButton;
      tbNew: TToolButton;
      ToolButton3: TToolButton;
      ToolButton4: TToolButton;
      cbNextFalse: TComboBox;
      Label5: TLabel;
      Image3: TImage;
      Label7: TLabel;
      Label8: TLabel;
      Shape1: TShape;
      ImageList: TImageList;
      ToolButton2: TToolButton;
      ToolButton5: TToolButton;
      ToolButton6: TToolButton;
      ToolButton7: TToolButton;
      cbIgnoreSpaces: TCheckBox;
      ToolButton8: TToolButton;
      pLeft: TPanel;
      pLog: TPanel;
      Splitter1: TSplitter;
      tbStop: TToolButton;
      Panel2: TPanel;
      Splitter2: TSplitter;
      reReslt: TRichEdit;
      PC: TJvgPageControl;
      tsLog: TTabSheet;
      mLog: TMemo;
      tsDictionary: TTabSheet;
      mDictionary: TMemo;
      Shape2: TShape;
      Shape3: TShape;
      Shape4: TShape;
      Shape5: TShape;
      Shape6: TShape;
      Shape7: TShape;
      Shape8: TShape;
      Shape9: TShape;
      Shape10: TShape;
      Shape11: TShape;
      Shape12: TShape;
      Shape13: TShape;
      TabSet1: TTabSet;
      PROCEDURE SBEraseBkgndEvent(Sender: TObject; DC: HDC);
      PROCEDURE cbNextChange(Sender: TObject);
      PROCEDURE cbModeChange(Sender: TObject);
      PROCEDURE tbNewClick(Sender: TObject);
      PROCEDURE cbNextFalseChange(Sender: TObject);
      PROCEDURE eStepNameChange(Sender: TObject);
      PROCEDURE FormShow(Sender: TObject);
      PROCEDURE ToolButton5Click(Sender: TObject);
      PROCEDURE ToolButton7Click(Sender: TObject);
      PROCEDURE cbIgnoreSpacesClick(Sender: TObject);
      PROCEDURE pLeftDockOver(Sender: TObject; Source: TDragDockObject; X,
         Y: Integer; State: TDragState; VAR Accept: Boolean);
      PROCEDURE pLeftUnDock(Sender: TObject; Client: TControl;
         NewTarget: TWinControl; VAR Allow: Boolean);
      PROCEDURE pLeftDockDrop(Sender: TObject; Source: TDragDockObject; X,
         Y: Integer);
      PROCEDURE ToolButton8Click(Sender: TObject);
      PROCEDURE tbStopClick(Sender: TObject);
      PROCEDURE TabSet1Change(Sender: TObject; NewTab: Integer;
         VAR AllowChange: Boolean);
   PRIVATE
      FActiveBox: TJvgGroupBoxPlus;
      LogicProducer: TJvgLogicProducer;
      Logics: TJvgLogics;

      PROCEDURE MouseDown_(Sender: TObject; Button: TMouseButton; Shift:
         TShiftState; X, Y: Integer);
      PROCEDURE MouseMove_(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      PROCEDURE MouseUp_(Sender: TObject; Button: TMouseButton; Shift:
         TShiftState; X, Y: Integer);
      PROCEDURE DblClick_(Sender: TObject);
      PROCEDURE SetActiveBox(CONST Value: TJvgGroupBoxPlus);
      PROCEDURE UpdateView;
      PROCEDURE AddBox(LogicElement_: TJvgLogicElement);
      PROCEDURE AddShape(CommentArea: TJvgCommentArea);

      PROCEDURE OnTraceMessage(Sender: TJvgLogics; fStepResult: boolean; CONST
         StepResult, ParsedResult, Msg: STRING);
   PUBLIC
      FUNCTION Execute(LogicProducer: TJvgLogicProducer): boolean;
      PROPERTY ActiveBox: TJvgGroupBoxPlus READ FActiveBox WRITE SetActiveBox;
   END;

   TJvgGroupBoxPlus = CLASS(TJvgGroupBox)
   PUBLIC
      pt: TPoint;
      Selected: boolean;
      LogicElement: TJvgLogicElement;
      //    fAsLogical: boolean;
      //    constructor Create(AOwner: TComponent); override;
      //    destructor Destroy; override;
      PROCEDURE Paint; OVERRIDE;
   END;

   TJvgShapePlus = CLASS(TShape)
   PUBLIC
      pt: TPoint;
      Selected: boolean;
      CommentArea: TJvgCommentArea;
      PROCEDURE Paint; OVERRIDE;
   END;

VAR
   glLogicsEditor             : TJvgLogicsEditor;

IMPLEMENTATION
USES JvgTypes,
   JvgUtils,
   JvgLogicItemEditor,
   clipbrd;
{$R *.DFM}

{ TJvgLogicsEditor }

FUNCTION TJvgLogicsEditor.Execute(LogicProducer: TJvgLogicProducer): boolean;
VAR
   i                          : integer;
BEGIN
   fLogicItemEditor := TJvgLogicItemEditor.Create(NIL);

   mDictionary.Lines.Assign(LogicProducer.Dictionary);
   TRY

      self.LogicProducer := LogicProducer;
      self.Logics := LogicProducer.Logics;

      Logics.OnTraceMessage := OnTraceMessage;

      cbNext.Items.Clear;
      cbNextFalse.Items.Clear;
      cbNext.Items.Add('');
      cbNextFalse.Items.Add('');

      FOR i := 0 TO Logics.Count - 1 DO
         AddBox(Logics[i]);

      FOR i := 0 TO LogicProducer.CommentAreas.Count - 1 DO
         AddShape(LogicProducer.CommentAreas[i]);

      Result := ShowModal = mrOK;
   FINALLY
      fLogicItemEditor.Free;
   END;

   //  pLog.Dock(pLeft, rect(1,1,10,10));
   //  pLog.Dock(pLeft, rect(1,1,10,10));
   //  pLeft.Dock(pLog, rect(1,1,1,1));
END;

PROCEDURE TJvgLogicsEditor.AddBox(LogicElement_: TJvgLogicElement);
VAR
   Box                        : TJvgGroupBoxPlus;
BEGIN
   Box := TJvgGroupBoxPlus.Create(self);
   WITH Box DO
   BEGIN
      Parent := SB;
      Left := LogicElement_.Left;
      Top := LogicElement_.Top;
      Width := 100;
      Height := 50;
      Caption := LogicElement_.Caption;
      Options := Options - [fgoCanCollapse];
      CaptionAlignment := fcaWidth;
      CaptionBorder.Inner := bvRaised;
      CaptionBorder.Outer := bvLowered;
      Colors.Caption := clBtnShadow;
      Colors.TextActive := clBtnHighlight;
      OnMouseDown := MouseDown_;
      OnMouseMove := MouseMove_;
      OnMouseUp := MouseUp_;
      OnDblClick := DblClick_;
      //Border.Inner := bvRaised;
      //Border.Outer := bvNone;
      Colors.Client := clWhite;
      Colors.ClientActive := clWhite;
      Box.LogicElement := LogicElement_;

      cbNext.Items.AddObject(LogicElement_.Caption, LogicElement);
      cbNextFalse.Items.AddObject(LogicElement_.Caption, LogicElement);

   END;
END;

PROCEDURE TJvgLogicsEditor.AddShape(CommentArea: TJvgCommentArea);
VAR
   Shape                      : TJvgShapePlus;
BEGIN
   Shape := TJvgShapePlus.Create(self);
   Shape.CommentArea := CommentArea;
   WITH Shape DO
   BEGIN
      Parent := SB;
      Left := CommentArea.Left;
      Top := CommentArea.Top;
      Width := CommentArea.Width;
      Height := CommentArea.Height;
      Pen.Style := psDashDot;
      Brush.Style := bsClear;

      OnMouseDown := MouseDown_;
      OnMouseMove := MouseMove_;
      OnMouseUp := MouseUp_;
      OnDblClick := DblClick_;
   END;

END;

PROCEDURE TJvgLogicsEditor.MouseDown_(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
VAR
   i                          : integer;
BEGIN
   IF Sender IS TJvgShapePlus THEN
      WITH Sender AS TJvgShapePlus DO
      BEGIN
         pt.X := X;
         pt.Y := Y;
         pt := ClientToScreen(pt);
         Selected := true;
         Tag := 1;
         IF (X >= Width - 5) AND (X < Width) AND (Y >= Height - 5) AND (Y <
            Height) THEN
            Tag := 2;
         exit;
      END;

   WITH TJvgGroupBoxPlus(Sender) DO
   BEGIN
      pt.X := X;
      pt.Y := Y;
      pt := ClientToScreen(pt);
      pt.Y := pt.Y;                     // + SB.VertScrollBar.ScrollPos;
      Tag := 1;
      Options := Options + [fgoDelineatedText];
      Colors.Caption := clBtnHighlight;
      Colors.TextActive := clBlack;
      Font.Style := [fsBold];
      Selected := true;
      ActiveBox := TJvgGroupBoxPlus(Sender);
   END;

   FOR i := 0 TO SB.ControlCount - 1 DO
      IF (SB.Controls[i] IS TJvgGroupBoxPlus) THEN
         WITH TJvgGroupBoxPlus(SB.Controls[i]) DO
         BEGIN
            IF ActiveBox = TJvgGroupBoxPlus(SB.Controls[i]) THEN
               continue;
            Options := Options - [fgoDelineatedText];
            Colors.Caption := clBtnShadow;
            Colors.TextActive := clBtnHighlight;
            Font.Style := [];
            Selected := false;
            Repaint;
         END;
END;

PROCEDURE TJvgLogicsEditor.MouseMove_(Sender: TObject; Shift: TShiftState; X, Y:
   Integer);
VAR
   pt_new                     : TPoint;
BEGIN
   IF Sender IS TJvgShapePlus THEN
      WITH Sender AS TJvgShapePlus DO
      BEGIN
         CASE Tag OF
            1:
               BEGIN
                  pt_new.X := X;
                  pt_new.Y := Y;
                  pt_new := ClientToScreen(pt_new);
                  Left := Left + pt_new.X - pt.X;
                  Top := Top + pt_new.Y - pt.Y;
                  CommentArea.Left := Left + SB.HorzScrollBar.ScrollPos;
                  CommentArea.Top := Top + SB.VertScrollBar.ScrollPos;
               END;
            2:
               BEGIN
                  pt_new.X := X;
                  pt_new.Y := Y;
                  pt_new := ClientToScreen(pt_new);
                  Width := Width + pt_new.X - pt.X;
                  Height := Height + pt_new.Y - pt.Y;
                  IF Width < 50 THEN
                     Width := 50;
                  IF Height < 50 THEN
                     Height := 50;
                  CommentArea.Width := Width;
                  CommentArea.Height := Height;
               END;
         END;
         pt.X := pt_new.X;
         pt.Y := pt_new.Y;
         exit;
      END;

   WITH TJvgGroupBoxPlus(Sender) DO
   BEGIN
      IF bool(Tag) THEN
      BEGIN
         pt_new.X := X;
         pt_new.Y := Y;
         pt_new := ClientToScreen(pt_new);
         pt_new.Y := pt_new.Y;          // + SB.VertScrollBar.ScrollPos;
         Left := Left + pt_new.X - pt.X;
         Top := Top + pt_new.Y - pt.Y;

         LogicElement.Left := Left + SB.HorzScrollBar.ScrollPos;
         LogicElement.Top := Top + SB.VertScrollBar.ScrollPos;

         SBar.SimpleText := IntToStr(Left) + ':' + IntToStr(Top);

         UpdateView;
      END;
      pt.X := pt_new.X;
      pt.Y := pt_new.Y;
   END;
END;

PROCEDURE TJvgLogicsEditor.UpdateView;
VAR
   DC                         : HDC;
BEGIN
   DC := GetDC(SB.Handle);
   SendMessage(SB.Handle, WM_EraseBkgnd, WPARAM(DC), 0);
   ReleaseDC(SB.Handle, DC);
END;

PROCEDURE TJvgLogicsEditor.MouseUp_(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
VAR
   i                          : integer;
BEGIN
   TControl(Sender).Tag := 0;
   FOR i := 0 TO SB.ControlCount - 1 DO
      IF (SB.Controls[i] IS TJvgShapePlus) THEN
         (SB.Controls[i] AS TJvgShapePlus).Paint;
END;

PROCEDURE TJvgLogicsEditor.DblClick_(Sender: TObject);
VAR
   str                        : STRING;
BEGIN
   TControl(Sender).Tag := 0;
   IF Sender IS TJvgShapePlus THEN
      WITH Sender AS TJvgShapePlus DO
      BEGIN
         str := CommentArea.Text;
         IF InputQuery('Caption', 'Comments', str) THEN
            CommentArea.Text := str;
         PostMessage(TWinControl(Parent).Handle, WM_LBUTTONUP, 1, 1);
         exit;
      END;

   fLogicItemEditor.Execute(Logics, TJvgGroupBoxPlus(Sender).LogicElement);
   PostMessage(TWinControl(Sender).Handle, WM_LBUTTONUP, 1, 1);
END;

PROCEDURE TJvgLogicsEditor.SetActiveBox(CONST Value: TJvgGroupBoxPlus);
VAR
   i, Index                   : integer;
   Box                        : TJvgGroupBoxPlus;
BEGIN
   FActiveBox := Value;

   Index := cbNext.Items.IndexOfObject(Value.LogicElement.NextElement);
   IF Index <> -1 THEN
      cbNext.ItemIndex := Index
   ELSE
      cbNext.ItemIndex := 0;

   Index :=
      cbNextFalse.Items.IndexOfObject(Value.LogicElement.NextFalseElement);
   IF Index <> -1 THEN
      cbNextFalse.ItemIndex := Index
   ELSE
      cbNextFalse.ItemIndex := 0;

   cbMode.ItemIndex := integer(FActiveBox.LogicElement.IsFirst);
   eStepName.Text := FActiveBox.LogicElement.Caption;
END;

PROCEDURE TJvgLogicsEditor.OnTraceMessage(Sender: TJvgLogics; fStepResult:
   boolean; CONST StepResult, ParsedResult, Msg: STRING);
BEGIN
   mLog.Lines.Add(Msg);
   IF reReslt.Text = '' THEN
      reReslt.Tag := 0;

   IF length(ParsedResult) = 0 THEN
      exit;
   tag := 1 - tag;

   //  reReslt.Lines.BeginUpdate;

   reReslt.Text := reReslt.Text + ParsedResult;

   reReslt.SelStart := length(reReslt.Text) - length(ParsedResult);
   reReslt.SelLength := length(ParsedResult);
   IF tag = 0 THEN
      reReslt.SelAttributes.Color := clRed
   ELSE
      reReslt.SelAttributes.Color := clGreen;
   //  reReslt.SelAttributes.Color := RGB(100+Random(100), 100+Random(100), 100+Random(100));

   reReslt.SelLength := 0;
   //  reReslt.Lines.EndUpdate;
END;

{ TJvgGroupBoxPlus }

PROCEDURE TJvgGroupBoxPlus.Paint;
VAR
   i                          : integer;
   R                          : TRect;
   str                        : STRING;
BEGIN
   INHERITED;
   //  ChangeBitmapColor((Owner as TJvgLogicsEditor).iLink.Picture.Bitmap, GetPixel((Owner as TJvgLogicsEditor).iLink.Picture.Bitmap.Canvas.Handle, 0,0), IIF(LogicElement.NextElement<>nil, clGreen, clRed));
   //  BitBlt(Canvas.handle, 100-14-3, 3, 14, 13, (Owner as TJvgLogicsEditor).iLink.Picture.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);

   Canvas.Font.Color := clTeal;         //clBlue;
   Canvas.Font.Style := [];
   str := LogicElement.Expression + ' ' + LogicRuleLabels[LogicElement.Rule] +
      ' ' + LogicElement.Value;
   R := Bounds(3, 20, Width - 6, Height - 22);
   DrawText(Canvas.handle, PChar(str), length(str), R, DT_WORDBREAK OR
      DT_END_ELLIPSIS OR DT_MODIFYSTRING);
END;

PROCEDURE TJvgLogicsEditor.SBEraseBkgndEvent(Sender: TObject; DC: HDC);
VAR
   Canvas                     : TCanvas;
   LogicElement               : TJvgLogicElement;
   i                          : integer;
   PenFalse, Pen, PenTrue, OldPen, PenGrid: HPen;
   Brush, OldBrush            : HBrush;
   NextBox, PrevBox, PrevFalseBox: TJvgGroupBoxPlus;
   bmp                        : TBitmap;

   FUNCTION FindBox(LogicElement: TJvgLogicElement): TJvgGroupBoxPlus;
   VAR
      i                       : integer;
   BEGIN
      Result := NIL;
      IF LogicElement = NIL THEN
         exit;
      FOR i := 0 TO SB.ControlCount - 1 DO
         IF SB.Controls[i] IS TJvgGroupBoxPlus THEN
            IF TJvgGroupBoxPlus(SB.Controls[i]).LogicElement = LogicElement THEN
               Result := TJvgGroupBoxPlus(SB.Controls[i]);
   END;

   PROCEDURE Line(X, Y, X2, Y2: integer; isTrueLine: boolean);
   CONST
      R                       = 5;
   BEGIN
      IF isTrueLine THEN
         SelectObject(DC, PenTrue)
      ELSE
         SelectObject(DC, PenFalse);
      MoveToEx(DC, X, Y, NIL);
      LineTo(DC, X2, Y2);
      SelectObject(DC, Pen);
      IF (abs(X2 - X) < 500) AND (abs(Y2 - Y) < 500) THEN
         MoveToEx(DC, X, Y + 1, NIL);
      LineTo(DC, X2, Y2 + 1);
      Ellipse(DC, X - R - 2, Y - R - 2, X + R * 2 - 2, Y + R * 2 - 2);
      Ellipse(DC, X2 - R - 2, Y2 - R - 2, X2 + R * 2 - 2, Y2 + R * 2 - 2);
   END;

   PROCEDURE DrawGrid;
   VAR
      i, j                    : integer;
   CONST
      step                    = 14;
   BEGIN
      FillRect(DC, SB.ClientRect, Brush);
      FOR i := 1 TO SB.Width DIV step DO
      BEGIN
         j := i * 14;
         MoveToEx(DC, j, 0, NIL);
         LineTo(DC, j, SB.Height);
      END;
      FOR i := 1 TO SB.Height DIV step DO
      BEGIN
         j := i * 14;
         MoveToEx(DC, 0, j, NIL);
         LineTo(DC, SB.Width, j);
      END;
   END;
BEGIN
   TRY
      Brush := CreateSolidBrush(clWhite);
      Pen := CreatePen(PS_SOLID, 1, clBlack);
      PenGrid := CreatePen(PS_SOLID, 1, $E0E0E0);
      //PenLong := CreatePen( PS_DASHDOT, 1, $E0E0E0 );
      PenTrue := CreatePen(PS_SOLID, 1, $FF9090);
      PenFalse := CreatePen(PS_SOLID, 1, $009090);
      OldPen := SelectObject(DC, PenGrid);
      OldBrush := SelectObject(DC, Brush);

      DrawGrid;

      FOR i := 0 TO SB.ControlCount - 1 DO
      BEGIN
         IF NOT (SB.Controls[i] IS TJvgGroupBoxPlus) THEN
            continue;
         LogicElement := TJvgGroupBoxPlus(SB.Controls[i]).LogicElement;
         IF LogicElement = NIL THEN
            exit;

         IF LogicElement.IsFirst THEN
         BEGIN
            MoveToEx(DC, 0, 0, NIL);
            LineTo(DC, SB.Controls[i].Left, SB.Controls[i].Top);
         END;

         PrevBox := FindBox(LogicElement.NextElement);
         IF Assigned(PrevBox) THEN
         BEGIN
            Line(SB.Controls[i].Left + SB.Controls[i].Width, SB.Controls[i].Top,
               PrevBox.Left, PrevBox.Top, true);
            DeleteObject(SelectObject(DC, OldPen));
         END;

         PrevFalseBox := FindBox(LogicElement.NextFalseElement);
         IF Assigned(PrevFalseBox) THEN
         BEGIN
            Line(SB.Controls[i].Left + SB.Controls[i].Width, SB.Controls[i].Top
               + SB.Controls[i].Height, PrevFalseBox.Left, PrevFalseBox.Top,
               false);
            DeleteObject(SelectObject(DC, OldPen));
         END;

         bmp := TBitmap.Create;
         IF LogicElement.NextElement <> NIL THEN
         BEGIN
            ImageList.GetBitmap(0, bmp);
            BitBlt(DC, SB.Controls[i].Left + SB.Controls[i].Width - 3,
               SB.Controls[i].Top, bmp.width, bmp.height, bmp.Canvas.Handle, 0, 0,
               SRCCOPY);
         END;

         IF LogicElement.NextFalseElement <> NIL THEN
         BEGIN
            ImageList.GetBitmap(1, bmp);
            BitBlt(DC, SB.Controls[i].Left + SB.Controls[i].Width - 3,
               SB.Controls[i].Top + SB.Controls[i].Height - 17, bmp.width,
               bmp.height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
         END;

         IF LogicElement.IsFirst THEN
         BEGIN
            ImageList.GetBitmap(2, bmp);
            BitBlt(DC, SB.Controls[i].Left - 20, SB.Controls[i].Top, bmp.width,
               bmp.height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
         END;

         bmp.Free;

      END;

   FINALLY
      SelectObject(DC, Pen);
      DeleteObject(SelectObject(DC, OldPen));
      DeleteObject(PenTrue);
      DeleteObject(PenFalse);
      DeleteObject(PenGrid);
      DeleteObject(Brush);
   END;

   //  for i := 0 to SB.ControlCount-1 do
   //    if (SB.Controls[i] is TJvgShapePlus) then (SB.Controls[i] as TJvgShapePlus).Paint;

END;

PROCEDURE TJvgLogicsEditor.cbNextChange(Sender: TObject);
BEGIN
   IF FActiveBox = NIL THEN
      exit;
   IF FActiveBox.LogicElement <> cbNext.items.Objects[cbNext.ItemIndex] THEN
   BEGIN
      FActiveBox.LogicElement.NextElement :=
         TJvgLogicElement(cbNext.items.Objects[cbNext.ItemIndex]);
   END;

   UpdateView;
END;

PROCEDURE TJvgLogicsEditor.cbModeChange(Sender: TObject);
BEGIN
   IF FActiveBox = NIL THEN
      exit;
   FActiveBox.LogicElement.IsFirst := cbMode.ItemIndex = 1;
END;

{ TJvgLogicsComponentEditor }

PROCEDURE TJvgLogicsComponentEditor.ExecuteVerb(Index: Integer);
BEGIN
   INHERITED;
   ShowEditor(TJvgLogicProducer(Component));
END;

FUNCTION TJvgLogicsComponentEditor.GetVerb(Index: Integer): STRING;
BEGIN
   CASE Index OF
      0: Result := 'Edit component...';
   END;
END;

FUNCTION TJvgLogicsComponentEditor.GetVerbCount: Integer;
BEGIN
   Result := 1;
END;

PROCEDURE TJvgLogicsComponentEditor.ShowEditor(LogicProducer:
   TJvgLogicProducer);
VAR
   glLogicsEditor             : TJvgLogicsEditor;
   Logics                     : TJvgLogics;
BEGIN
   Logics := LogicProducer.Logics;
   {  with Logics.Add do
     begin
       Left := 10; Top := 10;
       IsFirst := true;
     end;

     with Logics.Add do
     begin
       Left := 200; Top := 30;
       PrevElementID := Logics[0].ID;
     end;

     Logics[0].NextElementID := Logics[1].ID;

     with Logics.Add do
     begin
       Left := 200; Top := 100;
     end;
   }
   TRY
      glLogicsEditor := TJvgLogicsEditor.Create(NIL);
      glLogicsEditor.Execute(LogicProducer);
   FINALLY
      FreeAndNil(glLogicsEditor);
   END;
END;

PROCEDURE TJvgLogicsEditor.tbNewClick(Sender: TObject);
VAR
   LogicElement               : TJvgLogicElement;
BEGIN
   LogicElement := Logics.Add;
   WITH LogicElement DO
   BEGIN
      Left := SB.Width DIV 2;
      Top := SB.Height DIV 2;
      AddBox(LogicElement);
   END;
END;

PROCEDURE TJvgLogicsEditor.cbNextFalseChange(Sender: TObject);
BEGIN
   IF FActiveBox = NIL THEN
      exit;
   IF FActiveBox.LogicElement <> cbNextFalse.items.Objects[cbNextFalse.ItemIndex]
      THEN
   BEGIN
      FActiveBox.LogicElement.NextFalseElement :=
         TJvgLogicElement(cbNextFalse.items.Objects[cbNextFalse.ItemIndex]);
   END;
   UpdateView;
END;

PROCEDURE TJvgLogicsEditor.eStepNameChange(Sender: TObject);
BEGIN
   IF NOT Assigned(ActiveBox) THEN
      exit;
   ActiveBox.LogicElement.Caption := eStepName.Text;
   ActiveBox.Caption := eStepName.Text;
END;

PROCEDURE TJvgLogicsEditor.FormShow(Sender: TObject);
BEGIN
   SB.BufferedDraw := true;
END;

PROCEDURE TJvgLogicsEditor.ToolButton5Click(Sender: TObject);
VAR
   i                          : integer;
BEGIN
   pLog.Visible := true;

   mLog.Lines.Clear;
   reReslt.Text := '';

   Logics.Analyze;

   FOR i := 0 TO SB.ControlCount - 1 DO
      IF (SB.Controls[i] IS TJvgGroupBoxPlus) THEN
         IF TJvgGroupBoxPlus(SB.Controls[i]).LogicElement.IsTrue THEN
            TJvgGroupBoxPlus(SB.Controls[i]).Colors.Caption := clGreen;

END;

PROCEDURE TJvgLogicsEditor.ToolButton7Click(Sender: TObject);
VAR
   CommentArea                : TJvgCommentArea;
BEGIN
   CommentArea := LogicProducer.CommentAreas.Add;
   WITH CommentArea DO
   BEGIN
      Left := SB.Width DIV 2;
      Top := SB.Height DIV 2;
      Width := 100;
      Height := 100;
      AddShape(CommentArea);
   END;
END;

{ TJvgShapePlus }

PROCEDURE TJvgShapePlus.Paint;
VAR
   i                          : integer;
   R                          : TRect;
   str                        : STRING;
BEGIN
   INHERITED;

   Canvas.Font.Color := clBlue;
   Canvas.Font.Style := [fsBold];
   str := CommentArea.Text;
   R := Bounds(3, 2, Width, Height);
   DrawText(Canvas.handle, PChar(str), length(str), R, DT_WORDBREAK);
END;

PROCEDURE TJvgLogicsEditor.cbIgnoreSpacesClick(Sender: TObject);
BEGIN
   LogicProducer.IgnoreSpaces := cbIgnoreSpaces.Checked;
END;

PROCEDURE TJvgLogicsEditor.pLeftDockOver(Sender: TObject; Source:
   TDragDockObject; X, Y: Integer; State: TDragState; VAR Accept: Boolean);
BEGIN
   Accept := true;
END;

PROCEDURE TJvgLogicsEditor.pLeftUnDock(Sender: TObject; Client: TControl;
   NewTarget: TWinControl; VAR Allow: Boolean);
BEGIN
   (Sender AS TWinControl).Height := 6;
END;

PROCEDURE TJvgLogicsEditor.pLeftDockDrop(Sender: TObject; Source:
   TDragDockObject; X, Y: Integer);
BEGIN
   (Sender AS TWinControl).Height := 100;
   SBar.Top := 1500;
END;

PROCEDURE TJvgLogicsEditor.ToolButton8Click(Sender: TObject);
VAR
   i                          : integer;
BEGIN

   IF Logics.TraceItem = NIL THEN
   BEGIN
      Logics.StartAnalyze;
      reReslt.Text := '';
   END;
   tbStop.Enabled := true;

   Logics.AnalyzeStep;

   FOR i := 0 TO SB.ControlCount - 1 DO
      IF (SB.Controls[i] IS TJvgGroupBoxPlus) THEN
         IF TJvgGroupBoxPlus(SB.Controls[i]).LogicElement.IsTrue THEN
            TJvgGroupBoxPlus(SB.Controls[i]).Colors.Caption := clGreen;

   //  ShowMessage(Logics.Result);

END;

PROCEDURE TJvgLogicsEditor.tbStopClick(Sender: TObject);
VAR
   i                          : integer;
BEGIN
   Logics.TraceItem := NIL;
   tbStop.Enabled := false;

   FOR i := 0 TO SB.ControlCount - 1 DO
      IF (SB.Controls[i] IS TJvgGroupBoxPlus) THEN
         IF TJvgGroupBoxPlus(SB.Controls[i]).LogicElement.IsTrue THEN
            TJvgGroupBoxPlus(SB.Controls[i]).Colors.Caption := clBtnShadow;
END;

PROCEDURE TJvgLogicsEditor.TabSet1Change(Sender: TObject; NewTab: Integer; VAR
   AllowChange: Boolean);
BEGIN
   PC.ActivePageIndex := NewTab;
END;

END.

