{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLogicsEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgLogicsEditorForm;

interface

uses
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ToolWin, ImgList, RichEdit, Tabs,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvgPage, JvgLogics, JvgScrollBox, JvComponent, JvgGroupBox;

type
  TJvgLogicsComponentEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  private
    procedure ShowEditor(LogicProducer: TJvgLogicProducer);
  end;

  TJvgGroupBoxPlus = class(TJvgGroupBox)
  public
    Pt: TPoint;
    Selected: Boolean;
    LogicElement: TJvgLogicElement;
    //    fAsLogical: Boolean;
    //    constructor Create(AOwner: TComponent); override;
    //    destructor Destroy; override;
    procedure Paint; override;
  end;

  TJvgShapePlus = class(TShape)
  public
    Pt: TPoint;
    Selected: Boolean;
    CommentArea: TJvgCommentArea;
    procedure Paint; override;
  end;

  TJvgLogicsEditor = class(TJvForm)
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
    procedure SBEraseBkgndEvent(Sender: TObject; DC: HDC);
    procedure cbNextChange(Sender: TObject);
    procedure cbModeChange(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure cbNextFalseChange(Sender: TObject);
    procedure eStepNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure cbIgnoreSpacesClick(Sender: TObject);
    procedure pLeftDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pLeftUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure pLeftDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure ToolButton8Click(Sender: TObject);
    procedure tbStopClick(Sender: TObject);
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
  private
    FActiveBox: TJvgGroupBoxPlus;
    LogicProducer: TJvgLogicProducer;
    Logics: TJvgLogics;
    procedure MouseDown_(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure MouseMove_(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp_(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure DblClick_(Sender: TObject);
    procedure SetActiveBox(const Value: TJvgGroupBoxPlus);
    procedure UpdateView;
    procedure AddBox(ALogicElement: TJvgLogicElement);
    procedure AddShape(CommentArea: TJvgCommentArea);
    procedure OnTraceMessage(Sender: TJvgLogics; AStepResult: Boolean;
      const StepResult, ParsedResult, Msg: string);
  public
    function Execute(ALogicProducer: TJvgLogicProducer): Boolean;
    property ActiveBox: TJvgGroupBoxPlus read FActiveBox write SetActiveBox;
  end;

implementation

uses
  ClipBrd,
  JvgTypes, JvgUtils, JvgLogicItemEditorForm, JvDsgnConsts;

{$R *.dfm}

var
  JvgLogicItemEditor: TJvgLogicItemEditor;

//=== { TJvgLogicsEditor } ===================================================

function TJvgLogicsEditor.Execute(ALogicProducer: TJvgLogicProducer): Boolean;
var
  I: Integer;
begin
  JvgLogicItemEditor := TJvgLogicItemEditor.Create(nil);

  mDictionary.Lines.Assign(ALogicProducer.Dictionary);
  try
    LogicProducer := ALogicProducer;
    Logics := ALogicProducer.Logics;

    Logics.OnTraceMessage := OnTraceMessage;

    cbNext.Items.Clear;
    cbNextFalse.Items.Clear;
    cbNext.Items.Add('');
    cbNextFalse.Items.Add('');

    for I := 0 to Logics.Count - 1 do
      AddBox(Logics[I]);

    for I := 0 to LogicProducer.CommentAreas.Count - 1 do
      AddShape(LogicProducer.CommentAreas[I]);

    Result := ShowModal = mrOk;
  finally
    JvgLogicItemEditor.Free;
  end;
  //  pLog.Dock(pLeft, Rect(1, 1, 10, 10));
  //  pLog.Dock(pLeft, Rect(1, 1, 10, 10));
  //  pLeft.Dock(pLog, Rect(1, 1, 1, 1));
end;

procedure TJvgLogicsEditor.AddBox(ALogicElement: TJvgLogicElement);
var
  Box: TJvgGroupBoxPlus;
begin
  Box := TJvgGroupBoxPlus.Create(Self);
  with Box do
  begin
    Parent := SB;
    Left := ALogicElement.Left;
    Top := ALogicElement.Top;
    Width := 100;
    Height := 50;
    Caption := ALogicElement.Caption;
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
    Box.LogicElement := ALogicElement;

    cbNext.Items.AddObject(ALogicElement.Caption, LogicElement);
    cbNextFalse.Items.AddObject(ALogicElement.Caption, LogicElement);
  end;
end;

procedure TJvgLogicsEditor.AddShape(CommentArea: TJvgCommentArea);
var
  Shape: TJvgShapePlus;
begin
  Shape := TJvgShapePlus.Create(Self);
  Shape.CommentArea := CommentArea;
  with Shape do
  begin
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
  end;
end;

procedure TJvgLogicsEditor.MouseDown_(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  if Sender is TJvgShapePlus then
    with Sender as TJvgShapePlus do
    begin
      Pt.X := X;
      Pt.Y := Y;
      Pt := ClientToScreen(Pt);
      Selected := True;
      Tag := 1;
      if (X >= Width - 5) and (X < Width) and (Y >= Height - 5) and (Y <
        Height) then
        Tag := 2;
      Exit;
    end;

  with TJvgGroupBoxPlus(Sender) do
  begin
    Pt.X := X;
    Pt.Y := Y;
    Pt := ClientToScreen(Pt);
    Pt.Y := Pt.Y; // + SB.VertScrollBar.ScrollPos;
    Tag := 1;
    Options := Options + [fgoDelineatedText];
    Colors.Caption := clBtnHighlight;
    Colors.TextActive := clBlack;
    Font.Style := [fsBold];
    Selected := True;
    ActiveBox := TJvgGroupBoxPlus(Sender);
  end;

  for I := 0 to SB.ControlCount - 1 do
    if SB.Controls[I] is TJvgGroupBoxPlus then
      with TJvgGroupBoxPlus(SB.Controls[I]) do
        if ActiveBox <> TJvgGroupBoxPlus(SB.Controls[I]) then
        begin
          Options := Options - [fgoDelineatedText];
          Colors.Caption := clBtnShadow;
          Colors.TextActive := clBtnHighlight;
          Font.Style := [];
          Selected := False;
          Repaint;
        end;
end;

procedure TJvgLogicsEditor.MouseMove_(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  NewPt: TPoint;
begin
  if Sender is TJvgShapePlus then
    with Sender as TJvgShapePlus do
    begin
      case Tag of
        1:
          begin
            NewPt.X := X;
            NewPt.Y := Y;
            NewPt := ClientToScreen(NewPt);
            Left := Left + NewPt.X - Pt.X;
            Top := Top + NewPt.Y - Pt.Y;
            CommentArea.Left := Left + SB.HorzScrollBar.ScrollPos;
            CommentArea.Top := Top + SB.VertScrollBar.ScrollPos;
          end;
        2:
          begin
            NewPt.X := X;
            NewPt.Y := Y;
            NewPt := ClientToScreen(NewPt);
            Width := Width + NewPt.X - Pt.X;
            Height := Height + NewPt.Y - Pt.Y;
            if Width < 50 then
              Width := 50;
            if Height < 50 then
              Height := 50;
            CommentArea.Width := Width;
            CommentArea.Height := Height;
          end;
      end;
      Pt.X := NewPt.X;
      Pt.Y := NewPt.Y;
      Exit;
    end;

  with TJvgGroupBoxPlus(Sender) do
  begin
    if Tag <> 0 then
    begin
      NewPt.X := X;
      NewPt.Y := Y;
      NewPt := ClientToScreen(NewPt);
      NewPt.Y := NewPt.Y; // + SB.VertScrollBar.ScrollPos;
      Left := Left + NewPt.X - Pt.X;
      Top := Top + NewPt.Y - Pt.Y;

      LogicElement.Left := Left + SB.HorzScrollBar.ScrollPos;
      LogicElement.Top := Top + SB.VertScrollBar.ScrollPos;

      SBar.SimpleText := IntToStr(Left) + ':' + IntToStr(Top);

      UpdateView;
    end;
    Pt.X := NewPt.X;
    Pt.Y := NewPt.Y;
  end;
end;

procedure TJvgLogicsEditor.UpdateView;
var
  DC: HDC;
begin
  DC := GetDC(SB.Handle);
  SendMessage(SB.Handle, WM_ERASEBKGND, WPARAM(DC), 0);
  ReleaseDC(SB.Handle, DC);
end;

procedure TJvgLogicsEditor.MouseUp_(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  TControl(Sender).Tag := 0;
  for I := 0 to SB.ControlCount - 1 do
    if (SB.Controls[I] is TJvgShapePlus) then
      (SB.Controls[I] as TJvgShapePlus).Paint;
end;

procedure TJvgLogicsEditor.DblClick_(Sender: TObject);
var
  S: string;
begin
  TControl(Sender).Tag := 0;
  if Sender is TJvgShapePlus then
    with Sender as TJvgShapePlus do
    begin
      S := CommentArea.Text;
      if InputQuery(RsCaption, RsComments, S) then
        CommentArea.Text := S;
      PostMessage(TWinControl(Parent).Handle, WM_LBUTTONUP, 1, 1);
      Exit;
    end;

  JvgLogicItemEditor.Execute(Logics, TJvgGroupBoxPlus(Sender).LogicElement);
  PostMessage(TWinControl(Sender).Handle, WM_LBUTTONUP, 1, 1);
end;

procedure TJvgLogicsEditor.SetActiveBox(const Value: TJvgGroupBoxPlus);
var
  Index: Integer;
begin
  FActiveBox := Value;

  Index := cbNext.Items.IndexOfObject(Value.LogicElement.NextElement);
  if Index <> -1 then
    cbNext.ItemIndex := Index
  else
    cbNext.ItemIndex := 0;

  Index := cbNextFalse.Items.IndexOfObject(Value.LogicElement.NextFalseElement);
  if Index <> -1 then
    cbNextFalse.ItemIndex := Index
  else
    cbNextFalse.ItemIndex := 0;

  cbMode.ItemIndex := Integer(FActiveBox.LogicElement.IsFirst);
  eStepName.Text := FActiveBox.LogicElement.Caption;
end;

procedure TJvgLogicsEditor.OnTraceMessage(Sender: TJvgLogics;
  AStepResult: Boolean; const StepResult, ParsedResult, Msg: string);
begin
  mLog.Lines.Add(Msg);
  if reReslt.Text = '' then
    reReslt.Tag := 0;

  if Length(ParsedResult) = 0 then
    Exit;
  Tag := 1 - Tag;

  //  reReslt.Lines.BeginUpdate;

  reReslt.Text := reReslt.Text + ParsedResult;

  reReslt.SelStart := Length(reReslt.Text) - Length(ParsedResult);
  reReslt.SelLength := Length(ParsedResult);
  if tag = 0 then
    reReslt.SelAttributes.Color := clRed
  else
    reReslt.SelAttributes.Color := clGreen;
  //  reReslt.SelAttributes.Color := RGB(100+Random(100), 100+Random(100), 100+Random(100));

  reReslt.SelLength := 0;
  //  reReslt.Lines.EndUpdate;
end;

//=== { TJvgGroupBoxPlus } ===================================================

procedure TJvgGroupBoxPlus.Paint;
var
  R: TRect;
  S: string;
begin
  inherited Paint;
  //  ChangeBitmapColor((Owner as TJvgLogicsEditor).iLink.Picture.Bitmap, GetPixel((Owner as TJvgLogicsEditor).iLink.Picture.Bitmap.Canvas.Handle, 0,0), IIF(LogicElement.NextElement<>nil, clGreen, clRed));
  //  BitBlt(Canvas.Handle, 100-14-3, 3, 14, 13, (Owner as TJvgLogicsEditor).iLink.Picture.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);

  Canvas.Font.Color := clTeal; //clBlue;
  Canvas.Font.Style := [];
  S := LogicElement.Expression + ' ' + LogicRuleLabels[LogicElement.Rule] +
    ' ' + LogicElement.Value;
  R := Bounds(3, 20, Width - 6, Height - 22);
  DrawText(Canvas.Handle, PChar(S), Length(S), R,
    DT_WORDBREAK or DT_END_ELLIPSIS or DT_MODIFYSTRING);
end;

procedure TJvgLogicsEditor.SBEraseBkgndEvent(Sender: TObject; DC: HDC);
var
  LogicElement: TJvgLogicElement;
  I: Integer;
  PenFalse, Pen, PenTrue, OldPen, PenGrid: HPen;
  Brush: HBrush;
  PrevBox, PrevFalseBox: TJvgGroupBoxPlus;
  Bmp: TBitmap;

  function FindBox(LogicElement: TJvgLogicElement): TJvgGroupBoxPlus;
  var
    I: Integer;
  begin
    Result := nil;
    if LogicElement <> nil then
      for I := 0 to SB.ControlCount - 1 do
        if SB.Controls[I] is TJvgGroupBoxPlus then
          if TJvgGroupBoxPlus(SB.Controls[I]).LogicElement = LogicElement then
          begin
            Result := TJvgGroupBoxPlus(SB.Controls[I]);
            Break;
          end;
  end;

  procedure Line(X, Y, X2, Y2: Integer; IsTrueLine: Boolean);
  const
    R = 5;
  begin
    if IsTrueLine then
      SelectObject(DC, PenTrue)
    else
      SelectObject(DC, PenFalse);
    MoveToEx(DC, X, Y, nil);
    LineTo(DC, X2, Y2);
    SelectObject(DC, Pen);
    if (Abs(X2 - X) < 500) and (Abs(Y2 - Y) < 500) then
      MoveToEx(DC, X, Y + 1, nil);
    LineTo(DC, X2, Y2 + 1);
    Ellipse(DC, X - R - 2, Y - R - 2, X + R * 2 - 2, Y + R * 2 - 2);
    Ellipse(DC, X2 - R - 2, Y2 - R - 2, X2 + R * 2 - 2, Y2 + R * 2 - 2);
  end;

  procedure DrawGrid;
  var
    I: Integer;
  const
    cStep = 14;
  begin
    FillRect(DC, SB.ClientRect, Brush);
    for I := 1 to SB.Width div cStep do
    begin
      MoveToEx(DC, I*cStep, 0, nil);
      LineTo(DC, I*cStep, SB.Height);
    end;
    for I := 1 to SB.Height div cStep do
    begin
      MoveToEx(DC, 0, I*cStep, nil);
      LineTo(DC, SB.Width, I*cStep);
    end;
  end;

begin
  OldPen := 0;
  PenGrid := 0;
  try
    Brush := CreateSolidBrush(clWhite);
    Pen := CreatePen(PS_SOLID, 1, clBlack);
    PenGrid := CreatePen(PS_SOLID, 1, $E0E0E0);
    //PenLong := CreatePen( PS_DASHDOT, 1, $E0E0E0 );
    PenTrue := CreatePen(PS_SOLID, 1, $FF9090);
    PenFalse := CreatePen(PS_SOLID, 1, $009090);
    OldPen := SelectObject(DC, PenGrid);
//    OldBrush := SelectObject(DC, Brush);

    DrawGrid;

    for I := 0 to SB.ControlCount - 1 do
      if SB.Controls[I] is TJvgGroupBoxPlus then
      begin
        LogicElement := TJvgGroupBoxPlus(SB.Controls[I]).LogicElement;
        if LogicElement = nil then
          Exit;

        if LogicElement.IsFirst then
        begin
          MoveToEx(DC, 0, 0, nil);
          LineTo(DC, SB.Controls[I].Left, SB.Controls[I].Top);
        end;

        PrevBox := FindBox(LogicElement.NextElement);
        if Assigned(PrevBox) then
        begin
          Line(SB.Controls[I].Left + SB.Controls[I].Width, SB.Controls[I].Top,
            PrevBox.Left, PrevBox.Top, True);
          DeleteObject(SelectObject(DC, OldPen));
        end;

        PrevFalseBox := FindBox(LogicElement.NextFalseElement);
        if Assigned(PrevFalseBox) then
        begin
          Line(SB.Controls[I].Left + SB.Controls[I].Width, SB.Controls[I].Top +
            SB.Controls[I].Height, PrevFalseBox.Left, PrevFalseBox.Top, False);
          DeleteObject(SelectObject(DC, OldPen));
        end;

        Bmp := TBitmap.Create;
        if LogicElement.NextElement <> nil then
        begin
          ImageList.GetBitmap(0, Bmp);
          BitBlt(DC, SB.Controls[I].Left + SB.Controls[I].Width - 3,
            SB.Controls[I].Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0,
            SRCCOPY);
        end;

        if LogicElement.NextFalseElement <> nil then
        begin
          ImageList.GetBitmap(1, Bmp);
          BitBlt(DC, SB.Controls[I].Left + SB.Controls[I].Width - 3,
            SB.Controls[I].Top + SB.Controls[I].Height - 17, Bmp.Width,
            Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        end;

        if LogicElement.IsFirst then
        begin
          ImageList.GetBitmap(2, Bmp);
          BitBlt(DC, SB.Controls[I].Left - 20, SB.Controls[I].Top, Bmp.Width,
            Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        end;

        Bmp.Free;
      end;

  finally
    SelectObject(DC, Pen);
    DeleteObject(SelectObject(DC, OldPen));
    DeleteObject(PenTrue);
    DeleteObject(PenFalse);
    DeleteObject(PenGrid);
    DeleteObject(Brush);
  end;
  //  for I := 0 to SB.ControlCount-1 do
  //    if (SB.Controls[I] is TJvgShapePlus) then (SB.Controls[I] as TJvgShapePlus).Paint;
end;

procedure TJvgLogicsEditor.cbNextChange(Sender: TObject);
begin
  if FActiveBox <> nil then
  begin
    if FActiveBox.LogicElement <> cbNext.Items.Objects[cbNext.ItemIndex] then
      FActiveBox.LogicElement.NextElement :=
        TJvgLogicElement(cbNext.Items.Objects[cbNext.ItemIndex]);
    UpdateView;
  end;
end;

procedure TJvgLogicsEditor.cbModeChange(Sender: TObject);
begin
  if FActiveBox <> nil then
    FActiveBox.LogicElement.IsFirst := cbMode.ItemIndex = 1;
end;

//=== { TJvgLogicsComponentEditor } ==========================================

procedure TJvgLogicsComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    ShowEditor(TJvgLogicProducer(Component));
end;

function TJvgLogicsComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsEditComponentEllipsis;
  end;
end;

function TJvgLogicsComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TJvgLogicsComponentEditor.ShowEditor(LogicProducer: TJvgLogicProducer);
var
  glLogicsEditor: TJvgLogicsEditor;
//  Logics: TJvgLogics;
begin
//  Logics := LogicProducer.Logics;
  {  with Logics.Add do
    begin
      Left := 10; Top := 10;
      IsFirst := True;
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
  try
    glLogicsEditor := TJvgLogicsEditor.Create(nil);
    glLogicsEditor.Execute(LogicProducer);
  finally
    FreeAndNil(glLogicsEditor);
  end;
end;

procedure TJvgLogicsEditor.tbNewClick(Sender: TObject);
var
  LogicElement: TJvgLogicElement;
begin
  LogicElement := Logics.Add;
  with LogicElement do
  begin
    Left := SB.Width div 2;
    Top := SB.Height div 2;
    AddBox(LogicElement);
  end;
end;

procedure TJvgLogicsEditor.cbNextFalseChange(Sender: TObject);
begin
  if FActiveBox <> nil then
  begin
    if FActiveBox.LogicElement <> cbNextFalse.Items.Objects[cbNextFalse.ItemIndex] then
    begin
      FActiveBox.LogicElement.NextFalseElement :=
        TJvgLogicElement(cbNextFalse.Items.Objects[cbNextFalse.ItemIndex]);
    end;
    UpdateView;
  end;
end;

procedure TJvgLogicsEditor.eStepNameChange(Sender: TObject);
begin
  if Assigned(ActiveBox) then
  begin
    ActiveBox.LogicElement.Caption := eStepName.Text;
    ActiveBox.Caption := eStepName.Text;
  end;
end;

procedure TJvgLogicsEditor.FormShow(Sender: TObject);
begin
  SB.BufferedDraw := True;
end;

procedure TJvgLogicsEditor.ToolButton5Click(Sender: TObject);
var
  I: Integer;
begin
  pLog.Visible := True;

  mLog.Lines.Clear;
  reReslt.Text := '';

  Logics.Analyze;

  for I := 0 to SB.ControlCount - 1 do
    if SB.Controls[I] is TJvgGroupBoxPlus then
      if TJvgGroupBoxPlus(SB.Controls[I]).LogicElement.IsTrue then
        TJvgGroupBoxPlus(SB.Controls[I]).Colors.Caption := clGreen;
end;

procedure TJvgLogicsEditor.ToolButton7Click(Sender: TObject);
var
  CommentArea: TJvgCommentArea;
begin
  CommentArea := LogicProducer.CommentAreas.Add;
  with CommentArea do
  begin
    Left := SB.Width div 2;
    Top := SB.Height div 2;
    Width := 100;
    Height := 100;
    AddShape(CommentArea);
  end;
end;

//=== { TJvgShapePlus } ======================================================

procedure TJvgShapePlus.Paint;
var
  R: TRect;
  S: string;
begin
  inherited Paint;
  Canvas.Font.Color := clBlue;
  Canvas.Font.Style := [fsBold];
  S := CommentArea.Text;
  R := Bounds(3, 2, Width, Height);
  DrawText(Canvas.Handle, PChar(S), Length(S), R, DT_WORDBREAK);
end;

procedure TJvgLogicsEditor.cbIgnoreSpacesClick(Sender: TObject);
begin
  LogicProducer.IgnoreSpaces := cbIgnoreSpaces.Checked;
end;

procedure TJvgLogicsEditor.pLeftDockOver(Sender: TObject; Source:
  TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TJvgLogicsEditor.pLeftUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  (Sender as TWinControl).Height := 6;
end;

procedure TJvgLogicsEditor.pLeftDockDrop(Sender: TObject; Source:
  TDragDockObject; X, Y: Integer);
begin
  (Sender as TWinControl).Height := 100;
  // (rom) needs reevaluation. Move it out of the screen needs bigger values.
  SBar.Top := 1500;
end;

procedure TJvgLogicsEditor.ToolButton8Click(Sender: TObject);
var
  I: Integer;
begin
  if Logics.TraceItem = nil then
  begin
    Logics.StartAnalyze;
    reReslt.Text := '';
  end;
  tbStop.Enabled := True;

  Logics.AnalyzeStep;

  for I := 0 to SB.ControlCount - 1 do
    if SB.Controls[I] is TJvgGroupBoxPlus then
      if TJvgGroupBoxPlus(SB.Controls[I]).LogicElement.IsTrue then
        TJvgGroupBoxPlus(SB.Controls[I]).Colors.Caption := clGreen;
end;

procedure TJvgLogicsEditor.tbStopClick(Sender: TObject);
var
  I: Integer;
begin
  Logics.TraceItem := nil;
  tbStop.Enabled := False;

  for I := 0 to SB.ControlCount - 1 do
    if SB.Controls[I] is TJvgGroupBoxPlus then
      if TJvgGroupBoxPlus(SB.Controls[I]).LogicElement.IsTrue then
        TJvgGroupBoxPlus(SB.Controls[I]).Colors.Caption := clBtnShadow;
end;

procedure TJvgLogicsEditor.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  PC.ActivePageIndex := NewTab;
end;

end.
