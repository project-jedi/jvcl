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

unit JvgLogicsEditor;

interface

uses
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

type
  TJvgGroupBoxPlus = class;

  TJvgLogicsComponentEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  private
    procedure ShowEditor(LogicProducer: TJvgLogicProducer);
  end;

  TJvgLogicsEditor = class(TForm)
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
    procedure pLeftDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure pLeftUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure pLeftDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
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
    procedure AddBox(LogicElement_: TJvgLogicElement);
    procedure AddShape(CommentArea: TJvgCommentArea);

    procedure OnTraceMessage(Sender: TJvgLogics; fStepResult: boolean; const
      StepResult, ParsedResult, Msg: string);
  public
    function Execute(LogicProducer: TJvgLogicProducer): boolean;
    property ActiveBox: TJvgGroupBoxPlus read FActiveBox write SetActiveBox;
  end;

  TJvgGroupBoxPlus = class(TJvgGroupBox)
  public
    pt: TPoint;
    Selected: boolean;
    LogicElement: TJvgLogicElement;
    //    fAsLogical: boolean;
    //    constructor Create(AOwner: TComponent); override;
    //    destructor Destroy; override;
    procedure Paint; override;
  end;

  TJvgShapePlus = class(TShape)
  public
    pt: TPoint;
    Selected: boolean;
    CommentArea: TJvgCommentArea;
    procedure Paint; override;
  end;

var
  glLogicsEditor: TJvgLogicsEditor;

implementation
uses JvgTypes,
  JvgUtils,
  JvgLogicItemEditor,
  clipbrd;
{$R *.DFM}

{ TJvgLogicsEditor }

function TJvgLogicsEditor.Execute(LogicProducer: TJvgLogicProducer): boolean;
var
  i: integer;
begin
  fLogicItemEditor := TJvgLogicItemEditor.Create(nil);

  mDictionary.Lines.Assign(LogicProducer.Dictionary);
  try

    self.LogicProducer := LogicProducer;
    self.Logics := LogicProducer.Logics;

    Logics.OnTraceMessage := OnTraceMessage;

    cbNext.Items.Clear;
    cbNextFalse.Items.Clear;
    cbNext.Items.Add('');
    cbNextFalse.Items.Add('');

    for i := 0 to Logics.Count - 1 do
      AddBox(Logics[i]);

    for i := 0 to LogicProducer.CommentAreas.Count - 1 do
      AddShape(LogicProducer.CommentAreas[i]);

    Result := ShowModal = mrOK;
  finally
    fLogicItemEditor.Free;
  end;

  //  pLog.Dock(pLeft, rect(1,1,10,10));
  //  pLog.Dock(pLeft, rect(1,1,10,10));
  //  pLeft.Dock(pLog, rect(1,1,1,1));
end;

procedure TJvgLogicsEditor.AddBox(LogicElement_: TJvgLogicElement);
var
  Box: TJvgGroupBoxPlus;
begin
  Box := TJvgGroupBoxPlus.Create(self);
  with Box do
  begin
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

  end;
end;

procedure TJvgLogicsEditor.AddShape(CommentArea: TJvgCommentArea);
var
  Shape: TJvgShapePlus;
begin
  Shape := TJvgShapePlus.Create(self);
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
  i: integer;
begin
  if Sender is TJvgShapePlus then
    with Sender as TJvgShapePlus do
    begin
      pt.X := X;
      pt.Y := Y;
      pt := ClientToScreen(pt);
      Selected := true;
      Tag := 1;
      if (X >= Width - 5) and (X < Width) and (Y >= Height - 5) and (Y <
        Height) then
        Tag := 2;
      exit;
    end;

  with TJvgGroupBoxPlus(Sender) do
  begin
    pt.X := X;
    pt.Y := Y;
    pt := ClientToScreen(pt);
    pt.Y := pt.Y; // + SB.VertScrollBar.ScrollPos;
    Tag := 1;
    Options := Options + [fgoDelineatedText];
    Colors.Caption := clBtnHighlight;
    Colors.TextActive := clBlack;
    Font.Style := [fsBold];
    Selected := true;
    ActiveBox := TJvgGroupBoxPlus(Sender);
  end;

  for i := 0 to SB.ControlCount - 1 do
    if (SB.Controls[i] is TJvgGroupBoxPlus) then
      with TJvgGroupBoxPlus(SB.Controls[i]) do
      begin
        if ActiveBox = TJvgGroupBoxPlus(SB.Controls[i]) then
          continue;
        Options := Options - [fgoDelineatedText];
        Colors.Caption := clBtnShadow;
        Colors.TextActive := clBtnHighlight;
        Font.Style := [];
        Selected := false;
        Repaint;
      end;
end;

procedure TJvgLogicsEditor.MouseMove_(Sender: TObject; Shift: TShiftState; X, Y:
  Integer);
var
  pt_new: TPoint;
begin
  if Sender is TJvgShapePlus then
    with Sender as TJvgShapePlus do
    begin
      case Tag of
        1:
          begin
            pt_new.X := X;
            pt_new.Y := Y;
            pt_new := ClientToScreen(pt_new);
            Left := Left + pt_new.X - pt.X;
            Top := Top + pt_new.Y - pt.Y;
            CommentArea.Left := Left + SB.HorzScrollBar.ScrollPos;
            CommentArea.Top := Top + SB.VertScrollBar.ScrollPos;
          end;
        2:
          begin
            pt_new.X := X;
            pt_new.Y := Y;
            pt_new := ClientToScreen(pt_new);
            Width := Width + pt_new.X - pt.X;
            Height := Height + pt_new.Y - pt.Y;
            if Width < 50 then
              Width := 50;
            if Height < 50 then
              Height := 50;
            CommentArea.Width := Width;
            CommentArea.Height := Height;
          end;
      end;
      pt.X := pt_new.X;
      pt.Y := pt_new.Y;
      exit;
    end;

  with TJvgGroupBoxPlus(Sender) do
  begin
    if bool(Tag) then
    begin
      pt_new.X := X;
      pt_new.Y := Y;
      pt_new := ClientToScreen(pt_new);
      pt_new.Y := pt_new.Y; // + SB.VertScrollBar.ScrollPos;
      Left := Left + pt_new.X - pt.X;
      Top := Top + pt_new.Y - pt.Y;

      LogicElement.Left := Left + SB.HorzScrollBar.ScrollPos;
      LogicElement.Top := Top + SB.VertScrollBar.ScrollPos;

      SBar.SimpleText := IntToStr(Left) + ':' + IntToStr(Top);

      UpdateView;
    end;
    pt.X := pt_new.X;
    pt.Y := pt_new.Y;
  end;
end;

procedure TJvgLogicsEditor.UpdateView;
var
  DC: HDC;
begin
  DC := GetDC(SB.Handle);
  SendMessage(SB.Handle, WM_EraseBkgnd, WPARAM(DC), 0);
  ReleaseDC(SB.Handle, DC);
end;

procedure TJvgLogicsEditor.MouseUp_(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  TControl(Sender).Tag := 0;
  for i := 0 to SB.ControlCount - 1 do
    if (SB.Controls[i] is TJvgShapePlus) then
      (SB.Controls[i] as TJvgShapePlus).Paint;
end;

procedure TJvgLogicsEditor.DblClick_(Sender: TObject);
var
  str: string;
begin
  TControl(Sender).Tag := 0;
  if Sender is TJvgShapePlus then
    with Sender as TJvgShapePlus do
    begin
      str := CommentArea.Text;
      if InputQuery('Caption', 'Comments', str) then
        CommentArea.Text := str;
      PostMessage(TWinControl(Parent).Handle, WM_LBUTTONUP, 1, 1);
      exit;
    end;

  fLogicItemEditor.Execute(Logics, TJvgGroupBoxPlus(Sender).LogicElement);
  PostMessage(TWinControl(Sender).Handle, WM_LBUTTONUP, 1, 1);
end;

procedure TJvgLogicsEditor.SetActiveBox(const Value: TJvgGroupBoxPlus);
var
  i, Index: integer;
  Box: TJvgGroupBoxPlus;
begin
  FActiveBox := Value;

  Index := cbNext.Items.IndexOfObject(Value.LogicElement.NextElement);
  if Index <> -1 then
    cbNext.ItemIndex := Index
  else
    cbNext.ItemIndex := 0;

  Index :=
    cbNextFalse.Items.IndexOfObject(Value.LogicElement.NextFalseElement);
  if Index <> -1 then
    cbNextFalse.ItemIndex := Index
  else
    cbNextFalse.ItemIndex := 0;

  cbMode.ItemIndex := integer(FActiveBox.LogicElement.IsFirst);
  eStepName.Text := FActiveBox.LogicElement.Caption;
end;

procedure TJvgLogicsEditor.OnTraceMessage(Sender: TJvgLogics; fStepResult:
  boolean; const StepResult, ParsedResult, Msg: string);
begin
  mLog.Lines.Add(Msg);
  if reReslt.Text = '' then
    reReslt.Tag := 0;

  if length(ParsedResult) = 0 then
    exit;
  tag := 1 - tag;

  //  reReslt.Lines.BeginUpdate;

  reReslt.Text := reReslt.Text + ParsedResult;

  reReslt.SelStart := length(reReslt.Text) - length(ParsedResult);
  reReslt.SelLength := length(ParsedResult);
  if tag = 0 then
    reReslt.SelAttributes.Color := clRed
  else
    reReslt.SelAttributes.Color := clGreen;
  //  reReslt.SelAttributes.Color := RGB(100+Random(100), 100+Random(100), 100+Random(100));

  reReslt.SelLength := 0;
  //  reReslt.Lines.EndUpdate;
end;

{ TJvgGroupBoxPlus }

procedure TJvgGroupBoxPlus.Paint;
var
  i: integer;
  R: TRect;
  str: string;
begin
  inherited;
  //  ChangeBitmapColor((Owner as TJvgLogicsEditor).iLink.Picture.Bitmap, GetPixel((Owner as TJvgLogicsEditor).iLink.Picture.Bitmap.Canvas.Handle, 0,0), IIF(LogicElement.NextElement<>nil, clGreen, clRed));
  //  BitBlt(Canvas.handle, 100-14-3, 3, 14, 13, (Owner as TJvgLogicsEditor).iLink.Picture.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);

  Canvas.Font.Color := clTeal; //clBlue;
  Canvas.Font.Style := [];
  str := LogicElement.Expression + ' ' + LogicRuleLabels[LogicElement.Rule] +
    ' ' + LogicElement.Value;
  R := Bounds(3, 20, Width - 6, Height - 22);
  DrawText(Canvas.handle, PChar(str), length(str), R, DT_WORDBREAK or
    DT_END_ELLIPSIS or DT_MODIFYSTRING);
end;

procedure TJvgLogicsEditor.SBEraseBkgndEvent(Sender: TObject; DC: HDC);
var
  Canvas: TCanvas;
  LogicElement: TJvgLogicElement;
  i: integer;
  PenFalse, Pen, PenTrue, OldPen, PenGrid: HPen;
  Brush, OldBrush: HBrush;
  NextBox, PrevBox, PrevFalseBox: TJvgGroupBoxPlus;
  bmp: TBitmap;

  function FindBox(LogicElement: TJvgLogicElement): TJvgGroupBoxPlus;
  var
    i: integer;
  begin
    Result := nil;
    if LogicElement = nil then
      exit;
    for i := 0 to SB.ControlCount - 1 do
      if SB.Controls[i] is TJvgGroupBoxPlus then
        if TJvgGroupBoxPlus(SB.Controls[i]).LogicElement = LogicElement then
          Result := TJvgGroupBoxPlus(SB.Controls[i]);
  end;

  procedure Line(X, Y, X2, Y2: integer; isTrueLine: boolean);
  const
    R = 5;
  begin
    if isTrueLine then
      SelectObject(DC, PenTrue)
    else
      SelectObject(DC, PenFalse);
    MoveToEx(DC, X, Y, nil);
    LineTo(DC, X2, Y2);
    SelectObject(DC, Pen);
    if (abs(X2 - X) < 500) and (abs(Y2 - Y) < 500) then
      MoveToEx(DC, X, Y + 1, nil);
    LineTo(DC, X2, Y2 + 1);
    Ellipse(DC, X - R - 2, Y - R - 2, X + R * 2 - 2, Y + R * 2 - 2);
    Ellipse(DC, X2 - R - 2, Y2 - R - 2, X2 + R * 2 - 2, Y2 + R * 2 - 2);
  end;

  procedure DrawGrid;
  var
    i, j: integer;
  const
    step = 14;
  begin
    FillRect(DC, SB.ClientRect, Brush);
    for i := 1 to SB.Width div step do
    begin
      j := i * 14;
      MoveToEx(DC, j, 0, nil);
      LineTo(DC, j, SB.Height);
    end;
    for i := 1 to SB.Height div step do
    begin
      j := i * 14;
      MoveToEx(DC, 0, j, nil);
      LineTo(DC, SB.Width, j);
    end;
  end;
begin
  try
    Brush := CreateSolidBrush(clWhite);
    Pen := CreatePen(PS_SOLID, 1, clBlack);
    PenGrid := CreatePen(PS_SOLID, 1, $E0E0E0);
    //PenLong := CreatePen( PS_DASHDOT, 1, $E0E0E0 );
    PenTrue := CreatePen(PS_SOLID, 1, $FF9090);
    PenFalse := CreatePen(PS_SOLID, 1, $009090);
    OldPen := SelectObject(DC, PenGrid);
    OldBrush := SelectObject(DC, Brush);

    DrawGrid;

    for i := 0 to SB.ControlCount - 1 do
    begin
      if not (SB.Controls[i] is TJvgGroupBoxPlus) then
        continue;
      LogicElement := TJvgGroupBoxPlus(SB.Controls[i]).LogicElement;
      if LogicElement = nil then
        exit;

      if LogicElement.IsFirst then
      begin
        MoveToEx(DC, 0, 0, nil);
        LineTo(DC, SB.Controls[i].Left, SB.Controls[i].Top);
      end;

      PrevBox := FindBox(LogicElement.NextElement);
      if Assigned(PrevBox) then
      begin
        Line(SB.Controls[i].Left + SB.Controls[i].Width, SB.Controls[i].Top,
          PrevBox.Left, PrevBox.Top, true);
        DeleteObject(SelectObject(DC, OldPen));
      end;

      PrevFalseBox := FindBox(LogicElement.NextFalseElement);
      if Assigned(PrevFalseBox) then
      begin
        Line(SB.Controls[i].Left + SB.Controls[i].Width, SB.Controls[i].Top
          + SB.Controls[i].Height, PrevFalseBox.Left, PrevFalseBox.Top,
          false);
        DeleteObject(SelectObject(DC, OldPen));
      end;

      bmp := TBitmap.Create;
      if LogicElement.NextElement <> nil then
      begin
        ImageList.GetBitmap(0, bmp);
        BitBlt(DC, SB.Controls[i].Left + SB.Controls[i].Width - 3,
          SB.Controls[i].Top, bmp.width, bmp.height, bmp.Canvas.Handle, 0, 0,
          SRCCOPY);
      end;

      if LogicElement.NextFalseElement <> nil then
      begin
        ImageList.GetBitmap(1, bmp);
        BitBlt(DC, SB.Controls[i].Left + SB.Controls[i].Width - 3,
          SB.Controls[i].Top + SB.Controls[i].Height - 17, bmp.width,
          bmp.height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;

      if LogicElement.IsFirst then
      begin
        ImageList.GetBitmap(2, bmp);
        BitBlt(DC, SB.Controls[i].Left - 20, SB.Controls[i].Top, bmp.width,
          bmp.height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;

      bmp.Free;

    end;

  finally
    SelectObject(DC, Pen);
    DeleteObject(SelectObject(DC, OldPen));
    DeleteObject(PenTrue);
    DeleteObject(PenFalse);
    DeleteObject(PenGrid);
    DeleteObject(Brush);
  end;

  //  for i := 0 to SB.ControlCount-1 do
  //    if (SB.Controls[i] is TJvgShapePlus) then (SB.Controls[i] as TJvgShapePlus).Paint;

end;

procedure TJvgLogicsEditor.cbNextChange(Sender: TObject);
begin
  if FActiveBox = nil then
    exit;
  if FActiveBox.LogicElement <> cbNext.items.Objects[cbNext.ItemIndex] then
  begin
    FActiveBox.LogicElement.NextElement :=
      TJvgLogicElement(cbNext.items.Objects[cbNext.ItemIndex]);
  end;

  UpdateView;
end;

procedure TJvgLogicsEditor.cbModeChange(Sender: TObject);
begin
  if FActiveBox = nil then
    exit;
  FActiveBox.LogicElement.IsFirst := cbMode.ItemIndex = 1;
end;

{ TJvgLogicsComponentEditor }

procedure TJvgLogicsComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  ShowEditor(TJvgLogicProducer(Component));
end;

function TJvgLogicsComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit component...';
  end;
end;

function TJvgLogicsComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TJvgLogicsComponentEditor.ShowEditor(LogicProducer:
  TJvgLogicProducer);
var
  glLogicsEditor: TJvgLogicsEditor;
  Logics: TJvgLogics;
begin
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
  if FActiveBox = nil then
    exit;
  if FActiveBox.LogicElement <> cbNextFalse.items.Objects[cbNextFalse.ItemIndex] then
  begin
    FActiveBox.LogicElement.NextFalseElement :=
      TJvgLogicElement(cbNextFalse.items.Objects[cbNextFalse.ItemIndex]);
  end;
  UpdateView;
end;

procedure TJvgLogicsEditor.eStepNameChange(Sender: TObject);
begin
  if not Assigned(ActiveBox) then
    exit;
  ActiveBox.LogicElement.Caption := eStepName.Text;
  ActiveBox.Caption := eStepName.Text;
end;

procedure TJvgLogicsEditor.FormShow(Sender: TObject);
begin
  SB.BufferedDraw := true;
end;

procedure TJvgLogicsEditor.ToolButton5Click(Sender: TObject);
var
  i: integer;
begin
  pLog.Visible := true;

  mLog.Lines.Clear;
  reReslt.Text := '';

  Logics.Analyze;

  for i := 0 to SB.ControlCount - 1 do
    if (SB.Controls[i] is TJvgGroupBoxPlus) then
      if TJvgGroupBoxPlus(SB.Controls[i]).LogicElement.IsTrue then
        TJvgGroupBoxPlus(SB.Controls[i]).Colors.Caption := clGreen;

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

{ TJvgShapePlus }

procedure TJvgShapePlus.Paint;
var
  i: integer;
  R: TRect;
  str: string;
begin
  inherited;

  Canvas.Font.Color := clBlue;
  Canvas.Font.Style := [fsBold];
  str := CommentArea.Text;
  R := Bounds(3, 2, Width, Height);
  DrawText(Canvas.handle, PChar(str), length(str), R, DT_WORDBREAK);
end;

procedure TJvgLogicsEditor.cbIgnoreSpacesClick(Sender: TObject);
begin
  LogicProducer.IgnoreSpaces := cbIgnoreSpaces.Checked;
end;

procedure TJvgLogicsEditor.pLeftDockOver(Sender: TObject; Source:
  TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := true;
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
  SBar.Top := 1500;
end;

procedure TJvgLogicsEditor.ToolButton8Click(Sender: TObject);
var
  i: integer;
begin

  if Logics.TraceItem = nil then
  begin
    Logics.StartAnalyze;
    reReslt.Text := '';
  end;
  tbStop.Enabled := true;

  Logics.AnalyzeStep;

  for i := 0 to SB.ControlCount - 1 do
    if (SB.Controls[i] is TJvgGroupBoxPlus) then
      if TJvgGroupBoxPlus(SB.Controls[i]).LogicElement.IsTrue then
        TJvgGroupBoxPlus(SB.Controls[i]).Colors.Caption := clGreen;

  //  ShowMessage(Logics.Result);

end;

procedure TJvgLogicsEditor.tbStopClick(Sender: TObject);
var
  i: integer;
begin
  Logics.TraceItem := nil;
  tbStop.Enabled := false;

  for i := 0 to SB.ControlCount - 1 do
    if (SB.Controls[i] is TJvgGroupBoxPlus) then
      if TJvgGroupBoxPlus(SB.Controls[i]).LogicElement.IsTrue then
        TJvgGroupBoxPlus(SB.Controls[i]).Colors.Caption := clBtnShadow;
end;

procedure TJvgLogicsEditor.TabSet1Change(Sender: TObject; NewTab: Integer; var
  AllowChange: Boolean);
begin
  PC.ActivePageIndex := NewTab;
end;

end.
