unit geLogics;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, glLogics,
  glSBox, StdCtrls, glGrBox, ExtCtrls, ComCtrls, dsgnintf, ToolWin, ImgList, richEdit,
  glPage, Tabs;

type
  TglGroupBoxPlus = class;

  TglLogicsComponentEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  private
    procedure ShowEditor(LogicProducer: TLogicProducer);
  end;

  TglLogicsEditor = class(TForm)
    SB: TglScrollBox;
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
    PC: TglPageControl;
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
    FActiveBox: TglGroupBoxPlus;
    LogicProducer: TLogicProducer;
    Logics: TLogics;

    procedure MouseDown_(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove_(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp_(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DblClick_(Sender: TObject);
    procedure SetActiveBox(const Value: TglGroupBoxPlus);
    procedure UpdateView;
    procedure AddBox(LogicElement_: TLogicElement);
    procedure AddShape(CommentArea: TCommentArea);

     procedure OnTraceMessage(Sender: TLogics; fStepResult: boolean; const StepResult, ParsedResult, Msg: string);
  public
    function Execute(LogicProducer: TLogicProducer): boolean;
    property ActiveBox: TglGroupBoxPlus read FActiveBox write SetActiveBox;
  end;

  TglGroupBoxPlus = class(TglGroupBox)
  public
    pt: TPoint;
    Selected: boolean;
    LogicElement: TLogicElement;
//    fAsLogical: boolean;
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
    procedure Paint; override;
  end;

  TglShapePlus = class(TShape)
  public
    pt: TPoint;
    Selected: boolean;
    CommentArea: TCommentArea;
    procedure Paint; override;
  end;

var
  glLogicsEditor: TglLogicsEditor;

implementation
uses glTypes, glUtils, geLogicItemEditor, clipbrd;
{$R *.DFM}

{ TglLogicsEditor }

function TglLogicsEditor.Execute(LogicProducer: TLogicProducer): boolean;
var
  i: integer;
begin
  fLogicItemEditor := TfLogicItemEditor.Create(nil);

  mDictionary.Lines.Assign(LogicProducer.Dictionary);
  try

    self.LogicProducer := LogicProducer;
    self.Logics := LogicProducer.Logics;

    Logics.OnTraceMessage := OnTraceMessage;

    cbNext.Items.Clear;
    cbNextFalse.Items.Clear;
    cbNext.Items.Add('');
    cbNextFalse.Items.Add('');

    for i := 0 to Logics.Count-1 do AddBox(Logics[i]);

    for i := 0 to LogicProducer.CommentAreas.Count-1 do AddShape(LogicProducer.CommentAreas[i]);

    Result := ShowModal = mrOK;
  finally
    fLogicItemEditor.Free;
  end;

//  pLog.Dock(pLeft, rect(1,1,10,10));
//  pLog.Dock(pLeft, rect(1,1,10,10));
//  pLeft.Dock(pLog, rect(1,1,1,1));
end;

procedure TglLogicsEditor.AddBox(LogicElement_: TLogicElement);
var
  Box: TglGroupBoxPlus;
begin
  Box := TglGroupBoxPlus.Create(self);
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

procedure TglLogicsEditor.AddShape(CommentArea: TCommentArea);
var
  Shape: TglShapePlus;
begin
  Shape := TglShapePlus.Create(self);
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

procedure TglLogicsEditor.MouseDown_(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  if Sender is TglShapePlus then with Sender as TglShapePlus do
  begin
    pt.X := X; pt.Y := Y;
    pt := ClientToScreen(pt);
    Selected := true;
    Tag := 1;
    if (X>=Width-5)and(X<Width)and(Y>=Height-5)and(Y<Height) then Tag := 2;
    exit;
  end;

  with TglGroupBoxPlus(Sender) do
  begin
    pt.X := X; pt.Y := Y;
    pt := ClientToScreen(pt);
    pt.Y := pt.Y;// + SB.VertScrollBar.ScrollPos;
    Tag := 1;
    Options := Options + [fgoDelineatedText];
    Colors.Caption := clBtnHighlight;
    Colors.TextActive := clBlack;
    Font.Style := [fsBold];
    Selected := true;
    ActiveBox := TglGroupBoxPlus(Sender);
  end;

  for i:=0 to SB.ControlCount-1 do
    if (SB.Controls[i] is TglGroupBoxPlus) then with TglGroupBoxPlus(SB.Controls[i]) do
    begin
      if ActiveBox = TglGroupBoxPlus(SB.Controls[i]) then continue;
      Options := Options - [fgoDelineatedText];
      Colors.Caption := clBtnShadow;
      Colors.TextActive := clBtnHighlight;
      Font.Style := [];
      Selected := false;
      Repaint;
    end;
end;

procedure TglLogicsEditor.MouseMove_(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  pt_new: TPoint;
begin
  if Sender is TglShapePlus then with Sender as TglShapePlus do
  begin
    case Tag of
    1:
    begin
      pt_new.X := X; pt_new.Y := Y;
      pt_new := ClientToScreen(pt_new);
      Left := Left + pt_new.X - pt.X;
      Top := Top + pt_new.Y - pt.Y;
      CommentArea.Left := Left + SB.HorzScrollBar.ScrollPos;
      CommentArea.Top := Top + SB.VertScrollBar.ScrollPos;
    end;
    2:
    begin
      pt_new.X := X; pt_new.Y := Y;
      pt_new := ClientToScreen(pt_new);
      Width := Width + pt_new.X - pt.X;
      Height := Height + pt_new.Y - pt.Y;
      if Width < 50 then Width := 50; if Height < 50 then Height := 50;
      CommentArea.Width := Width;
      CommentArea.Height := Height;
    end;
    end;
    pt.X := pt_new.X; pt.Y := pt_new.Y;
    exit;
  end;

  with TglGroupBoxPlus(Sender) do
  begin
    if bool(Tag) then
    begin
      pt_new.X := X; pt_new.Y := Y;
      pt_new := ClientToScreen(pt_new);
pt_new.Y := pt_new.Y;// + SB.VertScrollBar.ScrollPos;
      Left := Left + pt_new.X - pt.X;
      Top := Top + pt_new.Y - pt.Y;

      LogicElement.Left := Left + SB.HorzScrollBar.ScrollPos;
      LogicElement.Top := Top + SB.VertScrollBar.ScrollPos;

      SBar.SimpleText := IntToStr(Left) + ':' + IntToStr(Top);

      UpdateView;
    end;
    pt.X := pt_new.X; pt.Y := pt_new.Y;
  end;
end;

procedure TglLogicsEditor.UpdateView;
var
  DC: HDC;
begin
  DC := GetDC(SB.Handle);
  SendMessage(SB.Handle, WM_EraseBkgnd, WPARAM(DC),0);
  ReleaseDC(SB.Handle, DC);
end;

procedure TglLogicsEditor.MouseUp_(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  TControl(Sender).Tag := 0;
  for i := 0 to SB.ControlCount-1 do
    if (SB.Controls[i] is TglShapePlus) then (SB.Controls[i] as TglShapePlus).Paint;
end;

procedure TglLogicsEditor.DblClick_(Sender: TObject);
var str: string;
begin
  TControl(Sender).Tag := 0;
  if Sender is TglShapePlus then with Sender as TglShapePlus do
  begin
    str := CommentArea.Text;
    if InputQuery('Caption', 'Comments', str) then CommentArea.Text := str;
    PostMessage(TWinControl(Parent).Handle, WM_LBUTTONUP, 1, 1);
    exit;
  end;

  fLogicItemEditor.Execute(Logics, TglGroupBoxPlus(Sender).LogicElement);
  PostMessage(TWinControl(Sender).Handle, WM_LBUTTONUP, 1, 1);
end;


procedure TglLogicsEditor.SetActiveBox(const Value: TglGroupBoxPlus);
var
  i, Index: integer;
  Box: TglGroupBoxPlus;
begin
  FActiveBox := Value;

  Index := cbNext.Items.IndexOfObject(Value.LogicElement.NextElement);
  if Index <> -1 then cbNext.ItemIndex := Index else cbNext.ItemIndex := 0;

  Index := cbNextFalse.Items.IndexOfObject(Value.LogicElement.NextFalseElement);
  if Index <> -1 then cbNextFalse.ItemIndex := Index else cbNextFalse.ItemIndex := 0;

  cbMode.ItemIndex := integer(FActiveBox.LogicElement.IsFirst);
  eStepName.Text := FActiveBox.LogicElement.Caption;
end;

procedure TglLogicsEditor.OnTraceMessage(Sender: TLogics; fStepResult: boolean; const StepResult, ParsedResult, Msg: string);
begin
  mLog.Lines.Add(Msg);
  if reReslt.Text = '' then reReslt.Tag := 0;

  if length(ParsedResult) = 0 then exit;
  tag := 1 - tag;

//  reReslt.Lines.BeginUpdate;

  reReslt.Text := reReslt.Text + ParsedResult;

  reReslt.SelStart := length(reReslt.Text) - length(ParsedResult);
  reReslt.SelLength := length(ParsedResult);
  if tag = 0 then reReslt.SelAttributes.Color := clRed else reReslt.SelAttributes.Color := clGreen;
//  reReslt.SelAttributes.Color := RGB(100+Random(100), 100+Random(100), 100+Random(100));

  reReslt.SelLength := 0;
//  reReslt.Lines.EndUpdate;
end;

{ TglGroupBoxPlus }

procedure TglGroupBoxPlus.Paint;
var
  i: integer;
  R: TRect;
  str: string;
begin
  inherited;
//  ChangeBitmapColor((Owner as TglLogicsEditor).iLink.Picture.Bitmap, GetPixel((Owner as TglLogicsEditor).iLink.Picture.Bitmap.Canvas.Handle, 0,0), IIF(LogicElement.NextElement<>nil, clGreen, clRed));
//  BitBlt(Canvas.handle, 100-14-3, 3, 14, 13, (Owner as TglLogicsEditor).iLink.Picture.Bitmap.Canvas.Handle, 0, 0, SRCCOPY);

  Canvas.Font.Color := clTeal;//clBlue;
  Canvas.Font.Style := [];
  str := LogicElement.Expression + ' ' + LogicRuleLabels[LogicElement.Rule] + ' ' + LogicElement.Value;
  R := Bounds(3, 20, Width-6, Height-22);
  DrawText(Canvas.handle, PChar(str), length(str), R, DT_WORDBREAK or DT_END_ELLIPSIS or DT_MODIFYSTRING);
end;

procedure TglLogicsEditor.SBEraseBkgndEvent(Sender: TObject; DC: HDC);
var
  Canvas: TCanvas;
  LogicElement: TLogicElement;
  i: integer;
  PenFalse, Pen, PenTrue, OldPen, PenGrid: HPen;
  Brush, OldBrush: HBrush;
  NextBox, PrevBox, PrevFalseBox: TglGroupBoxPlus;
  bmp: TBitmap;

  function FindBox(LogicElement: TLogicElement): TglGroupBoxPlus;
  var
    i: integer;
  begin
    Result := nil;
    if LogicElement = nil then exit;
    for i := 0 to SB.ControlCount-1 do
      if SB.Controls[i] is TglGroupBoxPlus then
      if TglGroupBoxPlus(SB.Controls[i]).LogicElement = LogicElement then Result := TglGroupBoxPlus(SB.Controls[i]);
  end;
  procedure Line(X, Y, X2, Y2: integer; isTrueLine: boolean);
  const R = 5;
  begin
    if isTrueLine then SelectObject(DC, PenTrue) else SelectObject(DC, PenFalse);
    MoveToEx(DC, X, Y, nil); LineTo(DC, X2, Y2);
    SelectObject(DC, Pen);
    if (abs(X2-X) < 500)and(abs(Y2-Y) < 500) then
    MoveToEx(DC, X, Y+1, nil); LineTo(DC, X2, Y2+1);
    Ellipse(DC, X-R-2, Y-R-2, X+R*2-2, Y+R*2-2);
    Ellipse(DC, X2-R-2, Y2-R-2, X2+R*2-2, Y2+R*2-2);
  end;
  procedure DrawGrid;
  var i, j: integer;
  const step = 14;
  begin
    FillRect(DC, SB.ClientRect, Brush);
    for i:=1 to SB.Width div step do
    begin
      j := i*14;
      MoveToEx(DC, j, 0, nil); LineTo(DC, j, SB.Height);
    end;
    for i:=1 to SB.Height div step do
    begin
      j := i*14;
      MoveToEx(DC, 0, j, nil); LineTo(DC, SB.Width, j);
    end;
  end;
begin
  try
    Brush := CreateSolidBrush(clWhite);
    Pen := CreatePen( PS_SOLID, 1, clBlack );
    PenGrid := CreatePen( PS_SOLID, 1, $E0E0E0 );
    //PenLong := CreatePen( PS_DASHDOT, 1, $E0E0E0 );
    PenTrue := CreatePen( PS_SOLID, 1, $FF9090 );
    PenFalse := CreatePen( PS_SOLID, 1, $009090 );
    OldPen := SelectObject( DC, PenGrid );
    OldBrush := SelectObject( DC, Brush );

    DrawGrid;

    for i := 0 to SB.ControlCount-1 do
    begin
      if not(SB.Controls[i] is TglGroupBoxPlus) then continue;
      LogicElement := TglGroupBoxPlus(SB.Controls[i]).LogicElement;
      if LogicElement = nil then exit;

        if LogicElement.IsFirst then
        begin
          MoveToEx(DC, 0, 0, nil);
          LineTo(DC, SB.Controls[i].Left, SB.Controls[i].Top);
        end;

        PrevBox := FindBox(LogicElement.NextElement);
        if Assigned(PrevBox) then
        begin
          Line(SB.Controls[i].Left + SB.Controls[i].Width, SB.Controls[i].Top, PrevBox.Left, PrevBox.Top, true);
          DeleteObject( SelectObject( DC, OldPen ) );
        end;

        PrevFalseBox := FindBox(LogicElement.NextFalseElement);
        if Assigned(PrevFalseBox) then
        begin
          Line(SB.Controls[i].Left + SB.Controls[i].Width, SB.Controls[i].Top + SB.Controls[i].Height, PrevFalseBox.Left, PrevFalseBox.Top, false);
          DeleteObject( SelectObject( DC, OldPen ) );
        end;

    bmp := TBitmap.Create;
    if LogicElement.NextElement <> nil then
    begin
      ImageList.GetBitmap(0, bmp);
      BitBlt(DC, SB.Controls[i].Left + SB.Controls[i].Width-3, SB.Controls[i].Top, bmp.width, bmp.height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;

    if LogicElement.NextFalseElement <> nil then
    begin
      ImageList.GetBitmap(1, bmp);
      BitBlt(DC, SB.Controls[i].Left + SB.Controls[i].Width-3, SB.Controls[i].Top + SB.Controls[i].Height-17, bmp.width, bmp.height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;

    if LogicElement.IsFirst then
    begin
      ImageList.GetBitmap(2, bmp);
      BitBlt(DC, SB.Controls[i].Left - 20, SB.Controls[i].Top, bmp.width, bmp.height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
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
//    if (SB.Controls[i] is TglShapePlus) then (SB.Controls[i] as TglShapePlus).Paint;

end;

procedure TglLogicsEditor.cbNextChange(Sender: TObject);
begin
  if FActiveBox = nil then exit;
  if FActiveBox.LogicElement <> cbNext.items.Objects[cbNext.ItemIndex] then
  begin
    FActiveBox.LogicElement.NextElement := TlogicElement(cbNext.items.Objects[cbNext.ItemIndex]);
  end;

  UpdateView;
end;

procedure TglLogicsEditor.cbModeChange(Sender: TObject);
begin
  if FActiveBox = nil then exit;
  FActiveBox.LogicElement.IsFirst := cbMode.ItemIndex = 1;
end;

{ TglLogicsComponentEditor }

procedure TglLogicsComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  ShowEditor(TLogicProducer(Component));
end;

function TglLogicsComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit component...';
  end;
end;

function TglLogicsComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TglLogicsComponentEditor.ShowEditor(LogicProducer: TLogicProducer);
var
  glLogicsEditor: TglLogicsEditor;
  Logics: TLogics;
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
    glLogicsEditor := TglLogicsEditor.Create(nil);
    glLogicsEditor.Execute(LogicProducer);
  finally
    FreeAndNil(glLogicsEditor);
  end;
end;


procedure TglLogicsEditor.tbNewClick(Sender: TObject);
var
  LogicElement: TLogicElement;
begin
  LogicElement := Logics.Add;
  with LogicElement do
  begin
    Left := SB.Width div 2; Top := SB.Height div 2;
    AddBox(LogicElement);
  end;
end;

procedure TglLogicsEditor.cbNextFalseChange(Sender: TObject);
begin
  if FActiveBox = nil then exit;
  if FActiveBox.LogicElement <> cbNextFalse.items.Objects[cbNextFalse.ItemIndex] then
  begin
    FActiveBox.LogicElement.NextFalseElement := TlogicElement(cbNextFalse.items.Objects[cbNextFalse.ItemIndex]);
  end;
  UpdateView;
end;

procedure TglLogicsEditor.eStepNameChange(Sender: TObject);
begin
  if not Assigned(ActiveBox) then exit;
  ActiveBox.LogicElement.Caption := eStepName.Text;
  ActiveBox.Caption := eStepName.Text;
end;

procedure TglLogicsEditor.FormShow(Sender: TObject);
begin
  SB.BufferedDraw := true;
end;

procedure TglLogicsEditor.ToolButton5Click(Sender: TObject);
var i: integer;
begin
  pLog.Visible := true;
  
  mLog.Lines.Clear;
  reReslt.Text := '';

  Logics.Analyze;

  for i := 0 to SB.ControlCount-1 do
    if (SB.Controls[i] is TglGroupBoxPlus) then
      if TglGroupBoxPlus(SB.Controls[i]).LogicElement.IsTrue then TglGroupBoxPlus(SB.Controls[i]).Colors.Caption := clGreen;

end;

procedure TglLogicsEditor.ToolButton7Click(Sender: TObject);
var
  CommentArea: TCommentArea;
begin
  CommentArea := LogicProducer.CommentAreas.Add;
  with CommentArea do
  begin
    Left := SB.Width div 2; Top := SB.Height div 2;
    Width := 100; Height := 100;
    AddShape(CommentArea);
  end;
end;

{ TglShapePlus }

procedure TglShapePlus.Paint;
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

procedure TglLogicsEditor.cbIgnoreSpacesClick(Sender: TObject);
begin
  LogicProducer.IgnoreSpaces := cbIgnoreSpaces.Checked;
end;

procedure TglLogicsEditor.pLeftDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := true;
end;

procedure TglLogicsEditor.pLeftUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  (Sender as TWinControl).Height := 6;
end;

procedure TglLogicsEditor.pLeftDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  (Sender as TWinControl).Height := 100;
  SBar.Top := 1500;
end;

procedure TglLogicsEditor.ToolButton8Click(Sender: TObject);
var i: integer;
begin

  if Logics.TraceItem = nil then
  begin
    Logics.StartAnalyze;
    reReslt.Text := '';
  end;
  tbStop.Enabled :=true;

  Logics.AnalyzeStep;

  for i := 0 to SB.ControlCount-1 do
    if (SB.Controls[i] is TglGroupBoxPlus) then
      if TglGroupBoxPlus(SB.Controls[i]).LogicElement.IsTrue then TglGroupBoxPlus(SB.Controls[i]).Colors.Caption := clGreen;

//  ShowMessage(Logics.Result);

end;

procedure TglLogicsEditor.tbStopClick(Sender: TObject);
var i: integer;
begin
  Logics.TraceItem := nil;
  tbStop.Enabled :=false;

  for i := 0 to SB.ControlCount-1 do
    if (SB.Controls[i] is TglGroupBoxPlus) then
      if TglGroupBoxPlus(SB.Controls[i]).LogicElement.IsTrue then TglGroupBoxPlus(SB.Controls[i]).Colors.Caption := clBtnShadow;
end;

procedure TglLogicsEditor.TabSet1Change(Sender: TObject; NewTab: Integer; var AllowChange: Boolean);
begin
  PC.ActivePageIndex := NewTab;
end;

end.
