{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit TimelineMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Menus, ImgList, JvComponent, JvTimeLine,
  JvExControls;

type
  TTimelineMainForm = class(TForm)
    ImageList1: TImageList;
    Splitter1: TSplitter;
    PopupMenu1: TPopupMenu;
    Changecaption1: TMenuItem;
    remove1: TMenuItem;
    Move1: TMenuItem;
    N1: TMenuItem;
    ImageList2: TImageList;
    StatusBar1: TStatusBar;
    Notes1: TMenuItem;
    N2: TMenuItem;
    Up1: TMenuItem;
    Down1: TMenuItem;
    TimeLine1: TJvTimeLine;
    Panel2: TPanel;
    Label6: TLabel;
    Label8: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnAdd: TButton;
    edCaption: TEdit;
    edImIndex: TEdit;
    dtpItemdate: TDateTimePicker;
    udImIndex: TUpDown;
    edLevel: TEdit;
    udLevel: TUpDown;
    btnColor: TButton;
    GroupBox2: TGroupBox;
    chkMonths: TCheckBox;
    chkMulti: TCheckBox;
    chkNoImages: TCheckBox;
    chkWidthAs: TCheckBox;
    chkAutosize: TCheckBox;
    chkSupport: TCheckBox;
    chkLarge: TCheckBox;
    chkOwnerDraw: TCheckBox;
    btnAuto: TButton;
    chkReset: TCheckBox;
    chkComplete: TCheckBox;
    chkFlat: TCheckBox;
    chkHelpYear: TCheckBox;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    btnYrFont: TButton;
    btnFont: TButton;
    edYrSize: TEdit;
    udYrSize: TUpDown;
    edItemHeight: TEdit;
    udItemHeight: TUpDown;
    dtpFirstDate: TDateTimePicker;
    btnSave: TButton;
    btnLoad: TButton;
    ColorBtn: TButton;
    Disable1: TMenuItem;
    cbDragging: TComboBox;
    procedure btnAddClick(Sender: TObject);
    procedure chkMonthsClick(Sender: TObject);
    procedure chkMultiClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnYrFontClick(Sender: TObject);
    procedure TimeLine1Click(Sender: TObject);
    procedure Changecaption1Click(Sender: TObject);
    procedure remove1Click(Sender: TObject);
    procedure Move1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure chkNoImagesClick(Sender: TObject);
    procedure chkWidthAsClick(Sender: TObject);
    procedure chkAutosizeClick(Sender: TObject);
    procedure chkSupportClick(Sender: TObject);
    procedure chkLargeClick(Sender: TObject);
    procedure udYrSizeClick(Sender: TObject; Button: TUDBtnType);
    procedure TimeLine1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure udItemHeightClick(Sender: TObject; Button: TUDBtnType);
    procedure chkOwnerDrawClick(Sender: TObject);
    procedure Notes1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimeLine1SaveItem(Sender: TObject; Item: TJvTimeItem; Stream: TStream);
    procedure TimeLine1LoadItem(Sender: TObject; Item: TJvTimeItem; Stream: TStream);
    procedure TimeLine1DrawItem(Sender: TObject; Canvas: TCanvas;
      Item: TJvTimeItem; var R: TRect);
    procedure dtpFirstDateChange(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
    procedure Up1Click(Sender: TObject);
    procedure Down1Click(Sender: TObject);
    procedure btnAutoClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chkFlatClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure chkHelpYearClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
    procedure Disable1Click(Sender: TObject);
    procedure TimeLine1ItemClick(Sender: TObject; Item: TJvTimeItem);
    procedure TimeLine1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TimeLine1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimeLine1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cbDraggingChange(Sender: TObject);
    procedure TimeLine1ItemMoved(Sender: TObject; Item: TJvTimeItem;
      var NewStartDate: TDateTime; var NewLevel: Integer);
  private
    { Private declarations }
    FCurColor: TColor;
  public
    { Public declarations }
  end;

var
  TimelineMainForm: TTimelineMainForm;

implementation

uses TimelineNotesFormU;

{$R *.DFM}

{ a record to save notes data in }
type
  PItemData = ^TItemData;
  TItemData = record
    Text: string;
  end;

procedure TTimelineMainForm.btnAddClick(Sender: TObject);
var aItem: TJvTimeItem;
begin
  aItem := TimeLine1.Items.Add;
  aItem.Caption := edCaption.Text;
  aItem.ImageIndex := StrToIntDef(edImIndex.Text, -1);
  aItem.Level := StrToIntDef(edLevel.Text, 0);
  aItem.Date := dtpItemDate.Date;
  aItem.Color := FCurColor;
  if FCurColor <> clWhite then
    aItem.TextColor := clWhite
  else
    aItem.TextColor := clBlack;
end;

procedure TTimelineMainForm.chkMonthsClick(Sender: TObject);
begin
  TimeLine1.ShowMonthNames := chkMonths.Checked;
end;

procedure TTimelineMainForm.chkMultiClick(Sender: TObject);
begin
  TimeLine1.MultiSelect := chkMulti.Checked;
end;

procedure TTimelineMainForm.btnFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  begin
    Font := TimeLine1.Font;
    if Execute then
      TimeLine1.Font := Font;
    Free;
  end;
end;

procedure TTimelineMainForm.btnYrFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  begin
    Font := TimeLine1.YearFont;
    if Execute then
      TimeLine1.YearFont := Font;
    Free;
  end;
end;

procedure TTimelineMainForm.TimeLine1Click(Sender: TObject);
begin
  if TimeLine1.Selected <> nil then
    Caption := Format('%s (%s)', [TimeLine1.Selected.Caption, DateToStr(TimeLine1.Selected.Date)]);
end;

procedure TTimelineMainForm.Changecaption1Click(Sender: TObject);
var S: string;
begin
  if TimeLine1.Selected <> nil then
  begin
    S := TimeLine1.Selected.Caption;
    if InputQuery('Change caption', 'Change caption to:', S) then
      TimeLine1.Selected.Caption := S;
  end;
end;

procedure TTimelineMainForm.remove1Click(Sender: TObject);
begin
  if TimeLine1.Selected <> nil then
    if MessageDlg('Sure you want to delete this item?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      if TimeLine1.Selected.Data <> nil then
        Dispose(PItemData(TimeLine1.Selected.Data));
      TimeLine1.Selected.Remove;
    end;
end;

procedure TTimelineMainForm.Move1Click(Sender: TObject);
var S: string;
begin
  if TimeLine1.Selected <> nil then
  begin
    S := DateToStr(TimeLine1.Selected.Date);
    if InputQuery('Move item', 'Move to new date:', S) then
      TimeLine1.Selected.Date := StrToDate(S);
  end;
end;

procedure TTimelineMainForm.PopupMenu1Popup(Sender: TObject);
const
  aCaption: array[boolean] of string = ('E&nable', 'D&isable');
var i: integer;
begin
  if TimeLine1.Selected = nil then
    for i := 0 to PopUpMenu1.Items.Count - 1 do
      PopUpMenu1.Items[i].Enabled := false
  else
  begin
    for i := 0 to PopUpMenu1.Items.Count - 1 do
      PopUpMenu1.Items[i].Enabled := true;
    Disable1.Caption := aCaption[TimeLine1.Selected.Enabled];
  end

end;

procedure TTimelineMainForm.chkNoImagesClick(Sender: TObject);
begin
  if chkNoImages.Checked then
  begin
    TimeLine1.Images := nil;
    TimeLine1.ItemHeight := 16;
    udItemHeight.Position := 16;
  end
  else
    chkLargeClick(nil);
end;

procedure TTimelineMainForm.chkWidthAsClick(Sender: TObject);
const
  aType: array[boolean] of TJvTimeItemType = (asPixels, asDays);
var i: integer;
begin
  for i := 0 to TimeLine1.Items.Count - 1 do
    TimeLine1.Items[i].WidthAs := aType[chkWidthAs.Checked];
end;

procedure TTimelineMainForm.chkAutosizeClick(Sender: TObject);
begin
  TimeLine1.AutoSize := chkAutosize.Checked;
end;

procedure TTimelineMainForm.chkSupportClick(Sender: TObject);
begin
  TimeLine1.VertSupports := chkSupport.Checked;
end;

procedure TTimelineMainForm.chkLargeClick(Sender: TObject);
begin
  if chkNoImages.Checked then
    Exit;
  if chkLarge.Checked then
  begin
    TimeLine1.Images := ImageList2;
    TimeLine1.ItemHeight := 50;
    udItemHeight.Position := 50;
  end
  else
  begin
    TimeLine1.Images := ImageList1;
    TimeLine1.ItemHeight := 36;
    udItemHeight.Position := 36;
  end;
end;

procedure TTimelineMainForm.udYrSizeClick(Sender: TObject; Button: TUDBtnType);
begin
  TimeLine1.YearWidth := udYrSize.Position;
end;

procedure TTimelineMainForm.TimeLine1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  StatusBar1.Panels[0].Text := DateToStr(TimeLine1.DateAtPos(X)) + ' (approx.)';
end;

procedure TTimelineMainForm.udItemHeightClick(Sender: TObject; Button: TUDBtnType);
begin
  TimeLine1.ItemHeight := udItemHeight.Position;
end;

procedure TTimelineMainForm.chkOwnerDrawClick(Sender: TObject);
begin
  if chkOwnerDraw.Checked then
  begin
    chkAutoSize.Checked := false;
    TimeLine1.Style := tlOwnerDrawFixed;
  end
  else
    TimeLine1.Style := tlDefault;
end;

{ Show a memo to write notes about this item in. Allocate memory even if nothing is
written ( makes it easier when we want to save it later)  }

procedure TTimelineMainForm.Notes1Click(Sender: TObject);
var aData: PItemData; P: TPoint;
begin
  if TimeLine1.Selected <> nil then
  begin
    if TimeLine1.Selected.Data = nil then
    begin
      New(aData);
      aData^.Text := '';
    end
    else
      aData := TimeLine1.Selected.Data;
    TimelineNotesForm.Caption := Format('Notes for "%s":', [TimeLine1.Selected.Caption]);
    TimelineNotesForm.Memo1.Text := aData^.Text;
    GetCursorPos(P);
    TimelineNotesForm.Left := P.x - 12;
    TimelineNotesForm.Top := P.y - 32;
    TimelineNotesForm.ShowModal;
    aData^.Text := TimelineNotesForm.Memo1.Text;
    TimeLine1.Selected.Data := aData;
  end;
end;

{ save all notes data and dispose of memory }

procedure TTimelineMainForm.FormCreate(Sender: TObject);
begin
  FCurColor := TimeLine1.Color;
  cbDragging.ItemIndex := 0;
  TimelineNotesForm := TTimelineNotesForm.Create(nil);
  cbDraggingChange(nil);
  TimeLine1.ShowSelection := false;
  TimeLine1.DoubleBuffered := false;
end;


procedure TTimelineMainForm.TimeLine1SaveItem(Sender: TObject; Item: TJvTimeItem;
  Stream: TStream);
var S: string;
begin
  { I use #27 (ESC) as terminator as it doesn't appear naturally in text. #13 can't be
    used if we want to use it in the text (and we do). Also, when nothing is written in the memo,
    starting off a text with 3 @'s is highly unlikely especially if that's all that's there,
    so I use it to tell when no notes have been saved in this item }
  S := '';
  if (Item.Data <> nil) then
    S := PItemData(Item.Data)^.Text;
  if Length(S) = 0 then
    S := '@@@' + #27
  else
    S := S + #27;
  Stream.Write(S[1], Length(S));
end;

{ let's read our previously saved data from the file }

procedure TTimelineMainForm.TimeLine1LoadItem(Sender: TObject; Item: TJvTimeItem;
  Stream: TStream);
var S: string; aData: PItemData; ch: char;
begin
  Stream.Read(ch, 1);
  while ch <> #27 do
  begin
    S := S + ch;
    Stream.Read(ch, 1);
  end;

  if (S <> '@@@') then { nothing there }
  begin
    New(aData);
    aData^.Text := S;
    Item.Data := aData;
  end;
end;

function strrev(p: string): string;
var pend, pstart: integer;
begin
  Result := p;
  pend := Length(p);
  pstart := 1;
  while (pend >= 1) do
  begin
    p[pstart] := char(integer(p[pstart]) xor integer(Result[pend]));
    Result[pend] := char(integer(Result[pend]) xor integer(p[pstart]));
    Inc(pstart);
    Dec(pend);
  end;
end;

{ let's draw something funny ourselves }

procedure TTimelineMainForm.TimeLine1DrawItem(Sender: TObject; Canvas: TCanvas;
  Item: TJvTimeItem; var R: TRect);
var S: string;
begin
  //  Canvas.Brush.Color := clBlack;
  //  Canvas.FrameRect(R);

  //  Canvas.Brush.Color := RGB(Random(255),Random(255),Random(255));
  Canvas.FillRect(R);
  if TimeLine1.Images <> nil then
  begin
    TimeLine1.Images.Draw(Canvas, R.Left, R.Top, Item.ImageIndex);
    {
        if Item.Selected then
          TimeLine1.Images.Draw(Canvas,R.Left,R.Top,Random(TimeLine1.Images.Count))
        else
          TimeLine1.Images.Draw(Canvas,R.Right - TimeLine1.Images.Width,R.Top,Item.ImageIndex);
        }
  end;

  //  if (Random > 0.5) and Item.Selected then
  //    S := strrev(Item.Caption)
  //  else
  S := Item.Caption;
  //  Canvas.Font.Color := Canvas.Brush.Color xor clWhite;
  DrawText(Canvas.Handle, PChar(' ' + S), -1, R, DT_LEFT or DT_BOTTOM or DT_SINGLELINE);
end;

procedure TTimelineMainForm.dtpFirstDateChange(Sender: TObject);
begin
  TimeLine1.FirstVisibleDate := dtpFirstDate.Date;
  dtpFirstDate.Date := TimeLine1.FirstVisibleDate;
end;

procedure TTimelineMainForm.btnColorClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do
  begin
    Color := FCurColor;
    if Execute then
      FCurColor := Color;
    Free;
  end;
end;

procedure TTimelineMainForm.Up1Click(Sender: TObject);
begin
  if (TimeLine1.Selected <> nil) and (TimeLine1.Selected.Level > 0) then
  begin
    TimeLine1.Selected.Level := TimeLine1.Selected.Level - 1;
  end
  else if TimeLine1.Selected <> nil then
    ShowMessage('Can''t move this item further up!');
end;

procedure TTimelineMainForm.Down1Click(Sender: TObject);
begin
  TimeLine1.Selected.Level := TimeLine1.Selected.Level + 1;
end;

procedure TTimelineMainForm.btnAutoClick(Sender: TObject);
begin
  TimeLine1.BeginUpdate;
  TimeLine1.AutoLevels(chkComplete.Checked, chkReset.Checked);
  TimeLine1.EndUpdate;
end;

procedure TTimelineMainForm.btnLoadClick(Sender: TObject);
var i: integer;
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter := 'Item files (*.itm) |*.itm| All files (*.*)|*.*';
    InitialDir := ExtractFilePath(Application.Exename);
    if Execute then
    begin
      TimeLine1.BeginUpdate;
      for i := 0 to TimeLine1.Items.Count - 1 do
        if TimeLine1.Items[i].Data <> nil then
        begin
          Dispose(PItemData(TimeLine1.Items[i].Data));
          TimeLine1.Items[i].Data := nil;
        end;
      TimeLine1.Items.Clear;
      TimeLine1.LoadFromFile(Filename);
      TimeLine1.EndUpdate;
    end;
    Free;
  end;
end;

procedure TTimelineMainForm.btnSaveClick(Sender: TObject);
begin
  with TSaveDialog.Create(self) do
  begin
    Filter := 'Item files (*.itm) |*.itm| All files (*.*)|*.*';
    InitialDir := ExtractFilePath(Application.Exename);
    if Execute then
    begin
      TimeLine1.BeginUpdate;
      TimeLine1.SaveToFile(Filename);
      TimeLine1.EndUpdate;
    end;
    Free;
  end;
end;

procedure TTimelineMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  TimelineNotesForm.free;
  { free allocated memory }
  for i := 0 to TimeLine1.Items.Count - 1 do
    if TimeLine1.Items[i].Data <> nil then
    begin
      Dispose(PItemData(TimeLine1.Items[i].Data));
      TimeLine1.Items[i].Data := nil;
    end;
end;

procedure TTimelineMainForm.chkFlatClick(Sender: TObject);
begin
  TimeLine1.Flat := chkFlat.Checked;
end;

procedure TTimelineMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TTimelineMainForm.chkHelpYearClick(Sender: TObject);
begin
  TimeLine1.HelperYears := chkHelpYear.Checked;
end;

procedure TTimelineMainForm.FormResize(Sender: TObject);
begin
  TimeLine1.Height := Height div 2;
  if Panel2.Height < 250 then
    TimeLine1.Height := Height - 300;

end;

procedure TTimelineMainForm.ColorBtnClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do
  begin
    Color := TimeLine1.Color;
    if Execute then
      TimeLine1.Color := Color;
    Free;
  end;
end;

procedure TTimelineMainForm.Disable1Click(Sender: TObject);
begin
  if TimeLine1.Selected <> nil then
    TimeLine1.Selected.Enabled := not TimeLine1.Selected.Enabled;
end;

procedure TTimelineMainForm.TimeLine1ItemClick(Sender: TObject; Item: TJvTimeItem);
begin
  Caption := Item.Caption;
end;

procedure TTimelineMainForm.TimeLine1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = Source;
end;

procedure TTimelineMainForm.TimeLine1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (cbDragging.ItemIndex = 1) then
    TimeLine1.BeginDrag(Mouse.DragImmediate, Mouse.DragThreshold);
end;

procedure TTimelineMainForm.TimeLine1DragDrop(Sender, Source: TObject; X, Y: Integer);
var S: string;
begin
  if (Sender = Source) and (TimeLine1.Selected <> nil) then
  begin
    S := DateToStr(TimeLine1.DateAtPos(X));
    if InputQuery('Confirm move', Format('Move "%s" to new date:', [TimeLine1.Selected.Caption]), S) then
    begin
      TimeLine1.Selected.Date := StrToDate(S);
      TimeLine1.Selected.Level := TimeLine1.LevelAtPos(Y);
    end;
  end;
end;

procedure TTimelineMainForm.cbDraggingChange(Sender: TObject);
begin
  TimeLine1.DragLine := false;
  case cbDragging.ItemIndex of
    0,1:
      TimeLine1.DragMode := dmManual;
    2:
      TimeLine1.DragMode := dmAutomatic;
  end;
end;

procedure TTimelineMainForm.TimeLine1ItemMoved(Sender: TObject; Item: TJvTimeItem;
  var NewStartDate: TDateTime; var NewLevel: Integer);
var S: string;
begin
  if TimeLine1.Dragging then
    Exit;
  S := DateToStr(NewStartDate);
  if not InputQuery('Confirm move', Format('Move "%s" to new date:', [Item.Caption]), S) then
  begin
    NewStartDate := Item.Date;
    NewLevel := Item.Level;
  end;
end;

end.

