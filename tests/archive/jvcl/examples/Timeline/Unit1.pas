unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,JvTimeLine,
  ComCtrls, StdCtrls, ExtCtrls, Menus, ImgList, JvComponent;
type
  TForm1 = class(TForm)
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
    procedure TimeLine1SaveItem(Sender: TObject; Item: TJvTimeItem;
      Stream: TStream);
    procedure TimeLine1LoadItem(Sender: TObject; Item: TJvTimeItem;
      Stream: TStream);
    procedure TimeLine1DrawItem(Sender: TObject; Canvas: TCanvas;
      Item: TJvTimeItem; var R: TRect);
    procedure dtpFirstDateChange(Sender: TObject);
    procedure TimeLine1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimeLine1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TimeLine1DragDrop(Sender, Source: TObject; X, Y: Integer);
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
    procedure TimeLine1DblClick(Sender: TObject);
  private
    { Private declarations }
    FCurColor:TColor;
    DeltaX,DeltaY:integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}
{ a record to save notes data in }
type
  PItemData=^TItemData;
  TItemData=record
    Text:string;
  end;

procedure TForm1.btnAddClick(Sender: TObject);
var aItem:TJvTimeItem;
begin
   aItem := TimeLine1.Items.Add;
   aItem.Caption := edCaption.Text;
   aItem.ImageIndex := StrToIntDef(edImIndex.Text,-1);
   aItem.Level := StrToIntDef(edLevel.Text,0);
   aItem.Date := dtpItemDate.Date;
   aItem.Color := FCurColor;
   if FCurColor <> clWhite then
     aItem.TextColor := clWhite
   else
     aItem.TextColor := clBlack;
end;

procedure TForm1.chkMonthsClick(Sender: TObject);
begin
  TimeLine1.ShowMonthNames := chkMonths.Checked;
end;

procedure TForm1.chkMultiClick(Sender: TObject);
begin
  TimeLine1.MultiSelect := chkMulti.Checked;
end;

procedure TForm1.btnFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  begin
    Font := TimeLine1.Font;
    if Execute then
      TimeLine1.Font := Font;
    Free;
  end;
end;

procedure TForm1.btnYrFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  begin
    Font := TimeLine1.YearFont;
    if Execute then
      TimeLine1.YearFont := Font;
    Free;
  end;
end;

procedure TForm1.TimeLine1Click(Sender: TObject);
begin
  if TimeLine1.Selected <> nil then
    Caption := Format ('%s (%s)',[TimeLine1.Selected.Caption,DateToStr(TimeLine1.Selected.Date)]);
end;

procedure TForm1.Changecaption1Click(Sender: TObject);
var S:string;
begin
  if TimeLine1.Selected <> nil then
  begin
    S := TimeLine1.Selected.Caption;
    if InputQuery('Change caption','Change caption to:',S) then
      TimeLine1.Selected.Caption := S;
  end;
end;

procedure TForm1.remove1Click(Sender: TObject);
begin
  if TimeLine1.Selected <> nil then
    if MessageDlg('Sure you want to delete this item?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
      if TimeLine1.Selected.Data <> nil then
        Dispose(PItemData(TimeLine1.Selected.Data));
      TimeLine1.Selected.Remove;
    end;
end;

procedure TForm1.Move1Click(Sender: TObject);
var S:string;
begin
  if TimeLine1.Selected <> nil then
  begin
    S := DateToStr(TimeLine1.Selected.Date);
    if InputQuery('Move item','Move to new date:',S)then
      TimeLine1.Selected.Date := StrToDate(S);
  end;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
const
  aCaption:array [boolean] of string = ('E&nable','D&isable');
var i:integer;
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

procedure TForm1.chkNoImagesClick(Sender: TObject);
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

procedure TForm1.chkWidthAsClick(Sender: TObject);
const
  aType:array [boolean] of TJvTimeItemType =(asPixels,asDays);
var i:integer;
begin
  for i := 0 to TimeLine1.Items.Count - 1 do
     TimeLine1.Items[i].WidthAs := aType[chkWidthAs.Checked];
end;

procedure TForm1.chkAutosizeClick(Sender: TObject);
begin
  TimeLine1.AutoSize := chkAutosize.Checked;
end;

procedure TForm1.chkSupportClick(Sender: TObject);
begin
  TimeLine1.VertSupports := chkSupport.Checked;
end;

procedure TForm1.chkLargeClick(Sender: TObject);
begin
  if chkNoImages.Checked then Exit;
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

procedure TForm1.udYrSizeClick(Sender: TObject; Button: TUDBtnType);
begin
  TimeLine1.YearWidth := udYrSize.Position;
end;


procedure TForm1.TimeLine1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  StatusBar1.Panels[0].Text := DateToStr(TimeLine1.DateAtPos(X)) + ' (approx.)';
end;

procedure TForm1.udItemHeightClick(Sender: TObject; Button: TUDBtnType);
begin
  TimeLine1.ItemHeight := udItemHeight.Position;
end;

procedure TForm1.chkOwnerDrawClick(Sender: TObject);
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
procedure TForm1.Notes1Click(Sender: TObject);
var aData:PItemData;P:TPoint;
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
    Form2.Caption := Format('Notes for "%s":',[TimeLine1.Selected.Caption]);
    Form2.Memo1.Text := aData^.Text;
    GetCursorPos(P);
    Form2.Left := P.x - 12;
    Form2.Top := P.y - 32;
    Form2.ShowModal;
    aData^.Text := Form2.Memo1.Text;
    TimeLine1.Selected.Data := aData;
  end;
end;

{ save all notes data and dispose of memory }
procedure TForm1.FormCreate(Sender: TObject);
begin
  FCurColor := TimeLine1.Color;
end;

procedure TForm1.TimeLine1SaveItem(Sender: TObject; Item: TJvTimeItem;
  Stream: TStream);
var S:string;
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
  Stream.Write(S[1],Length(S));
end;

{ let's read our previously saved data from the file }
procedure TForm1.TimeLine1LoadItem(Sender: TObject; Item: TJvTimeItem;
  Stream: TStream);
var S:string;aData:PItemData;ch:char;
begin
  Stream.Read(ch,1);
  while ch <> #27 do
  begin
    AppendStr(S,ch);
    Stream.Read(ch,1);
  end;

  if (S <> '@@@') then { nothing there }
  begin
    New(aData);
    aData^.Text := S;
    Item.Data := aData;
  end;
end;

function strrev(p:string):string;
var pend,pstart:integer;
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
procedure TForm1.TimeLine1DrawItem(Sender: TObject; Canvas: TCanvas;
  Item: TJvTimeItem; var R: TRect);
var S:string;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.FrameRect(R);

  Canvas.Brush.Color := RGB(Random(255),Random(255),Random(255));
  Canvas.FillRect(R);
  if TimeLine1.Images <> nil then
  begin
    if Item.Selected then
      TimeLine1.Images.Draw(Canvas,R.Left,R.Top,Random(TimeLine1.Images.Count))
    else
      TimeLine1.Images.Draw(Canvas,R.Right - TimeLine1.Images.Width,R.Top,Item.ImageIndex);
  end;

  if (Random > 0.5) and Item.Selected then
    S := strrev(Item.Caption)
  else
    S := Item.Caption;
  Canvas.Font.Color := Canvas.Brush.Color xor clWhite;
  DrawText(Canvas.Handle,PChar(' ' + S),-1,R,DT_LEFT or DT_BOTTOM or DT_SINGLELINE);
end;

procedure TForm1.dtpFirstDateChange(Sender: TObject);
begin
  TimeLine1.FirstVisibleDate := dtpFirstDate.Date;
  dtpFirstDate.Date := TimeLine1.FirstVisibleDate;
end;

procedure TForm1.TimeLine1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (TimeLine1.Selected <> nil) then
    TimeLine1.BeginDrag(False);
end;

procedure TForm1.TimeLine1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = Source;
  if Accept then
    Caption := Format('Current date: %s',[DateToStr(TimeLine1.DateAtPos(X))]);
end;

procedure TForm1.TimeLine1DragDrop(Sender, Source: TObject; X, Y: Integer);
var aDate:TDateTime;S:string;
begin
   if (Sender = Source) and Assigned(TimeLine1.Selected) then
   begin
     aDate := TimeLine1.DateAtPos(X);
     S := DateToStr(aDate);
     { make sure the user really intended to do the drag to this date: }
     if InputQuery('Confirm move',Format('Move "%s" to new date:',[TimeLine1.Selected.Caption]),S) then
     begin
       TimeLine1.Selected.Date := StrToDate(S);
       TimeLine1.Selected.Level := TimeLine1.LevelAtPos(Y);
     end;
   end;
   TimeLine1.EndDrag(Sender = Source);
end;

procedure TForm1.btnColorClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do
  begin
    Color := FCurColor;
    if Execute then
      FCurColor := Color;
    Free;
  end;
end;

procedure TForm1.Up1Click(Sender: TObject);
begin
  if (TimeLine1.Selected <> nil) and (TimeLine1.Selected.Level > 0) then
  begin
    TimeLine1.Selected.Level := TimeLine1.Selected.Level - 1;
  end else if TimeLine1.Selected <> nil then
    ShowMessage('Can''t move this item further up!');
end;

procedure TForm1.Down1Click(Sender: TObject);
begin
  TimeLine1.Selected.Level := TimeLine1.Selected.Level + 1;
end;

procedure TForm1.btnAutoClick(Sender: TObject);
begin
  TimeLine1.BeginUpdate;
  TimeLine1.AutoLevels(chkComplete.Checked,chkReset.Checked);
  TimeLine1.EndUpdate;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
var i:integer;
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

procedure TForm1.btnSaveClick(Sender: TObject);
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

procedure TForm1.FormDestroy(Sender: TObject);
var i:integer;
begin
  { free allocated memory }
  for i := 0 to TimeLine1.Items.Count - 1 do
    if TimeLine1.Items[i].Data <> nil then
    begin
      Dispose(PItemData(TimeLine1.Items[i].Data));
      TimeLine1.Items[i].Data := nil;
    end;
end;

procedure TForm1.chkFlatClick(Sender: TObject);
begin
  TimeLine1.Flat := chkFlat.Checked;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.chkHelpYearClick(Sender: TObject);
begin
  TimeLine1.HelperYears := chkHelpYear.Checked;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  TimeLine1.Height := Height div 2;
  if Panel2.Height < 250 then
    TimeLine1.Height := Height - 300;

end;

procedure TForm1.ColorBtnClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do
  begin
    Color := TimeLine1.Color;
    if Execute then
      TimeLine1.Color := Color;
    Free;
  end;
end;

procedure TForm1.Disable1Click(Sender: TObject);
begin
  if TimeLine1.Selected <> nil then
    TimeLine1.Selected.Enabled := not TimeLine1.Selected.Enabled;
end;

procedure TForm1.TimeLine1ItemClick(Sender: TObject; Item: TJvTimeItem);
begin
  Caption := Item.Caption;
end;

procedure TForm1.TimeLine1DblClick(Sender: TObject);
var S:string;
begin
{ Doesn't work - drag image appears - what's up ? }
  if TimeLine1.Selected <> nil then
  begin
   S := DateToStr(TimeLine1.Selected.Date);
   if InputQuery('Confirm move',Format('Move "%s" to new date:',[TimeLine1.Selected.Caption]),S) then
     TimeLine1.Selected.Date := StrToDate(S);
  end;
  TimeLine1.EndDrag(false);
  CancelDrag;
end;

end.
