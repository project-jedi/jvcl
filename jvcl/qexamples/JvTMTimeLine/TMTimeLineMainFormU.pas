{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit TMTimeLineMainFormU;
{
  This demo demonstrates the different properties, methods and events unique to
  the Team Manager Timeline.
}
interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  QExtCtrls, QComCtrls, QStdCtrls, QButtons, QImgList,
  QMenus, QCheckLst, JvQTMTimeLine, QMask, QComboEdits, JvQExComboEdits,
  JvQToolEdit, QTypes, QComCtrlsEx;

type
  TTMTimeLineMainForm = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    Label6: TLabel;
    lvImages: TListView;
    popTimeLine: TPopupMenu;
    mnuToday: TMenuItem;
    mnuInsertImage: TMenuItem;
    mnuRemoveImage: TMenuItem;
    N1: TMenuItem;
    mnuEditMemo: TMenuItem;
    mnuGotoDate: TMenuItem;
    StatusBar: TStatusBar;
    il16: TImageList;
    gbWidths: TGroupBox;
    gbDates: TGroupBox;
    gbAppearance: TGroupBox;
    chkReadOnly: TCheckBox;
    chkFlat: TCheckBox;
    chkRClick: TCheckBox;
    chkEnabled: TCheckBox;
    chkShowToday: TCheckBox;
    chkShowWeeks: TCheckBox;
    chkShowMonths: TCheckBox;
    Label13: TLabel;
    lbObjFontStyle: TCheckListBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edImageNo: TEdit;
    udImageNo: TUpDown;
    btnAdd: TButton;
    Label1: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    edDayWidth: TEdit;
    udDayWidth: TUpDown;
    edPenWidth: TEdit;
    udPenWidth: TUpDown;
    edScrollSmall: TEdit;
    udScrollSmall: TUpDown;
    edScrollLarge: TEdit;
    udScrollLarge: TUpDown;
    edButtonWidth: TEdit;
    udButtonWidth: TUpDown;
    Label11: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    gbMisc: TGroupBox;
    btnLoad: TButton;
    btnSave: TButton;
    gbFonts: TGroupBox;
    btnFont: TButton;
    btnColor: TButton;
    btnMonthFont: TButton;
    btnTodayColor: TButton;
    btnLineColor: TButton;
    btnPenColor: TButton;
    Label7: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    dtpFirstDate: TJvDateEdit;
    dtpSelDate: TJvDateEdit;
    dtpImageDate: TJvDateEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnMonthFontClick(Sender: TObject);
    procedure chkReadOnlyClick(Sender: TObject);
    procedure dtpFirstDateChange(Sender: TObject);
    procedure dtpSelDateChange(Sender: TObject);
    procedure udDayWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure btnColorClick(Sender: TObject);
    procedure chkFlatClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure lblWebPageClick(Sender: TObject);
    procedure btnPenColorClick(Sender: TObject);
    procedure udPenWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure udScrollSmallClick(Sender: TObject; Button: TUDBtnType);
    procedure lvImagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnTodayColorClick(Sender: TObject);
    procedure chkRClickClick(Sender: TObject);
    procedure mnuTodayClick(Sender: TObject);
    procedure mnuInsertImageClick(Sender: TObject);
    procedure udScrollLargeClick(Sender: TObject; Button: TUDBtnType);
    procedure mnuRemoveImageClick(Sender: TObject);
    procedure mnuEditMemoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure udButtonWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure lbObjFontStyleClickCheck(Sender: TObject);
    procedure chkEnabledClick(Sender: TObject);
    procedure mnuGotoDateClick(Sender: TObject);
    procedure StatusBarResize(Sender: TObject);
    procedure chkShowMonthsClick(Sender: TObject);
    procedure chkShowWeeksClick(Sender: TObject);
    procedure chkShowTodayClick(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure btnLineColorClick(Sender: TObject);
  private
    { Private declarations }
    sl:TJvTMTimeline;
    procedure DoClick(Sender: TObject);
    procedure DoDateChange(Sender: TObject);
    procedure DoDblClick(Sender: TObject);
    procedure DoObjectLoad(Sender: TObject; Stream: TStream;var AObject: TObject);
    procedure DoObjectSave(Sender: TObject; Stream: TStream;const AObject: TObject);
  end;

var
  TMTimeLineMainForm: TTMTimeLineMainForm;

implementation

uses
  ShellAPI, frmMemoEdit;

{$R *.xfm}

procedure TTMTimeLineMainForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
//  ForceCurrentDirectory := true;
  sl              := TJvTMTimeline.Create(self);
  sl.Parent       := self;
  sl.PopUpMenu    := popTimeline;
  sl.OnChange     := DoDateChange;
  sl.OnClick      := DoClick;
  sl.OnDblClick   := DoDblClick;
  sl.Images       := il16;
  sl.Align        := alClient;
  sl.Hint         := 'Double-click a date to edit it''s memo content.'#13#10'Right-click to display pop-up menu.';
  dtpSelDate.Date     := Date;
  dtpFirstDate.Date   := Date-7;
  dtpImageDate.Date   := Date+7;
  udDayWidth.Position := sl.DayWidth;
  chkReadOnly.Checked := sl.ReadOnly;
  sl.Date := dtpFirstDate.Date;
  sl.SelDate := dtpSelDate.Date;
  lbObjFontStyle.Checked[2] := true;
  for i := 0 to il16.Count - 1 do
  begin
    with lvImages.Items.Add do
    begin
      ImageIndex := i;
      Caption := IntToStr(i);
    end;
  end;
  Splitter1.Top := sl.Height + 5;
end;

// Free any stringlists still around in the Objects array by calling the ClearObjects method
// Do NOT call this method unless the Objects array actually contains
// TObjects (or descendants): calling this method if you store ordinal values
// (like integers), will cause an AV
// You can freely mix object types in the array: the will be freed correctly anyway
procedure TTMTimeLineMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  sl.ClearObjects;
end;

// just update the controls when an item in the listview is clicked
procedure TTMTimeLineMainForm.lvImagesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Assigned(Item) and Selected then
    udImageNo.Position := Item.ImageIndex;
end;

// simple trick that launches your default web-browser and tries to go to my web-page:
// this even works with modem-connections
procedure TTMTimeLineMainForm.lblWebPageClick(Sender: TObject);
begin
  ShellExecute(0, PChar('open'),PChar('http://www.peter3.com'),nil,nil,SW_SHOWNORMAL);
end;

procedure TTMTimeLineMainForm.btnLoadClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := 'Timeline files|*.tm|All files|*.*';
    DefaultExt := 'TM';
    if Execute then
    begin
      sl.OnReadObject := DoObjectLoad;
      sl.LoadFromFile(Filename);
    end;
  finally
    Free;
  end;
end;

procedure TTMTimeLineMainForm.btnSaveClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    Filter := 'Timeline files|*.tm|All files|*.*';
    DefaultExt := 'TM';
    if Execute then
    begin
      sl.OnWriteObject := DoObjectSave;
      sl.SaveToFile(Filename);
    end;
  finally
    Free;
  end;
end;

//****************************************************************************//

// set imageindex for the chosen date
procedure TTMTimeLineMainForm.btnAddClick(Sender: TObject);
begin
  sl.ImageIndex[dtpImageDate.Date] := udImageNo.Position;
end;

// OnChange is called whenever the FirstDate (the first visible date) changes
// This happens when the user scrolls the display or when you set FirstDate programmatically
procedure TTMTimeLineMainForm.DoDateChange(Sender:TObject);
begin
  dtpFirstDate.Date := sl.Date;
  StatusBarResize(Sender);
end;

// the click event is called when the timeline is clicked. To keep track of Date
// (i.e. the currently selected date), assign a handler to this event
// If right-click select is true, Date is updated when you right-click too
procedure TTMTimeLineMainForm.DoClick(Sender:TObject);
begin
  dtpSelDate.Date := sl.SelDate;
  dtpImageDate.Date := sl.SelDate;
end;

// display the memo contents when double.clicking: the day number is
// automatically underlined when the Objects property has a non-nil value
procedure TTMTimeLineMainForm.DoDblClick(Sender:TObject);
begin
  mnuEditMemo.Click;
end;

// the timeline font is the font used for the day numbers
procedure TTMTimeLineMainForm.btnFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Font := sl.Font;
    if Execute then
      sl.Font := Font;
  finally
    Free;
  end;
end;

// the timelines monthfont is the one used for displaying month names and the year
procedure TTMTimeLineMainForm.btnMonthFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Font := sl.MonthFont;
    if Execute then
      sl.MonthFont := Font;
  finally
    Free;
  end;
end;

// if the timeline is readonly, the scrollbuttons disappear
// and the user cannot scroll with the mouse or keyboard
// the selection frame is also removed
procedure TTMTimeLineMainForm.chkReadOnlyClick(Sender: TObject);
begin
  sl.ReadOnly := chkReadOnly.Checked;
  StatusBarResize(Sender);
end;

// enabled is not the same as read-only!
procedure TTMTimeLineMainForm.chkEnabledClick(Sender: TObject);
begin
  sl.Enabled := chkEnabled.Checked;
end;

// DayWidth is simply the width in pixels for one day
procedure TTMTimeLineMainForm.udDayWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  sl.DayWidth := udDayWidth.Position;
  udDayWidth.Position := sl.DayWidth;
  StatusBarResize(Sender);
end;

// Color is the background color of the control
procedure TTMTimeLineMainForm.btnColorClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do
  try
    Color := sl.Color;
    if Execute then
      sl.Color := Color;
  finally
    Free;
  end;
end;

// the timeline doesn't have a Flat property: when setting BorderStyle to bsNone,
// the scroll buttons are automatically changed to have a flat appearance
procedure TTMTimeLineMainForm.chkFlatClick(Sender: TObject);
const
  cBStyle:array [boolean] of TBorderStyle = (bsSingle,bsNone);
begin
  sl.BorderStyle := cBStyle[chkFlat.Checked];
end;

// update the first visible date from the datetimepicker
procedure TTMTimeLineMainForm.dtpFirstDateChange(Sender: TObject);
begin
  sl.Date := dtpFirstDate.Date;
end;

// update the selected date from the datetimepicker
procedure TTMTimeLineMainForm.dtpSelDateChange(Sender: TObject);
begin
  sl.SelDate := dtpSelDate.Date;
end;

// change the selection frame Pen color
// You can also change any other property of the Selection, like
// Pen.Width, Pen.Style, Pen.Mode and whether the Selection frame is visible or not
// Setting Pen.Width to 0, has the same effect as Selection.Visible := false
procedure TTMTimeLineMainForm.btnPenColorClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do
  try
    Color := sl.Selection.Pen.Color;
    if Execute then
    begin
      sl.Selection.Pen.Color := Color;
    end;
  finally
    Free;
  end;
end;

// change the Selection frame Pen's width
procedure TTMTimeLineMainForm.udPenWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  sl.Selection.Pen.Width := udPenWidth.Position;
end;

// changes the background Color of Today 
procedure TTMTimeLineMainForm.btnTodayColorClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do
  try
    Color := sl.TodayColor;
    if Execute then
      sl.TodayColor := Color;
  finally
    Free;
  end;
end;

// when RightClickSelect is true, a right-click changes
// the TJvTLTimeline.Date value
procedure TTMTimeLineMainForm.chkRClickClick(Sender: TObject);
begin
  sl.RightClickSelect := chkRClick.Checked;
end;

// move today to the middle of the timeline; looks better
procedure TTMTimeLineMainForm.mnuTodayClick(Sender: TObject);
begin
  sl.Date := Date - sl.VisibleDays div 2;
end;

// add or replace the image at the currently selected date
// if the imageindex is < 0 or > Images.Count - 1, the image is removed
procedure TTMTimeLineMainForm.mnuInsertImageClick(Sender: TObject);
begin
  sl.ImageIndex[sl.SelDate] := udImageNo.Position;
end;

// the SmallChange value is used when you scroll without holding any additional
// keys down (like Shift or Ctrl). You can scroll both with the mouse and with
// the arrow-keys
procedure TTMTimeLineMainForm.udScrollSmallClick(Sender: TObject; Button: TUDBtnType);
begin
  sl.SmallChange := udScrollSmall.Position;
end;

// the LargeChange value is used when you hold the Ctrl key while scrolling
procedure TTMTimeLineMainForm.udScrollLargeClick(Sender: TObject; Button: TUDBtnType);
begin
  sl.LargeChange := udScrollLarge.Position;
end;

// remove an image by setting the imageindex to -1
procedure TTMTimeLineMainForm.mnuRemoveImageClick(Sender: TObject);
begin
  sl.ImageIndex[sl.SelDate] := -1;
end;

// Get or create a TStringlist for the selected date.
// If the user empties the memo, the TStringlist is freed
procedure TTMTimeLineMainForm.mnuEditMemoClick(Sender: TObject);
var
  S: TStringlist;
  i: integer;
  Ico: TIcon;
  Bmp: TBitmap;
begin
// WARNING: if you store integers or other ordinal values in the Objects array
// you will get an AV if you call the ClearObjects method:
//  sl.Objects[sl.Date] := TObject(Random(100));

  S := TStringlist(sl.Objects[sl.SelDate]);
  // here's a trick: extract the image from the imagelist and assign t to the icon property of the form:
  i := sl.ImageIndex[sl.SelDate];
  if i > -1 then
  begin
    Ico := TIcon.Create;
    Bmp := TBitmap.Create;
    il16.GetBitmap(i, Bmp);
    Ico.Assign(Bmp);
    Bmp.Free;
  end
  else
    Ico := nil;

  if S = nil then
    S := TStringlist.Create;
  // TIP: add a class function to dialogs that you show modally.
  // That way, you can keep all the creating and freeing stuff in the
  // dialog unit instead of in the calling unit.
  // This reduces the dialog call to a one-liner:
  TMemoEditFrm.Edit(S,sl.SelDate,Ico); // the Edit function automatically updates S if the user clicked OK in the dialog
  
  if Length(trim(S.Text)) = 0 then
  begin // there is no text, so free the stringlist to conserve memory
    S.Free;
    S := nil;
  end;
  sl.Objects[sl.SelDate] := S; // either way, store the value (nil or TStringlist)
  // if Objects[sl.Date] has a non-nil value, the day number is underlined for that date
  Ico.Free;
end;

// changes the widths of the scrollbuttons
procedure TTMTimeLineMainForm.udButtonWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  sl.ButtonWidth := udButtonWidth.Position;
  StatusBarResize(Sender);
end;

// By default, the timeline only stores the imageindex for the dates so if you need
// a little more functionality, here's where you have your chance:
// The OnWriteObject event is called for any date that has a non-nil Objects.
// Because we are storing stringlists in the objects array, we save each of them as a
// long text string to the stream here.
// If you want to store other values, you will have to save them accordingly.
procedure TTMTimeLineMainForm.DoObjectSave(Sender:TObject;Stream:TStream;const AObject:TObject);
var S:string;Count:integer;
begin
  S := TStringlist(AObject).Text;
  Count := Length(S);
  // save length of string
  Stream.Write(Count,sizeof(Count));
  // need we store anything ?
  if Count > 0 then
    Stream.Write(S[1],Count);
end;

// The OnReadObject event is called for each object stored in a data file.
// The Timeline keeps track of all the dates that have non-nil objects and calls
// this method for each one of them.
// You don't have to worry about the actual date the object belongs to, because the
// timeline handles this for you.
//
// You must do two things in this handler:
// 1. Create an instance of your class (if it is a class you are storing)
// 2. Read the data from the stream and save it in AObject

procedure TTMTimeLineMainForm.DoObjectLoad(Sender:TObject;Stream:TStream;var AObject:TObject);
var S:string;Count:integer;
begin
  // Get the length of the string:
  Stream.Read(Count,sizeof(Count));
  SetLength(S,Count);
  // need we read any more ?
  if Count > 0 then
  begin
    Stream.Read(S[1],Count);
    AObject := TStringlist.Create;
    TStringlist(AObject).Text := S;
  end;
end;

procedure TTMTimeLineMainForm.lbObjFontStyleClickCheck(Sender: TObject);
var F:TFontStyles;
begin
  F := [];
  with lbObjFontStyle do
  begin
    if Checked[0] then
      Include(F,fsBold);
    if Checked[1] then
      Include(F,fsItalic);
    if Checked[2] then
      Include(F,fsUnderline);
    if Checked[3] then
      Include(F,fsStrikeOut);
  end;
  sl.ObjectsFontStyle := F;
end;

// move to the selected day and center it on the display
procedure TTMTimeLineMainForm.mnuGotoDateClick(Sender: TObject);
begin
  sl.Date := sl.SelDate - sl.VisibleDays div 2;
end;

// update the statusbar whnever anything changes
procedure TTMTimeLineMainForm.StatusBarResize(Sender: TObject);
begin
  StatusBar.Panels[0].Text := Format('Visible days: %d',[sl.VisibleDays]);
  StatusBar.Panels[1].Text := Format('Last visible date: %s',[DateToStr(sl.LastVisibleDate)]);
end;

// display options:
procedure TTMTimeLineMainForm.chkShowMonthsClick(Sender: TObject);
begin
  sl.ShowMonths := chkShowMonths.Checked;
end;

procedure TTMTimeLineMainForm.chkShowWeeksClick(Sender: TObject);
begin
  sl.ShowWeeks := chkShowWeeks.Checked;
end;

procedure TTMTimeLineMainForm.chkShowTodayClick(Sender: TObject);
begin
  sl.ShowToday := chkShowToday.Checked;
end;

// handling the wheel:
procedure TTMTimeLineMainForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if  not sl.Focused and (ControlAtPos(ScreenToClient(MousePos),false,true) is TJvTMTimeline) then
  begin
    Handled := true;
    if ssCtrl in Shift then
      sl.ScrollDate(self,-udScrollSmall.Position)
    else
      sl.ScrollDate(self,-udScrollLarge.Position);
  end;
end;

procedure TTMTimeLineMainForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if  not sl.Focused and (ControlAtPos(ScreenToClient(MousePos),false,true) is TJvTMTimeline) then
  begin
    Handled := true;
    if ssCtrl in Shift then
      sl.ScrollDate(self,udScrollSmall.Position)
    else
      sl.ScrollDate(self,udScrollLarge.Position);
  end;
end;

procedure TTMTimeLineMainForm.btnLineColorClick(Sender: TObject);
begin
  with TColorDialog.Create(nil) do
  try
    Color := sl.LineColor;
    if Execute then
      sl.LineColor := Color;
  finally
    Free;
  end;

end;

end.
