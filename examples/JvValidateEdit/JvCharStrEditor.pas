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

{$I jvcl.inc}

unit JvCharStrEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus, ActnList, ImgList;

type
  TfrmJvCharEditDlg = class(TForm)
    Panel1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    lvCharacters: TListView;
    PopupMenu1: TPopupMenu;
    SelectAll1: TMenuItem;
    UnselectAll1: TMenuItem;
    Invertselection1: TMenuItem;
    ActionList1: TActionList;
    acCheckAll: TAction;
    acUnCheckAll: TAction;
    acInvertCheck: TAction;
    acAlpha: TAction;
    acAlphaNum: TAction;
    acHex: TAction;
    acFloat: TAction;
    acScientific: TAction;
    acCurrency: TAction;
    acInteger: TAction;
    Special1: TMenuItem;
    Alpha1: TMenuItem;
    acAlphaNum1: TMenuItem;
    acInteger1: TMenuItem;
    acFloat1: TMenuItem;
    acHex1: TMenuItem;
    acScientific1: TMenuItem;
    acCurrency1: TMenuItem;
    acLarge: TAction;
    acSmall: TAction;
    acList: TAction;
    acReport: TAction;
    View1: TMenuItem;
    Large1: TMenuItem;
    Small1: TMenuItem;
    List1: TMenuItem;
    Report1: TMenuItem;
    ImageList1: TImageList;
    acCheckSel: TAction;
    acUnCheckSel: TAction;
    CheckSelected1: TMenuItem;
    UnCheckSelected1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    cbFonts: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure lvCharactersInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: string);
    procedure acInvertCheckExecute(Sender: TObject);
    procedure acUnCheckAllExecute(Sender: TObject);
    procedure acCheckAllExecute(Sender: TObject);
    procedure acAlphaExecute(Sender: TObject);
    procedure acAlphaNumExecute(Sender: TObject);
    procedure acHexExecute(Sender: TObject);
    procedure acFloatExecute(Sender: TObject);
    procedure acScientificExecute(Sender: TObject);
    procedure acCurrencyExecute(Sender: TObject);
    procedure acIntegerExecute(Sender: TObject);
    procedure acLargeExecute(Sender: TObject);
    procedure acSmallExecute(Sender: TObject);
    procedure acListExecute(Sender: TObject);
    procedure acReportExecute(Sender: TObject);
    procedure lvCharactersResize(Sender: TObject);
    procedure acCheckSelExecute(Sender: TObject);
    procedure acUnCheckSelExecute(Sender: TObject);
    procedure lvCharactersEnter(Sender: TObject);
    procedure cbFontsCloseUp(Sender: TObject);
    procedure cbFontsKeyPress(Sender: TObject; var Key: Char);
    procedure lvCharactersAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure lvCharactersSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    function getCharacters: string;
    procedure setCharacters(const Value: string);
    procedure UnCheckAll;
    { Private declarations }
  public
    { Public declarations }
    class function Edit(var Characters: string): boolean;
    property Characters: string read getCharacters write setCharacters;
  end;

  
const
  cAsciiNames:array [char] of PChar =
  (
  'NUL', // null
  'SOH', // start of heading
  'STX', // start of text
  'ETX', // end of text
  'EOT', // end of transmission
  'ENQ', // enquiry
  'ACK', // acknowledge
  'BEL', // bell
  'BS',  // backspace
  'TAB', // horizontal tab
  'LF',  // line feed
  'VT',  // vertical tab
  'FF',  // form feed
  'CR',  // carriage return
  'SO',  // shift out
  'SI',  // shift in
  'DLE', // data link escape
  'DC1', // device control 1
  'DC2', // device control 2
  'DC3', // device control 3
  'DC4', // device control 4
  'NAK', // negative acknowledge
  'SYN', // synch idle
  'ETB', // end of trans. block
  'CAN', // cancel
  'EM',  // end of medium
  'SUB', // substitute
  'ESC', // escape
  'FS',  // file separator
  'GS',  // group separator
  'RS',  // record separator
  'US',  // unit separator
  'SPACE', // space
                  #33, #34, #35, #36, #37, #38, #39,
   #40, #41, #42, #43, #44, #45, #46, #47, #48, #49,
   #50, #51, #52, #53, #54, #55, #56, #57, #58, #59,
   #60, #61, #62, #63, #64, #65, #66, #67, #68, #69,
   #70, #71, #72, #73, #74, #75, #76, #77, #78, #79,
   #80, #81, #82, #83, #84, #85, #86, #87, #88, #89,
   #90, #91, #92, #93, #94, #95, #96, #97, #98, #99,
  #100,#101,#102,#103,#104,#105,#106,#107,#108,#109,
  #110,#111,#112,#113,#114,#115,#116,#117,#118,#119,
  #120,#121,#122,#123,#124,#125,#126,#127,#128,#129,
  #130,#131,#132,#133,#134,#135,#136,#137,#138,#139,
  #140,#141,#142,#143,#144,#145,#146,#147,#148,#149,
  #150,#151,#152,#153,#154,#155,#156,#157,#158,#159,
  #160,#161,#162,#163,#164,#165,#166,#167,#168,#169,
  #170,#171,#172,#173,#174,#175,#176,#177,#178,#179,
  #180,#181,#182,#183,#184,#185,#186,#187,#188,#189,
  #190,#191,#192,#193,#194,#195,#196,#197,#198,#199,
  #200,#201,#202,#203,#204,#205,#206,#207,#208,#209,
  #210,#211,#212,#213,#214,#215,#216,#217,#218,#219,
  #220,#221,#222,#223,#224,#225,#226,#227,#228,#229,
  #230,#231,#232,#233,#234,#235,#236,#237,#238,#239,
  #240,#241,#242,#243,#244,#245,#246,#247,#248,#249,
  #250,#251,#252,#253,#254,#255);
  
// converts a syschar set to it's textual display representation
// as is displayed in the OI, f ex
// NOTE: this is *not* the opposite of StringToSysCharSet below!
function SysCharSetToString(ASet:TSysCharSet;Brackets:boolean):string;
// converts a string to a SysCharSet
function StringToSysCharSet(const S:string):TSysCharSet;
// returns either an unquoted name, like NUL, or a quoted character, like 'A'
function GetCharName(const Ch:char):string;

resourcestring
  SFormCaption = 'TJvFormatEdit.Characters Editor ($%.2x, #%.2u, "%s")';

implementation

{$R *.DFM}

function StringToSysCharSet(const S:string):TSysCharSet;
var i:integer;
begin
  Result := [];
  for i := 1 to Length(S) do
  begin
    Include(Result,S[i]);
    if Result = [#0..#255] then Exit; // everything included, so no need to continue
  end;
end;

function GetCharName(const Ch:char):string;
var FTmpType:word;
begin
  Result := cAsciiNames[Ch];
  GetStringTypeEx(LOCALE_USER_DEFAULT,CT_CTYPE1,@Ch,1,FTmpType);
  if (FTmpType and C1_CNTRL = 0) and (Ch <> #32) then
    Result := #39 + Result + #39;
end;

// far from perfect, but kind of works...
function SysCharSetToString(ASet:TSysCharSet;Brackets:boolean):string;
var i,LastChar,PrevChar:char;
begin
  PrevChar := #255;
  LastChar := #0;
  for i := #0 to #255 do
  begin
    if i in ASet then
    begin
//      if PrevChar = #0 then
      if Ord(i)-Ord(PrevChar) <> 1 then
      begin
        if Result <> '' then
          Result := Result + ',' + getCharName(i)
        else
          Result := getCharName(i);
        LastChar := i;
      end
      else if i = #255 then
      begin
        if Result = '' then
          Result := GetCharName(i)
        else if Ord(i) - Ord(LastChar) > 1 then
          Result := Result + '...' + GetCharName(i)
        else
          Result := Result + ',' + GetCharName(i);
        Break;
      end;
      PrevChar := i;
    end
    else
    begin
      if Ord(i) - Ord(LastChar) > 1 then
        Result := Result + '...' + GetCharName(Pred(i))
      else if (LastChar = #0) and (Pred(i) <> LastChar) and (i <> #0) then
        Result := Result + ',' + GetCharName(Pred(i));
      PrevChar := #255;
      LastChar := i;
    end;
  end;
  if (Length(Result) > 0) and (AnsiLastChar(Result) = ',') then
    SetLength(Result,Length(Result)-1);
  if Brackets then
    Result := '[' + Result + ']';
end;

{ TfrmJvCharEditDlg }

class function TfrmJvCharEditDlg.Edit(
  var Characters: string): boolean;
var
  frmJvCharEditDlg: TfrmJvCharEditDlg;
begin
  frmJvCharEditDlg := self.Create(Application);
  try
    frmJvCharEditDlg.Characters := Characters;
    Result := frmJvCharEditDlg.ShowModal = mrOK;
    if Result then
      Characters := frmJvCharEditDlg.Characters;
  finally
    frmJvCharEditDlg.Free;
  end;
end;

function TfrmJvCharEditDlg.getCharacters: string;
var i: integer;
begin
  Result := '';
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      if Items[i].Checked then
        Result := Result + Char(Items[i].Data);
end;

procedure TfrmJvCharEditDlg.setCharacters(const Value: string);
var i: integer;
begin
  acUnCheckAll.Execute;
  with lvCharacters do
    for i := 1 to Length(Value) do
      Items[Ord(Value[i])].Checked := true;
end;

procedure TfrmJvCharEditDlg.FormCreate(Sender: TObject);
var i: char;j:integer;
begin
  with lvCharacters do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for i := #0 to #255 do
        with Items.Add do
        begin
          Caption := Format('%s', [cAsciiNames[i]]);
          Data := Pointer(i);
        end;
    finally
      Items.EndUpdate;
    end;
  end;
  cbFonts.Items := Screen.Fonts;
  j := cbFonts.Items.IndexOf(Font.Name);
  if j = -1 then
    cbFonts.ItemIndex := cbFonts.Items.Add(Font.Name)
  else
    cbFonts.ItemIndex := j;
  Caption := Format(SFormCaption,[0,0,cAsciiNames[#0]]);
end;

procedure TfrmJvCharEditDlg.lvCharactersInfoTip(Sender: TObject;
  Item: TListItem; var InfoTip: string);
begin
  if Item <> nil then
    InfoTip := Format('$%.2x, #%.2d, "%s"', [integer(Item.Data), integer(Item.Data),cAsciiNames[Char(Item.Data)]]);
end;

procedure TfrmJvCharEditDlg.acInvertCheckExecute(Sender: TObject);
var i: integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      Items[i].Checked := not Items[i].Checked;
end;

procedure TfrmJvCharEditDlg.acUnCheckAllExecute(Sender: TObject);
var i: integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      Items[i].Checked := false;
end;

procedure TfrmJvCharEditDlg.acCheckAllExecute(Sender: TObject);
var i: integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      Items[i].Checked := true;
end;

procedure TfrmJvCharEditDlg.acAlphaExecute(Sender: TObject);
var i: integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      Items[i].Checked := IsCharAlpha(char(Items[i].Data));
end;

procedure TfrmJvCharEditDlg.acAlphaNumExecute(Sender: TObject);
var i: integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      Items[i].Checked := IsCharAlphaNumeric(char(Items[i].Data));
end;

procedure TfrmJvCharEditDlg.acHexExecute(Sender: TObject);
var i: integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      Items[i].Checked := char(Items[i].Data) in ['0'..'9', 'A'..'F', 'a'..'f'];
end;

procedure TfrmJvCharEditDlg.acFloatExecute(Sender: TObject);
var i: integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      Items[i].Checked := (char(Items[i].Data) in ['0'..'9', '-','+', DecimalSeparator]);
end;

procedure TfrmJvCharEditDlg.acScientificExecute(Sender: TObject);
var i: integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      Items[i].Checked := (char(Items[i].Data) in ['0'..'9', 'E','e','-','+', DecimalSeparator]);
end;

procedure TfrmJvCharEditDlg.acCurrencyExecute(Sender: TObject);
begin
  acFloat.Execute;
end;

procedure TfrmJvCharEditDlg.acIntegerExecute(Sender: TObject);
var i: integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      Items[i].Checked := (char(Items[i].Data) in ['0'..'9', '-','+']);
end;

procedure TfrmJvCharEditDlg.UnCheckAll;
begin
  acLarge.Checked := false;
  acSmall.Checked := false;
  acList.Checked := false;
  acReport.Checked := false;
end;

procedure TfrmJvCharEditDlg.acLargeExecute(Sender: TObject);
begin
  UnCheckAll;
  acLarge.Checked := true;
  lvCharacters.ViewStyle := vsIcon;
end;

procedure TfrmJvCharEditDlg.acSmallExecute(Sender: TObject);
begin
  UnCheckAll;
  acSmall.Checked := true;
  lvCharacters.ViewStyle := vsSmallIcon;
end;

procedure TfrmJvCharEditDlg.acListExecute(Sender: TObject);
begin
  UnCheckAll;
  acList.Checked := true;
  lvCharacters.ViewStyle := vsList;
end;

procedure TfrmJvCharEditDlg.acReportExecute(Sender: TObject);
begin
  UnCheckAll;
  acReport.Checked := true;
  lvCharacters.ViewStyle := vsReport;
  lvCharacters.Columns[0].Width := -2;
end;

procedure TfrmJvCharEditDlg.lvCharactersResize(Sender: TObject);
begin
  lvCharacters.Columns[0].Width := -2;
end;

procedure TfrmJvCharEditDlg.acCheckSelExecute(Sender: TObject);
var i:integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      if Items[i].Selected then
        Items[i].Checked := true;
end;

procedure TfrmJvCharEditDlg.acUnCheckSelExecute(Sender: TObject);
var i:integer;
begin
  with lvCharacters do
    for i := 0 to Items.Count - 1 do
      if Items[i].Selected then
        Items[i].Checked := false;
end;

procedure TfrmJvCharEditDlg.lvCharactersEnter(Sender: TObject);
begin
  if lvCharacters.Selected = nil then
    lvCharacters.Selected := lvCharacters.Items[0];
  lvCharacters.Selected.Focused := true;
end;

procedure TfrmJvCharEditDlg.cbFontsCloseUp(Sender: TObject);
begin
  lvCharacters.Font.Name := cbFonts.Text;
end;

procedure TfrmJvCharEditDlg.cbFontsKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
    cbFontsCloseUp(Sender);
end;

procedure TfrmJvCharEditDlg.lvCharactersAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  DefaultDraw := true;
  if Item.Checked then
  begin
    lvCharacters.Canvas.Font.Style := [fsBold];
    lvCharacters.Canvas.Font.Color := clWhite;
    lvCharacters.Canvas.Brush.Color := clMaroon;
  end;
end;

procedure TfrmJvCharEditDlg.lvCharactersSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Item <> nil then
    Caption := Format(SFormCaption,[integer(Item.Data),integer(Item.Data),cAsciiNames[Char(Item.Data)]]);
end;

end.


