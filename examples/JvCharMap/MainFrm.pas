{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

unit MainFrm;

interface

// Enable this define (remove the dot) if you have Troy Wolbrink's Tnt Controls installed
// (http://home.ccci.org/wolbrink/tnt/delphi_unicode_controls.htm)
{.$DEFINE USETNT}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus,
  JvColorCombo, JvCharMap, JvCombobox
  {$IFDEF USETNT}, TntStdCtrls, JvExStdCtrls{$ENDIF};

type
  TForm1 = class(TForm)
    FontDialog1: TFontDialog;
    Panel1: TPanel;
    btnFont: TButton;
    chkZoomPanel: TCheckBox;
    Label1: TLabel;
    edStart: TEdit;
    udStart: TUpDown;
    Label2: TLabel;
    edEnd: TEdit;
    udEnd: TUpDown;
    Label3: TLabel;
    edCols: TEdit;
    udColumns: TUpDown;
    cbColor: TJvColorComboBox;
    cbFont: TJvFontComboBox;
    chkUnicode: TCheckBox;
    reInfo: TRichEdit;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    btnSelect: TButton;
    lblFilter: TLabel;
    cbFilter: TComboBox;
    Label4: TLabel;
    cbLocales: TComboBox;
    lblChars: TLabel;
    chkShadow: TCheckBox;
    chkDisplayAll: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure chkZoomPanelClick(Sender: TObject);
    procedure chkUnicodeClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure cbColorChange(Sender: TObject);
    procedure cbFontChange(Sender: TObject);
    procedure cbFilterClick(Sender: TObject);
    procedure cbLocalesClick(Sender: TObject);
    procedure chkShadowClick(Sender: TObject);
    procedure chkDisplayAllClick(Sender: TObject);
    procedure edColsChange(Sender: TObject);
    procedure edEndChange(Sender: TObject);
    procedure edStartChange(Sender: TObject);
  private
    {$IFDEF USETNT}
    edCharacter: TTntEdit;
    {$ELSE}
    edCharacter: TEdit;
    {$ENDIF}
    procedure FillFilter;
    procedure FillLocales;
    procedure DoJMSelectChar(Sender:TObject; AChar:WideChar);
    procedure DoJMResize(Sender:TObject);
    procedure DoJMValidateChar(Sender: TObject; AChar: WideChar; var
    Valid: Boolean);
    procedure DisplayInfo(AChar:WideChar);
  public
    JM: TJvCharMap;
    Changing: Boolean;
  end;

var
  Form1: TForm1;

implementation

uses
  TypInfo, JclUnicode,
  JvClipBrd;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  JM := TJvCharMap.Create(self);
//  JM.Align := alClient;
  JM.Parent := self;
  JM.CharRange.EndChar := 255;
  JM.OnSelectChar := DoJMSelectChar;
//  JM.OnKeyUp := DoJMKeyUp;
//  JM.OnMouseUp := DoJMMouseUp;
//  JM.OnMouseWheel := DoJMMouseWheel;
  JM.OnResize := DoJMResize;
  JM.Anchors := [akTop, akBottom];
  JM.PopupMenu := PopupMenu1;
  JM.OnValidateChar := DoJMValidateChar;
  JM.AutoSizeWidth := true;
  JM.Left := (ClientWidth - JM.Width) div 2;
  JM.Top := lblChars.Top + lblChars.Height + 2;
  JM.Height := Panel1.Top - JM.Top - 20;
  lblChars.FocusControl := JM;

  chkZoomPanel.Checked := JM.ShowZoomPanel;
  udStart.Position := JM.CharRange.StartChar;
  udEnd.Position := JM.CharRange.EndChar;
  udColumns.Position := JM.Columns;
  cbColor.ColorValue := JM.Color;
  cbFont.Fontname := JM.Font.Name;
  cbColor.OnChange := cbColorChange;
  cbFont.OnChange := cbFontChange;
  chkShadow.Checked := JM.ShowShadow;

  {$IFDEF USETNT}
  edCharacter := TTntEdit.Create(self);
  {$ELSE}
  edCharacter := TEdit.Create(self);
  {$ENDIF}
  edCharacter.Parent := Panel1;
  edCharacter.Left := reInfo.Left;
  edCharacter.Top := btnSelect.Top + 4;
  edCharacter.Width := btnSelect.Left - reInfo.Left - 7;
  edCharacter.Height := 22;
  edCharacter.Anchors := [akLeft, akTop, akRight];
  edCharacter.TabOrder := 11;
  FillFilter;
  FillLocales;
  ActiveControl := JM;
end;

procedure TForm1.btnFontClick(Sender: TObject);
begin
  FontDialog1.Font := JM.Font;
  if FontDialog1.Execute then
  begin
    JM.Font := FontDialog1.Font;
    cbFont.Fontname := JM.Font.Name;
  end;
end;

procedure TForm1.chkZoomPanelClick(Sender: TObject);
begin
  JM.ShowZoomPanel := chkZoomPanel.Checked;
end;

function GetTypeString1(AChar: WideChar): WideString;
var
  ACharInfo: word;
begin
  Result := '';
  ACharInfo := 0;
  if GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE1, @AChar, 1, ACharInfo) then
  begin
    if ACharInfo and C1_UPPER = C1_UPPER then
      Result := Result + ', UPPER CASE';
    if ACharInfo and C1_LOWER = C1_LOWER then
      Result := Result + ', LOWER CASE';
    if ACharInfo and C1_DIGIT = C1_DIGIT then
      Result := Result + ', DIGIT';
    if ACharInfo and C1_SPACE = C1_SPACE then
      Result := Result + ', SPACE';
    if ACharInfo and C1_PUNCT = C1_PUNCT then
      Result := Result + ', PUNCTUATION';
    if ACharInfo and C1_CNTRL = C1_CNTRL then
      Result := Result + ', CONTROL';
    if ACharInfo and C1_BLANK = C1_BLANK then
      Result := Result + ', BLANK';
    if ACharInfo and C1_XDIGIT = C1_XDIGIT then
      Result := Result + ', HEX DIGIT';
    if ACharInfo and C1_ALPHA = C1_ALPHA then
      Result := Result + ', ALPHA';
  end;
  if Result <> '' then
    Result := Copy(Result, 2, MaxInt);
end;

function GetTypeString2(AChar: WideChar): WideString;
var
  ACharInfo: word;
begin
  Result := '';
  ACharInfo := 0;
  if GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE2, @AChar, 1, ACharInfo) then
  begin
    if ACharInfo and C2_LEFTTORIGHT = C2_LEFTTORIGHT then
      Result := Result + ', LEFT TO RIGHT';
    if ACharInfo and C2_RIGHTTOLEFT = C2_RIGHTTOLEFT then
      Result := Result + ', RIGHT TO LEFT';
    if ACharInfo and C2_EUROPENUMBER = C2_EUROPENUMBER then
      Result := Result + ', EUROPEAN NUMBER';
    if ACharInfo and C2_EUROPESEPARATOR = C2_EUROPESEPARATOR then
      Result := Result + ', EUROPEAN SEPARATOR';
    if ACharInfo and C2_EUROPETERMINATOR = C2_EUROPETERMINATOR then
      Result := Result + ', EUROPEAN TERMINATOR';
    if ACharInfo and C2_ARABICNUMBER = C2_ARABICNUMBER then
      Result := Result + ', ARABIC NUMBER';
    if ACharInfo and C2_COMMONSEPARATOR = C2_COMMONSEPARATOR then
      Result := Result + ', COMMON SEPARATOR';
    if ACharInfo and C2_BLOCKSEPARATOR = C2_BLOCKSEPARATOR then
      Result := Result + ', BLOCK SEPARATOR';
    if ACharInfo and C2_SEGMENTSEPARATOR = C2_SEGMENTSEPARATOR then
      Result := Result + ', SEGMENT SEPARATOR';
    if ACharInfo and C2_WHITESPACE = C2_WHITESPACE then
      Result := Result + ', WHITESPACE';
    if ACharInfo and C2_OTHERNEUTRAL = C2_OTHERNEUTRAL then
      Result := Result + ', OTHER NEUTRAL';
  end;
  if Result <> '' then
    Result := Copy(Result, 2, MaxInt);
end;

function GetTypeString3(AChar: WideChar): WideString;
var
  ACharInfo: word;
begin
  Result := '';
  ACharInfo := 0;
  if GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE3, @AChar, 1, ACharInfo) then
  begin
    if ACharInfo and C3_NONSPACING = C3_NONSPACING then
      Result := Result + ', NON SPACING';
    if ACharInfo and C3_DIACRITIC = C3_DIACRITIC then
      Result := Result + ', DIACRITIC ';
    if ACharInfo and C3_VOWELMARK = C3_VOWELMARK then
      Result := Result + ', VOWELMARK';
    if ACharInfo and C3_SYMBOL = C3_SYMBOL then
      Result := Result + ', SYMBOL';
    if ACharInfo and C3_KATAKANA = C3_KATAKANA then
      Result := Result + ', PUNCTUATION';
    if ACharInfo and C3_HIRAGANA = C3_HIRAGANA then
      Result := Result + ', HIRAGANA';
    if ACharInfo and C3_HALFWIDTH = C3_HALFWIDTH then
      Result := Result + ', HALFWIDTH';
    if ACharInfo and C3_FULLWIDTH = C3_FULLWIDTH then
      Result := Result + ', FULLWIDTH';
    if ACharInfo and C3_IDEOGRAPH = C3_IDEOGRAPH then
      Result := Result + ', IDEOGRAPH';
    if ACharInfo and C3_KASHIDA = C3_KASHIDA then
      Result := Result + ', KASHIDA';
    if ACharInfo and C3_ALPHA = C3_ALPHA then
      Result := Result + ', ALPHA';
  end;
  if Result <> '' then
    Result := Copy(Result, 2, MaxInt);
end;

procedure TForm1.DisplayInfo(AChar:WideChar);
begin
  reInfo.Clear;
  reInfo.Lines.Add('Character Type: ' + GetTypeString1(AChar));
  reInfo.Lines.Add('Bidirectional Layout: ' + GetTypeString2(AChar));
  reInfo.Lines.Add('Text Processing:' + GetTypeString3(AChar));
  reInfo.Lines.Add(Format('Keyboard Code: U+%.4x, Alt+%.4d', [Cardinal(AChar),Cardinal(AChar)]));
  reInfo.Hint := trim(reInfo.Lines.Text);
end;

procedure TForm1.chkUnicodeClick(Sender: TObject);
begin
  cbFilter.Enabled := chkUnicode.Checked;
  if chkUnicode.Checked then
    JM.CharRange.Filter := TJvCharMapUnicodeFilter(cbFilter.ItemIndex)
  else
  begin
    JM.CharRange.Filter := ufUndefined;
    JM.CharRange.EndChar := udEnd.Position;
  end;
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
  JvClipboard.AsWideText := JM.Character;
end;

procedure TForm1.btnSelectClick(Sender: TObject);
begin
  edCharacter.Text := edCharacter.Text + JM.Character;
end;

procedure TForm1.cbColorChange(Sender: TObject);
begin
  if JM <> nil then
    JM.Color := cbColor.ColorValue;
end;

procedure TForm1.cbFontChange(Sender: TObject);
begin
  if JM <> nil then
    JM.Font.Name := cbFont.FontName;
end;

procedure TForm1.FillFilter;
var
  I: TJvCharMapUnicodeFilter;
begin
  cbFilter.Items.BeginUpdate;
  try
    cbFilter.Items.Clear;
    for I := Low(TJvCharMapUnicodeFilter) to High(TJvCharMapUnicodeFilter) do
      cbFilter.Items.Add(GetEnumName(TypeInfo(TJvCharMapUnicodeFilter), Ord(I)));
  finally
    cbFilter.Items.EndUpdate;
  end;
  cbFilter.ItemIndex := Ord(JM.CharRange.Filter);
end;

procedure TForm1.cbFilterClick(Sender: TObject);
begin
  if chkUnicode.Checked and (cbFilter.ItemIndex > -1) then
    JM.CharRange.Filter := TJvCharMapUnicodeFilter(cbFilter.ItemIndex);
  DisplayInfo(JM.Character);
end;

procedure TForm1.FillLocales;
var
  I: Integer;
begin
  cbLocales.Items.BeginUpdate;
  try
    cbLocales.Items.Clear;
    cbLocales.Items.AddObject('System Default',TObject(LOCALE_SYSTEM_DEFAULT));
    cbLocales.Items.AddObject('User Default',TObject(LOCALE_USER_DEFAULT));
    for I := 0 to Languages.Count - 1 do
      cbLocales.Items.AddObject(Languages.Name[I], TObject(Languages.LocaleID[I]));
  finally
    cbLocales.Items.EndUpdate;
  end;
  cbLocales.ItemIndex := cbLocales.Items.IndexOfObject(TObject(JM.Locale));
  cbLocales.Enabled := Win32Platform <> VER_PLATFORM_WIN32_NT;
end;

procedure TForm1.cbLocalesClick(Sender: TObject);
begin
  with cbLocales do
    if ItemIndex > -1 then
      JM.Locale := LCID(Items.Objects[ItemIndex]);
end;

procedure TForm1.DoJMResize(Sender: TObject);
begin
  JM.Left := (ClientWidth - JM.Width) div 2;
  lblChars.Left := JM.Left;
  if lblChars.Left < 8 then
    lblChars.Left := 8;
end;

procedure TForm1.chkShadowClick(Sender: TObject);
begin
  JM.ShowShadow := chkShadow.Checked;
  JM.ShadowSize := Random(4) + 2;
end;

procedure TForm1.DoJMSelectChar(Sender: TObject; AChar: WideChar);
begin
  DisplayInfo(AChar);
end;

procedure TForm1.DoJMValidateChar(Sender: TObject; AChar: WideChar;
  var Valid: Boolean);
begin
  Valid := Valid or chkDisplayAll.Checked;
end;

procedure TForm1.chkDisplayAllClick(Sender: TObject);
begin
  JM.Invalidate;
end;

procedure TForm1.edColsChange(Sender: TObject);
begin
  if not Changing then
  begin
    Changing := True;
    try
      JM.Columns := StrToInt(edCols.Text);
    except
      JM.Columns := 16;
      edCols.Text := '16';
      udColumns.Position := 16;
    end;
    Changing := False;
  end;
end;

procedure TForm1.edEndChange(Sender: TObject);
begin
  if not Changing then
  begin
    Changing := True;
    try
      JM.CharRange.EndChar := StrToInt(edEnd.Text);
    except
      JM.CharRange.EndChar := 255;
      edEnd.Text := '255';
      udEnd.Position := 255;
    end;
    Changing := False;
  end;
end;

procedure TForm1.edStartChange(Sender: TObject);
begin
  if not Changing then
  begin
    Changing := True;
    try
      JM.CharRange.StartChar := StrToInt(edStart.Text);
    except
      JM.CharRange.StartChar := 0;
      edStart.Text := '0';
      udStart.Position := 0;
    end;
    Changing := False;
  end;
end;

end.

