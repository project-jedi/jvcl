{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit JvColorComboDemoMainFormU;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, JvQColorCombo, QStdCtrls, JvQCombobox, JvQExStdCtrls, QExtCtrls;

type
  TJvColorComboDemoMainForm = class(TForm)
    JvColorComboBox1: TJvColorComboBox;
    memInfo: TMemo;
    btnColorNames: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edNameTemplate: TEdit;
    chkAllowCustom: TCheckBox;
    btnViewCustom: TButton;
    Label3: TLabel;
    cbDisplayStyle: TComboBox;
    btnSaveCustom: TButton;
    btnLoadCustom: TButton;
    Label4: TLabel;
    btnClearCustom: TButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure btnColorNamesClick(Sender: TObject);
    procedure JvColorComboBox1NewColor(Sender: TObject; Color: TColor;
      var DisplayName: string; var AllowAdd: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnViewCustomClick(Sender: TObject);
    procedure cbDisplayStyleChange(Sender: TObject);
    procedure JvColorComboBox1Change(Sender: TObject);
    procedure btnSaveCustomClick(Sender: TObject);
    procedure btnLoadCustomClick(Sender: TObject);
    procedure btnClearCustomClick(Sender: TObject);
  private
    procedure LoadSettings;
    procedure SaveSettings;
  end;

var
  JvColorComboDemoMainForm: TJvColorComboDemoMainForm;

implementation

uses
  IniFiles;

{$R *.xfm}

procedure TJvColorComboDemoMainForm.FormCreate(Sender: TObject);
begin
  LoadSettings;
  cbDisplayStyle.ItemIndex := 1;
  cbDisplayStyleChange(nil);
end;

procedure TJvColorComboDemoMainForm.btnColorNamesClick(Sender: TObject);
begin
  memInfo.Lines := JvColorComboBox1.ColorNameMap;
end;

procedure TJvColorComboDemoMainForm.JvColorComboBox1NewColor(Sender: TObject; Color: TColor;
  var DisplayName: string; var AllowAdd: Boolean);
begin
  // make sure the new color isn't already in the list
  AllowAdd := JvColorComboBox1.FindColor(Color) < 0;
  if AllowAdd then
  begin
    if edNameTemplate.Text <> '' then
    // CustomColorCount isn't incremented until *after* this event has finished with AllowAdd = true, so add 1 here:
      DisplayName := Format(edNameTemplate.Text, [JvColorComboBox1.CustomColorCount + 1]);
  end;
end;

procedure TJvColorComboDemoMainForm.LoadSettings;
var CurColor: TColor;
  S: string;
  SL: TStringlist;
  i: integer;
begin
  // load color name map and saved custom colors
  CurColor := JvColorComboBox1.ColorValue;
  try
    S := ChangeFileExt(Application.ExeName, '.ini');
    if FileExists(S) then
    begin
      SL := TStringlist.Create;
      try
        with TIniFile.Create(S) do
        try
          edNameTemplate.Text := ReadString('Settings', 'Custom Template', edNameTemplate.Text);
          ReadSectionValues('Color Names', JvColorComboBox1.ColorNameMap);
          ReadSection('Custom Colors', SL);
          for i := 0 to SL.Count - 1 do
            JvColorComboBox1.AddColor(StringToColor(SL[i]),'');
        finally
          Free;
        end;
      finally      
        SL.Free;
      end;
    end;
  finally
    JvColorComboBox1.ColorValue := CurColor;
    cbDisplayStyleChange(nil);
  end;
end;

procedure TJvColorComboDemoMainForm.SaveSettings;
var i: integer; AList: TList;
begin
  // save color name map and current custom colors
  JvColorComboBox1.Options := JvColorComboBox1.Options - [coCustomColors];
  try
    with TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini')) do
    try
      AList := TList.Create;
      try
        JvColorComboBox1.GetCustomColors(AList);
        EraseSection('Custom Colors');
        for i := 0 to AList.Count - 1 do
          WriteString('Custom Colors', ColorToString(integer(AList[i])), '');
      finally
        AList.Free;
      end;
      WriteString('Settings', 'Custom Template', edNameTemplate.Text);
      // save color map so users can translate them if they wish
      // no need to save the custom names since they are
      // set dynamically at load time (see JvColorComboBox1NewColor)
      EraseSection('Color Names');
      with JvColorComboBox1.ColorNameMap do
        for i := 0 to Count - 1 do
          WriteString('Color Names', Names[i], Values[Names[i]]);
    finally
      Free;
    end;
  finally
    cbDisplayStyleChange(nil);
  end;
end;

procedure TJvColorComboDemoMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveSettings;
end;

procedure TJvColorComboDemoMainForm.btnViewCustomClick(Sender: TObject);
var AList: TList; i: integer;
begin
  AList := TList.Create;
  memInfo.Lines.Clear;
  try
    // the returned TList contains a list of TColor items
    JvColorComboBox1.GetCustomColors(AList);
    for i := 0 to AList.Count - 1 do
      memInfo.Lines.Add(ColorToString(integer(AList[i])));
  finally
    AList.Free;
  end;
end;

procedure TJvColorComboDemoMainForm.cbDisplayStyleChange(Sender: TObject);
var O: TJvColorComboOptions;
begin
  O := JvColorComboBox1.Options;
  O := O - [coText, coHex, coRGB, coCustomColors];
  if chkAllowCustom.Checked then
    Include(O, coCustomColors);
  case cbDisplayStyle.ItemIndex of
    1:
      Include(O, coText);
    2:                               
      Include(O, coHex);
    3:
      Include(O, coRGB);
  end;
  JvColorComboBox1.Options := O;
end;

procedure TJvColorComboDemoMainForm.JvColorComboBox1Change(
  Sender: TObject);
begin
  Caption := Format('Color: %s',[ColorToString(JvColorComboBox1.ColorValue)]);
end;

procedure TJvColorComboDemoMainForm.btnSaveCustomClick(Sender: TObject);
var
  AList: TList; i: integer;
  S:TStringlist;
begin
  AList := TList.Create;
  S := TStringlist.Create;
  try
    // the returned TList contains a list of TColor items
    JvColorComboBox1.GetCustomColors(AList);
    for i := 0 to AList.Count - 1 do
      S.Add(IntToStr(integer(AList[i])));
    S.SaveToFile(ChangeFileExt(Application.ExeName,'.col'));
  finally
    AList.Free;
  end;
end;

procedure TJvColorComboDemoMainForm.btnLoadCustomClick(Sender: TObject);
var
  i: integer;
  S:TStringlist;
begin
  if FileExists(ChangeFileExt(Application.ExeName,'.col')) then
  begin
    // remove custom colors
    JvColorComboBox1.Options := JvColorComboBox1.Options - [coCustomColors];
    JvColorComboBox1.GetColors;
    JvColorComboBox1.Options := JvColorComboBox1.Options + [coCustomColors];
    S := TStringlist.Create;
    try
      S.LoadFromFile(ChangeFileExt(Application.ExeName,'.col'));
      for i := 0 to S.Count - 1 do
        JvColorComboBox1.AddColor(StrToIntDef(S[i],0),'');
    finally
      S.Free;
    end;
  end;
end;

procedure TJvColorComboDemoMainForm.btnClearCustomClick(Sender: TObject);
begin
  // remove custom colors
  JvColorComboBox1.Options := JvColorComboBox1.Options - [coCustomColors];
  JvColorComboBox1.GetColors;
  JvColorComboBox1.Options := JvColorComboBox1.Options + [coCustomColors];
end;

end.

