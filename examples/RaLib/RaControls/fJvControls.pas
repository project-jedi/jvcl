{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit fJvControls;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, JvComponentPanel, StdCtrls, ComCtrls,
  JvButtons, Buttons, JvScrollMax, Mask, DBCtrls, JvBDEMove, 
  JvProgressComponent, JvHtControls, JvaScrollText, ImgList, JvComponent, JvCaptionButton,
  JvExControls, JvExExtCtrls, JvExStdCtrls, JvExButtons, JvComponentBase,
  JvAppStorage, JvAppIniStorage, JvExtComponent;

type
  TMainForm  = class(TForm)
    PopupMenu1: TPopupMenu;
    Item1: TMenuItem;
    Item2: TMenuItem;
    TabControl1: TTabControl;
    RAComponentPanel1: TJvComponentPanel;
    Memo1: TMemo;
    RACaptionButton1: TJvCaptionButton;
    RACaptionButton2: TJvCaptionButton;
    RACaptionButton3: TJvCaptionButton;
    ImageList1: TImageList;
    Notebook1: TNotebook;
    Panel2: TPanel;
    Label20: TLabel;
    RAScrollMax1: TJvScrollMax;
    RAScrollMaxBand1: TJvScrollMaxBand;
    Label11: TLabel;
    Label12: TLabel;
    Label16: TLabel;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit14: TEdit;
    RAScrollMaxBand2: TJvScrollMaxBand;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    RAScrollMaxBand3: TJvScrollMaxBand;
    Label17: TLabel;
    Label18: TLabel;
    Edit15: TEdit;
    Edit16: TEdit;
    RAColorButton1: TJvaColorButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RAScrollMax2: TJvScrollMax;
    RAScrollMaxBand4: TJvScrollMaxBand;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    RAScrollMaxBand5: TJvScrollMaxBand;
    Label10: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    RAScrollMaxBand6: TJvScrollMaxBand;
    Label23: TLabel;
    Label24: TLabel;
    Edit8: TEdit;
    Edit17: TEdit;
    CheckBox1: TCheckBox;
    Label19: TLabel;
    Label25: TLabel;
    ListBox1: TListBox;
    RAhtListBox1: TJvhtListBox;
    Memo2: TMemo;
    Button1: TButton;
    CheckBox2: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    RAProgressForm1: TJvProgressComponent;
    Label26: TLabel;
    RAhtComboBox1: TJvHTComboBox;
    RAScrollText1: TJvaScrollText;
    Label27: TLabel;
    RAhtLabel1: TJvHTLabel;
    RAhtButton1: TJvHTButton;
    RAhtLabel2: TJvHTLabel;
    JvAppIniFileStorage1: TJvAppIniFileStorage;
    procedure RAComponentPanel1Click(Sender: TObject; Button: Integer);
    procedure TabControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RACaptionButton1Click(Sender: TObject);
    procedure RANoFrameButton1Click(Sender: TObject);
    procedure RAColorButton1Click(Sender: TObject);
    procedure RAImage1KeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RAProgressForm1Show(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadHelpText;
    procedure LoadScrollText;
    procedure LoadScrollImages;
  end;

var
  MainForm: TMainForm ;

implementation

uses JvJCLUtils, JvJVCLUtils;

{$R *.DFM}

{$R JvStdCtrlsReg.dcr}
{$R JvDlgsReg.dcr}
{$R JvCustomReg.dcr}
{$R JvControlsReg.dcr}
{$R JvDBReg.dcr}
{$R JvBDEReg.dcr}
//{$R rai.dcr}
{$R res.dcr}

procedure TMainForm .TabControl1Change(Sender: TObject);
const
  sRAControls = 'TJvEditor,TJvHLEditor,TJvHLEdPropDlg,'+
                'TJvScrollMax,TJvaScrollText,'+
                'TJvhtListBox,TJvHTComboBox,TJvHTButton,TJvHTLabel,'+
                'TJvCaptionButton,TJvProgressComponent,TJvComponentPanel,' +
                //'TJvInterpreterProgram,TJvInterpreterFm,' +
                'TJvBDESQLScript,TJvDBTreeView,TJvDBLookupTreeView,'+
                'TJvDBLookupTreeViewCombo,TJvDBMove';
var
  Comps : string;
  S : string;
  i : integer;
begin
  RAScrollText1.Stop;
  case TabControl1.TabIndex of
    0 :
      begin
        Comps := sRAControls;
        RAComponentPanel1.ButtonCount := 20;
      end;
   { 2 :
      begin
        Comps := sRADB;
        RAComponentPanel1.ButtonCount := 7;
      end; }
    else
      begin
        Comps := '';
      end;
  end;
 // RAComponentPanel1.RecreateButtons;
  RAComponentPanel1.FirstVisible := 0;
  i := 0;
  S := SubStrBySeparator(Comps, i, ',');
  while S <> '' do
  begin
    RAComponentPanel1.Buttons[i].Hint := S;
    try
      RAComponentPanel1.Buttons[i].Glyph.LoadFromResourceName(hInstance, S);
    except
    end;
    inc(i);
    S := SubStrBySeparator(Comps, i, ',');
  end;
end;

procedure TMainForm .FormCreate(Sender: TObject);
var
  hRgn  : Windows.HRGN;
begin
  Application.Title := Caption;

  LoadHelpText;
  LoadScrollImages;
  LoadScrollText;

  RAScrollMax1.MouseClasses([TLabel]);
  TabControl1Change(nil);
  RAComponentPanel1Click(nil, 0);
  RAhtComboBox1.ItemIndex := 0;
  with RAScrollText1 do
  begin
    hRgn := CreateRoundRectRgn(0, 0, Width, Height, Width div 6, Width div 6);
    if (hRgn = 0) or not BOOL(SetWindowRgn(Handle, hRgn, true)) then {fail};
  end;    { with }
  RAhtLabel1.Caption :=
    '<b><c:Red>TJvHTLabel</b><c:WindowText> can displays caption with few font styles.'#13+
    'But only <u>one</u> font size allowed.'#13+
    '<u>WordWrap</u> not supported, but multiline text can'#13+
    'be showed, if caption contains <b>#13</b> characters.';
end;

procedure TMainForm .RAComponentPanel1Click(Sender: TObject; Button: Integer);
begin
  Memo1.Lines.Clear;
  if Button < 0 then exit;

  Memo1.Lines.BeginUpdate;
  try
    Memo1.Lines.Clear;
    JvAppIniFileStorage1.ReadStringList(RAComponentPanel1.Buttons[Button].Hint+'\Descript', Memo1.Lines);
  finally
    Memo1.Lines.EndUpdate;
  end;

  RAScrollText1.Stop;
  NoteBook1.ActivePage := Trim(JvAppIniFileStorage1.ReadString(RAComponentPanel1.Buttons[Button].Hint+'\Page', ''));
  Memo1.Visible := JvAppIniFileStorage1.ReadBoolean(RAComponentPanel1.Buttons[Button].Hint+'\Memo', True);
  if NoteBook1.ActivePage = 'JvaScrollText' then
    RAScrollText1.Scroll;
end;

procedure TMainForm .RACaptionButton1Click(Sender: TObject);
begin
  ShowMessage('JvCaptionButton displays glyph');
end;

procedure TMainForm .RANoFrameButton1Click(Sender: TObject);
begin
  ShowMessage('JvNoFrameButton has not border');
end;

procedure TMainForm .RAColorButton1Click(Sender: TObject);
begin
  ShowMessage('JvColorButton has color');
end;

procedure TMainForm .RAImage1KeyPress(Sender: TObject; var Key: Char);
begin
  ShowMessage('Key "'+Key+'" pressed !');
end;

procedure TMainForm .CheckBox1Click(Sender: TObject);
begin
  RAScrollMax1.OneExpanded := CheckBox1.Checked;
  RAScrollMax1.AutoHeight := CheckBox1.Checked;
end;

procedure TMainForm .Button1Click(Sender: TObject);
begin
  ListBox1.Items := Memo2.Lines;
  RAhtListBox1.Items := Memo2.Lines;
  RAhtComboBox1.Items := Memo2.Lines;
end;

procedure TMainForm .CheckBox2Click(Sender: TObject);
begin
  RAhtListBox1.HideSel := CheckBox2.Checked;
  RAhtComboBox1.HideSel := CheckBox2.Checked;
end;

procedure TMainForm .Button2Click(Sender: TObject);
begin
  ShowMessage(RAhtListBox1.PlainItems[0]);
end;

procedure TMainForm .RAProgressForm1Show(Sender: TObject);
begin
  RAProgressForm1.ProgressPosition := RAProgressForm1.ProgressMin;
  while RAProgressForm1.ProgressPosition < RAProgressForm1.ProgressMax do
  begin
    Sleep(50);
    RAProgressForm1.InfoLabel := 'Long operation in progress: item'+ IntToStr(Random(100));
    RAProgressForm1.ProgressPosition := RAProgressForm1.ProgressPosition + 1;
    Application.ProcessMessages;
    if RAProgressForm1.Cancel then Exit;
  end;    { while }
end;

procedure TMainForm .Button3Click(Sender: TObject);
begin
  RAProgressForm1.Execute;
end;

procedure TMainForm .FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  RAScrollText1.Stop;
end;

procedure TMainForm.LoadHelpText;
var
  Res: TResourceStream;
  HelpText: TStringList;
begin
  Res := TResourceStream.Create(HInstance, 'HELPTEXT', 'TEXT');
  try
    HelpText := TStringList.Create;
    try
      HelpText.LoadFromStream(Res);
      { This is a trick to initialize the ini storage with a TStringList instead
        of initializing it with a file }
      JvAppIniFileStorage1.IniFile.SetStrings(HelpText);
    finally
      HelpText.Free;
    end;
  finally
    Res.Free;
  end;
end;

procedure TMainForm.LoadScrollText;
begin
  RAScrollText1.Lines.Clear;
  JvAppIniFileStorage1.ReadStringList('\TJvScrollText\ScrollText', RAScrollText1.Lines);
end;

procedure TMainForm.LoadScrollImages;
begin
  RAScrollText1.ForeImage.Bitmap.Handle := LoadBitmap(HInstance, 'FOREIMAGE');
  RAScrollText1.BackImage.Bitmap.Handle := LoadBitmap(HInstance, 'BACKIMAGE');
end;

end.