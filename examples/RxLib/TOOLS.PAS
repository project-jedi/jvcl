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

unit Tools;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, TabNotBk, ExtCtrls, JvDualList,
  JvFormPlacement, JvComponent, JvBaseDlg, JvCalc, JvPicClip,
  JvSecretPanel, Spin, Mask, JvToolEdit, JvClipboardViewer, ComCtrls,
  JvExExtCtrls;

type
  TToolsForm = class(TForm)
    TabbedNotebook1: TTabbedNotebook;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Button1: TButton;
    GroupBox2: TGroupBox;
    ClipboardViewer1: TJvClipboardViewer;
    Label8: TLabel;
    Label9: TLabel;
    ComboBox1: TComboBox;
    GroupBox3: TGroupBox;
    GroupBox5: TGroupBox;
    Image1: TImage;
    Image3: TImage;
    Label13: TLabel;
    ComboBox2: TComboBox;
    Label17: TLabel;
    ListBox1: TListBox;
    Label21: TLabel;
    Button2: TButton;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    PicClip1: TJvPicClip ;
    Label27: TLabel;
    CheckBox1: TCheckBox;
    Label30: TLabel;
    Label31: TLabel;
    SpinEdit1: TSpinEdit;
    Label32: TLabel;
    Bevel1: TBevel;
    Image4: TImage;
    SecretPanel1: TJvSecretPanel ;
    Button3: TButton;
    Label34: TLabel;
    ComboBox5: TComboBox;
    Image5: TImage;
    Label22: TLabel;
    Image2: TImage;
    Label2: TLabel;
    Label3: TLabel;
    GroupBox4: TGroupBox;
    ComboEdit1: TJvComboEdit ;
    rxCalculator1: TJvCalculator ;
    Image6: TImage;
    Label4: TLabel;
    FormStorage1: TJvFormStorage ;
    PicImage: TImage;
    DualListDialog1: TJvDualListDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure ClipboardViewer1Change(Sender: TObject);
    procedure ComboEdit1ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SecretPanel1DblClick(Sender: TObject);
  private
    { Private declarations }
    procedure PopulateListBox1;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses
  Main, Clipbrd, JvJCLUtils;

procedure TToolsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TToolsForm.Button1Click(Sender: TObject);
var
  P: TPoint;
begin
  P := Point(MainForm.Toolbar.Left + 50, MainForm.Toolbar.Top + 6);
  P := MainForm.ClientToScreen(P);
  MainForm.ToolbarMenu.Popup(P.X, P.Y);
end;

procedure TToolsForm.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: Clipboard.AsText := MainForm.Caption;
    1:
      begin
        Clipboard.Assign(Image5.Picture);
      end;
    2: CopyIconToClipboard(Application.Icon, clWindow);
    3: Clipboard.SetComponent(Button1);
  end;
end;

procedure TToolsForm.FormShow(Sender: TObject);
begin
  ComboBox2.ItemIndex := 0;
  PopulateListBox1;
  SpinEdit1Change(Self);
  ComboBox5.ItemIndex := 2;
end;

procedure TToolsForm.ComboBox2Change(Sender: TObject);
var
  V: TClipboardViewFormat;
begin
  if ComboBox2.ItemIndex >= 0 then begin
    V := ClipboardFormatToView(Clipboard.Formats[
      Integer(ComboBox2.Items.Objects[ComboBox2.ItemIndex])]);
    ClipboardViewer1.ViewFormat := V;
  end;
end;

procedure TToolsForm.Button2Click(Sender: TObject);
begin
  DualListDialog1.Execute;
  PopulateListBox1;
end;

procedure TToolsForm.PopulateListBox1;
begin
  ListBox1.Clear;
  ListBox1.Items.AddStrings(DualListDialog1.List2);
end;

procedure TToolsForm.CheckBox1Click(Sender: TObject);
begin
  PicImage.Visible := CheckBox1.Checked;
end;

procedure TToolsForm.SpinEdit1Change(Sender: TObject);
begin
  Image4.Picture.Bitmap := PicClip1.GraphicCell[SpinEdit1.Value];
end;

procedure TToolsForm.Button3Click(Sender: TObject);
begin
  if ComboBox5.ItemIndex > 1 then
    SecretPanel1.Font.Size := 11
  else
    SecretPanel1.Font.Size := 10;
  SecretPanel1.Active := True;
end;

procedure TToolsForm.ComboBox5Change(Sender: TObject);
begin
  SecretPanel1.GlyphLayout := TGlyphLayout(ComboBox5.ItemIndex);
end;

procedure TToolsForm.ClipboardViewer1Change(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  ComboBox2.Items.Clear;
  ComboBox2.Items.AddObject('Auto', TObject(-1));
  for I := 0 to Clipboard.FormatCount do begin
    S := ClipboardViewer1.ClipboardFormatNames[I];
    if S <> '' then ComboBox2.Items.AddObject(S, TObject(I));
  end;
  ComboBox2.ItemIndex := 0;
end;

procedure TToolsForm.ComboEdit1ButtonClick(Sender: TObject);
begin
  try
    rxCalculator1.Value := StrToFloat(ComboEdit1.Text);
  except
    ComboEdit1.Text := '0';
  end;
  if rxCalculator1.Execute then
    ComboEdit1.Text := FloatToStr(rxCalculator1.Value);
end;

procedure TToolsForm.FormCreate(Sender: TObject);
begin
  PicImage.Picture := PicClip1.Picture;
end;

procedure TToolsForm.SecretPanel1DblClick(Sender: TObject);
begin
  if SecretPanel1.Active then SecretPanel1.Active := False;
end;

initialization
  {- to copy button to clipboard }
  RegisterClasses([TButton]);
end.
