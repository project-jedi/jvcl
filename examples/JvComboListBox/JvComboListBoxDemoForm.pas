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

unit JvComboListBoxDemoForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvComponent, JvClipboardViewer, ExtCtrls, ExtDlgs,
  ComCtrls, Menus, JvComboListBox, JvExForms;

type

  TJvComboListBoxDemoFrm = class(TForm)
    JvClipboardViewer1: TJvClipboardViewer;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    pnlImage: TPanel;
    Image1: TImage;
    btnLoadImage: TButton;
    btnCopyImage: TButton;
    btnCopyText: TButton;
    edItemHeight: TEdit;
    udItemHeight: TUpDown;
    cbDrawStyle: TComboBox;
    edButtonWidth: TEdit;
    udButtonWidth: TUpDown;
    btnLoadText: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    PopupMenu1: TPopupMenu;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Original1: TMenuItem;
    Stretch1: TMenuItem;
    Proportional1: TMenuItem;
    OpenDialog1: TOpenDialog;
    chkHotTrackCombo: TCheckBox;
    edColumns: TEdit;
    Label5: TLabel;
    udColumns: TUpDown;
    chkInsert: TCheckBox;
    cbPopupAlign: TComboBox;
    Label6: TLabel;
    chkCustomDrop: TCheckBox;
    chkIncludeFiles: TCheckBox;
    procedure JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
    procedure JvClipboardViewer1Text(Sender: TObject; Text: string);
    procedure btnCopyTextClick(Sender: TObject);
    procedure btnCopyImageClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure udItemHeightClick(Sender: TObject; Button: TUDBtnType);
    procedure FormCreate(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure cbDrawStyleChange(Sender: TObject);
    procedure Proportional1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure udButtonWidthClick(Sender: TObject; Button: TUDBtnType);
    procedure btnLoadTextClick(Sender: TObject);
    procedure chkHotTrackComboClick(Sender: TObject);
    procedure udColumnsClick(Sender: TObject; Button: TUDBtnType);
    procedure cbPopupAlignChange(Sender: TObject);
    procedure chkIncludeFilesClick(Sender: TObject);
  private
    LB: TJvComboListBox;
    procedure DoDropDown(Sender: TObject; Index, X, Y: integer; var AllowDrop:boolean);
    procedure DoAccept(Sender: TObject; Index: integer; const Value: string);
  public
  end;

var
  JvComboListBoxDemoFrm: TJvComboListBoxDemoFrm;

implementation

uses
  Math, Clipbrd, DropFrm;

{$R *.dfm}

function Max(Values: array of integer): integer;
var
  i: integer;
begin
  Result := Values[Low(Values)];
  for i := Low(Values) + 1 to High(Values) do
    if Values[i] > Result then
      Result := Values[i];
end;

procedure TJvComboListBoxDemoFrm.JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
var
  P: TPicture;
begin
  P := TPicture.Create;
  try
    P.Assign(Image);
    if chkInsert.Checked then
      LB.InsertImage(0, P)
    else
      LB.AddImage(P);
  finally
    P.Free; // AddImage creates a copy, so we can free this instance
  end;
//  LB.ItemHeight := Max(LB.ItemHeight, B.Height + 8);
  udItemHeight.Position := LB.ItemHeight;
  Caption := Format('Count: %d', [LB.Items.Count]);
end;

procedure TJvComboListBoxDemoFrm.JvClipboardViewer1Text(Sender: TObject; Text: string);
begin
  if chkInsert.Checked then
    LB.InsertText(0, StringReplace(Text, #13#10, ' ', [rfReplaceAll]))
  else
    LB.AddText(StringReplace(Text, #13#10, ' ', [rfReplaceAll]));
  Caption := Format('Clipboard count: %d', [LB.Items.Count]);
end;

procedure TJvComboListBoxDemoFrm.btnCopyTextClick(Sender: TObject);
begin
  if Memo1.SelLength = 0 then
    Memo1.SelectAll;
  Memo1.CopyToClipboard;
end;

procedure TJvComboListBoxDemoFrm.btnCopyImageClick(Sender: TObject);
var
  AFormat: Word;
  AData: Cardinal;
  APalette: HPALETTE;
  il: TImageList;
begin
  if Image1.Picture.Graphic is TIcon then
  begin
    // convert ico to bmp
    il := TImageList.CreateSize(Image1.Picture.Width, Image1.Picture.Height);
    try
      il.AddIcon(Image1.Picture.Icon);
      il.GetBitmap(0, Image1.Picture.Bitmap);
    finally
      il.Free;
    end;
  end;
  Image1.Picture.SaveToClipboardFormat(AFormat, AData, APalette);
  Clipboard.SetAsHandle(AFormat, AData);
end;

procedure TJvComboListBoxDemoFrm.btnLoadImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TJvComboListBoxDemoFrm.udItemHeightClick(Sender: TObject; Button: TUDBtnType);
begin
  LB.ItemHeight := udItemHeight.Position;
end;

procedure TJvComboListBoxDemoFrm.DoAccept(Sender:TObject;Index:integer; const Value:string);
begin
  if Index < 0 then Index := LB.ItemIndex;
  if Index >= 0 then
  begin
    LB.Items.Objects[Index].Free;
    LB.Items.Objects[Index] := nil;
    LB.Items[Index] := Value;
  end;
end;

procedure TJvComboListBoxDemoFrm.DoDropDown(Sender: TObject;Index, X,Y:integer; var AllowDrop:boolean);
var
  R:TRect;
  P:TPoint;
begin
  AllowDrop := not chkCustomDrop.Checked;
  if not AllowDrop then
  begin
    R := LB.ItemRect(Index);
    P := LB.ClientToScreen(Point(R.Right, R.Top));
    if frmDrop = nil then
      frmDrop := TfrmDrop.Create(Application);
    with frmDrop do
    begin
      IncludeFiles := chkIncludeFiles.Checked;
      Top := P.Y + LB.ItemHeight;
      Left := P.X - Width;
      // notify dialog when the user clicks outside the form
      OnAccept := DoAccept;
      Show;
    end;
  end;
end;

procedure TJvComboListBoxDemoFrm.FormCreate(Sender: TObject);
begin
  LB := TJvComboListBox.Create(Self);
  LB.Align := alClient;
  LB.Width := 200;
  LB.Parent := Self;
  LB.DropDownMenu := PopupMenu1;
  LB.OnDropDown := DoDropDown;
//  LB.ScrollBars := ssBoth;
//  LB.HotTrack := true;
  Splitter1.Left := LB.Left - 10;
  cbDrawStyle.ItemIndex := Ord(LB.DrawStyle);
  cbPopupAlign.ItemIndex := Ord(PopupMenu1.Alignment);
  LB.ItemHeight := udItemHeight.Position;
  udButtonWidth.Position := LB.ButtonWidth;
  udColumns.Position := LB.Columns;
end;

procedure TJvComboListBoxDemoFrm.Paste1Click(Sender: TObject);
begin
  with LB do
  begin
    if Items.Objects[ItemIndex] <> nil then
      Image1.Picture.Assign(TPicture(Items.Objects[ItemIndex]))
    else
      Memo1.Lines.Text := Items[ItemIndex];
  end;
end;

procedure TJvComboListBoxDemoFrm.Delete1Click(Sender: TObject);
begin
  with LB do
    if ItemIndex >= 0 then
      Delete(ItemIndex);
  Caption := Format('Clipboard count: %d', [LB.Items.Count]);
end;

procedure TJvComboListBoxDemoFrm.cbDrawStyleChange(Sender: TObject);
begin
  LB.DrawStyle := TJvComboListBoxDrawStyle(cbDrawStyle.ItemIndex);
end;

procedure TJvComboListBoxDemoFrm.Proportional1Click(Sender: TObject);
begin
  cbDrawStyle.ItemIndex := (Sender as TMenuItem).Tag;
  LB.DrawStyle := TJvComboListBoxDrawStyle(cbDrawStyle.ItemIndex);
  (Sender as TMenuItem).Checked := true;
end;

procedure TJvComboListBoxDemoFrm.PopupMenu1Popup(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to PopupMenu1.Items.Count - 1 do
    if PopupMenu1.Items[i].GroupIndex = 1 then
      PopupMenu1.Items[i].Checked := PopupMenu1.Items[i].Tag = cbDrawStyle.ItemIndex;
end;

procedure TJvComboListBoxDemoFrm.udButtonWidthClick(Sender: TObject; Button: TUDBtnType);
begin
  LB.ButtonWidth := udButtonWidth.Position;
end;

procedure TJvComboListBoxDemoFrm.btnLoadTextClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TJvComboListBoxDemoFrm.chkHotTrackComboClick(Sender: TObject);
begin
  LB.HotTrackCombo := chkHotTrackCombo.Checked;
end;

procedure TJvComboListBoxDemoFrm.udColumnsClick(Sender: TObject; Button: TUDBtnType);
begin
  LB.Columns := udColumns.Position;
  if LB.Columns > 0 then
    LB.ScrollBars := ssHorizontal
  else
    LB.ScrollBars := ssVertical;
end;

procedure TJvComboListBoxDemoFrm.cbPopupAlignChange(Sender: TObject);
begin
  PopupMenu1.Alignment := TPopupAlignment(cbPopupAlign.ItemIndex);
end;

procedure TJvComboListBoxDemoFrm.chkIncludeFilesClick(Sender: TObject);
begin
  if frmDrop <> nil then //recreate form
  begin
    frmDrop.Release;
    frmDrop := nil;
  end;
end;

end.

