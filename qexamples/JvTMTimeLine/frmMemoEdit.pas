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

unit frmMemoEdit;

interface

uses
  QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls, QExtCtrls, QMenus, QTypes;

type
  TMemoEditFrm = class(TForm)
    Panel1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    popMemo: TPopupMenu;
    Load1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Selectall1: TMenuItem;
    N2: TMenuItem;
    reLines: TMemo;
    procedure Load1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Selectall1Click(Sender: TObject);
    procedure reLinesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    class function Edit(Lines: TStrings;ADate:TDateTime;Icon:TIcon = nil): boolean;
  end;


implementation

{$R *.xfm}

{ TMemoEditFrm }

class function TMemoEditFrm.Edit(Lines: TStrings;ADate:TDateTime;Icon:TIcon = nil): boolean;
var f:TMemoEditFrm;
begin
  f := self.Create(nil);
  try
    f.Icon := Icon;
    f.Caption := Format(f.Caption,[DateToStr(ADate)]);
    f.reLines.Lines := Lines;
    Result := f.ShowModal = MROK;
    if Result then
      Lines.Assign(f.ReLines.Lines);
  finally
    f.Free;
  end;
end;

procedure TMemoEditFrm.Load1Click(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if Execute then
      reLines.Lines.LoadFromFile(Filename);
  finally
    Free;
  end;
end;

procedure TMemoEditFrm.Save1Click(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    if Execute then
      reLines.Lines.SaveToFile(Filename);
  finally
    Free;
  end;
end;

procedure TMemoEditFrm.Cut1Click(Sender: TObject);
begin
  reLines.CutToClipboard;
end;

procedure TMemoEditFrm.Copy1Click(Sender: TObject);
begin
  reLines.CopyToClipboard;
end;

procedure TMemoEditFrm.Paste1Click(Sender: TObject);
begin
  reLines.PasteFromClipboard;
end;

procedure TMemoEditFrm.Selectall1Click(Sender: TObject);
begin
  reLines.SelectAll;
end;

procedure TMemoEditFrm.reLinesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.

