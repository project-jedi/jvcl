{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxStrLEdit.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvxStrLEdit;

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} Classes,
  Graphics, Forms, Controls, Buttons, Dialogs,
  {$IFDEF Delphi6_Up}RTLConsts,DesignIntf, VCLEditors, DesignEditors,{$ELSE}DsgnIntf,{$ENDIF}
  StdCtrls, ExtCtrls;

type
  TJvStrEditDlg = class(TForm)
    Memo: TMemo;
    LineCount: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    LoadBtn: TButton;
    SaveBtn: TButton;
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure UpdateStatus(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HelpBtnClick(Sender: TObject);
  private
    SingleLine: string[15];
    MultipleLines: string[15];
  end;

implementation

{$R *.DFM}

uses SysUtils, LibHelp;

{ TStrListEditDlg }

procedure TJvStrEditDlg.FileOpen(Sender: TObject);
begin
  with OpenDialog do
    if Execute then Memo.Lines.LoadFromFile(FileName);
end;

procedure TJvStrEditDlg.FileSave(Sender: TObject);
begin
  SaveDialog.FileName := OpenDialog.FileName;
  with SaveDialog do
    if Execute then Memo.Lines.SaveToFile(FileName);
end;

procedure TJvStrEditDlg.UpdateStatus(Sender: TObject);
var
  Count: Integer;
begin
  Count := Memo.Lines.Count;
  if Count = 1 then
    LineCount.Caption := Format('%d %s', [Count, SingleLine])
  else
    LineCount.Caption := Format('%d %s', [Count, MultipleLines]);
end;

procedure TJvStrEditDlg.FormCreate(Sender: TObject);
begin
  HelpContext := hcDStringListEditor;
  OpenDialog.HelpContext := hcDStringListLoad;
  SaveDialog.HelpContext := hcDStringListSave;
  SingleLine := 'Line';
  MultipleLines := 'Lines';
end;

procedure TJvStrEditDlg.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then CancelBtn.Click;
end;

procedure TJvStrEditDlg.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
