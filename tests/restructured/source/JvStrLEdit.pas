{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrLEdit.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{ Tstrings property editor originally from the Rx library: duplicated for internal use) }
unit JvStrLEdit;

interface

uses Windows, Classes, Graphics, Forms, Controls, Buttons, Dialogs,
  StdCtrls, ExtCtrls, {$IFDEF Delphi6_UP}DesignIntf, DesignEditors
  {$ELSE}DsgnIntf{$ENDIF};

type
  TStrEditDlg = class(TForm)
    Memo:TMemo;
    LineCount:TLabel;
    OpenDialog:TOpenDialog;
    SaveDialog:TSaveDialog;
    OKBtn:TButton;
    CancelBtn:TButton;
    HelpBtn:TButton;
    LoadBtn:TButton;
    SaveBtn:TButton;
    procedure FileOpen(Sender:TObject);
    procedure FileSave(Sender:TObject);
    procedure UpdateStatus(Sender:TObject);
    procedure FormCreate(Sender:TObject);
    procedure MemoKeyDown(Sender:TObject; var Key:Word;
      Shift:TShiftState);
    procedure HelpBtnClick(Sender:TObject);
  private
    SingleLine:string[15];
    MultipleLines:string[15];
    FFilename:string;
  end;

implementation
uses SysUtils, LibHelp;

{$R *.DFM}

resourcestring
  STextFilter = 'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|Batch files (*.BAT)|*.BAT|All files (*.*)|*.*';
    //  STextFilter = 'Text files (*.txt)|*.txt|Config files (*.sys;*.ini)|*.sys;*.ini|Batch files (*.bat)|*.bat|All files (*.*)|*.*';

  { TStrListEditDlg }

procedure TStrEditDlg.FileOpen(Sender:TObject);
begin
  with OpenDialog do
  begin
    Filter := STextFilter;
    Filename := FFilename;
    if Execute then
    begin
      FFilename := Filename;
      Memo.Lines.LoadFromFile(FileName);
    end;
  end;
end;

procedure TStrEditDlg.FileSave(Sender:TObject);
begin
  if SaveDialog.FileName = '' then
    SaveDialog.FileName := FFilename;
  with SaveDialog do
  begin
    Filter := STextFilter;
    if Execute then
    begin
//      FFilename := Filename;
      Memo.Lines.SaveToFile(FileName);
    end;
  end;
end;

procedure TStrEditDlg.UpdateStatus(Sender:TObject);
var
  Count:Integer;
begin
  Count := Memo.Lines.Count;
  if Count = 1 then
    LineCount.Caption := Format('%d %s', [Count, SingleLine])
  else
    LineCount.Caption := Format('%d %s', [Count, MultipleLines]);
end;

procedure TStrEditDlg.FormCreate(Sender:TObject);
begin
  HelpContext := hcDStringListEditor;
  OpenDialog.HelpContext := hcDStringListLoad;
  SaveDialog.HelpContext := hcDStringListSave;
  SingleLine := 'Line';
  MultipleLines := 'Lines';
end;

procedure TStrEditDlg.MemoKeyDown(Sender:TObject; var Key:Word;
  Shift:TShiftState);
begin
  if Key = VK_ESCAPE then CancelBtn.Click;
end;

procedure TStrEditDlg.HelpBtnClick(Sender:TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.

