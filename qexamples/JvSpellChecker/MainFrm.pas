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

unit MainFrm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QMenus, QComCtrls, JvQComponent, QTypes, QExtCtrls;

type
  TfrmMain = class(TForm)
    mmMain: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    SpellCheck1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    N2: TMenuItem;
    Saveasimage1: TMenuItem;
    SaveDialog1: TSaveDialog;
    reText: TMemo;
    procedure Open1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure SpellCheck1Click(Sender: TObject);
    procedure Saveasimage1Click(Sender: TObject);
  private
    { Private declarations }
    // FOffset is used to adjust the insert point in the rich edit when the replaced word
    // has a different length than the original word
    FOffset: integer;
    // called when a word should be replaced
    procedure DoReplaceText(Sender: TObject; StartIndex, ALength: integer; const NewText: string);
    // called when a word needs to be highlighted. NB: set HideSelection := false or you
    // won't see the selection in the edit!
    procedure DoSelectText(Sender: TObject; StartIndex, ALength: integer);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  JvSpellCheckerForm;


{$R *.xfm}

procedure TfrmMain.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    reText.Lines.LoadFromFile(OpenDialog1.Filename);
    StatusBar1.Panels[0].Text := '  ' + OpenDialog1.Filename;
  end;
end;

procedure TfrmMain.Close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.SpellCheck1Click(Sender: TObject);
begin
  // set up the spell-checker form
  if not frmSpellChecker.Visible then
  begin
    // StartIndex is 1-based, SelStart is 0-based, so we set intial FOffset to 1
    FOffset := 1;
    // the original text to spell check
    frmSpellChecker.SpellText := reText.Lines.Text;
    // event handler for when a word is to be replaced
    frmSpellChecker.OnReplaceText := DoReplaceText;
    // event handler for when a word needs to be selected
    frmSpellChecker.OnSelectText := DoSelectText;
    frmSpellChecker.Show; // ShowModal also works
  end;
end;

procedure TfrmMain.DoSelectText(Sender: TObject; StartIndex, ALength: integer);
begin
  // just select the text in the rich edit so the user can see were he is
  reText.SelStart := StartIndex - FOffset;
  reText.SelLength := ALength;
end;

procedure TfrmMain.DoReplaceText(Sender: TObject; StartIndex,
  ALength: integer; const NewText: string);
begin
  reText.SelStart := StartIndex - FOffset;
  reText.SelLength := ALength;
  // replace the selected text
  reText.SelText := NewText;
  // adjust offset for next round
  Inc(FOffset, ALength - Length(NewText));
end;

procedure TfrmMain.Saveasimage1Click(Sender: TObject);
var Picture:TPicture;
begin
  if SaveDialog1.Execute then
  begin
    Picture := TPicture.Create;
    try
//      reText.SaveToImage(Picture);
//      Picture.SaveToFile(SaveDialog1.Filename);
    finally
      Picture.Free;
    end;
  end;
end;


end.

