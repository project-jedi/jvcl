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

unit JvSpellCheckerForm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, JvQSpellChecker, QActnList;

type
  { This is an example form with code that shows how to implement a spell check form
    that can be displayed to the end user. The TJvSpellChecker is created dynamically
    so you don't need to install it to run the demo: just make sure the JvSpellChecker and
    JvSpellIntf units are somewhere in your path.

    The main tasks of this form is to:
      * Scan for the next misspelled word (GetNextWord)
      * Display the misspelled word along with suggested replacements
      * Call an event handler to highlight the text in the original control
      * Call an event handler when the user wants to replace the word
      * Add a word to the user dictionary (btnAdd)
      * Add a word to the ignore list (btnIgnoreAll)

    This form doesn't implement everything needed for a professional looking form (i.e only
      enable buttons as needed) but it can serve as a base for a more complete implementation.
  }

  TJvReplaceTextEvent = procedure(Sender: TObject; StartIndex, ALength: integer; const NewText: string) of object;
  TJvSelectTextEvent = procedure(Sender: TObject; StartIndex, ALength: integer) of object;
  TfrmSpellChecker = class(TForm)
    Label1: TLabel;
    edNewWord: TEdit;
    Label2: TLabel;
    lbSuggestions: TListBox;
    btnIgnore: TButton;
    btnIgnoreAll: TButton;
    btnChange: TButton;
    btnClose: TButton;
    btnAdd: TButton;
    GroupBox1: TGroupBox;
    chkUpperCase: TCheckBox;
    chkNumber: TCheckBox;
    chkURL: TCheckBox;
    chkHTML: TCheckBox;
    lblNoSuggestions: TLabel;
    Label3: TLabel;
    edBadWord: TEdit;
    alSpell: TActionList;
    acIgnore: TAction;
    acIgnoreAll: TAction;
    acChange: TAction;
    acAdd: TAction;
    acClose: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acIgnoreExecute(Sender: TObject);
    procedure acIgnoreAllExecute(Sender: TObject);
    procedure acChangeExecute(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure alSpellUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure lbSuggestionsClick(Sender: TObject);
  private
    { Private declarations }
    FStartIndex, FLength: integer;
    ASpellChecker: TJvSpellChecker;
    FOnReplaceText: TJvReplaceTextEvent;
    FOnSelectText: TJvSelectTextEvent;
    procedure CloseAndReport(ReportSuccess: boolean);
    function GetNextWord: boolean;
    procedure DoReplaceText(Sender: TObject; StartIndex, ALength: integer; const NewText: string);
    procedure DoSelectText(Sender: TObject; StartIndex, ALength: integer);
    function GetSpellText: string;
    procedure SetSpellText(const Value: string);
    procedure DoCanIgnore(Sender: TObject; const Value: string; var CanIgnore: boolean);
    procedure CheckSuggestions;
  public
    { Public declarations }
    property SpellText: string read GetSpellText write SetSpellText;
    property OnReplaceText: TJvReplaceTextEvent read FOnReplaceText write FOnReplaceText;
    property OnSelectText: TJvSelectTextEvent read FOnSelectText write FOnSelectText;
  end;

var
  frmSpellChecker: TfrmSpellChecker;

implementation

{$R *.xfm}

procedure TfrmSpellChecker.FormCreate(Sender: TObject);
var S:string;
begin
  ASpellChecker := TJvSpellChecker.Create(self);
  // Dictionaries are plain text files, one word per row, preferably sorted.
  // If you don't load a dictionary, all words are misspelled and you won't get any suggestions
  S := ExtractFilePath(Application.ExeName) + 'english.dic';
  if not FileExists(S) then
    {$IFDEF MSWINDOWS}
    {$ENDIF MSWINDOWS}
    {$IFDEF MSWINDOWS}
    S := '..\..\Dict\english.dic';
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    S := '../../Dict/english.dic';
    {$ENDIF LINUX}
  if FileExists(S) then
    ASpellChecker.Dictionary := S
  else
    ShowMessage('Dictionary file not found: make sure you have an english.dic file in the exe folder!'); 
//  ASpellChecker.UserDictionary.LoadFromFile(Application.ExeName + 'custom.dic'); // you need to create this
  // set up a custom ignore filter:
  ASpellChecker.OnCanIgnore := DoCanIgnore;
end;

procedure TfrmSpellChecker.DoReplaceText(Sender: TObject; StartIndex, ALength: integer; const NewText: string);
begin
  // this events calls back to the main form where the content of the rich edit is updated
  if Assigned(FOnReplaceText) then
    FOnReplaceText(self, StartIndex, ALength, NewText);
end;

function TfrmSpellChecker.GetSpellText: string;
begin
  Result := ASpellChecker.Text;
end;

procedure TfrmSpellChecker.SetSpellText(const Value: string);
begin
  ASpellChecker.Text := Value;
end;

procedure TfrmSpellChecker.CheckSuggestions;
begin
  if lbSuggestions.Items.Count = 0 then
  begin
    lblNoSuggestions.Parent := lbSuggestions;
    lblNoSuggestions.Top := 4;
    lblNoSuggestions.Left := (lbSuggestions.ClientWidth - lblNoSuggestions.Width) div 2;
    lblNoSuggestions.Visible := true;
  end
  else
    lblNoSuggestions.Visible := false;
end;

function TfrmSpellChecker.GetNextWord: boolean;
begin
  // scan for the next misspelled word. Returns false if no more misspelled words are found
  Result := false;
  while ASpellChecker.SpellChecker.Next(FStartIndex, FLength) do
  begin
    edBadWord.Text := '';
    edNewWord.Text := '';
    Result := FLength > 0;
    if Result then
    begin
      edBadWord.Text := Copy(ASpellChecker.Text, FStartIndex, FLength);
      lbSuggestions.Items := ASpellChecker.SpellChecker.Suggestions;
      if lbSuggestions.Items.Count > 0 then
      begin
        edNewWord.Text := lbSuggestions.Items[0];
        lbSuggestions.ItemIndex := 0;
      end
      else
        edNewWord.Text := edBadWord.Text;
      edNewWord.SetFocus;
    end;
    CheckSuggestions;
    Exit;
  end;
end;

procedure TfrmSpellChecker.FormShow(Sender: TObject);
begin
  if GetNextWord then
    DoSelectText(self, FStartIndex, FLength)
  else
    CloseAndReport(false);
end;

procedure TfrmSpellChecker.DoSelectText(Sender: TObject; StartIndex,
  ALength: integer);
begin
  // this events calls back to the main form where the selection in the rich edit is updated
  if Assigned(FOnSelectText) then FOnSelectText(self, StartIndex, ALength);
end;

procedure TfrmSpellChecker.DoCanIgnore(Sender: TObject; const Value: string;
  var CanIgnore: boolean);
var
  i: integer;
begin
  // custom event to manage some of the options in the dialog

  // always ignore words shorter than four letter
  if Length(Value) < 4 then
  begin
    CanIgnore := true;
    Exit;
  end;

  // make some additional checks on the current word to determine if we need to spellcheck it
  if chkUpperCase.Checked and (AnsiUpperCase(Value) = Value) then // ignore all UPPERCASE words
  begin
    CanIgnore := true;
    Exit;
  end;

  if chkNumber.Checked then // ignore words that contains numbers
    for i := 1 to Length(Value) do
      if (Value[i] in ['0'..'9', '#', '%']) then
      begin
        CanIgnore := true;
        Exit;
      end;
  if chkURL.Checked then // ignore URL's and file paths (this code is in no way 100% effective...)
    for i := 1 to Length(Value) do
      if (Value[i] in [':', '/', '\']) then
      begin
        CanIgnore := true;
        Exit;
      end;
  if chkHTML.Checked then // ignore HTML tags (this code is in no way 100% effective...)
    CanIgnore := (Length(Value) < 2) or ((Value[1] = '<') or (Value[Length(Value)] = '>')) or
    ((Value[1] = '&') and (Value[Length(Value)] = ';'));
end;

procedure TfrmSpellChecker.CloseAndReport(ReportSuccess: boolean);
var
  S: string;
begin
  if ReportSuccess then
    S := 'Spell check completed!'
  else
    S := 'There is nothing to spell check';
  ShowMessage(S);
  // delay since we might have been called from the OnShow event (can't close in OnShow)
  Release; //PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TfrmSpellChecker.acIgnoreExecute(Sender: TObject);
begin
  // ignore = skip to next word but don't remember the word we just saw
  if GetNextWord then
    DoSelectText(self, FStartIndex, FLength)
  else
    CloseAndReport(true)
end;

procedure TfrmSpellChecker.acIgnoreAllExecute(Sender: TObject);
begin
  // ignore all = add to ignore list so it will be skipped in the future as well
  ASpellChecker.SpellChecker.Ignores.Add(AnsiLowerCase(edBadWord.Text));
  if GetNextWord then
    DoSelectText(self, FStartIndex, FLength)
  else
    CloseAndReport(true);
end;

procedure TfrmSpellChecker.acChangeExecute(Sender: TObject);
begin
  // replace the current selection with the word in the edit
  DoReplaceText(self, FStartIndex, FLength, edNewWord.Text);
  if GetNextWord then
    DoSelectText(self, FStartIndex, FLength)
  else
    CloseAndReport(true)
end;

procedure TfrmSpellChecker.acAddExecute(Sender: TObject);
begin
  // Add the misspelled word to the user dictionary. To persist, you must add code to call
  // UserDictionary.SaveToFile() at close down as well as UserDictionary.LoadFromFile() at start up.
  ASpellChecker.SpellChecker.UserDictionary.Add(edBadWord.Text);
  edNewWord.Text := edBadWord.Text;
  // change the word as well 
  if not acChange.Execute then
  begin
    // move on
    if GetNextWord then
      DoSelectText(self, FStartIndex, FLength)
    else
      CloseAndReport(true);
  end;
end;

procedure TfrmSpellChecker.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmSpellChecker.alSpellUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acIgnore.Enabled := edBadWord.Text <> '';
  acIgnoreAll.Enabled := acIgnore.Enabled;
  acChange.Enabled := not AnsiSameText(edBadWord.Text, edNewWord.Text);
  acAdd.Enabled := (edBadWord.Text <> '') and (ASpellChecker.UserDictionary.IndexOf(edBadWord.Text) < 0);
end;

procedure TfrmSpellChecker.lbSuggestionsClick(Sender: TObject);
begin
  with lbSuggestions do
    if ItemIndex > -1 then
      edNewWord.Text := Items[ItemIndex];
end;

end.

