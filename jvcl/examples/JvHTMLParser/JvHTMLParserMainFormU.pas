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

unit JvHTMLParserMainFormU;

interface

uses
  Windows, Messages, Forms, SysUtils, StdCtrls, Controls, ExtCtrls, JvPanel,
  JvSyncSplitter, JvHtmlParser, Classes, JvComCtrls, JvButton,
  ComCtrls, JvComponent, JvStatusBar, JvMemo, JvCtrls, JvExComCtrls,
  JvExStdCtrls, JvExExtCtrls, JvSplitter, Dialogs, JclSysInfo;

type
  TJvHTMLParserMainForm = class(TForm)
    JvPageControl1: TJvPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    JvTreeView1: TJvTreeView;
    JvStatusBar1: TJvStatusBar;
    TabSheet4: TTabSheet;
    JvSplitter1: TJvSyncSplitter;
    JvDisplayMemo1: TJvMemo;
    JvPanel1: TJvPanel;
    btnProcessTable: TJvImgBtn;
    JvHtmlParser1: TJvHtmlParser;
    JvDisplayMemo2: TJvMemo;
    JvDisplayMemo3: TJvMemo;
    JvDisplayMemo4: TJvMemo;
    JvPanel2: TJvPanel;
    btnProcessHTML2Text: TJvImgBtn;
    JvPanel3: TJvPanel;
    btnProcessURL: TJvImgBtn;
    JvPanel4: TJvPanel;
    btnProcessTags: TJvImgBtn;
    btnOpen: TButton;
    OpenDialog1: TOpenDialog;
    procedure btnProcessTableClick(Sender: TObject);
    procedure TableKeyFound(Sender: TObject; Key, Results, OriginalLine: string);
    procedure HTML2TextKeyFound(Sender: TObject; Key, Results, OriginalLine: string);
    procedure URLDetectKeyFound(Sender: TObject; Key, Results, OriginalLine: string);
    procedure TagsKeyFound(Sender: TObject; Key, Results, OriginalLine: string);
    procedure btnProcessHTML2TextClick(Sender: TObject);
    procedure btnProcessURLClick(Sender: TObject);
    procedure btnProcessTagsClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    CurNode: TTreeNode;
    FText: string;
    FStartTime: TTime;
  public
    function ReplaceSpecials(Str: string): string;
    procedure ShowStatus(t: TTime);
    procedure DoKeyFoundEx(Sender: TObject; Key, Results, OriginalLine: string; TagInfo:TTagInfo; Attributes:TStrings);
  end;

var
  JvHTMLParserMainForm: TJvHTMLParserMainForm;

implementation

uses
  JclStrings;

{$R *.dfm}

procedure TJvHTMLParserMainForm.btnProcessTableClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('TD', '<TD>', '</TD>');
    AddCondition('TH', '<TH>', '</TH>');
    AddCondition('TR', '<TR>', '</TR>');
    OnKeyFound := TableKeyFound;
  end;
  try
    JvTreeView1.Items.BeginUpdate;
    JvDisplayMemo1.Clear;
    JvDisplayMemo1.Lines.BeginUpdate;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
  finally
    JvTreeView1.Items.EndUpdate;
    JvDisplayMemo1.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TJvHTMLParserMainForm.btnProcessHTML2TextClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('Text', '>', '<');
    OnKeyFound := self.HTML2TextKeyFound;
  end;
  JvDisplayMemo2.Lines.BeginUpdate;
  try
    JvDisplayMemo2.Clear;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
    JvDisplayMemo2.Text := ReplaceSpecials(FText);
  finally
    FText := '';
    JvDisplayMemo2.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TJvHTMLParserMainForm.btnProcessURLClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('URL', 'href=http://', '>');
    AddCondition('URL', 'href="http://', '">');
    OnKeyFound := self.URLDetectKeyFound;
  end;
  JvDisplayMemo3.Lines.BeginUpdate;
  try
    JvDisplayMemo3.Clear;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
  finally
    JvDisplayMemo3.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TJvHTMLParserMainForm.btnProcessTagsClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('URL', '<', '>');
    OnKeyFound := nil;
    OnKeyFoundEx := DoKeyFoundEx;
  end;
  JvDisplayMemo4.Lines.BeginUpdate;
  try
    JvDisplayMemo4.Clear;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
  finally
    JvDisplayMemo4.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TJvHTMLParserMainForm.TableKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  Self.Tag := Self.Tag + 1;
  JvDisplayMemo1.Lines.Add(Key + #13#10 + Results);
  if Key = 'TR' then
    CurNode := JvTreeView1.Items.AddChild(nil, 'TR')
  else
    JvTreeView1.Items.AddChild(CurNode, Results);
end;

procedure TJvHTMLParserMainForm.HTML2TextKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  //this is only for sample!
  Self.Tag := Self.Tag + 1;
  if (FText <> '') and (FText[Length(FText)] <> ' ') then
    FText := FText + ' ';
  FText := FText + Results;
end;

procedure TJvHTMLParserMainForm.URLDetectKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  Self.Tag := Self.Tag + 1;
  JvDisplayMemo3.Lines.Add('http://' + Results);
end;

procedure TJvHTMLParserMainForm.TagsKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  Self.Tag := Self.Tag + 1;
  JvDisplayMemo4.Lines.Add('<' + Results + '>');
end;

procedure TJvHTMLParserMainForm.ShowStatus(t: TTime);
var
  h, m, s, ms: Word;
begin
  DecodeTime(t, h, m, s, ms);
  JvStatusBar1.SimpleText :=
    Format('%d tags are processed for period: %0.2d:%0.2d:%0.2d.%0.3d',
     [Self.Tag, h, m, s, ms]);
end;

function TJvHTMLParserMainForm.ReplaceSpecials(Str: string): string;
begin
  Result := Str;
  StrReplace(Result, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]);
  StrReplace(Result, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]);
  StrReplace(Result, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  StrReplace(Result, '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
  StrReplace(Result, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
  StrReplace(Result, '&copy;', #169, [rfReplaceAll, rfIgnoreCase]);
  // add more here...
end;

procedure TJvHTMLParserMainForm.btnOpenClick(Sender: TObject);
var
  I: Integer;
begin
  if OpenDialog1.Execute then
  begin
    JvHtmlParser1.FileName := OpenDialog1.FileName;
    for I := 0 to ComponentCount - 1 do
      if (Components[I] <> btnOpen) and (Components[I] is TButton) then
        TButton(Components[I]).Click;
  end;
end;

procedure TJvHTMLParserMainForm.FormCreate(Sender: TObject);
begin
  JvHtmlParser1.OnKeyFoundEx := DoKeyFoundEx;
end;

procedure TJvHTMLParserMainForm.DoKeyFoundEx(Sender: TObject; Key, Results,
  OriginalLine: string; TagInfo: TTagInfo; Attributes: TStrings);
begin
  Self.Tag := Self.Tag + 1;
  JvDisplayMemo4.Lines.Add('<' + Results + '>');
  if Attributes.Count > 0 then
  begin
    JvDisplayMemo4.Lines.Add('Attributes:');
    JvDisplayMemo4.Lines.AddStrings(Attributes);
  end;

end;

end.

