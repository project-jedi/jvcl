unit fHTMLParser;

interface

uses
  Windows, Messages, Forms, SysUtils, StdCtrls, Controls, ExtCtrls, JvPanel,
  JvSplitter, JvHtmlParser, JvDisplayMemo, Classes, JvComCtrls, JvButton,
  ComCtrls, JvComponent, JvStatusBar;

type
  TForm1 = class(TForm)
    JvPageControl1: TJvPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    JvTreeView1: TJvTreeView;
    JvStatusBar1: TJvStatusBar;
    TabSheet4: TTabSheet;
    JvSplitter1: TJvSplitter;
    JvDisplayMemo1: TJvDisplayMemo;
    JvPanel1: TJvPanel;
    btnProcessTable: TJvButton;
    JvHtmlParser1: TJvHtmlParser;
    JvDisplayMemo2: TJvDisplayMemo;
    JvDisplayMemo3: TJvDisplayMemo;
    JvDisplayMemo4: TJvDisplayMemo;
    JvPanel2: TJvPanel;
    btnProcessHTML2Text: TJvButton;
    JvPanel3: TJvPanel;
    btnProcessURL: TJvButton;
    JvPanel4: TJvPanel;
    btnProcessTags: TJvButton;
    procedure Button1Click(Sender: TObject);
    procedure TableKeyFound(Sender: TObject; Key, Results, OriginalLine:
      string);
    procedure HTML2TextKeyFound(Sender: TObject; Key, Results, OriginalLine:
      string);
    procedure URLDetectKeyFound(Sender: TObject; Key, Results, OriginalLine:
      string);
    procedure TagsKeyFound(Sender: TObject; Key, Results, OriginalLine: string);
    procedure btnProcessHTML2TextClick(Sender: TObject);
    procedure btnProcessURLClick(Sender: TObject);
    procedure btnProcessTagsClick(Sender: TObject);
  private
    CurNode: TTreeNode;
    FText: string;
    FStartTime: TTime;
  public
    function ReplaceSpecials(Str: string): string;
    procedure ShowStatus(t: TTime);
  end;

var
  Form1: TForm1;

implementation
uses JclStrings;
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
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
    OnKeyFound := Form1.TableKeyFound;
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

procedure TForm1.btnProcessHTML2TextClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('Text', '>', '<');
    OnKeyFound := Form1.HTML2TextKeyFound;
  end;
  try
    JvDisplayMemo2.Lines.BeginUpdate;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
    JvDisplayMemo2.Text := ReplaceSpecials(FText);
  finally
    FText := '';
    JvDisplayMemo2.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnProcessURLClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('URL', 'href=http://', '>');
    AddCondition('URL', 'href="http://', '">');
    OnKeyFound := Form1.URLDetectKeyFound;
  end;
  try
    JvDisplayMemo3.Lines.BeginUpdate;
    JvDisplayMemo2.Clear;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
  finally
    JvDisplayMemo3.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnProcessTagsClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Self.Tag := 0;
  FStartTime := Now();
  with JvHtmlParser1 do
  begin
    ClearConditions;
    AddCondition('URL', '<', '>');
    OnKeyFound := Form1.TagsKeyFound;
  end;
  try
    JvDisplayMemo4.Lines.BeginUpdate;
    JvDisplayMemo4.Clear;
    JvHtmlParser1.AnalyseFile;
    ShowStatus(Now() - FStartTime);
  finally
    JvDisplayMemo4.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.TableKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  Self.Tag := Self.Tag + 1;
  JvDisplayMemo1.Lines.Add(Key + #13#10 + Results);
  if Key = 'TR' then
    CurNode := JvTreeView1.Items.AddChild(nil, 'TR')
  else
    JvTreeView1.Items.AddChild(CurNode, Results);
end;

procedure TForm1.HTML2TextKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  //this is only for sample!
  Self.Tag := Self.Tag + 1;
  if (FText <> '') and (FText[Length(FText)] <> ' ') then
    FText := FText + ' ';
  FText := FText + Results;
end;

procedure TForm1.URLDetectKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  Self.Tag := Self.Tag + 1;
  JvDisplayMemo3.Lines.Add('http://' + Results);
end;

procedure TForm1.TagsKeyFound(Sender: TObject; Key, Results,
  OriginalLine: string);
begin
  Self.Tag := Self.Tag + 1;
  JvDisplayMemo4.Lines.Add('<' + Results + '>');
end;

procedure TForm1.ShowStatus(t: TTime);
var
  h, m, s, ms: Word;
begin
  DecodeTime(t, h, m, s, ms);
  JvStatusBar1.SimpleText :=
    Format('%d tags are processed for period: %0.2d:%0.2d:%0.2d.%0.3d', [Self.Tag,
    h, m, s, ms]);
end;

function TForm1.ReplaceSpecials(Str: string): string;
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

end.

