{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Main.pas, released on 2004-05-19.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Mask, JvExMask, JvToolEdit, ExtCtrls, StrUtils;

type
  TFormMain = class(TForm)
    BtnExecute: TButton;
    ProgressBar: TProgressBar;
    LblProgress: TLabel;
    EditOutDir: TJvDirectoryEdit;
    EditSingleFile: TJvFilenameEdit;
    RBtnSingleFile: TRadioButton;
    RBtnDir: TRadioButton;
    BtnQuit: TButton;
    Label1: TLabel;
    Label2: TLabel;
    EditJVCLDir: TJvDirectoryEdit;
    Bevel1: TBevel;
    CheckBoxReduceConditions: TCheckBox;
    CheckBoxKeepLines: TCheckBox;
    CheckBoxUnixLineBreaks: TCheckBox;
    CheckBoxForceOverwrite: TCheckBox;
    ListBox1: TListBox;
    RBtnAll: TRadioButton;
    EditDirectory: TJvDirectoryEdit;
    procedure BtnExecuteClick(Sender: TObject);
    procedure BtnQuitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    function GetQName(const Filename: string): string;
  public
    { Public-Deklarationen }
    procedure DoProgress(Sender: TObject; const Text: string; Position, Max: Integer);
  end;

var
  FormMain: TFormMain;

implementation

uses
  JvclVclClxCvt;

{$R *.dfm}

procedure TFormMain.BtnExecuteClick(Sender: TObject);
var
  Converter: TConverter;
  JVCLConverter: TJVCLConverter;
  Sr: TSearchRec;
  dof, kof: TStrings;
  dir:string;

begin
  ListBox1.Items.Clear;
  dof := TStringList.Create;
  kof := TStringList.Create;
  Converter := TConverter.Create(EditJVCLDir.Text);
  try
    if RBtnSingleFile.Checked or RBtnDir.Checked then
    begin
      JVCLConverter := TJVCLConverter.Create(ExtractFilePath(ParamStr(0)) + 'VclClxData',
        Converter.Model);
      try
          Dir := ExtractFileDir(ParamStr(0)) + PathDelim + 'VclClxData'+ PathDelim;
          if FileExists(Dir + 'qexamples.dof') then
            dof.loadfromfile(Dir + 'qexamples.dof');
          if FileExists(Dir + 'qexamples.kof') then
            kof.loadfromfile(Dir + 'qexamples.kof');
//        JVCLConverter.OnProgress := DoProgress;
        JVCLConverter.OutDirectory := EditOutDir.Text;
        JVCLConverter.ReduceConditions := CheckBoxReduceConditions.Checked;
        JVCLConverter.KeepLines := CheckBoxKeepLines.Checked;
        JVCLConverter.UnixLineBreak := CheckBoxUnixLineBreaks.Checked;
        JVCLConverter.ForceOverwrite := CheckBoxForceOverwrite.Checked;
        ForceDirectories(EditOutDir.Text);
        if RBtnDir.Checked  then
        begin
          if FindFirst(EditDirectory.Text + PathDelim + '*.*', faAnyFile and not faDirectory, Sr) = 0 then
          begin
            repeat
              with JVCLConverter do
              begin
                if (ExtractFileExt(Sr.Name) = '.pas')   then
                begin
                  Listbox1.Items.Add(Sr.name);
                  ParsePasFile(EditDirectory.Text + PathDelim + Sr.Name);
                end
                else if (ExtractFileExt(Sr.Name) = '.dfm') then
                begin
                  if not FileExists(EditOutDir.Text + PathDelim + Sr.Name) then
                  begin
                    Listbox1.Items.Add(Sr.name);
                    ParseDfmFile(EditDirectory.Text + PathDelim + Sr.Name);
                  end;
                end
                else if ExtractFileExt(Sr.Name) = '.dpr' then
                begin
                  Listbox1.Items.Add(Sr.name);
                  ParsePasFile(EditDirectory.Text + PathDelim + Sr.Name);
                  dof.SaveToFile(EditOutDir.Text + PathDelim +
                                 ChangeFileExt(GetQName(Sr.Name), '.dof'));
                  kof.SaveToFile(EditOutDir.Text + PathDelim +
                                  ChangeFileExt(GetQName(Sr.Name), '.kof'));
                end;
              end;
            until FindNext(sr) <> 0;
            FindClose(sr);
          end;
        end
        else
        begin
          JVCLConverter.ParsePasFile(EditSingleFile.Text);
        end;
        ShowMessage('Finished.');
      finally
        JVCLConverter.Free;
      end;
    end
    else
    begin
      Converter.OnProgress := DoProgress;
      Converter.Cvt.ReduceConditions := CheckBoxReduceConditions.Checked;
      Converter.Cvt.KeepLines := CheckBoxKeepLines.Checked;
      Converter.Cvt.UnixLineBreak := CheckBoxUnixLineBreaks.Checked;
      Converter.Cvt.ForceOverwrite := CheckBoxForceOverwrite.Checked;

      Converter.CreateClxFiles;
    end;
  finally
    Converter.Free;
  end;
  dof.Free;
  kof.Free;
end;

procedure TFormMain.DoProgress(Sender: TObject; const Text: string; Position,
  Max: Integer);
begin
  LblProgress.Caption := Text;
  ProgressBar.Max := Max;
  ProgressBar.Position := Position;
  ListBox1.Items.Add(Text);
  Application.ProcessMessages;
end;

procedure TFormMain.BtnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Dir, LastDir: string;
begin
  LblProgress.Caption := '';

  Dir := ExtractFileDir(ParamStr(0));
  repeat
    if DirectoryExists(Dir + PathDelim + 'packages' + PathDelim + 'd7clx') then
      Break;
    LastDir := Dir;
    Dir := ExtractFileDir(Dir);
    if Dir = LastDir then
      Dir := '';
  until Dir = '';
  if Dir = '' then
    Dir := ExtractFileDir(ParamStr(0));
  EditJVCLDir.Text := Dir;
end;

function TFormMain.GetQName(const Filename: string): string;
var Fn: string;
begin
  Result := ExtractFilePath(Filename);
  Fn := ExtractFileName(Filename);
  if AnsiStartsText('Jv', Fn) then
    Insert('Q', Fn, 3)
  else
    Fn := 'Q' + Fn;
  Result := Result + Fn;
end;


end.
