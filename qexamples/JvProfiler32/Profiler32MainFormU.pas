{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLookOut.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

unit Profiler32MainFormU;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QComCtrls, JvQComponent, JvQProfilerForm;

type
  TProfiler32MainForm = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    UseIdBtn: TButton;
    UseNameBtn: TButton;
    ResultBtn: TButton;
    Label1: TLabel;
    Progress: TProgressBar;
    P: TJvProfiler;
    procedure FormCreate(Sender: TObject);
    procedure ResultBtnClick(Sender: TObject);
    procedure UseNameBtnClick(Sender: TObject);
    procedure UseIdBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FTerminated:boolean;
  end;

var
  Profiler32MainForm: TProfiler32MainForm;

implementation

{$R *.xfm}
const
  DefCaption = 'JvProfiler 32 Test program';

procedure TProfiler32MainForm.FormCreate(Sender: TObject);
begin
  P.Names := ListBox1.Items;
  P.Sorted := true;
  P.Enabled := true;
  FTerminated := false;
end;

procedure TProfiler32MainForm.ResultBtnClick(Sender: TObject);
begin
  P.ShowReport;
end;

procedure TProfiler32MainForm.UseNameBtnClick(Sender: TObject);
var i,j,k:integer;
begin
  Randomize;
  { just randomize to get some results }
  Screen.Cursor := crHourGlass;
  UseIdBtn.Enabled := false;
  UseNameBtn.Enabled := false;
  ResultBtn.Enabled := false;
  P.Start;
  try
    k := Random(133);
    Progress.Max := k;
    for j := 0 to k do
    begin
      Progress.Position := j;
      Caption := Format('%s - to do: %d',[DefCaption,k - j]);
      i := Random(ListBox1.Items.Count);
      { use integer ID (Names[i] ID = i) }
      P.EnterID(i);
      Sleep(random(333));
      P.ExitID(i);
      Application.ProcessMessages;
      if FTerminated then
        Break;
    end;
  finally
    Screen.Cursor := crDefault;
    UseIdBtn.Enabled := true;
    UseNameBtn.Enabled := true;
    ResultBtn.Enabled := true;
  end;
  P.Stop;
  Beep;
  Progress.Position := 0;
end;

procedure TProfiler32MainForm.UseIdBtnClick(Sender: TObject);
var i,j,k:integer;
begin
  Randomize;
  P.Start;
  { make distributed randomize to get some results }
  Screen.Cursor := crHourGlass;
  UseIdBtn.Enabled := false;
  UseNameBtn.Enabled := false;
  ResultBtn.Enabled := false;
  try
    k := Random(100);
    Progress.Max := k;
    for j := 0 to k do
    begin
      Progress.Position := j;
      Caption := Format('%s - to do: %d',[DefCaption,k - j]);
      i := Random(ListBox1.Items.Count);
      { use string ID instead }
      P.EnterName(P.Names[i]);
      Sleep(10 * j);
      P.ExitName(P.Names[i]);
      Application.ProcessMessages;
      if FTerminated then
        Break;
    end;
  finally
    Screen.Cursor := crDefault;
    UseIdBtn.Enabled := true;
    UseNameBtn.Enabled := true;
    ResultBtn.Enabled := true;
  end;
  P.Stop;
  Beep;
  Progress.Position := 0;
end;

procedure TProfiler32MainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Fterminated := true;
  P.Enabled := false;
  P.Stop;
end;

procedure TProfiler32MainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then FTerminated := true;
end;

end.
