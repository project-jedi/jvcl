{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author:
   Remko Bonte

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

unit ConsoleExampleMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponent, JvCreateProcess, StdCtrls, Menus, ExtCtrls;

type
  TConsoleExampleMainForm = class(TForm)
    JvCreateProcess1: TJvCreateProcess;
    lsbConsole: TListBox;
    Panel1: TPanel;
    edtCommand: TEdit;
    PopupMenu1: TPopupMenu;
    mnuFont: TMenuItem;
    mnuClear: TMenuItem;
    btnExecute: TButton;
    Label1: TLabel;
    mnuBackgroundColor: TMenuItem;
    N1: TMenuItem;
    procedure edtCommandKeyPress(Sender: TObject; var Key: Char);
    procedure JvCreateProcess1Read(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
    procedure JvCreateProcess1Terminate(Sender: TObject;
      ExitCode: Cardinal);
    procedure FormShow(Sender: TObject);
    procedure mnuClearClick(Sender: TObject);
    procedure mnuFontClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuBackgroundColorClick(Sender: TObject);
    procedure edtCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FCommands: TStringList;
    FCurrentCommandIndex: Integer;
    function GetCurrentCommand: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddCommand(const ACommand: string);
    procedure AddNewLine(const S: string);
    procedure ChangeLastLine(const S: string);
    procedure ClearScreen;
    procedure ColorDialog;
    procedure ExecuteCommand;
    procedure FontDialog;
    procedure GotoCommand(const Delta: Integer);
    procedure StartCommandProcessor;
    property CurrentCommand: string read GetCurrentCommand;
  end;

var
  ConsoleExampleMainForm: TConsoleExampleMainForm;

implementation

uses
  JclSysInfo, JvDSADialogs;

{$R *.dfm}

resourcestring
  sProcessTerminated = 'Process "%s" terminated, ExitCode: %.8x';

procedure TConsoleExampleMainForm.AddCommand(const ACommand: string);
begin
  if not SameText(CurrentCommand, ACommand) then
    FCurrentCommandIndex := FCommands.Add(ACommand);
end;

procedure TConsoleExampleMainForm.AddNewLine(const S: string);
begin
  with lsbConsole do
  begin
    Items.Add(S);
    ItemIndex := Count - 1;
  end;
end;

procedure TConsoleExampleMainForm.btnExecuteClick(Sender: TObject);
begin
  ExecuteCommand;
end;

procedure TConsoleExampleMainForm.ChangeLastLine(const S: string);
begin
  with lsbConsole do
  begin
    if Count > 0 then
      Items[Count - 1] := S
    else
      AddNewLine(S);
    ItemIndex := Count - 1;
  end;
end;

procedure TConsoleExampleMainForm.ClearScreen;
begin
  lsbConsole.Clear;
end;

procedure TConsoleExampleMainForm.ColorDialog;
begin
  with TColorDialog.Create(Application) do
  try
    Color := lsbConsole.Color;
    if Execute then
    begin
      lsbConsole.Color := Color;
      { Force refresh of the window, maybe can be done more elegant? }
      lsbConsole.Perform(CM_RECREATEWND, 0, 0);
    end;
  finally
    Free;
  end;
end;

constructor TConsoleExampleMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommands := TStringList.Create;
  FCurrentCommandIndex := -1;
end;

destructor TConsoleExampleMainForm.Destroy;
begin
  FCommands.Free;
  inherited Destroy;
end;

procedure TConsoleExampleMainForm.edtCommandKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    ExecuteCommand;
    Key := #0;
  end;
end;

procedure TConsoleExampleMainForm.ExecuteCommand;
begin
  AddCommand(edtCommand.Text);
  JvCreateProcess1.WriteLn(edtCommand.Text);
  edtCommand.SelectAll;
end;

procedure TConsoleExampleMainForm.FontDialog;
begin
  with TFontDialog.Create(Application) do
  try
    Font := lsbConsole.Font;
    if Execute then
    begin
      lsbConsole.Font := Font;
      { Force refresh of the window, maybe can be done more elegant? }
      lsbConsole.Perform(CM_RECREATEWND, 0, 0);
    end;
  finally
    Free;
  end;
end;

procedure TConsoleExampleMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
const
  cButtonStrs: array [Boolean] of string = ('Close', 'Cancel');
  cButtonResults: array [Boolean] of Integer = (mrYes, mrNo);
begin
  { Application can be closed, if no console is running, or if the user
    confirms that the application should be closed }
  CanClose :=
    (JvCreateProcess1.State = psReady) or
    (JvDSADialogs.MessageDlgEx('Command processor is still running, close anyway?',
     mtConfirmation, cButtonStrs, cButtonResults, 0) = mrYes);
end;

procedure TConsoleExampleMainForm.FormShow(Sender: TObject);
begin
  StartCommandProcessor;
end;

function TConsoleExampleMainForm.GetCurrentCommand: string;
begin
  if (FCurrentCommandIndex >= 0) and (FCurrentCommandIndex < FCommands.Count) then
    Result := FCommands[FCurrentCommandIndex]
  else
    Result := '';
end;

procedure TConsoleExampleMainForm.GotoCommand(const Delta: Integer);
begin
  Inc(FCurrentCommandIndex, Delta);

  if FCurrentCommandIndex >= FCommands.Count then
    FCurrentCommandIndex := FCommands.Count - 1
  else
  if FCurrentCommandIndex < 0 then
    FCurrentCommandIndex := 0
  else
  begin
    edtCommand.Text := FCommands[FCurrentCommandIndex];
    edtCommand.SelectAll;
  end
end;

procedure TConsoleExampleMainForm.JvCreateProcess1Read(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
  // $0C is the Form Feed char.
  if S = #$C then
    ClearScreen
  else
  if StartsOnNewLine then
    AddNewLine(S)
  else
    ChangeLastLine(S);
end;

procedure TConsoleExampleMainForm.JvCreateProcess1Terminate(Sender: TObject;
  ExitCode: Cardinal);
begin
  AddNewLine(Format(sProcessTerminated, [JvCreateProcess1.CommandLine, ExitCode]));
end;

procedure TConsoleExampleMainForm.mnuBackgroundColorClick(Sender: TObject);
begin
  ColorDialog;
end;

procedure TConsoleExampleMainForm.mnuClearClick(Sender: TObject);
begin
  ClearScreen;
end;

procedure TConsoleExampleMainForm.mnuFontClick(Sender: TObject);
begin
  FontDialog
end;

procedure TConsoleExampleMainForm.StartCommandProcessor;
var
  CommandLine: string;
begin
  { Retrieve the command processor name }
  if not JclSysInfo.GetEnvironmentVar('COMSPEC', CommandLine) or (Length(CommandLine) = 0) then
    { Paranoid }
    CommandLine := 'COMMAND.EXE';

  JvCreateProcess1.CommandLine := CommandLine;
  { Redirect console output, we'll receive the output via the OnRead event }
  JvCreateProcess1.ConsoleOptions := JvCreateProcess1.ConsoleOptions + [coRedirect];
  { Hide the console window }
  JvCreateProcess1.StartupInfo.ShowWindow := swHide;
  JvCreateProcess1.StartupInfo.DefaultWindowState := False;
  { And start the console }
  JvCreateProcess1.Run;
end;

procedure TConsoleExampleMainForm.edtCommandKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin
    GotoCommand(1);
    Key := 0;
  end
  else
  if Key = VK_UP then
  begin
    GotoCommand(-1);
    Key := 0;
  end;
end;

end.

