{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUninstaller.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvUninstaller;

{*******************************************************}
{  Modifications:                                       }
{    1/10/2000  Added the function DeleteFromScript     }
{    2/10/2000  Added the | in the DeleteFromScript,    }
{               Registry-Key uninstall                  }
{   10/10/2000  Added the function UninstallShared      }
{*******************************************************}

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, Jpeg, FileCtrl,
  JvFormUninst, JvTypes, JvInstaller, JvInstallerPage, JvButtonPersistent, JvComponent;

type
  TJvUninstallerButtons = class(TPersistent)
  private
    FCancel: TJvButtonPersistent;
    FFinish: TJvButtonPersistent;
    FOnChange: TNotifyEvent;
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure ChangeButtons(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Finish: TJvButtonPersistent read FFinish write FFinish;
    property Cancel: TJvButtonPersistent read FCancel write FCancel;
  end;

  TJvUninstallerOptions = class(TPersistent)
  private
    FProgram: string;
    FFile: string;
    FPath: string;
    FText: string;
    FOnChange: TNotifyEvent;
    FPicture: TPicture;
    procedure SetFile(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetPicture(const Value: TPicture);
    procedure SetProgram(const Value: string);
    procedure SetText(const Value: string);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property Text: string read FText write SetText;
    property PathLabel: string read FPath write SetPath;
    property FileLabel: string read FFile write SetFile;
    property ProgramLabel: string read FProgram write SetProgram;
  end;

  TJvUnInstaller = class(TJvComponent)
  private
    FForm: TFormUnin;
    FOnCancel: TNotifyEvent;
    FOnLoaded: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnQuit: TNotifyEvent;
    FOnFailed: TOnDelete;
    FOnDelete: TOnDelete;
    FButtons: TJvUninstallerButtons;
    FOptions: TJvUninstallerOptions;
    procedure Finish(Sender: TObject);
  protected
    procedure ChangeOptions(Sender: TObject);
    procedure ChangeButtons(Sender: TObject);
    procedure Cancel(Sender: TObject);
    procedure DeleteOneFile(Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;

    procedure UninstallShared(FileName: string);
  published
    property Buttons: TJvUninstallerButtons read FButtons write FButtons;
    property Options: TJvUninstallerOptions read FOptions write FOptions;

    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;
    property OnFinishClick: TNotifyEvent read FOnFinish write FOnFinish;
    property OnQuitClick: TNotifyEvent read FOnQuit write FOnQuit;
    property OnDeleteFile: TOnDelete read FOnDelete write FOnDelete;
    property OnDeleteFailed: TOnDelete read FOnFailed write FOnFailed;

    procedure DeleteFiles(Files: TStringList);
    procedure DeleteDirectory(Path: string; Subdir: Boolean);
    procedure DeleteFromScript(FileName: string);
    procedure Execute;
    procedure RemoveUninstall(Title: string);
    procedure Terminate;
  end;

implementation

resourcestring
  RC_ButtonCaption = '&Quit';
  RC_UninstallKey = 'Software\Microsoft\Windows\CurrentVersion\Uninstall';
  RC_DisplayName = 'DisplayName';
  RC_UninstString = 'UninstallString';

  ///////////////////////////////////////////////////////////
  // TJvUnInstaller
  ///////////////////////////////////////////////////////////

procedure TJvUnInstaller.Cancel(Sender: TObject);
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

{***********************************************}

constructor TJvUnInstaller.Create(AOwner: TComponent);
begin
  inherited;
  FForm := TFormUnin.Create(Application);

  FButtons := TJvUninstallerButtons.Create;
  FButtons.OnChange := ChangeButtons;

  FOptions := TJvUninstallerOptions.Create(Self);
  FOptions.OnChange := ChangeOptions;
end;

{************************************************************}

procedure TJvUnInstaller.ChangeOptions(Sender: TObject);
begin
  FForm.Image1.Picture.Assign(FOptions.Picture);
  if FOptions.Text <> '' then
    FForm.StaticText1.Caption := FOptions.Text;
  FForm.CurrFile.Caption := FOptions.FileLabel;
  FForm.Path.Caption := FOptions.PathLabel;
  FForm.ProgramName.Caption := FOptions.ProgramLabel;
end;

{************************************************************}

procedure TJvUnInstaller.ChangeButtons(Sender: TObject);
begin
  FButtons.Finish.AssignTo(FForm.BUButton2);
  FButtons.Cancel.AssignTo(FForm.BUButton3);
end;

{************************************************************}

procedure TJvUnInstaller.DeleteDirectory(Path: string; Subdir: Boolean);
var
  t: TSearchRec;
  res: Integer;
begin
  Application.ProcessMessages;

  if (Path <> '') and (Path[Length(Path)] <> '\') then
    Path := Path + '\';
  res := FindFirst(Path + '*.*', faAnyFile, t);

  while res = 0 do
  begin
    if (t.Name <> '.') and (t.Name <> '..') then
    begin
      if (DirectoryExists(Path + t.Name)) then
      begin
        if (subdir) then
          DeleteDirectory(Path + t.Name, Subdir);
      end
      else
      begin
        Application.ProcessMessages;
        DeleteOneFile(Path + t.Name);
      end;
    end;
    res := FindNext(t);
  end;

  try
    RemoveDir(Path);
  except
  end;
  FindClose(t);
end;

{***********************************************}

procedure TJvUnInstaller.DeleteOneFile(Value: string);
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, Value);
  try
    DeleteFile(Value);
    Options.PathLabel := ExtractFilePath(Value);
    Options.FileLabel := Value;
  except
    if Assigned(FOnFailed) then
      FOnFailed(Self, Value);
  end;
end;

{***********************************************}

procedure TJvUnInstaller.UninstallShared(FileName: string);
var
  i: Integer;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey('Software\Microsoft\Windows\CurrentVersion\SharedDLLs\', True);
    if ValueExists(FileName) then
    begin
      i := ReadInteger(FileName) - 1;
      if i = 0 then
      begin
        DeleteValue(FileName);
        DeleteOneFile(FileName);
      end
      else
        WriteInteger(FileName, i);
    end
    else
      DeleteOneFile(FileName);
    Free;
  end;
end;

{***********************************************}

procedure TJvUnInstaller.DeleteFiles(Files: TStringList);
var
  i: Integer;
begin
  for i := 0 to Files.Count - 1 do
  begin
    Application.ProcessMessages;
    DeleteOneFile(Files[i]);
  end;
end;

{***********************************************}

procedure TJvUnInstaller.DeleteFromScript(FileName: string);
var
  i: Integer;

  procedure ParseAndDelete(Value: string);
  var
    st, st2: string;
    i: Integer;
  begin
    i := Pos(':', Value);
    if (Value = '') or (i = 0) then
      Exit;
    st := Copy(Value, 1, i - 1);
    Value := Copy(Value, i + 1, Length(Value));
    if st = 'FILE' then
      DeleteOneFile(Value)
    else if st = 'DIRECTORY' then
    begin
      try
        RemoveDir(Value);
      except
      end;
    end
    else if st = 'SHARED' then
      UninstallShared(Value)
    else if st = 'REGISTRY-KEY' then
    begin
      try
        with TRegistry.Create do
        begin
          DeleteKey(Value);
          Free;
        end;
      except
      end;
    end
    else if st = 'REGISTRY-VALUE' then
    begin
      try
        with TRegistry.Create do
        begin
          if Pos('|', Value) <> 0 then
          begin
            st := Copy(Value, 1, Pos('|', Value) - 1);
            st2 := Copy(Value, Pos('|', Value) + 1, Length(Value));
          end
          else
          begin
            st := ExtractFilePath(Value);
            st2 := ExtractFileName(Value);
          end;
          OpenKey(st, False);
          DeleteValue(st2);
        end;
      except
      end;
    end;
  end;

begin
  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    for i := 0 to Count - 1 do
    begin
      ParseAndDelete(Strings[i]);
      Application.ProcessMessages;
    end;
    Free;
  end;
end;

{***********************************************}

destructor TJvUnInstaller.Destroy;
begin
  FButtons.Free;
  FOptions.Free;
  inherited;
end;

{***********************************************}

procedure TJvUnInstaller.Execute;
begin
  FForm.buButton3.OnClick := Cancel;
  FForm.buButton2.OnClick := Finish;

  ChangeOptions(nil);
  ChangeButtons(nil);

  FForm.Tag := 0;
  FForm.FormStyle := fsStayOnTop;
  FForm.Show;
end;

{***********************************************}

procedure TJvUnInstaller.Finish(Sender: TObject);
begin
  if FForm.BuButton2.Caption = RC_ButtonCaption then
  begin
    if Assigned(FOnQuit) then
      FOnQuit(Self);
  end
  else if Assigned(FOnFinish) then
    FOnFinish(Self);
end;

{***********************************************}

procedure TJvUnInstaller.Loaded;
begin
  inherited;
  if Assigned(FOnLoaded) then
    FOnLoaded(Self);
end;

{***********************************************}

procedure TJvUnInstaller.RemoveUninstall(Title: string);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(RC_UninstallKey, False);
    if KeyExists(Title) then
    begin
      OpenKey(Title, True);
      if ValueExists(RC_DisplayName) then
        DeleteValue(RC_DisplayName);
      if ValueExists(RC_UninstString) then
        DeleteValue(RC_UninstString);
      closekey;
      OpenKey(RC_UninstallKey, False);
      DeleteKey(Title);
    end;
    Free;
  end;
end;

{***********************************************}

procedure TJvUnInstaller.Terminate;
begin
  FForm.Tag := 1;
  Buttons.FFinish.Caption := RC_ButtonCaption;
end;

///////////////////////////////////////////////////////////
// TJvUninstallerButtons
///////////////////////////////////////////////////////////

procedure TJvUninstallerButtons.ChangeButtons(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

constructor TJvUninstallerButtons.Create;
begin
  FCancel := TJvButtonPersistent.Create;
  FFinish := TJvButtonPersistent.Create;

  FCancel.Caption := '&Cancel';
  FFinish.Caption := '&Finish';

  FCancel.OnChanged := ChangeButtons;
  FFinish.OnChanged := ChangeButtons;
end;

{***********************************************}

destructor TJvUninstallerButtons.Destroy;
begin
  FCancel.Free;
  FFinish.Free;
  inherited;
end;

///////////////////////////////////////////////////////////
// TJvUninstallerOptions
///////////////////////////////////////////////////////////

constructor TJvUninstallerOptions.Create(Owner: TComponent);
begin
  FPicture := TPicture.Create;
  if not (csDesigning in Owner.ComponentState) then
    FPicture.Bitmap.LoadFromResourceName(HInstance, 'INSTALL');
end;

{***********************************************}

destructor TJvUninstallerOptions.Destroy;
begin
  FPicture.Free;
  inherited;
end;

{***********************************************}

procedure TJvUninstallerOptions.SetFile(const Value: string);
begin
  FFile := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvUninstallerOptions.SetPath(const Value: string);
begin
  FPath := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvUninstallerOptions.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvUninstallerOptions.SetProgram(const Value: string);
begin
  FProgram := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvUninstallerOptions.SetText(const Value: string);
begin
  FText := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
