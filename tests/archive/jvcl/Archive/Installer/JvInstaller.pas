{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInstaller.PAS, released on 2001-02-28.

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

unit JvInstaller;

{*******************************************************}
{  Modifications:                                       }
{    11/10/2000 Added the InstallAsShared method        }
{*******************************************************}

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Jpeg, Registry, JvTrayBar, JvComponent;

// (rom) a JCL candidate

type
  TJvInstallerOptions = class(TPersistent)
  private
    FProg: string;
    FMaximized: Boolean;
    FOnChange: TNotifyEvent;
    FPicture: TPicture;
    procedure SetMaximized(const Value: Boolean);
    procedure SetPicture(const Value: TPicture);
    procedure SetProg(const Value: string);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ProgramName: string read FProg write SetProg;
    property Maximized: Boolean read FMaximized write SetMaximized default False;
    property Picture: TPicture read FPicture write SetPicture;
  end;

  TJvInstaller = class(TJvComponent)
  private
    FForm: TForm;
    FOnCancel: TNotifyEvent;
    FOnLoaded: TNotifyEvent;
    FTray: TJvTrayBar;
    FOptions: TJvInstallerOptions;
  protected
    procedure OptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cancel(Sender: TObject);
    procedure Loaded; override;

    function InstallAsShared(FileName: string): Integer;
  published
    property Options: TJvInstallerOptions read FOptions write FOptions;

    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnLoaded: TNotifyEvent read FOnLoaded write FOnLoaded;

    procedure AddUninstall(Title, Path: string);
  end;

implementation

resourcestring
  RC_SharedDLLs = 'Software\Microsoft\Windows\CurrentVersion\SharedDLLs\';
  RC_UninstallKey = 'Software\Microsoft\Windows\CurrentVersion\Uninstall';
  RC_DisplayName = 'DisplayName';
  RC_UninstString = 'UninstallString';
  RC_OwnerForm = 'Owner must be of type TForm';

{$R RES_Install.res}

  ///////////////////////////////////////////////////////////
  // TJvInstaller
  ///////////////////////////////////////////////////////////

procedure TJvInstaller.AddUninstall(Title, Path: string);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(RC_UninstallKey, False);
    OpenKey(Title, True);
    WriteString(RC_DisplayName, Title);
    WriteString(RC_UninstString, Path);
    Free;
  end;
end;

{***********************************************}

procedure TJvInstaller.Cancel(Sender: TObject);
begin
  if Assigned(FonCancel) then
    FOnCancel(Self);
end;

{***********************************************}

constructor TJvInstaller.Create(AOwner: TComponent);
begin
  inherited;

  if not (Aowner is TForm) then
    raise Exception.Create(RC_OwnerForm)
  else
    FForm := Aowner as TForm;

  FOptions := TJvInstallerOptions.Create;
  // does not load it if it's in design time, cause Delphi would
  // save it in the resources !
  if not (csDesigning in Owner.ComponentState) then
    FOptions.Picture.Bitmap.LoadFromResourceName(HInstance, 'INSTALL');
  FOptions.OnChange := OptionsChanged;

  FTray := TJvTrayBar.Create(Self);
end;

{***********************************************}

destructor TJvInstaller.Destroy;
begin
  FOptions.Free;
  FTray.Free;
  inherited;
end;

{***********************************************}

function TJvInstaller.InstallAsShared(FileName: string): Integer;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    OpenKey(RC_SharedDLLs, True);
    if ValueExists(FileName) then
      Result := ReadInteger(FileName) + 1
    else
      Result := 1;
    WriteInteger(FileName, Result);
    Free;
  end;
end;

{***********************************************}

procedure TJvInstaller.Loaded;
begin
  inherited;
  if Assigned(FOnLoaded) then
    FOnLoaded(Self);
end;

{***********************************************}

procedure TJvInstaller.OptionsChanged(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    if Options.Maximized then
    begin
      FTray.HideTraybar;
      SetWindowLong(FForm.Handle, GWL_STYLE, GetWindowLong(FForm.Handle, GWL_STYLE) and not WS_CAPTION);
      // (rom) Bug or trick?
      FForm.Height := FForm.Height + 1;
      FForm.Height := FForm.Height - 1;
      FForm.Constraints.MinHeight := Screen.Height;
      FForm.Left := 0;
      FForm.Top := 0;
      FForm.Constraints.MinWidth := Screen.Width;
      FForm.WindowState := wsMaximized;
    end
    else
    begin
      FForm.Constraints.MinHeight := 0;
      FForm.Left := 0;
      FForm.Top := 0;
      FForm.Constraints.MinWidth := 0;
      FForm.WindowState := wsNormal;
      FTray.ShowTraybar;
      SetWindowLong(FForm.Handle, GWL_STYLE, GetWindowLong(FForm.Handle, GWL_STYLE) or WS_CAPTION);
      FForm.Height := FForm.Height + 1;
      FForm.Height := FForm.Height - 1;
    end;
  end;
end;

///////////////////////////////////////////////////////////
// TJvInstallerOptions
///////////////////////////////////////////////////////////

constructor TJvInstallerOptions.Create;
begin
  FPicture := TPicture.Create;
  FMaximized := False;
end;

{***********************************************}

destructor TJvInstallerOptions.Destroy;
begin
  FPicture.Free;
  inherited;
end;

{***********************************************}

procedure TJvInstallerOptions.SetMaximized(const Value: Boolean);
begin
  FMaximized := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvInstallerOptions.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvInstallerOptions.SetProg(const Value: string);
begin
  FProg := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
