{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInstallerPage.PAS, released on 2001-02-28.

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

unit JvInstallerPage;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvInstaller, JvButtonPersistent, JvComponent;

type
  TAcceptEvent = procedure(Sender: TObject; var Accept: Boolean) of object;

  TJvInstallerButtons = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FNext: TJvButtonPersistent;
    FPrevious: TJvButtonPersistent;
    FCancel: TJvButtonPersistent;
  protected
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure OptionsChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Cancel: TJvButtonPersistent read FCancel write FCancel;
    property Next: TJvButtonPersistent read FNext write FNext;
    property Previous: TJvButtonPersistent read FPrevious write FPrevious;
  end;

  TJvInstallerPage = class(TJvComponent)
  private
    FNextPage: TJvInstallerPage;
    FPreviousPage: TJvInstallerPage;
    FOnPreviousPage: TAcceptEvent;
    FOnNextPage: TAcceptEvent;
    FOnPrevious: TNotifyEvent;
    FOnNext: TNotifyEvent;
    FButtons: TJvInstallerButtons;
    FInstaller: TJvInstaller;
    FOnNextShown: TNotifyEvent;
    FOnPreviousShown: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    procedure SetInstaller(const Value: TJvInstaller);
  protected
    procedure NextClick(Sender: TObject); virtual;
    procedure PreviousClick(Sender: TObject); virtual;
    procedure Changed(Sender: TObject);
    function ImageNotEmpty: Boolean;
    procedure Finish; virtual;
    procedure UpdateButtons; virtual;
  public
    procedure Execute; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnNextPage: TAcceptEvent read FOnNextPage write FOnNextPage;
    property OnPreviousPage: TAcceptEvent read FOnPreviousPage write FOnPreviousPage;
    property OnNextPageShown: TNotifyEvent read FOnNextShown write FOnNextShown;
    property OnPreviousPageShown: TNotifyEvent read FOnPreviousShown write FOnPreviousShown;
    property OnNext: TNotifyEvent read FOnNext write FOnNext;
    property OnPrevious: TNotifyEvent read FOnPrevious write FOnPrevious;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;

    property Buttons: TJvInstallerButtons read FButtons write FButtons;
    property Installer: TJvInstaller read FInstaller write SetInstaller;
    property NextPage: TJvInstallerPage read FNextPage write FNextPage;
    property PreviousPage: TJvInstallerPage read FPreviousPage write FPreviousPage;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvInstallerPage
///////////////////////////////////////////////////////////

procedure TJvInstallerPage.Changed(Sender: TObject);
begin
  UpdateButtons;
end;

{***********************************************}

constructor TJvInstallerPage.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FButtons := TJvInstallerButtons.Create;
  FButtons.OnChanged := Changed;

  //Try to find the installer
  if csDesigning in ComponentState then
    for i := 0 to AOwner.ComponentCount - 1 do
      if AOwner.Components[i] is TJvInstaller then
        SetInstaller(TJvInstaller(AOwner.Components[i]));
end;

{***********************************************}

destructor TJvInstallerPage.Destroy;
begin
  FButtons.Free;
  inherited;
end;

{***********************************************}

procedure TJvInstallerPage.Execute;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

{***********************************************}

procedure TJvInstallerPage.Finish;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

{***********************************************}

function TJvInstallerPage.ImageNotEmpty: Boolean;
begin
  Result := (Installer.Options.Picture.Width <> 0) and
    (Installer.Options.Picture.Height <> 0);
end;

{***********************************************}

procedure TJvInstallerPage.NextClick(Sender: TObject);
var
  Accept: Boolean;
begin
  if Assigned(NextPage) then
  begin
    Accept := True;
    if Assigned(FOnNextPage) then
      FOnNextPage(Self, Accept);
    if Accept then
    begin
      Finish;
      NextPage.Execute;
      if Assigned(FOnNextShown) then
        FOnNextShown(Self);
    end;
  end
  else
  begin
    Finish;
    if Assigned(FOnNext) then
      FOnNext(Self);
  end;
end;

{***********************************************}

procedure TJvInstallerPage.PreviousClick(Sender: TObject);
var
  Accept: Boolean;
begin
  if Assigned(PreviousPage) then
  begin
    Accept := True;
    if Assigned(FOnPreviousPage) then
      FOnPreviousPage(Self, Accept);
    if Accept then
    begin
      Finish;
      PreviousPage.Execute;
      if Assigned(FOnPreviousShown) then
        FOnPreviousShown(Self);
    end;
  end
  else
  begin
    if Assigned(FOnPrevious) then
      FOnPrevious(Self);
    Finish;
  end;
end;

{***********************************************}

procedure TJvInstallerPage.SetInstaller(const Value: TJvInstaller);
begin
  FInstaller := Value;
end;

///////////////////////////////////////////////////////////
// TJvInstallerButtons
///////////////////////////////////////////////////////////

constructor TJvInstallerButtons.Create;
begin
  FNext := TJvButtonPersistent.Create;
  FPrevious := TJvButtonPersistent.Create;
  FCancel := TJvButtonPersistent.Create;

  FNext.OnChanged := OptionsChanged;
  FPrevious.OnChanged := OptionsChanged;
  FCancel.OnChanged := OptionsChanged;

  FNext.Caption := '&Next';
  FPrevious.Caption := '&Previous';
  FCancel.Caption := '&Cancel';
end;

{***********************************************}

destructor TJvInstallerButtons.Destroy;
begin
  FNext.Free;
  FPrevious.Free;
  FCancel.Free;
  inherited;
end;

{***********************************************}

procedure TJvInstallerButtons.OptionsChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{***********************************************}

procedure TJvInstallerPage.UpdateButtons;
begin
  // (rom) TODO?
end;

end.
