{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvConfirmCancel.PAS, released on 2001-02-28.

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

unit JvConfirmCancel;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvInstaller, JvFormCancel, JvButtonPersistent ,JvComponent;

type
  TJvConfirmCancelOptions = class(TPersistent)
  private
    FText3: string;
    FText1: string;
    FText2: string;
    FOnChange: TNotifyEvent;
    FExit: TJvButtonPersistent;
    FResume: TJvButtonPersistent;
    procedure SetText1(const Value: string);
    procedure SetText2(const Value: string);
    procedure SetText3(const Value: string);
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    procedure OptionsChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy;override;
  published
    property Text1:string read FText1 write SetText1;
    property Text2:string read FText2 write SetText2;
    property Text3:string read FText3 write SetText3;
    property Resume:TJvButtonPersistent read FResume write FResume;
    property Exit:TJvButtonPersistent read FExit write FExit;
  end;

  TJvConfirmCancel = class(TJvComponent)
  private
    FForm:TFormCanc;
    FInstaller:TJvInstaller;
    FOnResume: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOptions: TJvConfirmCancelOptions;
  protected
    procedure OptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property Installer:TJvInstaller read FInstaller write FInstaller;
    property OnResume:TNotifyEvent read FonResume write FonResume;
    property OnExit:TNotifyEvent read FOnExit write FOnExit;
    property Options:TJvConfirmCancelOptions read FOptions write FOptions;
    procedure Execute;
  end;

implementation

resourcestring
   RC_InstallerFilled       =       'Property "Installer" must be filled';

{***********************************************}
constructor TJvConfirmCancel.Create(AOwner: TComponent);
var
 i:Integer;
begin
  inherited;
  FOptions:=TJvConfirmCancelOptions.Create;
  FOptions.OnChange:=OptionsChanged;

  //Try to find the installer
  if csDesigning in ComponentState then
    for i:=0 to AOwner.ComponentCount-1 do
      if AOwner.Components[i] is TJvInstaller then
        Installer:=TJvInstaller(AOwner.Components[i]);
end;
{***********************************************}
destructor TJvConfirmCancel.Destroy;
begin
  FOptions.Free;
  inherited;
end;
{***********************************************}
procedure TJvConfirmCancel.Execute;
begin
  if FInstaller<>nil then
  begin
    if FForm=nil then
      FForm:=TFormCanc.Create(Application);

    OptionsChanged(nil);

    FForm.Tag:=0;
    FForm.ShowModal;

    if FForm.tag=1 then
    begin
      if Assigned(FOnResume) then FonResume(self);
    end
    else
      if Assigned(FonExit) then FonExit(self);
  end
  else
      raise Exception.Create(RC_InstallerFilled);
end;
{***********************************************}
procedure TJvConfirmCancel.OptionsChanged(Sender: TObject);
begin
  if FForm<>nil then
  begin
    if Options.Text1<>'' then FForm.StaticText1.Caption:=Options.Text1;
    if Options.Text2<>'' then FForm.StaticText1.Caption:=Options.Text2;
    if Options.Text3<>'' then FForm.StaticText1.Caption:=Options.Text3;

    Options.Resume.AssignTo(FForm.BuButton2);
    Options.Exit.AssignTo(FForm.BuButton1);
  end;
end;
{***********************************************}


{ TJvConfirmCancelOptions }

{***********************************************}
constructor TJvConfirmCancelOptions.Create;
begin
  FExit:=TJvButtonPersistent.Create;
  FExit.Caption:='&Exit';
  FExit.OnChanged:=OptionsChanged;

  FResume:=TJvButtonPersistent.Create;
  FResume.Caption:='&Resume';
  FResume.OnChanged:=OptionsChanged;
end;
{***********************************************}
destructor TJvConfirmCancelOptions.Destroy;
begin
  FResume.Free;
  FExit.Free;
  inherited;
end;
{***********************************************}
procedure TJvConfirmCancelOptions.OptionsChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvConfirmCancelOptions.SetText1(const Value: string);
begin
  FText1 := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvConfirmCancelOptions.SetText2(const Value: string);
begin
  FText2 := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvConfirmCancelOptions.SetText3(const Value: string);
begin
  FText3 := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}

end.
