{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBaseDsgnForm.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBaseDsgnForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, 
  {$ENDIF VCL}
  Graphics, Controls, Forms, Dialogs,
  JvComponent;

type
  TJvBaseDesign = class(TJvForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  {$IFDEF VCL}
  private
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
  {$ENDIF VCL}
  protected
    {$IFDEF VisualCLX}
    procedure ShowingChanged; override;
    {$ENDIF VisualCLX}
    { Determines the key to write the settings to or read from. Generally you don't need to override
      this method.
      Default will return (DELPHIRootKey)\Property Editors\(DesignerFormName)\(ClassName), where
        (DELPHIRootKey) is the root registry key for this Delphi version,
        (DesignerFormName) is the return value of the DesignerFormName function,
        (ClassName) is the return value of ClassName. }
    function GetRegKey: string; dynamic;
    { Editor name. Defaults to 'JEDI-VCL Editor' but should be renamed to an appropiate editor type
      name (e.g. 'Provider Editor' or 'Form Storage Editor'). }
    { asn: Linux defaults to ~/.borland/.Jvcl3 }
    function DesignerFormName: string; dynamic;
    { Determines if the settings for this class should be automatically stored/restored upon class
      destruction/streaming back in. Defaults to False. }
    function AutoStoreSettings: Boolean; dynamic;
    { Store the settings for this form. Descendants that want to store additional settings should
      override this method. You should always call the inherited method (which stores the position
      and size information). }
    procedure StoreSettings; dynamic;
    { Restore the settings for this form. Descendants that want to restore additional settings
      should override this method. You should always call the inherited method (which restores the
      position and size information). }
    procedure RestoreSettings; dynamic;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TCompareDsgFunc = function(DsgnForm: TJvBaseDesign; const Args: array of const): Boolean;

function GetDesignerForm(CompareFunc: TCompareDsgFunc; const Args: array of const): TJvBaseDesign;

implementation

uses
  {$IFDEF MSWINDOWS}
  Registry,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  JvQRegistryIniFile,
  {$ENDIF UNIX}
  JvBaseDsgnFrame, JvConsts, JvDsgnConsts;

{$R *.dfm}

const
  cHeight = 'Height';
  cWidth = 'Width';
  cLeft = 'Left';
  cTop = 'Top';

var
  DsgnFrmList: TList = nil;

function GetDesignerForm(CompareFunc: TCompareDsgFunc; const Args: array of const): TJvBaseDesign;
var
  I: Integer;
begin
  Result := nil;
  if (DsgnFrmList <> nil) and Assigned(CompareFunc) then
  begin
    I := DsgnFrmList.Count - 1;
    while (I >= 0) and not CompareFunc(TJvBaseDesign(DsgnFrmList[I]), Args) do
      Dec(I);
    if I >= 0 then
      Result := TJvBaseDesign(DsgnFrmList[I]);
  end
end;

{$IFDEF VCL}
procedure TJvBaseDesign.CMShowingChanged(var Msg: TMessage);
begin
  inherited;
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvBaseDesign.ShowingChanged;
begin
  inherited ShowingChanged;
{$ENDIF VisualCLX}
  if not (csDesigning in ComponentState) and AutoStoreSettings then
  try
    if Showing then
      RestoreSettings
    else
      StoreSettings;
  except
    Application.HandleException(Self);
  end;
end;

function TJvBaseDesign.GetRegKey: string;
begin
  Result := {$IFDEF VCL} SDelphiKey + {$ENDIF} RsPropertyEditors +
    RegPathDelim + Trim(DesignerFormName) + RegPathDelim + ClassName;
end;

function TJvBaseDesign.DesignerFormName: string;
begin
  Result := RsBaseDesignFormName;
end;

function TJvBaseDesign.AutoStoreSettings: Boolean;
begin
  Result := False;
end;

procedure TJvBaseDesign.StoreSettings;
var
  I: Integer;
begin
  {$IFDEF UNIX}
  with TJvRegistryIniFile.Create do
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  with TRegistry.Create do
  {$ENDIF MSWINDOWS}
    try
      LazyWrite := False;
      if OpenKey(GetRegKey, True) then
        try
          WriteInteger(cLeft, Left);
          WriteInteger(cTop, Top);
          WriteInteger(cWidth, Width);
          WriteInteger(cHeight, Height);
        finally
          CloseKey;
        end;
    finally
      Free;
    end;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TfmeJvBaseDesign then
      TfmeJvBaseDesign(Components[I]).StoreSettings;
end;

procedure TJvBaseDesign.RestoreSettings;
var
  I: Integer;
begin
  {$IFDEF UNIX}
  with TJvRegistryIniFile.Create do
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  with TRegistry.Create do
  {$ENDIF MSWINDOWS}
    try
      if OpenKey(GetRegKey, False) then
        try
          if ValueExists(cWidth) then
            Width := ReadInteger(cWidth);
          if ValueExists(cHeight) then
            Height := ReadInteger(cHeight);
          if ValueExists(cLeft) then
            Left := ReadInteger(cLeft);
          if ValueExists(cTop) then
            Top := ReadInteger(cTop);
        finally
          CloseKey;
        end;
    finally
      Free;
    end;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TfmeJvBaseDesign then
      TfmeJvBaseDesign(Components[I]).RestoreSettings;
end;

procedure TJvBaseDesign.AfterConstruction;
begin
  inherited AfterConstruction;
  if DsgnFrmList = nil then
    DsgnFrmList := TList.Create;
  if DsgnFrmList.IndexOf(Self) < 0 then
    DsgnFrmList.Add(Self);
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TJvBaseDesign.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if DsgnFrmList <> nil then
    DsgnFrmList.Remove(Self);
end;

procedure TJvBaseDesign.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

initialization

finalization
  FreeAndNil(DsgnFrmList);

end.

