{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCommonDialogD.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCommonDialogD;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SetupApi, JvTypes, JvComponent;

type
  TJvCommonDialogD = class(TJvComponent)
  private
  protected
    FHandle: THandle;
    FDll: THandle;
    FTitle: string;
    // (rom) needs some TODO
    FSetupPromptForDisk: TSetupPromptForDisk;
    FSetupRenameError: TSetupRenameError;
    FSetupDeleteError: TSetupDeleteError;
    FSetupCopyError: TSetupCopyError;
  public
    function Execute: TDiskRes; virtual; abstract;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DialogTitle: string read FTitle write FTitle;
  end;

implementation

resourcestring
  RC_ErrorSetupDll = 'Unable to find setupapi.dll';
  RC_ErrorOwner = 'Owner must be of type TWinControl';

  {**************************************************}

constructor TJvCommonDialogD.Create(AOwner: TComponent);
begin
  inherited;
  if Aowner is TWinControl then
  begin
    FTitle := '';
    FHandle := (Aowner as TWinControl).Handle;
    FDll := LoadLibrary('setupapi.dll');
    if FDll <> 0 then
    begin
      FSetupPromptForDisk := GetProcAddress(FDll, 'SetupPromptForDiskA');
      FSetupCopyError := GetProcAddress(FDll, 'SetupCopyErrorA');
      FSetupDeleteError := GetProcAddress(FDll, 'SetupDeleteErrorA');
      FSetupRenameError := GetProcAddress(FDll, 'SetupRenameErrorA');
    end
    else
      raise EJVCLException.Create(RC_ErrorSetupDll);
  end
  else
    raise EJVCLException.Create(RC_ErrorOwner);
end;

{**************************************************}

destructor TJvCommonDialogD.Destroy;
begin
  if FDll <> 0 then
    FreeLibrary(FDll);
  inherited;
end;

end.
