{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppletProperty.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAppletProperty;

interface

uses
  Windows, Classes, Controls, Forms, Dialogs,
  {$IFNDEF COMPILER6_UP}
  DsgnIntf;
  {$ELSE}
  DesignIntf, DesignEditors;
  {$ENDIF}

type
  TJvAppletFileProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

procedure TJvAppletFileProperty.Edit;
var
  AppletFileOpen: TOpenDialog;
begin
  AppletFileOpen := TOpenDialog.Create(Application);
  AppletFileOpen.Filename := GetValue;
  AppletFileOpen.Filter := 'Applet File (*.cpl)|*.cpl';
  AppletFileOpen.Options := AppletFileOpen.Options + [ofPathMustExist, ofFileMustExist];
  try
    if AppletFileOpen.Execute then
      SetValue(AppletFileOpen.Filename);
  finally
    AppletFileOpen.Free;
  end;
end;

function TJvAppletFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
