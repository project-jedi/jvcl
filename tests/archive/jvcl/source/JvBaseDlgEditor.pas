{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBaseDlgEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].
                Serhiy Perevoznyk [serge_perevoznyk@hotmail.com]

Last Modified: 2003-05-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBaseDlgEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  EditIntf, ToolIntf, TypInfo,
{$IFDEF COMPILER5}DsgnIntf, {$ENDIF}{$IFDEF COMPILER6_UP}DesignEditors, DesignIntf, {$ENDIF}
  ExptIntf,
  JvBaseDlg;

type
  TJvBaseDlgEditor = class(TDefaultEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

implementation

{*************************************************}

procedure TJvBaseDlgEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TJvBaseDlgEditor.ExecuteVerb(Index: Integer);
begin
  if Index <> 0 then
    Exit;
  if Component is TJvCommonDialog then
  begin
    TJvCommonDialog(Component).Execute;
  end
  else if Component is TJvCommonDialogP then
  begin
    TJvCommonDialogP(Component).Execute;
  end;
end;

{*************************************************}

function TJvBaseDlgEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Preview...';
  end;
end;

{*************************************************}

function TJvBaseDlgEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

