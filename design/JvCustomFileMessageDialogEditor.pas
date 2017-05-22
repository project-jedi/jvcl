{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCommonDialogDEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCustomFileMessageDialogEditor;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  SysUtils,
  DesignEditors, DesignIntf,
  JvCustomFileMessageDialog;

type
  TJvCustomFileMessageDialogEditor = class(TDefaultEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  JvDsgnConsts;

procedure TJvCustomFileMessageDialogEditor.ExecuteVerb(Index: Integer);
begin
  with Component as TJvCustomFileMessageDialog do
    case Index of
      0:
        Execute;
    end;
end;

function TJvCustomFileMessageDialogEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsPreviewEllipsis;
  end;
end;

function TJvCustomFileMessageDialogEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
