{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressComponentEditor.PAS, released on 2006-04-14.

The Initial Developer of the Original Code is Olivier Sannier.
Portions created by Olivier Sannier are Copyright (C) 2006 Olivier Sannier.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvProgressComponentEditor;

{$I jvcl.inc}

interface

uses
  DesignEditors, DesignIntf,
  {$IFNDEF COMPILER7_UP}
  EditIntf, ToolIntf, ExptIntf,
  {$ENDIF !COMPILER7_UP}
  JvBaseDlg;

type
  TJvProgressComponentEditor = class(TDefaultEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

implementation

uses
  JvDsgnConsts, JvProgressComponent;

procedure TJvProgressComponentEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TJvProgressComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index <> 0 then
    Exit;
  if Component is TJvProgressComponent then
    TJvProgressComponent(Component).Execute
  else
    Exit;

  { Some properties might have changed }
  Designer.Modified;
end;

function TJvProgressComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsPreviewEllipsis;
  end;
end;

function TJvProgressComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.