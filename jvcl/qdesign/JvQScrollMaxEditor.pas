{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScrollMaxEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQScrollMaxEditor;

interface

uses 
  DesignIntf, DesignEditors; 

type
  TJvScrollMaxEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

implementation

uses
  JvQScrollMax, JvQDsgnConsts;

function TJvScrollMaxEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
end;

function TJvScrollMaxEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := RsAddBand
  else
    Result := inherited GetVerb(Index);
end;

procedure TJvScrollMaxEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1 then
    Designer.CreateComponent(TJvScrollMaxBand, Component, 0, 0, 0, 50)
  else
    inherited ExecuteVerb(Index);
end;

procedure TJvScrollMaxEditor.Edit;
begin
  // We don't need to add band on double click
end;


end.
