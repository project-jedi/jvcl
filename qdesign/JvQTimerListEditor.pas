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

The Original Code is: JvTimerListEditor.PAS, released on 2003-07-13

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Property editor for the TJvTimerList component

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQTimerListEditor;

{$I jvcl.inc}

interface

uses
  SysUtils, 
  DesignEditors, DesignIntf; 

type
  TJvTimerListDefaultEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index: Integer); override; 
    procedure EditProperty(const Prop: IProperty; var Cont: Boolean); override; 
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  JvQDsgnConsts;


procedure TJvTimerListDefaultEditor.EditProperty(const Prop: IProperty; var Cont: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if SameText(PropName, 'Events') then // do not localize
  begin
    Prop.Edit;
    Cont := False;
  end;
end;


procedure TJvTimerListDefaultEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

function TJvTimerListDefaultEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := RsEventsEllipsis
  else
    Result := '';
end;

function TJvTimerListDefaultEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
