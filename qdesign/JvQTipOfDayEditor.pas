{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTipProperty.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

{ Property editor for the TJvTipOfDay component }

unit JvQTipOfDayEditor;

interface

uses
  SysUtils,
  
  DesignEditors, DesignIntf;
  

type
  { a component editor that shows the "Tip of the Day" dialog }
  TJvTipOfDayEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  JvQTipOfDay, JvQDsgnConsts;

procedure TJvTipOfDayEditor.Edit;
var
  SavedOptions: TJvTipOfDayOptions;
begin
  
  with GetComponent as TJvTipOfDay do
  begin
  
      SavedOptions := Options;
      Options := [toShowOnStartUp];
      try
        Execute;
      finally
        Options := SavedOptions;
      end;
    end;
end;

procedure TJvTipOfDayEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    Edit
  else
    inherited ExecuteVerb(Index);
end;

function TJvTipOfDayEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := RsPreviewEllipsis
  else
    Result := '';
end;

function TJvTipOfDayEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.

