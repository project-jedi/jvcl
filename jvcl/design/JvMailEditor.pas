{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMailEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvMailEditor;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, SysUtils, Classes, Dlgs, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvMail;
  
type
  TJvMailEditor = class(TComponentEditor)
  private
    procedure Address;
    procedure SendMail;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  JvDsgnConsts;

procedure TJvMailEditor.Address;
begin
  with Component as TJvMail do
    try
      Address(Owner.Name + '.' + Name);
    finally
      FreeSimpleMapi;
    end;
end;

procedure TJvMailEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      SendMail;
    1:
      Address;
  end;
end;

function TJvMailEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsSend;
    1:
      Result := RsAddress;
  end;
end;

function TJvMailEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TJvMailEditor.SendMail;
begin
  with Component as TJvMail do
    try
      SendMail;
    finally
      FreeSimpleMapi;
    end;
end;

end.
