{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGroupHeaderEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvGroupHeaderEditor;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Forms, Graphics, ImgList, Dialogs, Controls,
  {$IFDEF COMPILER6_UP}
  VCLEditors, DesignIntf, DesignEditors, DesignMenus;
  {$ELSE}
  DsgnIntf;
  {$ENDIF COMPILER6_UP}

type
  TJvGroupHeaderEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

implementation

uses
  JvTypes, JvGroupHeader, JvDsgnConsts;

function TJvGroupHeaderEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TJvGroupHeaderEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsStandardFlat;
    1:
      Result := RsWeb;
  end;
end;

procedure TJvGroupHeaderEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      with TJvGroupHeader(Component) do
      begin
        BevelOptions.Style := bsLowered;
        Font.Style := [];
      end;
    1:
      with TJvGroupHeader(Component) do
      begin
        BevelOptions.Style := bsShape;
        BevelOptions.Brush.Color := $00A97A1B;
        BevelOptions.Pen.Color := $00E1AD40;
        BevelOptions.Height := 3;
        Font.Style := [fsBold];
      end;
  end;
end;

procedure TJvGroupHeaderEditor.Edit;
begin
  // We don't need to add band on double click
end;


end.
