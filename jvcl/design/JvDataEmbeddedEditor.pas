{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataEmbeddedEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvDataEmbeddedEditor;

interface

uses
  SysUtils, Classes, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDataEmbedded;

type
  TJvDataEmbeddedEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

implementation

uses
  JvDsgnConsts;

procedure TJvDataEmbeddedEditor.Edit;
var
  Stream: TFileStream;
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream(GetOrdValue);
  with TOpenDialog.Create(nil) do
    try
      Filter := RsAllFilesFilter;
      if Execute then
      begin
        Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
        try
          MemStream.Clear;
          MemStream.CopyFrom(Stream, 0);
          SetOrdValue(Integer(MemStream));
        finally
          Stream.Free;
        end;
      end;
    finally
      Free;
    end;
end;

function TJvDataEmbeddedEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paSortList];
end;

function TJvDataEmbeddedEditor.GetValue: string;
begin
  Result := RsJvEditorString;
end;

procedure TJvDataEmbeddedEditor.GetValues(Proc: TGetStrProc);
begin
  SetStrValue(RsJvEditorString);
end;

procedure TJvDataEmbeddedEditor.SetValue(const Value: string);
begin
  SetStrValue(RsJvEditorString);
end;

end.

