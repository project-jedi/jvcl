{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHelpPanelEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgHelpPanelEditor;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignWindows, DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF COMPILER6_UP}

type
  TJvgHelpPanelEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  JvgHelpPanel, JvgRTFPreviewForm, JvDsgnConsts;

procedure TJvgHelpPanelEditor.ExecuteVerb(Index: Integer);
var
  OpenDialog: TOpenDialog;
  MemStream: TMemoryStream;
  JvgRTFPreview: TJvgRTFPreview;
begin
  // inherited ExecuteVerb(Index);
  case Index of
    0:
      begin
        OpenDialog := TOpenDialog.Create(nil);
        OpenDialog.Filter := RsRTFAndTextFilesrtftxtrtftxt;
        if OpenDialog.Execute then
          (Component as TJvgHelpPanel).Strings.LoadFromFile(OpenDialog.FileName);
        OpenDialog.Free;
      end;
    1:
      begin
        JvgRTFPreview := TJvgRTFPreview.Create(nil);
        try
          MemStream := TMemoryStream.Create;
          try
            (Component as TJvgHelpPanel).Strings.SaveToStream(MemStream);
            MemStream.Position := 0;
            JvgRTFPreview.Rich.Lines.LoadFromStream(MemStream);
            JvgRTFPreview.ShowModal;
          finally
            MemStream.Free;
          end;
        finally
          JvgRTFPreview.Free;
        end;
      end;
  end;
end;

function TJvgHelpPanelEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsLoadRTFFile;
    1:
      Result := RsPreviewRTFText;
  end;
end;

function TJvgHelpPanelEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.

