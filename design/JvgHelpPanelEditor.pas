{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgHelpPanelEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgHelpPanelEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, comctrls
  {$IFDEF COMPILER6_UP}, DesignIntf, DesignWindows, DesignEditors{$ELSE}{$IFDEF COMPILER4_UP}, dsgnintf{$ENDIF}{$ENDIF};

type
  TJvgHelpPanelEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation
uses JvgHelpPanel, JvgRTFPreviewEditor;
{ TJvgHelpPanelEditor }

procedure TJvgHelpPanelEditor.ExecuteVerb(Index: Integer);
var
  OpenDialog: TOpenDialog;
  ms: TMemoryStream;
begin
  inherited;
  case Index of
    0:
      begin
        OpenDialog := TOpenDialog.Create(nil);
        OpenDialog.Filter := 'RTF and Text files (*.rtf,*.txt)|*.rtf;*.txt';
        if OpenDialog.Execute then
        begin
          (Component as TJvgHelpPanel).Strings.LoadFromFile(OpenDialog.FileName);
        end;
        OpenDialog.Free;
      end;
    1:
      begin
        try
          JvgRTFPreview := TJvgRTFPreview.Create(nil);
          ms := TMemoryStream.Create;
          try
            (Component as TJvgHelpPanel).Strings.SaveToStream(ms);
            ms.Position := 0;
            JvgRTFPreview.Rich.Lines.LoadFromStream(ms);
            JvgRTFPreview.ShowModal;
          finally
            ms.Free;
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
    0: Result := 'Load RTF file';
    1: Result := 'Preview RTF text';
  end;
end;

function TJvgHelpPanelEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
