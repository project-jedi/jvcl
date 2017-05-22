{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPreviewReg.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3 att users dott sourceforge dott net] .
Portions created by Peter Th�rnqvist are Copyright (C) 2003 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPreviewReg;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  DesignIntf, DesignEditors;

type
  TJvPreviewerEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  Classes,
  JvDsgnConsts,
  JvPrvwDoc, JvPrvwRender;

{$R JvPreviewReg.dcr}

type
  TJvHackCustomPreviewer = class(TJvCustomPreviewRenderer);

procedure TJvPreviewerEditor.ExecuteVerb(Index: Integer);
var
  Pv: TJvCustomPreviewControl;
begin
  case Index of
    0:
      TJvHackCustomPreviewer(Component).CreatePreview(False);
    1:
      begin
        Pv := TJvHackCustomPreviewer(Component).PrintPreview;
        if Pv <> nil then
          Pv.Clear;
      end;
  end;
end;

function TJvPreviewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsCreatePreview;
    1:
      Result := RsClearPreview;
  end;
end;

function TJvPreviewerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure Register;
begin
  RegisterComponents(RsPalettePrintPreview, [TJvPreviewControl,
    TJvPreviewRenderRichEdit, TJvPreviewRenderJvRichEdit, TJvPreviewRenderStrings,
    TJvPreviewRenderGraphics, TJvPreviewRenderControl, TJvPreviewPrinter]);
  RegisterComponentEditor(TJvCustomPreviewRenderer, TJvPreviewerEditor);
end;

end.
