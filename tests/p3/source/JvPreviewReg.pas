{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPreviewReg.PAS, released on 2003-01-01.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-01-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvPreviewReg;

interface
uses
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF};

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
  Classes, JvPrvwDoc, JvPrvwRender;
{$R ..\resources\JvPreviewReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Print Preview', [TJvPreviewControl,
    TJvPreviewRenderRichEdit, TJvPreviewRenderStrings,
      TJvPreviewRenderGraphics, TJvPreviewRenderControl, TJvPreviewPrinter]);
  RegisterComponentEditor(TJvCustomPreviewRenderer, TJvPreviewerEditor);
end;

type
  TJvHackCustomPreviewer = class(TJvCustomPreviewRenderer);

  { TJvPreviewerEditor }

procedure TJvPreviewerEditor.ExecuteVerb(Index: Integer);
var pv: TJvCustomPreviewControl;
begin
  case Index of
    0:
      TJvHackCustomPreviewer(Component).CreatePreview(false);
    1:
      begin
        pv := TJvHackCustomPreviewer(Component).PrintPreview;
        if pv <> nil then
          pv.Clear;
      end;
  end;
end;

function TJvPreviewerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Create Preview';
    1:
      Result := 'Clear Preview';
  end;
end;

function TJvPreviewerEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.

