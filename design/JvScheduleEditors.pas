{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScheduleEditors.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are:

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvScheduleEditors;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  DesignIntf, DesignEditors,
  JvScheduleEditorForm, JvScheduledEvents;

type
  TJvScheduleProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TJvSchedEventEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  ColnEdit, Controls,
  JclSchedule,
  JvConsts, JvDsgnConsts;


//=== { TJvScheduleProperty } ================================================

procedure TJvScheduleProperty.Edit;
begin
  with TfrmScheduleEditor.Create(nil) do
    try
      Schedule := IJclSchedule({TEventSchedule(} GetOrdValue) {.Schedule};
      if ShowModal = mrOk then
        Self.Modified;
    finally
      Free;
    end;
end;

function TJvScheduleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvScheduleProperty.GetValue: string;
begin
  Result := '(IJclSchedule)';
end;

//=== { TJvSchedEventEditor } ================================================

procedure TJvSchedEventEditor.ExecuteVerb(Index: Integer);
begin
  ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
    TJvCustomScheduledEvents(Component).Events, 'Events');
end;

function TJvSchedEventEditor.GetVerb(Index: Integer): string;
begin
  Result := RsEventEditor;
end;

function TJvSchedEventEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.