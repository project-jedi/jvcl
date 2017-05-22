{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSchedEvntReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are:

Last Modified: 

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSchedEvntReg;

interface

uses
  SysUtils, Classes,
  JvScheduleEditorForm, JvScheduledEvents;

procedure Register;

implementation

uses
  ColnEdit, Controls,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF},
  JclSchedule, JvxDConst;

type
  TJvSchedulePropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TJvSchedEventComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

//=== TJvSchedulePropertyEditor ================================================

procedure TJvSchedulePropertyEditor.Edit;
begin
  with TfrmScheduleEditor.Create(nil) do
  try
    Schedule := IJclSchedule({TEventSchedule(}GetOrdValue){.Schedule};
    if ShowModal = mrOk then
      Self.Modified;
  finally
    Free;
  end;
end;

function TJvSchedulePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvSchedulePropertyEditor.GetValue: string;
begin
  Result := '(IJclSchedule)';
end;

//=== TJvSchedEventComponentEditor =============================================

procedure TJvSchedEventComponentEditor.ExecuteVerb(Index: Integer);
begin
  ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
    TJvCustomScheduledEvents(Component).Events, 'Events');
end;

function TJvSchedEventComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Event editor...';
end;

function TJvSchedEventComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure Register;
begin
  RegisterComponents(srJvAdditionalPalette, [TJvScheduledEvents]);
  RegisterPropertyEditor(TypeInfo(IJclSchedule), TJvEventCollectionItem, 'Schedule', TJvSchedulePropertyEditor);
  RegisterComponentEditor(TJvCustomScheduledEvents, TJvSchedEventComponentEditor);
end;

end.
