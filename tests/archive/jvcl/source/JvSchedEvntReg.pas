unit JvSchedEvntReg;

interface

{$I jvcl.inc}

uses
  SysUtils, Classes,
  JvScheduledEvents, ScheduleEditor;

procedure Register;

implementation

uses
  ColnEdit, Controls, {$IFDEF COMPILER6_UP}DesignIntf, DesignEditors{$ELSE}DsgnIntf{$ENDIF},  
  JclSchedule;

type
  TSchedulePropertyEditor = class(TPropertyEditor)
  private
  protected
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TSchedEventComponentEditor = class(TComponentEditor)
  private
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TSchedulePropertyEditor }

procedure TSchedulePropertyEditor.Edit;
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

function TSchedulePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TSchedulePropertyEditor.GetValue: string;
begin
  Result := '(IJclSchedule)';
end;

{ TSchedEventComponentEditor }

procedure TSchedEventComponentEditor.ExecuteVerb(Index: Integer);
begin
  ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
    TJvCustomScheduledEvents(Component).Events, 'Events');
end;

function TSchedEventComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Event editor...';
end;

function TSchedEventComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ Registration }

procedure Register;
begin
  RegisterComponents('Jv Additional', [TJvScheduledEvents]);
  RegisterPropertyEditor(TypeInfo(IJclSchedule), TJvEventCollectionItem, 'Schedule', TSchedulePropertyEditor);
  RegisterComponentEditor(TJvCustomScheduledEvents, TSchedEventComponentEditor);
end;

end.
