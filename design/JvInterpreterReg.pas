{$I JVCL.INC}

unit JvInterpreterReg;

interface

procedure Register;

implementation
uses
  Classes, DesignIntf, 
  JvInterpreterParser, JvInterpreter, JvInterpreter_all, JvInterpreter_Classes, JvInterpreter_ComCtrls, JvInterpreter_Contnrs,
  JvInterpreter_Controls, JvInterpreter_Db, JvInterpreter_DbCtrls, JvInterpreter_DbGrids, JvInterpreter_DBTables,
  JvInterpreter_Dialogs, JvInterpreter_ExtCtrls, JvInterpreter_Forms, JvInterpreter_Graphics, JvInterpreter_Grids,
  JvInterpreter_httpapp, JvInterpreter_JvEditor, JvInterpreter_JvInterpreter, JvInterpreter_JvRegAuto, JvInterpreter_JvUtils,
  JvInterpreter_Menus, JvInterpreter_Quickrpt, JvInterpreter_StdCtrls, JvInterpreter_System, JvInterpreter_SysUtils,
  JvInterpreter_Types, JvInterpreter_Windows, JvInterpreterConst, JvInterpreterFm, JvDsgnEditors;

{$R ..\resources\JvInterpreterReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Interpreter',[TJvInterpreterProgram,TJvInterpreterFm]);
  {.$DEFINE JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  {.$DEFINE JvInterpreter_INTEGERPROPERTY}
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  {$IFDEF JvInterpreter_INTEGERPROPERTY}
  RegisterPropertyEditor(TypeInfo(Integer), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Cardinal), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Longint), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Smallint), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Shortint), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Word), TObject, '', TJvIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Byte), TObject, '', TJvIntegerProperty);
  {$ENDIF JvInterpreter_INTEGERPROPERTY}
  {$ENDIF}
end;

end.
