unit MainForm;

{$INCLUDE JEDI.INC}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMain = class(TForm)
    RunDynamic: TButton;
    procedure RunDynamicClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

//uses JvInterpreter_StdCtrls, JvInterpreter_Classes;

{$R *.dfm}

const
{$IFDEF Delphi5}
  PackageFileName    = 'rai5.bpl';
  ALLPackageFileName = 'raia5.bpl';
{$ENDIF Delphi5}
{$IFDEF Delphi6}
  PackageFileName    = 'rai6.bpl';
  ALLPackageFileName = 'raia6.bpl';
{$ENDIF Delphi6}

procedure DynamicJvInterpreterRunFormModal(const FileName: TFileName);
var
  Pak, ALLPak: HModule;
  Proc     : procedure;
type
  TRunFormModal = function (const FileName: TFileName): TModalResult;
  TMakeForm     = function (const FileName: TFileName): TForm;
begin

  try
    Pak := LoadPackage(PackageFileName);
  except
    raise Exception.CreateFmt('Package %s konnte nicht geladen werden!', [ExtractFileName(PackageFileName)]);
  end;

  try
    ALLPak := LoadPackage(ALLPackageFileName);
  except
    raise Exception.CreateFmt('Package %s konnte nicht geladen werden!', [ExtractFileName(AllPackageFileName)]);
  end;

  try
     Proc := GetProcAddress(ALLPak, ('@JvInterpreter_all@initialization$qqrv'));

     if not Assigned(Proc) then
        raise Exception.CreateFmt('Funktion %s.Initialization nicht gefunden!', [ExtractFileName(AllPackageFileName)]);

     Proc;

     Proc := GetProcAddress(Pak, ('@JvInterpreterFm@JvInterpreterRunFormModal$qqrx17System@AnsiString'));
     if not Assigned(Proc) then
        raise Exception.CreateFmt('Funktion %s nicht gefunden!', ['RunFormModal']);

     TRunFormModal(Proc)(FileName);

 finally
     UnloadPackage(ALLPak);
     UnloadPackage(Pak);
  end;
end;

function LoadJvInterpreterPackage(const PackageFileName: TFileName; const UnitName: String): HModule;
var
  Proc: procedure;
  FuncName: String;
begin

  try
    Result := LoadPackage(PackageFileName);
  except
    raise Exception.CreateFmt('Package %s konnte nicht geladen werden!', [ExtractFileName(PackageFileName)]);
  end;
   FuncName := '@' + UnitName + '@initialization$qqrv';
   Proc := GetProcAddress(Result, PChar(FuncName));

   if not Assigned(Proc) then
      raise Exception.CreateFmt('Funktion %s.Initialization nicht gefunden!', [FuncName]);

   Proc;
end;

procedure TMain.RunDynamicClick(Sender: TObject);
var
  PackageFileName: String;
  JvInterpreter_MyLabelPackage: HModule;
begin
  PackageFileName := ExtractFilePath(Application.ExeName) + 'JvInterpreter_MyLabelPackage.bpl';
  JvInterpreter_MyLabelPackage := 0;
  try
    try
      JvInterpreter_MyLabelPackage := LoadJvInterpreterPackage(PackageFileName, 'JvInterpreter_mylabel');
    except
      raise Exception.CreateFmt('Package %s konnte nicht geladen werden!', [ExtractFileName(PackageFileName)]);
    end;
    DynamicJvInterpreterRunFormModal(ExtractFilePath(Application.ExeName) + 'ScriptForm.pas');
  finally
    FreeLibrary(JvInterpreter_MyLabelPackage);
  end;
end;

initialization
end.
