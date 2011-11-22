unit MainForm;
{ NOTE: to run this demo, compile the packages MyLabelPackage.dpk and JvInterpreter_MyLabelPackage.dpk (in this folder)
  and move the generated bpl/dcp files to the demos output folder (this is by default the jvcl\bin folder) }

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
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
{$IFDEF COMPILER6}
  PackageFileName    = 'JvInterpreterD6R.bpl';
  ALLPackageFileName = 'JvInterpreterD6R.bpl';
{$ENDIF COMPILER6}
{$IFDEF COMPILER7}
  PackageFileName    = 'JvInterpreterD7R.bpl';
  ALLPackageFileName = 'JvInterpreterD7R.bpl';
{$ENDIF COMPILER7}
{$IFDEF COMPILER9}
  PackageFileName    = 'JvInterpreterD9R.bpl';
  ALLPackageFileName = 'JvInterpreterD9R.bpl';
{$ENDIF COMPILER9}
{$IFDEF COMPILER10}
  PackageFileName    = 'JvInterpreterD10R.bpl';
  ALLPackageFileName = 'JvInterpreterD10R.bpl';
{$ENDIF COMPILER10}
{$IFDEF COMPILER11}
  PackageFileName    = 'JvInterpreterD11R.bpl';
  ALLPackageFileName = 'JvInterpreterD11R.bpl';
{$ENDIF COMPILER11}
{$IFDEF COMPILER12}
  PackageFileName    = 'JvInterpreterD12R.bpl';
  ALLPackageFileName = 'JvInterpreterD12R.bpl';
{$ENDIF COMPILER12}
{$IFDEF COMPILER14}
  PackageFileName    = 'JvInterpreterD14R.bpl';
  ALLPackageFileName = 'JvInterpreterD14R.bpl';
{$ENDIF COMPILER14}
{$IFDEF COMPILER15}
  PackageFileName    = 'JvInterpreterD15R.bpl';
  ALLPackageFileName = 'JvInterpreterD15R.bpl';
{$ENDIF COMPILER15}
{$IFDEF COMPILER16}
  PackageFileName    = 'JvInterpreterD16R.bpl';
  ALLPackageFileName = 'JvInterpreterD16R.bpl';
{$ENDIF COMPILER16}

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
    raise Exception.CreateFmt('Package %s couldn''t be loaded!', [ExtractFileName(PackageFileName)]);
  end;

  try
    ALLPak := LoadPackage(ALLPackageFileName);
  except
    raise Exception.CreateFmt('Package %s couldn''t be loaded!', [ExtractFileName(AllPackageFileName)]);
  end;

  try
     Proc := GetProcAddress(ALLPak, ('@JvInterpreter_all@initialization$qqrv'));

     if not Assigned(Proc) then
        raise Exception.CreateFmt('%s Initialization section not found!', [ChangeFileExt(ExtractFileName(AllPackageFileName),'')]);

     Proc;

     Proc := GetProcAddress(Pak, ('@JvInterpreterFm@JvInterpreterRunFormModal$qqrx17System@AnsiString'));
     if not Assigned(Proc) then
        raise Exception.CreateFmt('Function %s not found!', ['RunFormModal']);

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
    raise Exception.CreateFmt('Package %s couldn''t be loaded!', [ExtractFileName(PackageFileName)]);
  end;
   FuncName := '@' + UnitName + '@initialization$qqrv';
   Proc := GetProcAddress(Result, PChar(FuncName));

   if not Assigned(Proc) then
      raise Exception.CreateFmt('Function %s.Initialization not found!', [FuncName]);

   Proc;
end;

procedure TMain.RunDynamicClick(Sender: TObject);
var
  PackageFileName: String;
  MyLabelPackage: HModule;
begin
  PackageFileName := ExtractFilePath(Application.ExeName) + 'MyLabelPackage.bpl';
  MyLabelPackage := 0;
  try
    try
      MyLabelPackage := LoadJvInterpreterPackage(PackageFileName, 'Mylabel');
    except
      raise Exception.CreateFmt('Package %s couldn''t be loaded!', [ExtractFileName(PackageFileName)]);
    end;
    DynamicJvInterpreterRunFormModal(ExtractFilePath(Application.ExeName) + 'ScriptForm.pas');
  finally
    FreeLibrary(MyLabelPackage);
  end;
end;

initialization
end.
