unit AppDdeCmdMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, JvDdeCmd, JvComponent;

type
  TMainForm = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    JvAppDdeCmd1: TJvAppDdeCmd;
    procedure PvAppDdeCmd1ExecParsedCmd(Sender: TObject;
      const Command: string; Parameters: TStrings);
    procedure Button1Click(Sender: TObject);
    procedure PvAppDdeCmd1BusyChanged(Sender: TObject; IsBusy: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses AppDdeCmdModal;

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PvAppDdeCmd1BusyChanged(nil, False);
end;

procedure TMainForm.PvAppDdeCmd1ExecParsedCmd(Sender: TObject;
  const Command: string; Parameters: TStrings);
var
  I: Integer;
  S: String;
begin
  with Memo1.Lines do
  begin
    Add(Format('Command: %s', [Command]));
    for I := 0 to Parameters.Count - 1 do
    begin
      S := #9 + Parameters.Strings[I];
      if Assigned(Parameters.Objects[I]) then S := S + ' <- Integer Value';
      Add(S);
    end;
    Add('');
  end;
end;

procedure TMainForm.PvAppDdeCmd1BusyChanged(Sender: TObject; IsBusy: Boolean);
const
  EnabledStr: array[Boolean] of String = ('Ready', 'Busy');
begin
  Label1.Caption := EnabledStr[IsBusy];
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Form2.ShowModal;
end;

end.
